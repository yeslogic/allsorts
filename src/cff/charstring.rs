use std::convert::TryFrom;
use std::fmt;

use rustc_hash::FxHashSet;

use crate::binary::read::{ReadCtxt, ReadScope};
use crate::binary::{I16Be, U8};
use crate::error::ParseError;
use crate::tables::Fixed;

use super::{CFFVariant, Charset, Font, MaybeOwnedIndex, STANDARD_ENCODING};

mod argstack;

pub use argstack::ArgumentsStack;

// Limits according to the Adobe Technical Note #5177 Appendix B.
pub(crate) const STACK_LIMIT: u8 = 10;
pub(crate) const MAX_ARGUMENTS_STACK_LEN: usize = 48;

pub(crate) const TWO_BYTE_OPERATOR_MARK: u8 = 12;

pub(crate) trait IsEven {
    fn is_even(&self) -> bool;
    fn is_odd(&self) -> bool;
}

/// Just like TryFrom<N>, but for numeric types not supported by the Rust's std.
pub(crate) trait TryNumFrom<T>: Sized {
    /// Casts between numeric types.
    fn try_num_from(_: T) -> Option<Self>;
}

pub(crate) type GlyphId = u16;

/// A list of errors that can occur during a CFF CharString interpreting.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum CFFError {
    ParseError(ParseError),
    InvalidOperator,
    UnsupportedOperator,
    MissingEndChar,
    DataAfterEndChar,
    NestingLimitReached,
    ArgumentsStackLimitReached,
    InvalidArgumentsStackLength,
    BboxOverflow,
    MissingMoveTo,
    InvalidSubroutineIndex,
    NoLocalSubroutines,
    InvalidSeacCode,
}

struct CharStringParserContext1<'a, 'f> {
    font: &'f Font<'a>,
    global_subr_index: &'f MaybeOwnedIndex<'a>,
    width_parsed: bool,
    stems_len: u32,
    has_endchar: bool,
    has_seac: bool,
    glyph_id: GlyphId, // Required to parse local subroutine in CID fonts.
    local_subrs: Option<&'f MaybeOwnedIndex<'a>>,
    global_subr_used: FxHashSet<usize>,
    local_subr_used: Vec<usize>,
}

pub(crate) struct UsedSubrs {
    pub(crate) global_subr_used: FxHashSet<usize>,
    pub(crate) local_subr_used: Vec<usize>,
}

pub(crate) fn char_string_used_subrs<'a, 'f>(
    font: &'f Font<'a>,
    global_subr_index: &'f MaybeOwnedIndex<'a>,
    char_string: &'f [u8],
    glyph_id: GlyphId,
) -> Result<UsedSubrs, CFFError> {
    let local_subrs = match &font.data {
        CFFVariant::CID(_) => None, // Will be resolved on request.
        CFFVariant::Type1(type1) => type1.local_subr_index.as_ref(),
    };

    let mut ctx = CharStringParserContext1 {
        font,
        global_subr_index,
        width_parsed: false,
        stems_len: 0,
        has_endchar: false,
        has_seac: false,
        glyph_id,
        local_subrs,
        global_subr_used: FxHashSet::default(),
        local_subr_used: Vec::new(),
    };

    let mut stack = ArgumentsStack {
        data: &mut [0.0; MAX_ARGUMENTS_STACK_LEN], // 4b * 48 = 192b
        len: 0,
        max_len: MAX_ARGUMENTS_STACK_LEN,
    };
    scan_used_subrs(&mut ctx, char_string, 0, &mut stack)?;

    if !ctx.has_endchar {
        return Err(CFFError::MissingEndChar);
    }

    Ok(UsedSubrs {
        global_subr_used: ctx.global_subr_used,
        local_subr_used: ctx.local_subr_used,
    })
}

fn scan_used_subrs<'a, 'f>(
    ctx: &mut CharStringParserContext1<'a, 'f>,
    char_string: &[u8],
    depth: u8,
    // TODO: replace this with just the stack since I think that's all we actually need
    stack: &mut ArgumentsStack<'_>,
) -> Result<(), CFFError> {
    let mut s = ReadScope::new(char_string).ctxt();
    while s.bytes_available() {
        let op = s.read::<U8>()?;
        match op {
            0 | 2 | 9 | 13 | 15 | 16 | 17 => {
                // Reserved.
                return Err(CFFError::InvalidOperator);
            }
            operator::HORIZONTAL_STEM
            | operator::VERTICAL_STEM
            | operator::HORIZONTAL_STEM_HINT_MASK
            | operator::VERTICAL_STEM_HINT_MASK => {
                // If the stack length is uneven, then the first value is a `width`.
                let len = if stack.len().is_odd() && !ctx.width_parsed {
                    ctx.width_parsed = true;
                    stack.len() - 1
                } else {
                    stack.len()
                };

                ctx.stems_len += len as u32 >> 1;

                // We are ignoring the hint operators.
                stack.clear();
            }
            operator::VERTICAL_MOVE_TO => {
                if stack.len() == 2 && !ctx.width_parsed {
                    ctx.width_parsed = true;
                }
                stack.clear();
            }
            operator::LINE_TO
            | operator::HORIZONTAL_LINE_TO
            | operator::VERTICAL_LINE_TO
            | operator::CURVE_TO => {
                stack.clear();
            }
            operator::CALL_LOCAL_SUBROUTINE => {
                if stack.is_empty() {
                    return Err(CFFError::InvalidArgumentsStackLength);
                }

                if depth == STACK_LIMIT {
                    return Err(CFFError::NestingLimitReached);
                }

                // Parse and remember the local subroutine for the current glyph.
                // Since it's a pretty complex task, we're doing it only when
                // a local subroutine is actually requested by the glyphs charstring.
                if ctx.local_subrs.is_none() {
                    if let CFFVariant::CID(ref cid) = ctx.font.data {
                        // Choose the local subroutine index corresponding to the glyph/CID
                        ctx.local_subrs = cid.fd_select.font_dict_index(ctx.glyph_id).and_then(
                            |font_dict_index| match cid
                                .local_subr_indices
                                .get(usize::from(font_dict_index))
                            {
                                Some(Some(index)) => Some(index),
                                _ => None,
                            },
                        );
                    }
                }

                if let Some(local_subrs) = ctx.local_subrs {
                    let subroutine_bias = calc_subroutine_bias(local_subrs.len());
                    let index = conv_subroutine_index(stack.pop(), subroutine_bias)?;
                    let char_string = local_subrs
                        .read_object(index)
                        .ok_or(CFFError::InvalidSubroutineIndex)?;
                    ctx.local_subr_used.push(index);
                    scan_used_subrs(ctx, char_string, depth + 1, stack)?;
                } else {
                    return Err(CFFError::NoLocalSubroutines);
                }

                if ctx.has_endchar && !ctx.has_seac {
                    if s.bytes_available() {
                        return Err(CFFError::DataAfterEndChar);
                    }

                    break;
                }
            }
            operator::RETURN => {
                break;
            }
            TWO_BYTE_OPERATOR_MARK => {
                // flex
                let op2 = s.read::<U8>()?;
                match op2 {
                    operator::HFLEX | operator::FLEX | operator::HFLEX1 | operator::FLEX1 => {
                        stack.clear()
                    }
                    _ => return Err(CFFError::UnsupportedOperator), // FIXME(wm) can we avoid this
                }
            }
            operator::ENDCHAR => {
                if stack.len() == 4 || (!ctx.width_parsed && stack.len() == 5) {
                    // Process 'seac'.
                    let accent_char = seac_code_to_glyph_id(&ctx.font.charset, stack.pop())
                        .ok_or(CFFError::InvalidSeacCode)?;
                    let base_char = seac_code_to_glyph_id(&ctx.font.charset, stack.pop())
                        .ok_or(CFFError::InvalidSeacCode)?;
                    let _dy = stack.pop();
                    let _dx = stack.pop();

                    if !ctx.width_parsed {
                        stack.pop();
                        ctx.width_parsed = true;
                    }

                    ctx.has_seac = true;

                    let base_char_string = ctx
                        .font
                        .char_strings_index
                        .read_object(usize::from(base_char))
                        .ok_or(CFFError::InvalidSeacCode)?;
                    scan_used_subrs(ctx, base_char_string, depth + 1, stack)?;

                    let accent_char_string = ctx
                        .font
                        .char_strings_index
                        .read_object(usize::from(accent_char))
                        .ok_or(CFFError::InvalidSeacCode)?;
                    scan_used_subrs(ctx, accent_char_string, depth + 1, stack)?;
                } else if stack.len() == 1 && !ctx.width_parsed {
                    stack.pop();
                    ctx.width_parsed = true;
                }

                if s.bytes_available() {
                    return Err(CFFError::DataAfterEndChar);
                }

                ctx.has_endchar = true;

                break;
            }
            operator::HINT_MASK | operator::COUNTER_MASK => {
                let mut len = stack.len();

                // We are ignoring the hint operators.
                stack.clear();

                // If the stack length is uneven, than the first value is a `width`.
                if len.is_odd() && !ctx.width_parsed {
                    len -= 1;
                    ctx.width_parsed = true;
                }

                ctx.stems_len += len as u32 >> 1;

                // Skip the hints
                let _ = s
                    .read_slice(
                        usize::try_from((ctx.stems_len + 7) >> 3)
                            .map_err(|_| ParseError::BadValue)?,
                    )
                    .map_err(|_| ParseError::BadOffset)?;
            }
            operator::MOVE_TO => {
                if stack.len() == 3 && !ctx.width_parsed {
                    ctx.width_parsed = true;
                }
                stack.clear();
            }
            operator::HORIZONTAL_MOVE_TO => {
                if stack.len() == 2 && !ctx.width_parsed {
                    ctx.width_parsed = true;
                }
                stack.clear();
            }
            operator::CURVE_LINE
            | operator::LINE_CURVE
            | operator::VV_CURVE_TO
            | operator::HH_CURVE_TO
            | operator::VH_CURVE_TO
            | operator::HV_CURVE_TO => {
                stack.clear();
            }
            operator::SHORT_INT => {
                let n = s.read::<I16Be>()?;
                stack.push(f32::from(n))?;
            }
            operator::CALL_GLOBAL_SUBROUTINE => {
                if stack.is_empty() {
                    return Err(CFFError::InvalidArgumentsStackLength);
                }

                if depth == STACK_LIMIT {
                    return Err(CFFError::NestingLimitReached);
                }

                let subroutine_bias = calc_subroutine_bias(ctx.global_subr_index.len());
                let index = conv_subroutine_index(stack.pop(), subroutine_bias)?;
                ctx.global_subr_used.insert(index);
                let char_string = ctx
                    .global_subr_index
                    .read_object(index)
                    .ok_or(CFFError::InvalidSubroutineIndex)?;
                scan_used_subrs(ctx, char_string, depth + 1, stack)?;

                if ctx.has_endchar && !ctx.has_seac {
                    if s.bytes_available() {
                        return Err(CFFError::DataAfterEndChar);
                    }

                    break;
                }
            }
            32..=246 => {
                stack.push(crate::cff::charstring::parse_int1(op)?)?;
            }
            247..=250 => {
                stack.push(crate::cff::charstring::parse_int2(op, &mut s)?)?;
            }
            251..=254 => {
                stack.push(crate::cff::charstring::parse_int3(op, &mut s)?)?;
            }
            operator::FIXED_16_16 => {
                stack.push(crate::cff::charstring::parse_fixed(&mut s)?)?;
            }
        }
    }

    Ok(())
}

// CharString number parsing functions
pub fn parse_int1(op: u8) -> Result<f32, CFFError> {
    let n = i16::from(op) - 139;
    Ok(f32::from(n))
}

pub fn parse_int2(op: u8, s: &mut ReadCtxt<'_>) -> Result<f32, CFFError> {
    let b1 = s.read::<U8>()?;
    let n = (i16::from(op) - 247) * 256 + i16::from(b1) + 108;
    debug_assert!((108..=1131).contains(&n));
    Ok(f32::from(n))
}

pub fn parse_int3(op: u8, s: &mut ReadCtxt<'_>) -> Result<f32, CFFError> {
    let b1 = s.read::<U8>()?;
    let n = -(i16::from(op) - 251) * 256 - i16::from(b1) - 108;
    debug_assert!((-1131..=-108).contains(&n));
    Ok(f32::from(n))
}

pub fn parse_fixed(s: &mut ReadCtxt<'_>) -> Result<f32, CFFError> {
    let n = s.read::<Fixed>()?;
    Ok(f32::from(n))
}

// Conversions from biased subr index operands to unbiased value
pub(crate) fn conv_subroutine_index(index: f32, bias: u16) -> Result<usize, CFFError> {
    conv_subroutine_index_impl(index, bias).ok_or(CFFError::InvalidSubroutineIndex)
}

pub(crate) fn conv_subroutine_index_impl(index: f32, bias: u16) -> Option<usize> {
    let index = i32::try_num_from(index)?;
    let bias = i32::from(bias);

    let index = index.checked_add(bias)?;
    usize::try_from(index).ok()
}

// Adobe Technical Note #5176, Chapter 16 "Local / Global Subrs INDEXes"
pub(crate) fn calc_subroutine_bias(len: usize) -> u16 {
    if len < 1240 {
        107
    } else if len < 33900 {
        1131
    } else {
        32768
    }
}

// seac = standard encoding accented character, makes an accented character from two other
// characters.
pub(crate) fn seac_code_to_glyph_id(charset: &Charset<'_>, n: f32) -> Option<GlyphId> {
    let code = u8::try_num_from(n)?;

    let sid = STANDARD_ENCODING[usize::from(code)];

    match charset {
        Charset::ISOAdobe => {
            // ISO Adobe charset only defines string ids up to 228 (zcaron)
            if code <= 228 {
                Some(u16::from(sid))
            } else {
                None
            }
        }
        Charset::Expert | Charset::ExpertSubset => None,
        Charset::Custom(_) => charset.sid_to_gid(u16::from(sid)),
    }
}

impl IsEven for usize {
    fn is_even(&self) -> bool {
        (*self) & 1 == 0
    }

    fn is_odd(&self) -> bool {
        !self.is_even()
    }
}

impl TryNumFrom<f32> for u8 {
    fn try_num_from(v: f32) -> Option<Self> {
        i32::try_num_from(v).and_then(|v| u8::try_from(v).ok())
    }
}

impl TryNumFrom<f32> for i16 {
    fn try_num_from(v: f32) -> Option<Self> {
        i32::try_num_from(v).and_then(|v| i16::try_from(v).ok())
    }
}

impl TryNumFrom<f32> for u16 {
    fn try_num_from(v: f32) -> Option<Self> {
        i32::try_num_from(v).and_then(|v| u16::try_from(v).ok())
    }
}

impl TryNumFrom<f32> for i32 {
    fn try_num_from(v: f32) -> Option<Self> {
        // Based on https://github.com/rust-num/num-traits/blob/master/src/cast.rs

        // Float as int truncates toward zero, so we want to allow values
        // in the exclusive range `(MIN-1, MAX+1)`.

        // We can't represent `MIN-1` exactly, but there's no fractional part
        // at this magnitude, so we can just use a `MIN` inclusive boundary.
        const MIN: f32 = core::i32::MIN as f32;
        // We can't represent `MAX` exactly, but it will round up to exactly
        // `MAX+1` (a power of two) when we cast it.
        const MAX_P1: f32 = core::i32::MAX as f32;
        if v >= MIN && v < MAX_P1 {
            Some(v as i32)
        } else {
            None
        }
    }
}

impl From<ParseError> for CFFError {
    fn from(error: ParseError) -> CFFError {
        CFFError::ParseError(error)
    }
}

impl fmt::Display for CFFError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CFFError::ParseError(parse_error) => {
                write!(f, "parse error: ")?;
                parse_error.fmt(f)
            }
            CFFError::InvalidOperator => write!(f, "an invalid operator occurred"),
            CFFError::UnsupportedOperator => write!(f, "an unsupported operator occurred"),
            CFFError::MissingEndChar => write!(f, "the 'endchar' operator is missing"),
            CFFError::DataAfterEndChar => write!(f, "unused data left after 'endchar' operator"),
            CFFError::NestingLimitReached => write!(f, "subroutines nesting limit reached"),
            CFFError::ArgumentsStackLimitReached => write!(f, "arguments stack limit reached"),
            CFFError::InvalidArgumentsStackLength => {
                write!(f, "an invalid amount of items are in an arguments stack")
            }
            CFFError::BboxOverflow => write!(f, "outline's bounding box is too large"),
            CFFError::MissingMoveTo => write!(f, "missing moveto operator"),
            CFFError::InvalidSubroutineIndex => write!(f, "an invalid subroutine index"),
            CFFError::NoLocalSubroutines => write!(f, "no local subroutines"),
            CFFError::InvalidSeacCode => write!(f, "invalid seac code"),
        }
    }
}

impl std::error::Error for CFFError {}

/// Operators defined in Adobe Technical Note #5177, The Type  2 Charstring Format.
pub(crate) mod operator {
    pub const HORIZONTAL_STEM: u8 = 1;
    pub const VERTICAL_STEM: u8 = 3;
    pub const VERTICAL_MOVE_TO: u8 = 4;
    pub const LINE_TO: u8 = 5;
    pub const HORIZONTAL_LINE_TO: u8 = 6;
    pub const VERTICAL_LINE_TO: u8 = 7;
    pub const CURVE_TO: u8 = 8;
    pub const CALL_LOCAL_SUBROUTINE: u8 = 10;
    pub const RETURN: u8 = 11;
    pub const ENDCHAR: u8 = 14;
    pub const HORIZONTAL_STEM_HINT_MASK: u8 = 18;
    pub const HINT_MASK: u8 = 19;
    pub const COUNTER_MASK: u8 = 20;
    pub const MOVE_TO: u8 = 21;
    pub const HORIZONTAL_MOVE_TO: u8 = 22;
    pub const VERTICAL_STEM_HINT_MASK: u8 = 23;
    pub const CURVE_LINE: u8 = 24;
    pub const LINE_CURVE: u8 = 25;
    pub const VV_CURVE_TO: u8 = 26;
    pub const HH_CURVE_TO: u8 = 27;
    pub const SHORT_INT: u8 = 28;
    pub const CALL_GLOBAL_SUBROUTINE: u8 = 29;
    pub const VH_CURVE_TO: u8 = 30;
    pub const HV_CURVE_TO: u8 = 31;
    pub const HFLEX: u8 = 34;
    pub const FLEX: u8 = 35;
    pub const HFLEX1: u8 = 36;
    pub const FLEX1: u8 = 37;
    pub const FIXED_16_16: u8 = 255;
}
