// Portions of this file derived from ttf-parser, licenced under Apache-2.0.
// https://github.com/RazrFalcon/ttf-parser/tree/439aaaebd50eb8aed66302e3c1b51fae047f85b2

use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::fmt;

use pathfinder_geometry::line_segment::LineSegment2F;
use pathfinder_geometry::rect::RectI;
use pathfinder_geometry::vector::{vec2f, vec2i, Vector2F, Vector2I};

use crate::binary::read::ReadScope;
use crate::binary::{I16Be, U8};
use crate::cff::outline::argstack::ArgumentsStack;
use crate::cff::{CFFVariant, Charset, Font, MaybeOwnedIndex, CFF, STANDARD_ENCODING};
use crate::error::ParseError;
use crate::outline::{OutlineBuilder, OutlineSink};
use charstring::CharStringParser;

mod argstack;
mod charstring;

// Limits according to the Adobe Technical Note #5177 Appendix B.
const STACK_LIMIT: u8 = 10;
const MAX_ARGUMENTS_STACK_LEN: usize = 48;

const TWO_BYTE_OPERATOR_MARK: u8 = 12;

type GlyphId = u16;

trait IsEven {
    fn is_even(&self) -> bool;
    fn is_odd(&self) -> bool;
}

/// Just like TryFrom<N>, but for numeric types not supported by the Rust's std.
trait TryNumFrom<T>: Sized {
    /// Casts between numeric types.
    fn try_num_from(_: T) -> Option<Self>;
}

/// A list of errors that can occur during a CFF outline parsing.
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

pub(crate) struct Builder<'a, B>
where
    B: OutlineSink,
{
    builder: &'a mut B,
    bbox: BBox,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct BBox {
    x_min: f32,
    y_min: f32,
    x_max: f32,
    y_max: f32,
}

/// Operators defined in Adobe Technical Note #5177, The Type  2 Charstring Format.
mod operator {
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

impl BBox {
    fn new() -> Self {
        BBox {
            x_min: core::f32::MAX,
            y_min: core::f32::MAX,
            x_max: core::f32::MIN,
            y_max: core::f32::MIN,
        }
    }

    fn is_default(&self) -> bool {
        self.x_min == core::f32::MAX
            && self.y_min == core::f32::MAX
            && self.x_max == core::f32::MIN
            && self.y_max == core::f32::MIN
    }

    fn extend_by(&mut self, x: f32, y: f32) {
        self.x_min = self.x_min.min(x);
        self.y_min = self.y_min.min(y);
        self.x_max = self.x_max.max(x);
        self.y_max = self.y_max.max(y);
    }

    fn to_rect(&self) -> Option<RectI> {
        Some(RectI::from_points(
            vec2i(
                i32::from(i16::try_num_from(self.x_min)?),
                i32::from(i16::try_num_from(self.y_min)?),
            ),
            vec2i(
                i32::from(i16::try_num_from(self.x_max)?),
                i32::from(i16::try_num_from(self.y_max)?),
            ),
        ))
    }
}

impl<'a, B> Builder<'a, B>
where
    B: OutlineSink,
{
    fn move_to(&mut self, x: f32, y: f32) {
        self.bbox.extend_by(x, y);
        self.builder.move_to(vec2f(x, y));
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.bbox.extend_by(x, y);
        self.builder.line_to(vec2f(x, y));
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.bbox.extend_by(x1, y1);
        self.bbox.extend_by(x2, y2);
        self.bbox.extend_by(x, y);
        let from = vec2f(x1, y1);
        let to = vec2f(x2, y2);
        self.builder
            .cubic_curve_to(LineSegment2F::new(from, to), vec2f(x, y))
    }

    fn close(&mut self) {
        self.builder.close();
    }
}

impl<'a> OutlineBuilder for CFF<'a> {
    type Error = CFFError;

    fn visit<S: OutlineSink>(&mut self, glyph_index: u16, sink: &mut S) -> Result<(), Self::Error> {
        let font = self.fonts.first().ok_or(ParseError::MissingValue)?;
        let data = font
            .char_strings_index
            .read_object(usize::from(glyph_index))
            .ok_or(ParseError::BadIndex)?;

        let _ = parse_char_string(font, &self.global_subr_index, data, glyph_index, sink)?;

        Ok(())
    }
}

struct CharStringParserContext<'a, 'b> {
    font: &'b Font<'a>,
    global_subr_index: &'b MaybeOwnedIndex<'a>,
    width_parsed: bool,
    stems_len: u32,
    has_endchar: bool,
    has_seac: bool,
    glyph_id: GlyphId, // Required to parse local subroutine in CID fonts.
    local_subrs: Option<&'b MaybeOwnedIndex<'a>>,
}

fn parse_char_string<'a, 'f, B: OutlineSink>(
    font: &'f Font<'a>,
    global_subr_index: &'f MaybeOwnedIndex<'a>,
    data: &'f [u8],
    glyph_id: GlyphId,
    builder: &mut B,
) -> Result<RectI, CFFError> {
    let local_subrs = match &font.data {
        CFFVariant::CID(_) => None, // Will be resolved on request.
        CFFVariant::Type1(type1) => type1.local_subr_index.as_ref(),
    };

    let mut ctx = CharStringParserContext {
        font,
        global_subr_index,
        width_parsed: false,
        stems_len: 0,
        has_endchar: false,
        has_seac: false,
        glyph_id,
        local_subrs,
    };

    let mut inner_builder = Builder {
        builder,
        bbox: BBox::new(),
    };

    let stack = ArgumentsStack {
        data: &mut [0.0; MAX_ARGUMENTS_STACK_LEN], // 4b * 48 = 192b
        len: 0,
        max_len: MAX_ARGUMENTS_STACK_LEN,
    };
    let mut parser = CharStringParser {
        stack,
        builder: &mut inner_builder,
        x: 0.0,
        y: 0.0,
        has_move_to: false,
        is_first_move_to: true,
    };
    parse_char_string0(&mut ctx, data, 0, &mut parser)?;

    if !ctx.has_endchar {
        return Err(CFFError::MissingEndChar);
    }

    let bbox = parser.builder.bbox;

    // Check that bbox was changed.
    if bbox.is_default() {
        return Ok(RectI::new(Vector2I::zero(), Vector2I::zero()));
    }

    bbox.to_rect().ok_or(CFFError::BboxOverflow)
}

fn parse_char_string0<B: OutlineSink>(
    ctx: &mut CharStringParserContext<'_, '_>,
    char_string: &[u8],
    depth: u8,
    p: &mut CharStringParser<'_, B>,
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
                // y dy {dya dyb}* hstem
                // x dx {dxa dxb}* vstem
                // y dy {dya dyb}* hstemhm
                // x dx {dxa dxb}* vstemhm

                // If the stack length is uneven, then the first value is a `width`.
                let len = if p.stack.len().is_odd() && !ctx.width_parsed {
                    ctx.width_parsed = true;
                    p.stack.len() - 1
                } else {
                    p.stack.len()
                };

                ctx.stems_len += len as u32 >> 1;

                // We are ignoring the hint operators.
                p.stack.clear();
            }
            operator::VERTICAL_MOVE_TO => {
                let mut i = 0;
                if p.stack.len() == 2 && !ctx.width_parsed {
                    i += 1;
                    ctx.width_parsed = true;
                }

                p.parse_vertical_move_to(i)?;
            }
            operator::LINE_TO => {
                p.parse_line_to()?;
            }
            operator::HORIZONTAL_LINE_TO => {
                p.parse_horizontal_line_to()?;
            }
            operator::VERTICAL_LINE_TO => {
                p.parse_vertical_line_to()?;
            }
            operator::CURVE_TO => {
                p.parse_curve_to()?;
            }
            operator::CALL_LOCAL_SUBROUTINE => {
                if p.stack.is_empty() {
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
                    let index = conv_subroutine_index(p.stack.pop(), subroutine_bias)?;
                    let char_string = local_subrs
                        .read_object(index)
                        .ok_or(CFFError::InvalidSubroutineIndex)?;
                    parse_char_string0(ctx, char_string, depth + 1, p)?;
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
                    operator::HFLEX => p.parse_hflex()?,
                    operator::FLEX => p.parse_flex()?,
                    operator::HFLEX1 => p.parse_hflex1()?,
                    operator::FLEX1 => p.parse_flex1()?,
                    _ => return Err(CFFError::UnsupportedOperator),
                }
            }
            operator::ENDCHAR => {
                if p.stack.len() == 4 || (!ctx.width_parsed && p.stack.len() == 5) {
                    // Process 'seac'.
                    let accent_char = seac_code_to_glyph_id(&ctx.font.charset, p.stack.pop())
                        .ok_or(CFFError::InvalidSeacCode)?;
                    let base_char = seac_code_to_glyph_id(&ctx.font.charset, p.stack.pop())
                        .ok_or(CFFError::InvalidSeacCode)?;
                    let dy = p.stack.pop();
                    let dx = p.stack.pop();

                    if !ctx.width_parsed {
                        p.stack.pop();
                        ctx.width_parsed = true;
                    }

                    ctx.has_seac = true;

                    let base_char_string = ctx
                        .font
                        .char_strings_index
                        .read_object(usize::from(base_char))
                        .ok_or(CFFError::InvalidSeacCode)?;
                    parse_char_string0(ctx, base_char_string, depth + 1, p)?;
                    p.x = dx;
                    p.y = dy;

                    let accent_char_string = ctx
                        .font
                        .char_strings_index
                        .read_object(usize::from(accent_char))
                        .ok_or(CFFError::InvalidSeacCode)?;
                    parse_char_string0(ctx, accent_char_string, depth + 1, p)?;
                } else if p.stack.len() == 1 && !ctx.width_parsed {
                    p.stack.pop();
                    ctx.width_parsed = true;
                }

                if !p.is_first_move_to {
                    p.is_first_move_to = true;
                    p.builder.close();
                }

                if s.bytes_available() {
                    return Err(CFFError::DataAfterEndChar);
                }

                ctx.has_endchar = true;

                break;
            }
            operator::HINT_MASK | operator::COUNTER_MASK => {
                let mut len = p.stack.len();

                // We are ignoring the hint operators.
                p.stack.clear();

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
                let mut i = 0;
                if p.stack.len() == 3 && !ctx.width_parsed {
                    i += 1;
                    ctx.width_parsed = true;
                }

                p.parse_move_to(i)?;
            }
            operator::HORIZONTAL_MOVE_TO => {
                let mut i = 0;
                if p.stack.len() == 2 && !ctx.width_parsed {
                    i += 1;
                    ctx.width_parsed = true;
                }

                p.parse_horizontal_move_to(i)?;
            }
            operator::CURVE_LINE => {
                p.parse_curve_line()?;
            }
            operator::LINE_CURVE => {
                p.parse_line_curve()?;
            }
            operator::VV_CURVE_TO => {
                p.parse_vv_curve_to()?;
            }
            operator::HH_CURVE_TO => {
                p.parse_hh_curve_to()?;
            }
            operator::SHORT_INT => {
                let n = s.read::<I16Be>()?;
                p.stack.push(f32::from(n))?;
            }
            operator::CALL_GLOBAL_SUBROUTINE => {
                if p.stack.is_empty() {
                    return Err(CFFError::InvalidArgumentsStackLength);
                }

                if depth == STACK_LIMIT {
                    return Err(CFFError::NestingLimitReached);
                }

                let subroutine_bias = calc_subroutine_bias(ctx.global_subr_index.len());
                let index = conv_subroutine_index(p.stack.pop(), subroutine_bias)?;
                let char_string = ctx
                    .global_subr_index
                    .read_object(index)
                    .ok_or(CFFError::InvalidSubroutineIndex)?;
                parse_char_string0(ctx, char_string, depth + 1, p)?;

                if ctx.has_endchar && !ctx.has_seac {
                    if s.bytes_available() {
                        return Err(CFFError::DataAfterEndChar);
                    }

                    break;
                }
            }
            operator::VH_CURVE_TO => {
                p.parse_vh_curve_to()?;
            }
            operator::HV_CURVE_TO => {
                p.parse_hv_curve_to()?;
            }
            32..=246 => {
                p.parse_int1(op)?;
            }
            247..=250 => {
                p.parse_int2(op, &mut s)?;
            }
            251..=254 => {
                p.parse_int3(op, &mut s)?;
            }
            operator::FIXED_16_16 => {
                p.parse_fixed(&mut s)?;
            }
        }
    }

    // TODO: 'A charstring subroutine must end with either an endchar or a return operator.'

    Ok(())
}

pub(crate) fn scan_char_string<'a, 'f>(
    font: &'f Font<'a>,
    global_subr_index: &'f MaybeOwnedIndex<'a>,
    data: &'f [u8],
    glyph_id: GlyphId,
) -> Result<CharStringScanResult, CFFError> {
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
        global_subr_used: BTreeSet::new(),
        local_subr_used: Vec::new(),
    };

    let stack = ArgumentsStack {
        data: &mut [0.0; MAX_ARGUMENTS_STACK_LEN], // 4b * 48 = 192b
        len: 0,
        max_len: MAX_ARGUMENTS_STACK_LEN,
    };
    let mut builder = NullSink;
    let mut inner_builder = Builder {
        builder: &mut builder,
        bbox: BBox::new(),
    };
    let mut parser = CharStringParser {
        stack,
        builder: &mut inner_builder,
        x: 0.0,
        y: 0.0,
        has_move_to: false,
        is_first_move_to: true,
    };
    parse_char_string1(&mut ctx, data, 0, &mut parser)?;

    if !ctx.has_endchar {
        return Err(CFFError::MissingEndChar);
    }

    // let bbox = parser.builder.bbox;
    //
    // // Check that bbox was changed.
    // if bbox.is_default() {
    //     return Ok(RectI::new(Vector2I::zero(), Vector2I::zero()));
    // }
    //
    // bbox.to_rect().ok_or(CFFError::BboxOverflow)
    let res = CharStringScanResult {
        global_subr_used: ctx.global_subr_used,
        local_subr_used: ctx.local_subr_used,
    };
    Ok(res)
}

struct NullSink;

impl OutlineSink for NullSink {
    fn move_to(&mut self, _to: Vector2F) {}

    fn line_to(&mut self, _to: Vector2F) {}

    fn quadratic_curve_to(&mut self, _ctrl: Vector2F, _to: Vector2F) {}

    fn cubic_curve_to(&mut self, _ctrl: LineSegment2F, _to: Vector2F) {}

    fn close(&mut self) {}
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
    global_subr_used: BTreeSet<usize>,
    local_subr_used: Vec<usize>,
}

// TODO: Better name
pub(crate) struct CharStringScanResult {
    pub(crate) global_subr_used: BTreeSet<usize>,
    pub(crate) local_subr_used: Vec<usize>,
}

fn parse_char_string1<'a, 'f, B: OutlineSink>(
    ctx: &mut CharStringParserContext1<'a, 'f>,
    char_string: &[u8],
    depth: u8,
    // TODO: replace this with just the stack since I think that's all we actually need
    p: &mut CharStringParser<'_, B>,
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
                // y dy {dya dyb}* hstem
                // x dx {dxa dxb}* vstem
                // y dy {dya dyb}* hstemhm
                // x dx {dxa dxb}* vstemhm

                // If the stack length is uneven, then the first value is a `width`.
                let len = if p.stack.len().is_odd() && !ctx.width_parsed {
                    ctx.width_parsed = true;
                    p.stack.len() - 1
                } else {
                    p.stack.len()
                };

                ctx.stems_len += len as u32 >> 1;

                // We are ignoring the hint operators.
                p.stack.clear();
            }
            operator::VERTICAL_MOVE_TO => {
                // let mut i = 0;
                // if p.stack.len() == 2 && !ctx.width_parsed {
                //     i += 1;
                //     ctx.width_parsed = true;
                // }
                //
                // p.parse_vertical_move_to(i)?;
                p.stack.clear();
            }
            operator::LINE_TO => {
                // p.parse_line_to()?;
                p.stack.clear();
            }
            operator::HORIZONTAL_LINE_TO => {
                // p.parse_horizontal_line_to()?;
                p.stack.clear();
            }
            operator::VERTICAL_LINE_TO => {
                // p.parse_vertical_line_to()?;
                p.stack.clear();
            }
            operator::CURVE_TO => {
                // p.parse_curve_to()?;
                p.stack.clear();
            }
            operator::CALL_LOCAL_SUBROUTINE => {
                if p.stack.is_empty() {
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
                    let index = conv_subroutine_index(p.stack.pop(), subroutine_bias)?;
                    let char_string = local_subrs
                        .read_object(index)
                        .ok_or(CFFError::InvalidSubroutineIndex)?;
                    ctx.local_subr_used.push(index);
                    parse_char_string1(ctx, char_string, depth + 1, p)?;
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
                    // operator::HFLEX => p.parse_hflex()?,
                    // operator::FLEX => p.parse_flex()?,
                    // operator::HFLEX1 => p.parse_hflex1()?,
                    // operator::FLEX1 => p.parse_flex1()?,
                    operator::HFLEX | operator::FLEX | operator::HFLEX1 | operator::FLEX1 => {
                        p.stack.clear()
                    }
                    _ => return Err(CFFError::UnsupportedOperator), // FIXME(wm) can we avoid this
                }
            }
            operator::ENDCHAR => {
                if p.stack.len() == 4 || (!ctx.width_parsed && p.stack.len() == 5) {
                    // Process 'seac'.
                    let accent_char = seac_code_to_glyph_id(&ctx.font.charset, p.stack.pop())
                        .ok_or(CFFError::InvalidSeacCode)?;
                    let base_char = seac_code_to_glyph_id(&ctx.font.charset, p.stack.pop())
                        .ok_or(CFFError::InvalidSeacCode)?;
                    let dy = p.stack.pop();
                    let dx = p.stack.pop();

                    if !ctx.width_parsed {
                        p.stack.pop();
                        ctx.width_parsed = true;
                    }

                    ctx.has_seac = true;

                    // FIXME: We probably don't want to do this
                    let base_char_string = ctx
                        .font
                        .char_strings_index
                        .read_object(usize::from(base_char))
                        .ok_or(CFFError::InvalidSeacCode)?;
                    parse_char_string1(ctx, base_char_string, depth + 1, p)?;
                    p.x = dx;
                    p.y = dy;

                    let accent_char_string = ctx
                        .font
                        .char_strings_index
                        .read_object(usize::from(accent_char))
                        .ok_or(CFFError::InvalidSeacCode)?;
                    parse_char_string1(ctx, accent_char_string, depth + 1, p)?;
                } else if p.stack.len() == 1 && !ctx.width_parsed {
                    p.stack.pop();
                    ctx.width_parsed = true;
                }

                if !p.is_first_move_to {
                    p.is_first_move_to = true;
                    p.builder.close();
                }

                if s.bytes_available() {
                    return Err(CFFError::DataAfterEndChar);
                }

                ctx.has_endchar = true;

                break;
            }
            operator::HINT_MASK | operator::COUNTER_MASK => {
                let mut len = p.stack.len();

                // We are ignoring the hint operators.
                p.stack.clear();

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
                let mut i = 0;
                if p.stack.len() == 3 && !ctx.width_parsed {
                    i += 1;
                    ctx.width_parsed = true;
                }

                // p.parse_move_to(i)?;
                p.stack.clear();
            }
            operator::HORIZONTAL_MOVE_TO => {
                let mut i = 0;
                if p.stack.len() == 2 && !ctx.width_parsed {
                    i += 1;
                    ctx.width_parsed = true;
                }

                // p.parse_horizontal_move_to(i)?;
                p.stack.clear();
            }
            operator::CURVE_LINE
            | operator::LINE_CURVE
            | operator::VV_CURVE_TO
            | operator::HH_CURVE_TO
            | operator::VH_CURVE_TO
            | operator::HV_CURVE_TO => {
                // p.parse_curve_line()?;
                // p.parse_line_curve()?;
                // p.parse_vv_curve_to()?;
                // p.parse_hh_curve_to()?;
                // p.parse_vh_curve_to()?;
                // p.parse_hv_curve_to()?;
                p.stack.clear();
            }
            operator::SHORT_INT => {
                let n = s.read::<I16Be>()?;
                p.stack.push(f32::from(n))?;
            }
            operator::CALL_GLOBAL_SUBROUTINE => {
                if p.stack.is_empty() {
                    return Err(CFFError::InvalidArgumentsStackLength);
                }

                if depth == STACK_LIMIT {
                    return Err(CFFError::NestingLimitReached);
                }

                let subroutine_bias = calc_subroutine_bias(ctx.global_subr_index.len());
                let index = conv_subroutine_index(p.stack.pop(), subroutine_bias)?;
                ctx.global_subr_used.insert(index);
                let char_string = ctx
                    .global_subr_index
                    .read_object(index)
                    .ok_or(CFFError::InvalidSubroutineIndex)?;
                parse_char_string1(ctx, char_string, depth + 1, p)?;

                if ctx.has_endchar && !ctx.has_seac {
                    if s.bytes_available() {
                        return Err(CFFError::DataAfterEndChar);
                    }

                    break;
                }
            }
            // TODO: Maybe move these out of the parser (p)?
            32..=246 => {
                p.parse_int1(op)?;
            }
            247..=250 => {
                p.parse_int2(op, &mut s)?;
            }
            251..=254 => {
                p.parse_int3(op, &mut s)?;
            }
            operator::FIXED_16_16 => {
                p.parse_fixed(&mut s)?;
            }
        }
    }

    // TODO: 'A charstring subroutine must end with either an endchar or a return operator.'

    Ok(())
}

fn conv_subroutine_index(index: f32, bias: u16) -> Result<usize, CFFError> {
    conv_subroutine_index_impl(index, bias).ok_or(CFFError::InvalidSubroutineIndex)
}

fn conv_subroutine_index_impl(index: f32, bias: u16) -> Option<usize> {
    let index = i32::try_num_from(index)?;
    let bias = i32::from(bias);

    let index = index.checked_add(bias)?;
    usize::try_from(index).ok()
}

// Adobe Technical Note #5176, Chapter 16 "Local / Global Subrs INDEXes"
fn calc_subroutine_bias(len: usize) -> u16 {
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
fn seac_code_to_glyph_id(charset: &Charset<'_>, n: f32) -> Option<GlyphId> {
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

impl IsEven for usize {
    fn is_even(&self) -> bool {
        (*self) & 1 == 0
    }

    fn is_odd(&self) -> bool {
        !self.is_even()
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

#[cfg(test)]
mod tests {
    use std::fmt::Write;
    use std::marker::PhantomData;

    use super::*;
    use crate::binary::write::{WriteBinary, WriteBuffer};
    use crate::cff::{
        Encoding, Header, Index, MaybeOwnedIndex, Operand, Operator, PrivateDict, TopDict,
        Type1Data,
    };
    use crate::tests::writer::{self, TtfType::*};

    struct Builder(String);

    impl OutlineSink for Builder {
        fn move_to(&mut self, to: Vector2F) {
            write!(&mut self.0, "M {} {} ", to.x(), to.y()).unwrap();
        }

        fn line_to(&mut self, to: Vector2F) {
            write!(&mut self.0, "L {} {} ", to.x(), to.y()).unwrap();
        }

        fn quadratic_curve_to(&mut self, ctrl: Vector2F, to: Vector2F) {
            write!(
                &mut self.0,
                "Q {} {} {} {} ",
                ctrl.x(),
                ctrl.y(),
                to.x(),
                to.y()
            )
            .unwrap();
        }

        fn cubic_curve_to(&mut self, ctrl: LineSegment2F, to: Vector2F) {
            write!(
                &mut self.0,
                "C {} {} {} {} {} {} ",
                ctrl.from().x(),
                ctrl.from().y(),
                ctrl.to().x(),
                ctrl.to().y(),
                to.x(),
                to.y()
            )
            .unwrap();
        }

        fn close(&mut self) {
            write!(&mut self.0, "Z ").unwrap();
        }
    }

    fn gen_cff(
        global_subrs: &[&[writer::TtfType]],
        local_subrs: &[&[writer::TtfType]],
        chars: &[writer::TtfType],
    ) -> Vec<u8> {
        fn gen_subrs(subrs: &[&[writer::TtfType]]) -> Vec<u8> {
            let mut w = writer::Writer::new();
            for v1 in subrs {
                for v2 in v1.iter() {
                    w.write(*v2);
                }
            }
            w.data
        }

        // TODO: support multiple subrs
        assert!(global_subrs.len() <= 1);
        assert!(local_subrs.len() <= 1);

        let global_subrs_data = gen_subrs(global_subrs);
        let local_subrs_data = gen_subrs(local_subrs);
        let chars_data = writer::convert(chars);

        // FIXME: Explain
        assert!(global_subrs_data.len() < 255);
        assert!(local_subrs_data.len() < 255);
        assert!(chars_data.len() < 255);

        // Header
        let header = Header {
            major: 1,
            minor: 0,
            hdr_size: 4,
            off_size: 1,
        };

        let font_name = b"Test Font";
        let name_index = Index {
            count: 1,
            off_size: 1,
            offset_array: &[1, font_name.len() as u8 + 1],
            data_array: font_name,
        };

        let top_dict = TopDict {
            dict: vec![
                (Operator::CharStrings, vec![Operand::Offset(0)]), // offset filled in when writing
                (
                    Operator::Private,
                    vec![Operand::Offset(0), Operand::Offset(0)],
                ), // offsets are filled in when writing
            ],
            default: PhantomData,
        };

        let string_index = MaybeOwnedIndex::Borrowed(Index {
            count: 0,
            off_size: 0,
            offset_array: &[],
            data_array: &[],
        });

        let global_subr_index = Index {
            count: if global_subrs_data.is_empty() { 0 } else { 1 },
            off_size: 1,
            offset_array: &[1, global_subrs_data.len() as u8 + 1],
            data_array: &global_subrs_data,
        };

        let char_strings_index = Index {
            count: 1,
            off_size: 1,
            offset_array: &[1, chars_data.len() as u8 + 1],
            data_array: &chars_data,
        };

        let local_subrs_index = Index {
            count: if local_subrs_data.is_empty() { 0 } else { 1 },
            off_size: 1,
            offset_array: &[1, local_subrs_data.len() as u8 + 1],
            data_array: &local_subrs_data,
        };

        let (private_dict_data, local_subr_index) = if !local_subrs_data.is_empty() {
            (
                vec![
                    (Operator::Subrs, vec![Operand::Offset(0)]), // offset filled in when writing
                ],
                Some(MaybeOwnedIndex::Borrowed(local_subrs_index)),
            )
        } else {
            (Vec::new(), None)
        };

        let private_dict = PrivateDict {
            dict: private_dict_data,
            default: PhantomData,
        };

        let cff = CFF {
            header,
            name_index,
            string_index,
            global_subr_index: MaybeOwnedIndex::Borrowed(global_subr_index),
            fonts: vec![Font {
                top_dict,
                char_strings_index: MaybeOwnedIndex::Borrowed(char_strings_index),
                charset: Charset::ISOAdobe,
                data: CFFVariant::Type1(Type1Data {
                    encoding: Encoding::Standard,
                    private_dict,
                    local_subr_index,
                }),
            }],
        };

        let mut w = WriteBuffer::new();
        CFF::write(&mut w, &cff).unwrap();
        w.into_inner()
    }

    fn rect(x_min: i16, y_min: i16, x_max: i16, y_max: i16) -> RectI {
        RectI::from_points(
            vec2i(i32::from(x_min), i32::from(y_min)),
            vec2i(i32::from(x_max), i32::from(y_max)),
        )
    }

    // Helper for the test that parses the char string for glyph 0 and returns the result and
    // glyph path.
    fn parse_char_string0(data: &[u8]) -> (Result<RectI, CFFError>, String) {
        let glyph_id = 0;
        let metadata = ReadScope::new(data).read::<CFF<'_>>().unwrap();
        let mut builder = Builder(String::new());
        let font = metadata.fonts.first().unwrap();
        let char_str = font
            .char_strings_index
            .read_object(usize::from(glyph_id))
            .unwrap();
        let res = parse_char_string(
            font,
            &metadata.global_subr_index,
            char_str,
            glyph_id,
            &mut builder,
        );
        (res, builder.0)
    }

    macro_rules! test_cs_with_subrs {
        ($name:ident, $glob:expr, $loc:expr, $values:expr, $path:expr, $rect_res:expr) => {
            #[test]
            fn $name() {
                let data = gen_cff($glob, $loc, $values);
                let (res, path) = parse_char_string0(&data);
                let rect = res.unwrap();

                assert_eq!(path, $path);
                assert_eq!(rect, $rect_res);
            }
        };
    }

    macro_rules! test_cs {
        ($name:ident, $values:expr, $path:expr, $rect_res:expr) => {
            test_cs_with_subrs!($name, &[], &[], $values, $path, $rect_res);
        };
    }

    macro_rules! test_cs_err {
        ($name:ident, $values:expr, $err:expr) => {
            #[test]
            fn $name() {
                let data = gen_cff(&[], &[], $values);
                let (res, _path) = parse_char_string0(&data);
                assert_eq!(res.unwrap_err(), $err);
            }
        };
    }

    test_cs!(
        move_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 Z ",
        rect(10, 20, 10, 20)
    );

    test_cs!(
        move_to_with_width,
        &[
            CFFInt(5),
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 Z ",
        rect(10, 20, 10, 20)
    );

    test_cs!(
        hmove_to,
        &[
            CFFInt(10),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 0 Z ",
        rect(10, 0, 10, 0)
    );

    test_cs!(
        hmove_to_with_width,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 20 0 Z ",
        rect(20, 0, 20, 0)
    );

    test_cs!(
        vmove_to,
        &[
            CFFInt(10),
            UInt8(operator::VERTICAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 0 10 Z ",
        rect(0, 10, 0, 10)
    );

    test_cs!(
        vmove_to_with_width,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::VERTICAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 0 20 Z ",
        rect(0, 20, 0, 20)
    );

    test_cs!(
        line_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            UInt8(operator::LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 40 60 Z ",
        rect(10, 20, 40, 60)
    );

    test_cs!(
        line_to_with_multiple_pairs,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            UInt8(operator::LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 40 60 L 90 120 Z ",
        rect(10, 20, 90, 120)
    );

    test_cs!(
        hline_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            UInt8(operator::HORIZONTAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 40 20 Z ",
        rect(10, 20, 40, 20)
    );

    test_cs!(
        hline_to_with_two_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            UInt8(operator::HORIZONTAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 40 20 L 40 60 Z ",
        rect(10, 20, 40, 60)
    );

    test_cs!(
        hline_to_with_three_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::HORIZONTAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 40 20 L 40 60 L 90 60 Z ",
        rect(10, 20, 90, 60)
    );

    test_cs!(
        vline_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            UInt8(operator::VERTICAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 10 50 Z ",
        rect(10, 20, 10, 50)
    );

    test_cs!(
        vline_to_with_two_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            UInt8(operator::VERTICAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 10 50 L 50 50 Z ",
        rect(10, 20, 50, 50)
    );

    test_cs!(
        vline_to_with_three_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::VERTICAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 L 10 50 L 50 50 L 50 100 Z ",
        rect(10, 20, 50, 100)
    );

    test_cs!(
        curve_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            CFFInt(70),
            CFFInt(80),
            UInt8(operator::CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 C 40 60 90 120 160 200 Z ",
        rect(10, 20, 160, 200)
    );

    test_cs!(
        curve_to_with_two_sets_of_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            CFFInt(70),
            CFFInt(80),
            CFFInt(90),
            CFFInt(100),
            CFFInt(110),
            CFFInt(120),
            CFFInt(130),
            CFFInt(140),
            UInt8(operator::CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 C 40 60 90 120 160 200 C 250 300 360 420 490 560 Z ",
        rect(10, 20, 490, 560)
    );

    test_cs!(
        hh_curve_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            UInt8(operator::HH_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 C 40 20 80 70 140 70 Z ",
        rect(10, 20, 140, 70)
    );

    test_cs!(
        hh_curve_to_with_y,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            CFFInt(70),
            UInt8(operator::HH_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 C 50 50 100 110 170 110 Z ",
        rect(10, 20, 170, 110)
    );

    test_cs!(
        vv_curve_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            UInt8(operator::VV_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 C 10 50 50 100 50 160 Z ",
        rect(10, 20, 50, 160)
    );

    test_cs!(
        vv_curve_to_with_x,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            CFFInt(70),
            UInt8(operator::VV_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 20 C 40 60 90 120 90 190 Z ",
        rect(10, 20, 90, 190)
    );

    // #[test]
    // fn only_endchar() {
    //     let data = gen_cff(&[], &[], &[UInt8(operator::ENDCHAR)]);
    //     let metadata = parse_metadata(&data).unwrap();
    //     let mut builder = Builder(String::new());
    //     let char_str = metadata.char_strings.get(0).unwrap();
    //     assert!(parse_char_string(char_str, &metadata, GlyphId(0), &mut builder).is_err());
    // }

    test_cs_with_subrs!(
        local_subr,
        &[],
        &[&[
            CFFInt(30),
            CFFInt(40),
            UInt8(operator::LINE_TO),
            UInt8(operator::RETURN),
        ]],
        &[
            CFFInt(10),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            CFFInt(0 - 107), // subr index - subr bias
            UInt8(operator::CALL_LOCAL_SUBROUTINE),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 0 L 40 40 Z ",
        rect(10, 0, 40, 40)
    );

    test_cs_with_subrs!(
        endchar_in_subr,
        &[],
        &[&[
            CFFInt(30),
            CFFInt(40),
            UInt8(operator::LINE_TO),
            UInt8(operator::ENDCHAR),
        ]],
        &[
            CFFInt(10),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            CFFInt(0 - 107), // subr index - subr bias
            UInt8(operator::CALL_LOCAL_SUBROUTINE),
        ],
        "M 10 0 L 40 40 Z ",
        rect(10, 0, 40, 40)
    );

    test_cs_with_subrs!(
        global_subr,
        &[&[
            CFFInt(30),
            CFFInt(40),
            UInt8(operator::LINE_TO),
            UInt8(operator::RETURN),
        ]],
        &[],
        &[
            CFFInt(10),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            CFFInt(0 - 107), // subr index - subr bias
            UInt8(operator::CALL_GLOBAL_SUBROUTINE),
            UInt8(operator::ENDCHAR),
        ],
        "M 10 0 L 40 40 Z ",
        rect(10, 0, 40, 40)
    );

    test_cs_err!(
        reserved_operator,
        &[CFFInt(10), UInt8(2), UInt8(operator::ENDCHAR),],
        CFFError::InvalidOperator
    );

    test_cs_err!(
        line_to_without_move_to,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::MissingMoveTo
    );

    // Width must be set only once.
    test_cs_err!(
        two_vmove_to_with_width,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::VERTICAL_MOVE_TO),
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::VERTICAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        move_to_with_too_many_coords,
        &[
            CFFInt(10),
            CFFInt(10),
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        move_to_with_not_enought_coords,
        &[
            CFFInt(10),
            UInt8(operator::MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        hmove_to_with_too_many_coords,
        &[
            CFFInt(10),
            CFFInt(10),
            CFFInt(10),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        hmove_to_with_not_enought_coords,
        &[
            UInt8(operator::HORIZONTAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        vmove_to_with_too_many_coords,
        &[
            CFFInt(10),
            CFFInt(10),
            CFFInt(10),
            UInt8(operator::VERTICAL_MOVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        vmove_to_with_not_enought_coords,
        &[UInt8(operator::VERTICAL_MOVE_TO), UInt8(operator::ENDCHAR),],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        line_to_with_single_coord,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            UInt8(operator::LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        line_to_with_odd_number_of_coord,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        hline_to_without_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            UInt8(operator::HORIZONTAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        vline_to_without_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            UInt8(operator::VERTICAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        curve_to_with_invalid_num_of_coords_1,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            UInt8(operator::CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        curve_to_with_invalid_num_of_coords_2,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(60),
            CFFInt(70),
            CFFInt(80),
            CFFInt(90),
            UInt8(operator::CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        hh_curve_to_with_not_enought_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::HH_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        hh_curve_to_with_too_many_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::HH_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        vv_curve_to_with_not_enought_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::VV_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        vv_curve_to_with_too_many_coords,
        &[
            CFFInt(10),
            CFFInt(20),
            UInt8(operator::MOVE_TO),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            CFFInt(30),
            CFFInt(40),
            CFFInt(50),
            UInt8(operator::VV_CURVE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::InvalidArgumentsStackLength
    );

    test_cs_err!(
        multiple_endchar,
        &[UInt8(operator::ENDCHAR), UInt8(operator::ENDCHAR),],
        CFFError::DataAfterEndChar
    );

    test_cs_err!(
        operands_overflow,
        &[
            CFFInt(0),
            CFFInt(1),
            CFFInt(2),
            CFFInt(3),
            CFFInt(4),
            CFFInt(5),
            CFFInt(6),
            CFFInt(7),
            CFFInt(8),
            CFFInt(9),
            CFFInt(0),
            CFFInt(1),
            CFFInt(2),
            CFFInt(3),
            CFFInt(4),
            CFFInt(5),
            CFFInt(6),
            CFFInt(7),
            CFFInt(8),
            CFFInt(9),
            CFFInt(0),
            CFFInt(1),
            CFFInt(2),
            CFFInt(3),
            CFFInt(4),
            CFFInt(5),
            CFFInt(6),
            CFFInt(7),
            CFFInt(8),
            CFFInt(9),
            CFFInt(0),
            CFFInt(1),
            CFFInt(2),
            CFFInt(3),
            CFFInt(4),
            CFFInt(5),
            CFFInt(6),
            CFFInt(7),
            CFFInt(8),
            CFFInt(9),
            CFFInt(0),
            CFFInt(1),
            CFFInt(2),
            CFFInt(3),
            CFFInt(4),
            CFFInt(5),
            CFFInt(6),
            CFFInt(7),
            CFFInt(8),
            CFFInt(9),
        ],
        CFFError::ArgumentsStackLimitReached
    );

    test_cs_err!(
        operands_overflow_with_4_byte_ints,
        &[
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
            CFFInt(30000),
        ],
        CFFError::ArgumentsStackLimitReached
    );

    test_cs_err!(
        bbox_overflow,
        &[
            CFFInt(32767),
            UInt8(operator::HORIZONTAL_MOVE_TO),
            CFFInt(32767),
            UInt8(operator::HORIZONTAL_LINE_TO),
            UInt8(operator::ENDCHAR),
        ],
        CFFError::BboxOverflow
    );

    #[test]
    fn endchar_in_subr_with_extra_data_1() {
        let data = gen_cff(
            &[],
            &[&[
                CFFInt(30),
                CFFInt(40),
                UInt8(operator::LINE_TO),
                UInt8(operator::ENDCHAR),
            ]],
            &[
                CFFInt(10),
                UInt8(operator::HORIZONTAL_MOVE_TO),
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_LOCAL_SUBROUTINE),
                CFFInt(30),
                CFFInt(40),
                UInt8(operator::LINE_TO),
            ],
        );

        let (res, _path) = parse_char_string0(&data);
        assert_eq!(res.unwrap_err(), CFFError::DataAfterEndChar);
    }

    #[test]
    fn endchar_in_subr_with_extra_data_2() {
        let data = gen_cff(
            &[],
            &[&[
                CFFInt(30),
                CFFInt(40),
                UInt8(operator::LINE_TO),
                UInt8(operator::ENDCHAR),
                CFFInt(30),
                CFFInt(40),
                UInt8(operator::LINE_TO),
            ]],
            &[
                CFFInt(10),
                UInt8(operator::HORIZONTAL_MOVE_TO),
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_LOCAL_SUBROUTINE),
            ],
        );

        let (res, _path) = parse_char_string0(&data);
        assert_eq!(res.unwrap_err(), CFFError::DataAfterEndChar);
    }

    #[test]
    fn subr_without_return() {
        let data = gen_cff(
            &[],
            &[&[
                CFFInt(30),
                CFFInt(40),
                UInt8(operator::LINE_TO),
                UInt8(operator::ENDCHAR),
                CFFInt(30),
                CFFInt(40),
                UInt8(operator::LINE_TO),
            ]],
            &[
                CFFInt(10),
                UInt8(operator::HORIZONTAL_MOVE_TO),
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_LOCAL_SUBROUTINE),
            ],
        );

        let (res, _path) = parse_char_string0(&data);
        assert_eq!(res.unwrap_err(), CFFError::DataAfterEndChar);
    }

    #[test]
    fn recursive_local_subr() {
        let data = gen_cff(
            &[],
            &[&[
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_LOCAL_SUBROUTINE),
            ]],
            &[
                CFFInt(10),
                UInt8(operator::HORIZONTAL_MOVE_TO),
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_LOCAL_SUBROUTINE),
            ],
        );

        let (res, _path) = parse_char_string0(&data);
        assert_eq!(res.unwrap_err(), CFFError::NestingLimitReached);
    }

    #[test]
    fn recursive_global_subr() {
        let data = gen_cff(
            &[&[
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_GLOBAL_SUBROUTINE),
            ]],
            &[],
            &[
                CFFInt(10),
                UInt8(operator::HORIZONTAL_MOVE_TO),
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_GLOBAL_SUBROUTINE),
            ],
        );

        let (res, _path) = parse_char_string0(&data);
        assert_eq!(res.unwrap_err(), CFFError::NestingLimitReached);
    }

    #[test]
    fn recursive_mixed_subr() {
        let data = gen_cff(
            &[&[
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_LOCAL_SUBROUTINE),
            ]],
            &[&[
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_GLOBAL_SUBROUTINE),
            ]],
            &[
                CFFInt(10),
                UInt8(operator::HORIZONTAL_MOVE_TO),
                CFFInt(0 - 107), // subr index - subr bias
                UInt8(operator::CALL_GLOBAL_SUBROUTINE),
            ],
        );

        let (res, _path) = parse_char_string0(&data);
        assert_eq!(res.unwrap_err(), CFFError::NestingLimitReached);
    }

    // TODO: return from main
    // TODO: return without endchar
    // TODO: data after return
    // TODO: recursive subr
    // TODO: HORIZONTAL_STEM
    // TODO: VERTICAL_STEM
    // TODO: HORIZONTAL_STEM_HINT_MASK
    // TODO: HINT_MASK
    // TODO: COUNTER_MASK
    // TODO: VERTICAL_STEM_HINT_MASK
    // TODO: CURVE_LINE
    // TODO: LINE_CURVE
    // TODO: VH_CURVE_TO
    // TODO: HFLEX
    // TODO: FLEX
    // TODO: HFLEX1
    // TODO: FLEX1
}
