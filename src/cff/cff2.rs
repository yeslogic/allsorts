//! CFF2 font handling.
//!
//! Refer to [OpenType CFF2 spec](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2)
//! for more information.

use std::convert::TryFrom;

use super::{
    owned, read_local_subr_index, CFFError, Dict, DictDefault, DictDelta, FDSelect, Index,
    IndexU32, MaybeOwnedIndex, Operand, Operator, DEFAULT_BLUE_FUZZ, DEFAULT_BLUE_SCALE,
    DEFAULT_BLUE_SHIFT, DEFAULT_EXPANSION_FACTOR, DEFAULT_FONT_MATRIX, OFFSET_ZERO, OPERAND_ZERO,
};
use crate::binary::read::{ReadBinary, ReadCtxt, ReadScope};
use crate::binary::write::{WriteBinary, WriteBinaryDep, WriteBuffer, WriteContext};
use crate::binary::{I16Be, U16Be, U32Be, U8};
use crate::cff::charstring::{
    calc_subroutine_bias, operator, ArgumentsStack, IsEven, TryNumFrom, MAX_ARGUMENTS_STACK_LEN,
    STACK_LIMIT, TWO_BYTE_OPERATOR_MARK,
};
use crate::error::{ParseError, WriteError};
use crate::tables::variable_fonts::{ItemVariationStore, OwnedTuple};
use crate::tables::Fixed;
use crate::variations::VariationError;
use crate::SafeFrom;

/// Maximum number of operands in Top DICT, Font DICTs, Private DICTs and CharStrings.
///
/// > Operators in Top DICT, Font DICTs, Private DICTs and CharStrings may be preceded by up to a
/// > maximum of 513 operands.
pub const MAX_OPERANDS: usize = 513;

/// Top level representation of a CFF2 font file, typically read from a CFF2 OpenType table.
///
/// [OpenType CFF2 spec](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2)
#[derive(Clone)]
pub struct CFF2<'a> {
    /// CFF2 Header.
    pub header: Header,
    /// Top DICT with top-level properties of the font.
    pub top_dict: TopDict,
    /// INDEX of global subroutines.
    pub global_subr_index: MaybeOwnedIndex<'a>,
    /// INDEX of char strings (glyphs).
    pub char_strings_index: MaybeOwnedIndex<'a>,
    /// Item variation store. Required/present for variable fonts.
    pub vstore: Option<ItemVariationStore<'a>>,
    /// Font dict select. Maps glyph ids to Font DICTs.
    pub fd_select: Option<FDSelect<'a>>,
    /// Sub-font of this CFF2 font.
    ///
    /// Contains Font DICT, Private DICT, and optional local subroutine INDEX.
    pub fonts: Vec<Font<'a>>,
}

/// CFF2 Font Header
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/cff2#6-header>
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Header {
    /// Major version (2).
    pub major: u8,
    /// Minor version.
    pub minor: u8,
    /// Size of the header in the font (maybe larger than this structure).
    pub header_size: u8,
    /// Length of the Top DICT
    pub top_dict_length: u16,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TopDictDefault;

#[derive(Debug, PartialEq, Clone)]
pub struct FontDictDefault;

#[derive(Debug, PartialEq, Clone)]
pub struct PrivateDictDefault;

pub type TopDict = Dict<TopDictDefault>;

pub type FontDict = Dict<FontDictDefault>;

pub type PrivateDict = Dict<PrivateDictDefault>;

#[derive(Clone)]
pub struct Font<'a> {
    pub font_dict: FontDict,
    pub private_dict: PrivateDict,
    pub local_subr_index: Option<MaybeOwnedIndex<'a>>,
}

struct CharStringScannerContext {
    width_parsed: bool,
    stems_len: u32,
    has_blend: bool,
    vsindex: Option<u16>,
}

impl<'a> CFF2<'a> {
    // TODO: Make it possible to call this only if the font is supposed to be variable such as by including fvar
    pub fn instance_char_strings(&mut self, instance: &OwnedTuple) -> Result<(), VariationError> {
        let mut new_char_strings = Vec::with_capacity(self.char_strings_index.len());

        let mut stack = ArgumentsStack {
            data: &mut [StackValue::Int(0); MAX_ARGUMENTS_STACK_LEN], // 4b * 48 = 192b
            len: 0,
            max_len: MAX_ARGUMENTS_STACK_LEN,
        };

        let vstore = self
            .vstore
            .as_ref()
            .ok_or(CFFError::InvalidSubroutineIndex)?; // FIXME: Custom error

        // for char_string in font, apply variations
        for (glyph_id, char_string) in self.char_strings_index.iter().enumerate() {
            let glyph_id = glyph_id as u16;
            let mut new_char_string = WriteBuffer::new();

            let font_index = match &self.fd_select {
                Some(fd_select) => fd_select
                    .font_dict_index(glyph_id)
                    .ok_or(CFFError::InvalidSubroutineIndex)?, // FIXME: Custom error for bad font index
                None => 0,
            };
            let font = self
                .fonts
                .get(usize::from(font_index))
                .ok_or(CFFError::InvalidSubroutineIndex)?; // FIXME: Custom error for bad font index

            // TODO: For unchanged char_strings can we use Cow
            let mut ctx = CharStringScannerContext {
                width_parsed: false,
                stems_len: 0,
                has_blend: false,
                vsindex: None,
            };

            stack.clear();
            let depth = 0;
            self.scan_char_string(
                char_string,
                &mut new_char_string,
                font,
                vstore,
                &instance,
                &mut ctx,
                &mut stack,
                depth,
            )?;
            new_char_strings.push(new_char_string.into_inner());
        }

        // All subroutines should have been inlined, so they can be dropped now
        for font in self.fonts.iter_mut() {
            font.local_subr_index = None;
            font.private_dict.remove(Operator::Subrs);
        }
        self.global_subr_index = MaybeOwnedIndex::Owned(owned::Index { data: Vec::new() });
        self.char_strings_index = MaybeOwnedIndex::Owned(owned::Index {
            data: new_char_strings,
        });

        Ok(())
    }

    fn scan_char_string<W: WriteContext>(
        &self,
        char_string: &[u8],
        new_char_string: &mut W,
        font: &Font<'a>,
        vstore: &ItemVariationStore<'a>,
        instance: &OwnedTuple,
        ctx: &mut CharStringScannerContext,
        stack: &mut ArgumentsStack<'_, StackValue>,
        depth: u8,
    ) -> Result<(), VariationError> {
        let mut s = ReadScope::new(char_string).ctxt();
        while s.bytes_available() {
            let op = s.read::<U8>()?;
            match op {
                0 | 2 | 9 | 13 | 17 => {
                    // Reserved.
                    return Err(CFFError::InvalidOperator.into());
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

                    write_stack(new_char_string, stack)?;
                    U8::write(new_char_string, op)?;
                }
                operator::VERTICAL_MOVE_TO => {
                    if stack.len() == 2 && !ctx.width_parsed {
                        ctx.width_parsed = true;
                    }
                    write_stack(new_char_string, stack)?;
                    U8::write(new_char_string, op)?;
                }
                operator::LINE_TO
                | operator::HORIZONTAL_LINE_TO
                | operator::VERTICAL_LINE_TO
                | operator::CURVE_TO => {
                    write_stack(new_char_string, stack)?;
                    U8::write(new_char_string, op)?;
                }
                operator::CALL_LOCAL_SUBROUTINE => {
                    if stack.is_empty() {
                        return Err(CFFError::InvalidArgumentsStackLength.into());
                    }

                    if depth == STACK_LIMIT {
                        return Err(CFFError::NestingLimitReached.into());
                    }

                    if let Some(ref local_subrs) = font.local_subr_index {
                        let subroutine_bias = calc_subroutine_bias(local_subrs.len());
                        let subr_index = stack.pop();
                        let index = conv_subroutine_index(subr_index, subroutine_bias)?;
                        let subr_char_string = local_subrs
                            .read_object(index)
                            .ok_or(CFFError::InvalidSubroutineIndex)?;
                        self.scan_char_string(
                            subr_char_string,
                            new_char_string,
                            font,
                            vstore,
                            instance,
                            ctx,
                            stack,
                            depth + 1,
                        )?;
                    } else {
                        return Err(CFFError::NoLocalSubroutines.into());
                    }
                }
                operator::RETURN => {
                    // Removed in CFF2
                    return Err(CFFError::InvalidOperator.into());
                }
                TWO_BYTE_OPERATOR_MARK => {
                    // flex
                    let op2 = s.read::<U8>()?;
                    match op2 {
                        operator::HFLEX | operator::FLEX | operator::HFLEX1 | operator::FLEX1 => {
                            write_stack(new_char_string, stack)?;
                            U8::write(new_char_string, op)?;
                        }
                        _ => return Err(CFFError::UnsupportedOperator.into()),
                    }
                }
                operator::ENDCHAR => {
                    // Removed in CFF2
                    return Err(CFFError::InvalidOperator.into());
                }
                operator::VS_INDEX => {
                    // When used, vsindex must precede the first blend operator,
                    // and may occur only once in the CharString.
                    if ctx.vsindex.is_some() {
                        return Err(CFFError::DuplicateVsIndex.into());
                    } else if ctx.has_blend {
                        // FIXME: Error: can't vsindex after blend has been seen
                        return Err(CFFError::DuplicateVsIndex.into());
                    } else {
                        if stack.len() != 1 {
                            return Err(CFFError::InvalidArgumentsStackLength.into());
                        }
                        let item_variation_data_index =
                            stack.pop().try_as_u16().ok_or_else(|| {
                                VariationError::from(CFFError::InvalidArgumentsStackLength)
                            })?;
                        ctx.vsindex = Some(item_variation_data_index);
                    }
                }
                operator::BLEND => {
                    // For k regions, produces n interpolated result value(s) from n*(k + 1)
                    // operands. The last operand on the stack, n, specifies the number of operands
                    // that will be left on the stack for the next operator. (For example, if the
                    // blend operator is used in conjunction with the hflex operator, which
                    // requires 6 operands, then n would be set to 6.) This operand also informs
                    // the handler for the blend operator that the operator is preceded by n+1 sets
                    // of operands.

                    // Clear all but n values from the stack, leaving the values for the subsequent
                    // operator corresponding to the default instance

                    // Lookup the ItemVariationStore data to get the variation regions
                    let vs_index = ctx.vsindex.map(Ok).unwrap_or_else(|| {
                        // NOTE(unwrap): can't fail as Operator::VSIndex has a default
                        font.private_dict
                            .get_i32(Operator::VSIndex)
                            .unwrap()
                            .and_then(|val| u16::try_from(val).map_err(ParseError::from))
                    })?;

                    // Each region can now produce its scalar for the particular variation tuple
                    let scalars = vstore
                        .regions(vs_index)?
                        .map(|region| {
                            let region = region?;
                            Ok(region.scalar(instance.iter().copied()))
                        })
                        .collect::<Result<Vec<_>, ParseError>>()?; // TODO: cache this as vs_index does not vary within a CharString so regions remains the same too.

                    let k = scalars.len();
                    if stack.len < 1 {
                        return Err(CFFError::InvalidArgumentsStackLength.into());
                    }
                    let n = stack.pop().try_as_u16().map(usize::from).ok_or_else(|| {
                        VariationError::from(CFFError::InvalidArgumentsStackLength)
                    })?; // FIXME: Error, value is invalid not stack

                    let num_operands = n * (k + 1);
                    if stack.len() >= num_operands {
                        // Process n*k operands applying the scalars
                        // let mut final_deltas = vec![0.0; k];
                        let mut region_deltas = vec![0.0; n];

                        let operands = stack.pop_n(num_operands);
                        // for each set of deltas apply the scalar and calculate a new delta to
                        // apply to the default values
                        let (defaults, rest) = operands.split_at(n);
                        for (adjustment, deltas) in region_deltas.iter_mut().zip(rest.chunks(k)) {
                            for (delta, scalar) in deltas.iter().copied().zip(scalars.iter()) {
                                if let Some(scalar) = scalar {
                                    *adjustment += scalar * f32::from(delta);
                                }
                            }
                        }

                        let blended = defaults
                            .iter()
                            .copied()
                            .zip(region_deltas)
                            .map(|(default, delta)| f32::from(default) + delta)
                            .collect::<Vec<_>>();

                        // Now put res back on the stack
                        for operand in blended {
                            // FIXME: Can these be written out directly?
                            stack.push(StackValue::try_from(operand)?)?;
                        }
                        write_stack(new_char_string, stack)?;
                        U8::write(new_char_string, op)?;
                    } else {
                        return Err(CFFError::InvalidArgumentsStackLength.into());
                    }
                }
                operator::HINT_MASK | operator::COUNTER_MASK => {
                    let mut len = stack.len();

                    // We are ignoring the hint operators.
                    write_stack(new_char_string, stack)?; // TODO: Is this right?

                    // If the stack length is uneven, then the first value is a `width`.
                    if len.is_odd() && !ctx.width_parsed {
                        len -= 1;
                        ctx.width_parsed = true;
                    }

                    ctx.stems_len += len as u32 >> 1;

                    // Skip the hints
                    let hints = s
                        .read_slice(
                            usize::try_from((ctx.stems_len + 7) >> 3)
                                .map_err(|_| ParseError::BadValue)?,
                        )
                        .map_err(|_| ParseError::BadOffset)?;

                    new_char_string.write_bytes(hints)?; // TODO: Is this right?
                    U8::write(new_char_string, op)?;
                }
                operator::MOVE_TO => {
                    if stack.len() == 3 && !ctx.width_parsed {
                        ctx.width_parsed = true;
                    }
                    write_stack(new_char_string, stack)?;
                    U8::write(new_char_string, op)?;
                }
                operator::HORIZONTAL_MOVE_TO => {
                    if stack.len() == 2 && !ctx.width_parsed {
                        ctx.width_parsed = true;
                    }
                    write_stack(new_char_string, stack)?;
                    U8::write(new_char_string, op)?;
                }
                operator::CURVE_LINE
                | operator::LINE_CURVE
                | operator::VV_CURVE_TO
                | operator::HH_CURVE_TO
                | operator::VH_CURVE_TO
                | operator::HV_CURVE_TO => {
                    write_stack(new_char_string, stack)?;
                    U8::write(new_char_string, op)?;
                }
                operator::SHORT_INT => {
                    let n = s.read::<I16Be>()?;
                    stack.push(StackValue::Int(n))?;
                }
                operator::CALL_GLOBAL_SUBROUTINE => {
                    if stack.is_empty() {
                        return Err(CFFError::InvalidArgumentsStackLength.into());
                    }

                    if depth == STACK_LIMIT {
                        return Err(CFFError::NestingLimitReached.into());
                    }

                    let subroutine_bias = calc_subroutine_bias(self.global_subr_index.len());
                    let subr_index = stack.pop();
                    let index = conv_subroutine_index(subr_index, subroutine_bias)?;
                    let subr_char_string = self
                        .global_subr_index
                        .read_object(index)
                        .ok_or(CFFError::InvalidSubroutineIndex)?;
                    self.scan_char_string(
                        subr_char_string,
                        new_char_string,
                        font,
                        vstore,
                        instance,
                        ctx,
                        stack,
                        depth + 1,
                    )?;
                }
                32..=246 => {
                    stack.push(parse_int1(op)?)?;
                }
                247..=250 => {
                    stack.push(parse_int2(op, &mut s)?)?;
                }
                251..=254 => {
                    stack.push(parse_int3(op, &mut s)?)?;
                }
                operator::FIXED_16_16 => {
                    stack.push(parse_fixed(&mut s)?)?;
                }
            }
        }
        Ok(())
    }
}

impl<'b> ReadBinary for CFF2<'b> {
    type HostType<'a> = CFF2<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        // Get a scope that starts at the beginning of the CFF data. This is needed for reading
        // data that is specified as an offset from the start of the data later.
        let scope = ctxt.scope();

        let header = ctxt.read::<Header>()?;
        let top_dict_data = ctxt.read_slice(usize::from(header.top_dict_length))?;
        let top_dict = ReadScope::new(top_dict_data)
            .ctxt()
            .read_dep::<TopDict>(MAX_OPERANDS)?;
        let global_subr_index = ctxt.read::<IndexU32>().map(MaybeOwnedIndex::Borrowed)?;

        // CharStrings index
        let char_strings_offset = top_dict
            .get_i32(Operator::CharStrings)
            .unwrap_or(Err(ParseError::MissingValue))?;
        let char_strings_index = scope
            .offset(usize::try_from(char_strings_offset)?)
            .read::<IndexU32>()
            .map(MaybeOwnedIndex::Borrowed)?;
        let n_glyphs = char_strings_index.len();

        // Font DICT Index
        let fd_array_offset = top_dict
            .get_i32(Operator::FDArray)
            .unwrap_or(Err(ParseError::MissingValue))?;
        let fd_array = scope
            .offset(usize::try_from(fd_array_offset)?)
            .read::<IndexU32>()?;

        // FDSelect if more than one font is present
        let fd_select = if fd_array.count > 1 {
            let fs_select_offset = top_dict
                .get_i32(Operator::FDSelect)
                .unwrap_or(Err(ParseError::MissingValue))?;
            scope
                .offset(usize::try_from(fs_select_offset)?)
                .read_dep::<FDSelect<'a>>(n_glyphs)
                .map(Some)?
        } else {
            None
        };

        // VariationStore for variable fonts (optional)
        let vstore = top_dict
            .get_i32(Operator::VStore)
            .transpose()?
            .map(|offset| {
                let mut ctxt = scope.offset(usize::try_from(offset)?).ctxt();
                // The VariationStore data is comprised of two parts: a uint16 field that specifies
                // a length, followed by an Item Variation Store structure of the specified length.
                let _length = ctxt.read_u16be()?;
                ctxt.read::<ItemVariationStore<'_>>()
            })
            .transpose()?;

        // Font/glyph data
        let mut fonts = Vec::with_capacity(fd_array.count);
        for font_index in 0..fd_array.count {
            let font_dict = fd_array.read::<FontDict>(font_index, MAX_OPERANDS)?;
            let (private_dict, private_dict_offset) =
                font_dict.read_private_dict::<PrivateDict>(&scope, MAX_OPERANDS)?;
            let local_subr_index =
                read_local_subr_index::<_, IndexU32>(&scope, &private_dict, private_dict_offset)?
                    .map(MaybeOwnedIndex::Borrowed);

            fonts.push(Font {
                font_dict,
                private_dict,
                local_subr_index,
            });
        }

        Ok(CFF2 {
            header,
            top_dict,
            global_subr_index,
            char_strings_index,
            vstore,
            fd_select,
            fonts,
        })
    }
}

impl<'a> WriteBinary for CFF2<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, cff2: Self) -> Result<Self::Output, WriteError> {
        // Build new Top DICT
        let mut top_dict = TopDict::new();
        if let Some(font_matrix) = cff2.top_dict.get(Operator::FontMatrix) {
            top_dict
                .inner_mut()
                .push((Operator::FontMatrix, font_matrix.to_vec()))
        }
        // Add CharStrings INDEX, FDSelect, and FDArray offsets.
        // Actual offsets will be filled in when writing
        top_dict
            .inner_mut()
            .push((Operator::CharStrings, OFFSET_ZERO.to_vec()));
        top_dict
            .inner_mut()
            .push((Operator::FDArray, OFFSET_ZERO.to_vec()));
        if cff2.fonts.len() > 1 {
            // The FDSelect operator and the structure it points to are required if the Font DICT INDEX
            // contains more than one Font DICT, else it must be omitted.
            top_dict
                .inner_mut()
                .push((Operator::FDSelect, OFFSET_ZERO.to_vec()));
        }
        if cff2.vstore.is_some() {
            top_dict
                .inner_mut()
                .push((Operator::VStore, OFFSET_ZERO.to_vec()));
        }

        // Calculate size of TopDict
        let mut write_buffer = WriteBuffer::new();
        TopDict::write_dep(&mut write_buffer, &top_dict, DictDelta::new())?;
        let top_dict_length = u16::try_from(write_buffer.bytes_written())?;

        // Now that the size of the Top DICT is known we can write out the header
        let header = Header {
            top_dict_length,
            ..cff2.header
        };
        Header::write(ctxt, header)?;

        // Reserve space for the Top DICT to be filled in later when the offsets it holds are resolved.
        let top_dict_placeholder = ctxt.reserve::<TopDict, _>(usize::from(top_dict_length))?;

        // Write global Subr INDEX
        MaybeOwnedIndex::write32(ctxt, &cff2.global_subr_index)?;

        // CharStrings INDEX
        top_dict.replace(
            Operator::CharStrings,
            vec![Operand::Offset(i32::try_from(ctxt.bytes_written())?)],
        );
        MaybeOwnedIndex::write32(ctxt, &cff2.char_strings_index)?;

        // FDSelect
        match &cff2.fd_select {
            Some(fd_select) if cff2.fonts.len() > 1 => {
                top_dict.replace(
                    Operator::FDSelect,
                    vec![Operand::Offset(i32::try_from(ctxt.bytes_written())?)],
                );
                FDSelect::write(ctxt, fd_select)?;
            }
            // If there is more than one font then FDSelect is required
            None if cff2.fonts.len() > 1 => return Err(WriteError::BadValue),
            Some(_) | None => {}
        }

        // Write out Private DICTs and Local Subr INDEXes so the offsets can be updated
        let mut font_dicts = Vec::with_capacity(cff2.fonts.len());
        for font in &cff2.fonts {
            // Write Local Subr INDEX
            let local_subr_offset = if let Some(local_subr_index) = &font.local_subr_index {
                let local_subr_offset = i32::try_from(ctxt.bytes_written())?;
                MaybeOwnedIndex::write32(ctxt, local_subr_index)?;
                Some(local_subr_offset)
            } else {
                None
            };

            // Write Private DICT
            // NOTE: The offset to local subrs INDEX is from the start of the Private DICT.
            let private_dict_offset = i32::try_from(ctxt.bytes_written())?;
            let mut private_dict_deltas = DictDelta::new();
            if let Some(local_subr_offset) = local_subr_offset {
                private_dict_deltas.push(
                    Operator::Subrs,
                    vec![Operand::Offset(local_subr_offset - private_dict_offset)],
                );
            } else {
                // Ensure local subr offset is omitted from Private DICT
            }
            PrivateDict::write_dep(ctxt, &font.private_dict, private_dict_deltas)?;
            let private_dict_len = i32::try_from(ctxt.bytes_written())? - private_dict_offset;

            // Build and write out new Font DICT
            let mut font_dict = FontDict::new();
            font_dict.inner_mut().push((
                Operator::Private,
                vec![
                    Operand::Offset(private_dict_len),
                    Operand::Offset(private_dict_offset),
                ],
            ));

            let mut font_dict_buffer = WriteBuffer::new();
            FontDict::write_dep(&mut font_dict_buffer, &font_dict, DictDelta::new())?;
            font_dicts.push(font_dict_buffer.into_inner());
        }

        // Font DICT INDEX
        top_dict.replace(
            Operator::FDArray,
            vec![Operand::Offset(i32::try_from(ctxt.bytes_written())?)],
        );
        let font_dict_index = owned::Index { data: font_dicts };
        owned::IndexU32::write(ctxt, &font_dict_index)?;

        // Variation store, if present
        if let Some(variation_store) = &cff2.vstore {
            top_dict.replace(
                Operator::VStore,
                vec![Operand::Offset(i32::try_from(ctxt.bytes_written())?)],
            );
            ItemVariationStore::write(ctxt, variation_store)?;
        }

        // Now that the offsets are known, write out the Top DICT
        ctxt.write_placeholder_dep(top_dict_placeholder, &top_dict, DictDelta::new())?;

        Ok(())
    }
}

fn write_stack<W: WriteContext>(
    new_char_string: &mut W,
    stack: &mut ArgumentsStack<'_, StackValue>,
) -> Result<(), WriteError> {
    stack
        .pop_all()
        .iter()
        .try_for_each(|value| write_stack_value(*value, new_char_string))
}

fn write_stack_value<W: WriteContext>(
    value: StackValue,
    new_char_staring: &mut W,
) -> Result<(), WriteError> {
    StackValue::write(new_char_staring, value)
}

#[derive(Debug, Copy, Clone)]
enum StackValue {
    Int(i16),
    Fixed(Fixed),
}

impl StackValue {
    fn try_as_u16(self) -> Option<u16> {
        match self {
            StackValue::Int(int) => u16::try_from(int).ok(),
            StackValue::Fixed(fixed) => u16::try_num_from(f32::from(fixed)), // TODO: maybe we can do this better
        }
    }
}

impl WriteBinary for StackValue {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, val: Self) -> Result<Self::Output, WriteError> {
        match val {
            // Refer to Table 3 Operand Encoding in section 4 of Technical Note #5176 for details on the
            // integer encoding scheme.
            StackValue::Int(int) => {
                match int {
                    // NOTE: Casts are safe due to patterns limiting range
                    -107..=107 => U8::write(ctxt, (int + 139) as u8),
                    108..=1131 => {
                        let int = int - 108;
                        U8::write(ctxt, ((int >> 8) + 247) as u8)?;
                        U8::write(ctxt, int as u8)
                    }
                    -1131..=-108 => {
                        let int = -int - 108;
                        U8::write(ctxt, ((int >> 8) + 251) as u8)?;
                        U8::write(ctxt, int as u8)
                    }
                    -32768..=32767 => {
                        U8::write(ctxt, operator::SHORT_INT)?;
                        I16Be::write(ctxt, int)
                    }
                }
            }
            StackValue::Fixed(fixed) => {
                U8::write(ctxt, operator::FIXED_16_16)?;
                Fixed::write(ctxt, fixed)
            }
        }
    }
}

impl From<StackValue> for f32 {
    fn from(value: StackValue) -> Self {
        match value {
            StackValue::Int(int) => int as f32, // FIXME in range?
            StackValue::Fixed(fixed) => f32::from(fixed),
        }
    }
}

impl TryFrom<f32> for StackValue {
    type Error = ParseError;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        if value.fract() == 0.0 {
            Ok(StackValue::Int(value as i16))
        } else {
            Ok(StackValue::Fixed(Fixed::from(value))) // FIXME: What if the int part of value is too big for Fixed (> 16-bits)
        }
    }
}

// CharString number parsing functions
fn parse_int1(op: u8) -> Result<StackValue, CFFError> {
    let n = i16::from(op) - 139;
    Ok(StackValue::Int(n))
}

fn parse_int2(op: u8, s: &mut ReadCtxt<'_>) -> Result<StackValue, CFFError> {
    let b1 = s.read::<U8>()?;
    let n = (i16::from(op) - 247) * 256 + i16::from(b1) + 108;
    debug_assert!((108..=1131).contains(&n));
    Ok(StackValue::Int(n))
}

fn parse_int3(op: u8, s: &mut ReadCtxt<'_>) -> Result<StackValue, CFFError> {
    let b1 = s.read::<U8>()?;
    let n = -(i16::from(op) - 251) * 256 - i16::from(b1) - 108;
    debug_assert!((-1131..=-108).contains(&n));
    Ok(StackValue::Int(n))
}

fn parse_fixed(s: &mut ReadCtxt<'_>) -> Result<StackValue, CFFError> {
    let n = s.read::<Fixed>()?;
    Ok(StackValue::Fixed(n))
}

fn conv_subroutine_index(index: StackValue, bias: u16) -> Result<usize, CFFError> {
    let index = match index {
        StackValue::Int(int) => i32::from(int),
        StackValue::Fixed(_fixed) => return Err(CFFError::InvalidSubroutineIndex),
    };
    let bias = i32::from(bias);

    let index = index
        .checked_add(bias)
        .ok_or(CFFError::InvalidSubroutineIndex)?;
    usize::try_from(index).map_err(|_| CFFError::InvalidSubroutineIndex)
}

impl ReadBinary for Header {
    type HostType<'b> = Self;

    fn read(ctxt: &mut ReadCtxt<'_>) -> Result<Self, ParseError> {
        let major = ctxt.read_u8()?;
        ctxt.check(major == 2)?;
        let minor = ctxt.read_u8()?;
        let header_size = ctxt.read_u8()?;
        let top_dict_length = ctxt.read_u16be()?;

        if header_size < 5 {
            return Err(ParseError::BadValue);
        }

        // Skip any unknown data
        let _unknown = ctxt.read_slice((header_size - 5) as usize)?;

        Ok(Header {
            major,
            minor,
            header_size,
            top_dict_length,
        })
    }
}

impl WriteBinary for Header {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, header: Self) -> Result<Self::Output, WriteError> {
        U8::write(ctxt, header.major)?;
        U8::write(ctxt, header.minor)?;
        U8::write(ctxt, 1 + 1 + 1 + 2)?; // header size
        U16Be::write(ctxt, header.top_dict_length)?;
        Ok(())
    }
}

impl ReadBinary for IndexU32 {
    type HostType<'a> = Index<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let count = usize::safe_from(ctxt.read_u32be()?);
        super::read_index(ctxt, count)
    }
}

impl<'a> WriteBinary<&Index<'a>> for IndexU32 {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, index: &Index<'a>) -> Result<(), WriteError> {
        U32Be::write(ctxt, u16::try_from(index.count)?)?;
        super::write_index_body(ctxt, index)
    }
}

impl DictDefault for TopDictDefault {
    fn default(op: Operator) -> Option<&'static [Operand]> {
        match op {
            Operator::FontMatrix => Some(DEFAULT_FONT_MATRIX.as_ref()),
            _ => None,
        }
    }
}

impl DictDefault for FontDictDefault {
    fn default(_op: Operator) -> Option<&'static [Operand]> {
        None
    }
}

impl DictDefault for PrivateDictDefault {
    fn default(op: Operator) -> Option<&'static [Operand]> {
        match op {
            Operator::BlueScale => Some(DEFAULT_BLUE_SCALE.as_ref()),
            Operator::BlueShift => Some(&DEFAULT_BLUE_SHIFT),
            Operator::BlueFuzz => Some(&DEFAULT_BLUE_FUZZ),
            Operator::LanguageGroup => Some(&OPERAND_ZERO),
            Operator::ExpansionFactor => Some(DEFAULT_EXPANSION_FACTOR.as_ref()),
            Operator::VSIndex => Some(&OPERAND_ZERO),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tables::variable_fonts::avar::AvarTable;
    use crate::tables::variable_fonts::fvar::FvarTable;
    use crate::tables::{Fixed, OpenTypeData, OpenTypeFont};
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    fn read_cff2() {
        let buffer = read_fixture("tests/fonts/opentype/cff2/SourceSansVariable-Roman.abc.otf");
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();

        let offset_table = match otf.data {
            OpenTypeData::Single(ttf) => ttf,
            OpenTypeData::Collection(_) => unreachable!(),
        };

        let cff_table_data = offset_table
            .read_table(&otf.scope, tag::CFF2)
            .unwrap()
            .unwrap();
        let cff = cff_table_data
            .read::<CFF2<'_>>()
            .expect("error parsing CFF2 table");

        assert_eq!(cff.header.major, 2);
        let vstore = cff.vstore.as_ref().unwrap();
        println!(
            "vstore: regions {} data {}",
            vstore.variation_region_list.variation_regions.len(),
            vstore.item_variation_data.len()
        );
        for x in 0..vstore.item_variation_data.len() {
            println!("ItemVariationData");
            let ix = x as u16;
            for region in vstore.regions(ix).unwrap() {
                let region = region.unwrap();
                dbg!(region);
            }

            // println!("region_indexes len: {}", x.region_indexes.len());
            // println!("region_indexes_count: {}", x.region_index_count);
            // println!("word_delta_count: {}", x.word_delta_count);
            // println!("delta_sets len: {}", x.delta_sets.len());
        }
    }

    #[test]
    fn instance_char_strings() {
        let buffer = read_fixture("tests/fonts/opentype/cff2/SourceSansVariable-Roman.abc.otf");
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();

        let offset_table = match otf.data {
            OpenTypeData::Single(ttf) => ttf,
            OpenTypeData::Collection(_) => unreachable!(),
        };

        let cff2_table_data = offset_table
            .read_table(&otf.scope, tag::CFF2)
            .unwrap()
            .unwrap();
        let mut cff2 = cff2_table_data
            .read::<CFF2<'_>>()
            .expect("error parsing CFF2 table");
        let fvar_data = offset_table
            .read_table(&otf.scope, tag::FVAR)
            .unwrap()
            .unwrap();
        let fvar = fvar_data
            .read::<FvarTable<'_>>()
            .expect("unable to parse fvar");
        let avar_data = offset_table.read_table(&otf.scope, tag::FVAR).unwrap();
        let avar = avar_data
            .map(|avar_data| avar_data.read::<AvarTable<'_>>())
            .transpose()
            .expect("unable to parse avar table");
        let user_tuple = [Fixed::from(654.0)];
        let normalised_tuple = fvar
            .normalize(user_tuple.iter().copied(), avar.as_ref())
            .expect("unable to normalise user tuple");

        assert!(cff2.instance_char_strings(&normalised_tuple).is_ok());
    }
}
