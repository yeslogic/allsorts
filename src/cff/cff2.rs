//! CFF2 font handling.
//!
//! Refer to [OpenType CFF2 spec](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2)
//! for more information.

use std::convert::TryFrom;
use std::fmt::Debug;

use super::{
    owned, read_local_subr_index, CFFError, CFFFont, Dict, DictDefault, DictDelta, FDSelect, Index,
    IndexU32, MaybeOwnedIndex, Operand, Operator, DEFAULT_BLUE_FUZZ, DEFAULT_BLUE_SCALE,
    DEFAULT_BLUE_SHIFT, DEFAULT_EXPANSION_FACTOR, DEFAULT_FONT_MATRIX, OFFSET_ZERO, OPERAND_ZERO,
};
use crate::binary::read::{ReadBinary, ReadCtxt, ReadScope};
use crate::binary::write::{WriteBinary, WriteBinaryDep, WriteBuffer, WriteContext};
use crate::binary::{I16Be, U16Be, U32Be, U8};
use crate::cff::charstring::{
    operator, ArgumentsStack, CharStringVisitor, CharStringVisitorContext, TryNumFrom,
    VariableCharStringVisitorContext, VisitOp, MAX_ARGUMENTS_STACK_LEN, TWO_BYTE_OPERATOR_MARK,
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

struct CharStringInstancer<'a> {
    new_char_string: &'a mut WriteBuffer,
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
            .ok_or(CFFError::MissingVariationStore)?;

        // for char_string in font, apply variations
        for glyph_id in 0..self.char_strings_index.len() as u16 {
            let mut new_char_string = WriteBuffer::new();

            let font_index = match &self.fd_select {
                Some(fd_select) => fd_select
                    .font_dict_index(glyph_id)
                    .ok_or(CFFError::InvalidFontIndex)?,
                None => 0,
            };
            let font = self
                .fonts
                .get(usize::from(font_index))
                .ok_or(CFFError::InvalidFontIndex)?;

            // TODO: For unchanged char_strings can we use Cow
            let mut instancer = CharStringInstancer {
                new_char_string: &mut new_char_string,
            };
            let variable = VariableCharStringVisitorContext { vstore, instance };
            let mut ctx = CharStringVisitorContext::new(
                glyph_id,
                &self.char_strings_index,
                font.local_subr_index.as_ref(),
                &self.global_subr_index,
                Some(variable),
            );

            stack.clear();

            ctx.visit(CFFFont::CFF2(font), &mut stack, &mut instancer)?;

            new_char_strings.push(new_char_string.into_inner());
        }

        // All subroutines should have been inlined, so they can be dropped now
        for font in self.fonts.iter_mut() {
            font.local_subr_index = None;
            font.private_dict.remove(Operator::Subrs);
            // The Private DICT has to be instanced too since it can also contain a blend operator
            font.private_dict = font.private_dict.instance(instance, vstore)?;
        }
        // Global subr INDEX is required so make it empty
        self.global_subr_index = MaybeOwnedIndex::Owned(owned::Index { data: Vec::new() });
        self.char_strings_index = MaybeOwnedIndex::Owned(owned::Index {
            data: new_char_strings,
        });

        Ok(())
    }
}

impl<'a> CharStringVisitor<StackValue, VariationError> for CharStringInstancer<'a> {
    fn visit(
        &mut self,
        op: VisitOp,
        stack: &ArgumentsStack<'_, StackValue>,
    ) -> Result<(), VariationError> {
        match op {
            VisitOp::HorizontalStem
            | VisitOp::VerticalStem
            | VisitOp::HorizontalStemHintMask
            | VisitOp::VerticalStemHintMask
            | VisitOp::VerticalMoveTo
            | VisitOp::LineTo
            | VisitOp::HorizontalLineTo
            | VisitOp::VerticalLineTo
            | VisitOp::CurveTo
            | VisitOp::HintMask
            | VisitOp::CounterMask
            | VisitOp::MoveTo
            | VisitOp::HorizontalMoveTo
            | VisitOp::CurveLine
            | VisitOp::LineCurve
            | VisitOp::VvCurveTo
            | VisitOp::HhCurveTo
            | VisitOp::VhCurveTo
            | VisitOp::HvCurveTo => {
                write_stack(self.new_char_string, stack)?;
                Ok(U8::write(self.new_char_string, op)?)
            }
            VisitOp::Return | VisitOp::Endchar => {
                // Removed in CFF2
                Err(CFFError::InvalidOperator.into())
            }
            VisitOp::Hflex | VisitOp::Flex | VisitOp::Hflex1 | VisitOp::Flex1 => {
                write_stack(self.new_char_string, stack)?;
                U8::write(self.new_char_string, TWO_BYTE_OPERATOR_MARK)?;
                Ok(U8::write(self.new_char_string, op)?)
            }
            VisitOp::VsIndex | VisitOp::Blend => Ok(()),
        }
    }

    fn hint_data(&mut self, _op: VisitOp, hints: &[u8]) -> Result<(), VariationError> {
        Ok(self.new_char_string.write_bytes(hints)?)
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

fn write_stack(
    new_char_string: &mut WriteBuffer,
    stack: &ArgumentsStack<'_, StackValue>,
) -> Result<(), WriteError> {
    stack
        .all()
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

impl BlendOperand for StackValue {
    fn try_as_i32(self) -> Option<i32> {
        match self {
            StackValue::Int(int) => Some(i32::from(int)),
            StackValue::Fixed(fixed) => i32::try_num_from(f32::from(fixed)),
        }
    }
    fn try_as_u16(self) -> Option<u16> {
        match self {
            StackValue::Int(int) => u16::try_from(int).ok(),
            StackValue::Fixed(fixed) => u16::try_num_from(f32::from(fixed)),
        }
    }

    fn try_as_u8(self) -> Option<u8> {
        match self {
            StackValue::Int(int) => u8::try_from(int).ok(),
            StackValue::Fixed(fixed) => u8::try_num_from(f32::from(fixed)),
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

impl From<f32> for StackValue {
    fn from(value: f32) -> Self {
        if value.fract() == 0.0 {
            StackValue::Int(value as i16)
        } else {
            StackValue::Fixed(Fixed::from(value))
        }
    }
}

impl From<i16> for StackValue {
    fn from(value: i16) -> Self {
        StackValue::Int(value)
    }
}

impl From<Fixed> for StackValue {
    fn from(value: Fixed) -> Self {
        StackValue::Fixed(value)
    }
}

pub(crate) trait BlendOperand:
    Debug + Copy + Into<f32> + From<f32> + From<i16> + From<Fixed>
{
    fn try_as_i32(self) -> Option<i32>;

    fn try_as_u16(self) -> Option<u16>;

    fn try_as_u8(self) -> Option<u8>;
}

pub(super) fn blend<T: BlendOperand>(
    vs_index: u16,
    vstore: &ItemVariationStore<'_>,
    instance: &OwnedTuple,
    stack: &mut ArgumentsStack<'_, T>,
) -> Result<(), CFFError> {
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
    let n = stack
        .pop()
        .try_as_u16()
        .map(usize::from)
        .ok_or_else(|| CFFError::InvalidOperand)?;

    let num_operands = n * (k + 1);
    if stack.len() < num_operands {
        return Err(CFFError::InvalidArgumentsStackLength);
    }

    // Process n*k operands applying the scalars
    let mut blended = vec![0.0; n];
    let operands = stack.pop_n(num_operands);

    // for each set of deltas apply the scalar and calculate a new delta to
    // apply to the default values
    let (defaults, rest) = operands.split_at(n);
    for (adjustment, deltas) in blended.iter_mut().zip(rest.chunks(k)) {
        for (delta, scalar) in deltas.iter().copied().zip(scalars.iter()) {
            if let Some(scalar) = scalar {
                *adjustment += scalar * delta.into();
            }
        }
    }

    // apply the deltas to the default values
    defaults
        .iter()
        .copied()
        .zip(blended.iter_mut())
        .for_each(|(default, delta)| *delta = default.into() + *delta);

    // push the blended values back onto the stack
    blended
        .into_iter()
        .try_for_each(|value| stack.push(T::from(value)))
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
