//! CFF2 font handling.
//!
//! Refer to [OpenType CFF2 spec](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2)
//! for more information.

use std::convert::TryFrom;

use super::{
    read_local_subr_index, Dict, DictDefault, FDSelect, Index, MaybeOwnedIndex, Operand, Operator,
    DEFAULT_BLUE_FUZZ, DEFAULT_BLUE_SCALE, DEFAULT_BLUE_SHIFT, DEFAULT_EXPANSION_FACTOR,
    DEFAULT_FONT_MATRIX, OPERAND_ZERO,
};
use crate::binary::read::{ReadBinary, ReadCtxt, ReadScope};
use crate::error::ParseError;
use crate::tables::variable_fonts::ItemVariationStore;
use crate::SafeFrom;

/// Maximum number of operands to in Top DICT, Font DICTs, Private DICTs and CharStrings.
///
/// > Operators in Top DICT, Font DICTs, Private DICTs and CharStrings may be preceded by up to a
/// > maximum of 513 operands.
pub const MAX_OPERANDS: usize = 513;

/// Top level representation of a CFF2 font file, typically read from a CFF2 OpenType table.
///
/// [OpenType CFF2 spec](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2)
#[derive(Clone)]
pub struct CFF2<'a> {
    pub header: Header,
    pub top_dict: TopDict,
    pub global_subr_index: MaybeOwnedIndex<'a>,
    pub char_strings_index: MaybeOwnedIndex<'a>,
    pub vstore: Option<ItemVariationStore<'a>>,
    pub fd_select: Option<FDSelect<'a>>,
    pub fonts: Vec<Font<'a>>,
}

/// CFF Font Header described in Section 6 of Technical Note #5176
#[derive(Clone, Debug, PartialEq)]
pub struct Header {
    pub major: u8,
    pub minor: u8,
    pub header_size: u8,
    pub top_dict_length: u16,
}

struct IndexU32;

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

impl ReadBinary for IndexU32 {
    type HostType<'a> = Index<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let count = usize::safe_from(ctxt.read_u32be()?);
        super::read_index(ctxt, count)
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
    use crate::tables::{OpenTypeData, OpenTypeFont};
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
    }
}
