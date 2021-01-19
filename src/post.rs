//! `post` table parsing and writing.

use std::str;

use rustc_hash::FxHashSet;

use crate::binary::read::{ReadArray, ReadBinary, ReadCtxt};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::{I16Be, I32Be, U16Be, U32Be, U8};
use crate::error::{ParseError, WriteError};

pub struct PostTable<'a> {
    pub header: Header,
    pub opt_sub_table: Option<SubTable<'a>>,
}

pub struct Header {
    pub version: i32,
    pub italic_angle: i32,
    pub underline_position: i16,
    pub underline_thickness: i16,
    pub is_fixed_pitch: u32,
    pub min_mem_type_42: u32,
    pub max_mem_type_42: u32,
    pub min_mem_type_1: u32,
    pub max_mem_type_1: u32,
}

pub struct SubTable<'a> {
    pub num_glyphs: u16,
    pub glyph_name_index: ReadArray<'a, U16Be>,
    pub names: Vec<PascalString<'a>>,
}

#[derive(Clone)]
pub struct PascalString<'a> {
    pub bytes: &'a [u8],
}

impl<'a> ReadBinary<'a> for Header {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let version = ctxt.read_i32be()?;
        let italic_angle = ctxt.read_i32be()?;
        let underline_position = ctxt.read_i16be()?;
        let underline_thickness = ctxt.read_i16be()?;
        let is_fixed_pitch = ctxt.read_u32be()?;
        let min_mem_type_42 = ctxt.read_u32be()?;
        let max_mem_type_42 = ctxt.read_u32be()?;
        let min_mem_type_1 = ctxt.read_u32be()?;
        let max_mem_type_1 = ctxt.read_u32be()?;

        Ok(Header {
            version,
            italic_angle,
            underline_position,
            underline_thickness,
            is_fixed_pitch,
            min_mem_type_42,
            max_mem_type_42,
            min_mem_type_1,
            max_mem_type_1,
        })
    }
}

impl<'a> WriteBinary<&Self> for Header {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &Header) -> Result<(), WriteError> {
        I32Be::write(ctxt, table.version)?;
        I32Be::write(ctxt, table.italic_angle)?;
        I16Be::write(ctxt, table.underline_position)?;
        I16Be::write(ctxt, table.underline_thickness)?;
        U32Be::write(ctxt, table.is_fixed_pitch)?;
        U32Be::write(ctxt, table.min_mem_type_42)?;
        U32Be::write(ctxt, table.max_mem_type_42)?;
        U32Be::write(ctxt, table.min_mem_type_1)?;
        U32Be::write(ctxt, table.max_mem_type_1)?;

        Ok(())
    }
}

impl<'a> ReadBinary<'a> for PostTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let header = ctxt.read::<Header>()?;
        let opt_sub_table = match header.version {
            0x00020000 => {
                // May include some Format 1 glyphs
                let num_glyphs = ctxt.read_u16be()?;
                let num_glyphs_usize = usize::from(num_glyphs);
                let glyph_name_index = ctxt.read_array(num_glyphs_usize)?;

                let mut names = Vec::with_capacity(num_glyphs_usize);
                let mut seen =
                    FxHashSet::with_capacity_and_hasher(num_glyphs_usize, Default::default());
                for index in glyph_name_index.iter() {
                    // Skip standard names and indexes that we've already seen
                    if usize::from(index) < FORMAT_1_NAMES.len() || seen.contains(&index) {
                        continue;
                    }

                    let length = ctxt.read_u8()?;
                    let bytes = ctxt.read_slice(usize::from(length))?;
                    names.push(PascalString { bytes });
                    seen.insert(index);
                }

                // names was over provisioned so try to discard unused capacity
                names.shrink_to_fit();

                Some(SubTable {
                    num_glyphs,
                    glyph_name_index,
                    names,
                })
            }
            // TODO Handle post version 1.0, 2.5, 3.0
            0x00010000 | 0x00025000 | 0x00030000 => None,
            _ => return Err(ParseError::BadVersion),
        };

        Ok(PostTable {
            header,
            opt_sub_table,
        })
    }
}

impl<'a> WriteBinary<&Self> for PostTable<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &PostTable<'a>) -> Result<(), WriteError> {
        Header::write(ctxt, &table.header)?;
        if let Some(sub_table) = &table.opt_sub_table {
            SubTable::write(ctxt, sub_table)?;
        }

        Ok(())
    }
}

impl<'a> WriteBinary<&Self> for SubTable<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &SubTable<'a>) -> Result<(), WriteError> {
        U16Be::write(ctxt, table.num_glyphs)?;
        <&ReadArray<'_, _>>::write(ctxt, &table.glyph_name_index)?;
        for name in &table.names {
            PascalString::write(ctxt, name)?;
        }

        Ok(())
    }
}

impl<'a> WriteBinary<&Self> for PascalString<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, string: &PascalString<'a>) -> Result<(), WriteError> {
        if string.bytes.len() <= usize::from(std::u8::MAX) {
            // cast is safe due to check above
            U8::write(ctxt, string.bytes.len() as u8)?;
            ctxt.write_bytes(string.bytes)?;
            Ok(())
        } else {
            Err(WriteError::BadValue)
        }
    }
}

impl<'a> PostTable<'a> {
    /// Retrieve the glyph name for the supplied `glyph_index`.
    ///
    /// **Note:** Some fonts map more than one glyph to the same name so don't assume names are
    /// unique.
    pub fn glyph_name(&self, glyph_index: u16) -> Result<Option<&'a str>, ParseError> {
        if let Some(sub_table) = &self.opt_sub_table {
            if glyph_index >= sub_table.num_glyphs {
                return Ok(None);
            }
        }

        match &self.header.version {
            0x00010000 if usize::from(glyph_index) < FORMAT_1_NAMES.len() => {
                let name = FORMAT_1_NAMES[usize::from(glyph_index)];
                Ok(Some(name))
            }
            0x00020000 => match &self.opt_sub_table {
                Some(sub_table) => {
                    let name_index = sub_table
                        .glyph_name_index
                        .get_item(usize::from(glyph_index));

                    if usize::from(name_index) < FORMAT_1_NAMES.len() {
                        Ok(Some(FORMAT_1_NAMES[usize::from(name_index)]))
                    } else {
                        let index = usize::from(name_index) - FORMAT_1_NAMES.len();
                        let pascal_string = &sub_table.names[index];

                        match str::from_utf8(pascal_string.bytes) {
                            Ok(name) => Ok(Some(name)),
                            Err(_) => Err(ParseError::BadValue),
                        }
                    }
                }
                // If the table is version 2, the sub-table should exist
                None => Err(ParseError::BadValue),
            },
            _ => Ok(None),
        }
    }
}

static FORMAT_1_NAMES: &'static [&'static str; 258] = &[
    ".notdef",
    ".null",
    "nonmarkingreturn",
    "space",
    "exclam",
    "quotedbl",
    "numbersign",
    "dollar",
    "percent",
    "ampersand",
    "quotesingle",
    "parenleft",
    "parenright",
    "asterisk",
    "plus",
    "comma",
    "hyphen",
    "period",
    "slash",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "colon",
    "semicolon",
    "less",
    "equal",
    "greater",
    "question",
    "at",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "bracketleft",
    "backslash",
    "bracketright",
    "asciicircum",
    "underscore",
    "grave",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "braceleft",
    "bar",
    "braceright",
    "asciitilde",
    "Adieresis",
    "Aring",
    "Ccedilla",
    "Eacute",
    "Ntilde",
    "Odieresis",
    "Udieresis",
    "aacute",
    "agrave",
    "acircumflex",
    "adieresis",
    "atilde",
    "aring",
    "ccedilla",
    "eacute",
    "egrave",
    "ecircumflex",
    "edieresis",
    "iacute",
    "igrave",
    "icircumflex",
    "idieresis",
    "ntilde",
    "oacute",
    "ograve",
    "ocircumflex",
    "odieresis",
    "otilde",
    "uacute",
    "ugrave",
    "ucircumflex",
    "udieresis",
    "dagger",
    "degree",
    "cent",
    "sterling",
    "section",
    "bullet",
    "paragraph",
    "germandbls",
    "registered",
    "copyright",
    "trademark",
    "acute",
    "dieresis",
    "notequal",
    "AE",
    "Oslash",
    "infinity",
    "plusminus",
    "lessequal",
    "greaterequal",
    "yen",
    "mu",
    "partialdiff",
    "summation",
    "product",
    "pi",
    "integral",
    "ordfeminine",
    "ordmasculine",
    "Omega",
    "ae",
    "oslash",
    "questiondown",
    "exclamdown",
    "logicalnot",
    "radical",
    "florin",
    "approxequal",
    "Delta",
    "guillemotleft",
    "guillemotright",
    "ellipsis",
    "nonbreakingspace",
    "Agrave",
    "Atilde",
    "Otilde",
    "OE",
    "oe",
    "endash",
    "emdash",
    "quotedblleft",
    "quotedblright",
    "quoteleft",
    "quoteright",
    "divide",
    "lozenge",
    "ydieresis",
    "Ydieresis",
    "fraction",
    "currency",
    "guilsinglleft",
    "guilsinglright",
    "fi",
    "fl",
    "daggerdbl",
    "periodcentered",
    "quotesinglbase",
    "quotedblbase",
    "perthousand",
    "Acircumflex",
    "Ecircumflex",
    "Aacute",
    "Edieresis",
    "Egrave",
    "Iacute",
    "Icircumflex",
    "Idieresis",
    "Igrave",
    "Oacute",
    "Ocircumflex",
    "apple",
    "Ograve",
    "Uacute",
    "Ucircumflex",
    "Ugrave",
    "dotlessi",
    "circumflex",
    "tilde",
    "macron",
    "breve",
    "dotaccent",
    "ring",
    "cedilla",
    "hungarumlaut",
    "ogonek",
    "caron",
    "Lslash",
    "lslash",
    "Scaron",
    "scaron",
    "Zcaron",
    "zcaron",
    "brokenbar",
    "Eth",
    "eth",
    "Yacute",
    "yacute",
    "Thorn",
    "thorn",
    "minus",
    "multiply",
    "onesuperior",
    "twosuperior",
    "threesuperior",
    "onehalf",
    "onequarter",
    "threequarters",
    "franc",
    "Gbreve",
    "gbreve",
    "Idotaccent",
    "Scedilla",
    "scedilla",
    "Cacute",
    "cacute",
    "Ccaron",
    "ccaron",
    "dcroat",
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;

    #[test]
    fn duplicate_glyph_names() {
        // Test for post table that maps multiple glyphs to the same name index. Before a fix was
        // implemented this table failed to parse.
        let post_data = include_bytes!("../tests/fonts/opentype/post.bin");
        let post = ReadScope::new(post_data)
            .read::<PostTable<'_>>()
            .expect("unable to parse post table");
        match post.opt_sub_table {
            Some(ref sub_table) => assert_eq!(sub_table.names.len(), 1872),
            None => panic!("expected post table to have a sub-table"),
        }

        // These map to the same index (397)
        assert_eq!(post.glyph_name(257).unwrap().unwrap(), "Ldot");
        assert_eq!(post.glyph_name(1442).unwrap().unwrap(), "Ldot");
    }
}
