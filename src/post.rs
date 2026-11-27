//! `post` table parsing, subsetting, and writing.

use std::str;

use rustc_hash::FxHashMap;

use crate::binary::read::{ReadArray, ReadBinary, ReadCtxt};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::{I16Be, I32Be, U16Be, U32Be, U8};
use crate::error::{ParseError, WriteError};
use crate::subset::SubsetGlyphs;

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
    pub glyph_name_index: ReadArray<'a, U16Be>,
    pub names: Vec<PascalString<'a>>,
}

/// A `post` table built for a subset font by [`PostTable::subset`].
pub(crate) struct SubsetPostTable<'a> {
    pub(crate) header: Header,
    pub(crate) opt_sub_table: Option<SubsetSubTable<'a>>,
}

/// The glyph names of a subset version 2.0 `post` table.
pub(crate) struct SubsetSubTable<'a> {
    glyph_name_index: Vec<u16>,
    names: Vec<PascalString<'a>>,
}

#[derive(Clone)]
pub struct PascalString<'a> {
    pub bytes: &'a [u8],
}

impl ReadBinary for Header {
    type HostType<'b> = Self;

    fn read(ctxt: &mut ReadCtxt<'_>) -> Result<Self, ParseError> {
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

impl WriteBinary<&Self> for Header {
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

impl ReadBinary for PostTable<'_> {
    type HostType<'a> = PostTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let header = ctxt.read::<Header>()?;
        let opt_sub_table = match header.version {
            0x00020000 => {
                // May include some Format 1 glyphs
                let num_glyphs = ctxt.read_u16be()?;
                let glyph_name_index = ctxt.read_array(usize::from(num_glyphs))?;

                // Find the largest index used and use that to determine how many names to read
                let names_to_read = glyph_name_index.iter().max().map_or(0, |max| {
                    (usize::from(max) + 1).saturating_sub(FORMAT_1_NAMES.len())
                });

                // Read the names
                let mut names = Vec::with_capacity(names_to_read);
                for _ in 0..names_to_read {
                    let length = ctxt.read_u8()?;
                    let bytes = ctxt.read_slice(usize::from(length))?;
                    names.push(PascalString { bytes });
                }

                Some(SubTable {
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
        let num_glyphs = u16::try_from(table.glyph_name_index.len())?;
        U16Be::write(ctxt, num_glyphs)?;
        <&ReadArray<'_, _>>::write(ctxt, &table.glyph_name_index)?;
        for name in &table.names {
            PascalString::write(ctxt, name)?;
        }

        Ok(())
    }
}

impl<'a> WriteBinary<&Self> for SubsetPostTable<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &SubsetPostTable<'a>) -> Result<(), WriteError> {
        Header::write(ctxt, &table.header)?;
        if let Some(sub_table) = &table.opt_sub_table {
            U16Be::write(ctxt, u16::try_from(sub_table.glyph_name_index.len())?)?;
            for &name_index in &sub_table.glyph_name_index {
                U16Be::write(ctxt, name_index)?;
            }
            for name in &sub_table.names {
                PascalString::write(ctxt, name)?;
            }
        }

        Ok(())
    }
}

impl<'a> WriteBinary<&Self> for PascalString<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, string: &PascalString<'a>) -> Result<(), WriteError> {
        if string.bytes.len() <= usize::from(u8::MAX) {
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
        match &self.header.version {
            0x00010000 => Ok(FORMAT_1_NAMES.get(usize::from(glyph_index)).copied()),
            0x00020000 => match &self.opt_sub_table {
                Some(sub_table) => {
                    let Some(name_index) = sub_table
                        .glyph_name_index
                        .get_item(usize::from(glyph_index))
                        .map(usize::from)
                    else {
                        return Ok(None);
                    };

                    match FORMAT_1_NAMES.get(name_index) {
                        Some(name) => Ok(Some(*name)),
                        None => {
                            let index = name_index - FORMAT_1_NAMES.len();
                            // NOTE: indexing is safe as we read enough names to satisfy the max index
                            // in glyph_name_index.
                            let pascal_string = &sub_table.names[index];
                            pascal_string.to_str().map(Some).ok_or(ParseError::BadValue)
                        }
                    }
                }
                // If the table is version 2, the sub-table should exist
                None => Err(ParseError::BadValue),
            },
            _ => Ok(None),
        }
    }

    /// Create a `post` table for a subset of the glyphs in this font.
    ///
    /// `glyphs` is the subset of the glyphs in this font. If this table has glyph names (versions 1.0 and 2.0) the
    /// result is a version 2.0 table holding the names of the retained glyphs. Otherwise it is a
    /// version 3.0 table, which has no glyph names.
    ///
    /// Glyphs that this table doesn't name are named `.notdef` (name index 0) in the result. These
    /// are the glyphs past the 258 standard Macintosh names of a version 1.0 table, and the glyphs
    /// past those listed by a version 2.0 table.
    pub(crate) fn subset(&self, glyphs: &impl SubsetGlyphs) -> SubsetPostTable<'a> {
        let opt_sub_table = match (self.header.version, &self.opt_sub_table) {
            (0x00010000, _) => Some(SubsetSubTable::from_standard_names(glyphs)),
            (0x00020000, Some(sub_table)) => Some(sub_table.subset(glyphs)),
            _ => None,
        };
        let version = if opt_sub_table.is_some() {
            0x00020000
        } else {
            0x00030000
        };

        SubsetPostTable {
            header: Header {
                version,
                ..self.header
            },
            opt_sub_table,
        }
    }
}

impl<'a> SubsetSubTable<'a> {
    /// Build a sub-table for glyphs named by position in the standard Macintosh ordering, as in a
    /// version 1.0 table. Glyphs past the standard names are named `.notdef`.
    fn from_standard_names(glyphs: &impl SubsetGlyphs) -> Self {
        let glyph_name_index = old_ids(glyphs)
            .map(|glyph_id| {
                if usize::from(glyph_id) < FORMAT_1_NAMES.len() {
                    glyph_id
                } else {
                    0
                }
            })
            .collect();

        SubsetSubTable {
            glyph_name_index,
            names: Vec::new(),
        }
    }
}

impl<'a> SubTable<'a> {
    /// Build a sub-table holding the names of the glyphs in `glyphs`.
    ///
    /// Glyphs named by a standard Macintosh name keep or get its index, even if this sub-table
    /// stores the name. The other names are stored once and are only kept if a retained glyph refers
    /// to them. Glyphs without a name in this sub-table are named `.notdef`.
    fn subset(&self, glyphs: &impl SubsetGlyphs) -> SubsetSubTable<'a> {
        let mut glyph_name_index = Vec::with_capacity(glyphs.len());
        let mut names = Vec::new();
        // Maps the names already in `names` to their indexes, so that a name is stored once even if
        // the source table stores it more than once
        let mut new_name_indexes = FxHashMap::default();

        for glyph_id in old_ids(glyphs) {
            let name_index = self
                .glyph_name_index
                .get_item(usize::from(glyph_id))
                .unwrap_or(0);
            let opt_name = usize::from(name_index)
                .checked_sub(FORMAT_1_NAMES.len())
                .map(|index| self.names.get(index));
            let new_index = match opt_name {
                // Standard Macintosh names are referenced by index and not stored in the table
                None => name_index,
                Some(None) => 0,
                Some(Some(name)) => match standard_name_index(name.bytes) {
                    Some(index) => index,
                    None => *new_name_indexes.entry(name.bytes).or_insert_with(|| {
                        // Each stored name has its own source index, which is at least the number of
                        // standard names, so the new index fits in a `u16`
                        let new_index = (FORMAT_1_NAMES.len() + names.len()) as u16;
                        names.push(name.clone());
                        new_index
                    }),
                },
            };
            glyph_name_index.push(new_index);
        }

        SubsetSubTable {
            glyph_name_index,
            names,
        }
    }
}

/// The id in the source font of each glyph in the subset font, in order of the new glyph ids.
fn old_ids(glyphs: &impl SubsetGlyphs) -> impl Iterator<Item = u16> + '_ {
    (0..=u16::MAX)
        .take(glyphs.len())
        .map(|new_id| glyphs.old_id(new_id))
}

/// Returns the index of `name` among the standard Macintosh glyph names, if it is one.
fn standard_name_index(name: &[u8]) -> Option<u16> {
    FORMAT_1_NAMES
        .iter()
        .position(|standard| standard.as_bytes() == name)
        .map(|index| index as u16)
}

impl<'a> PascalString<'a> {
    /// Returns a `&str` slice if the `PascalString` is valid UTF-8.
    pub fn to_str(&self) -> Option<&'a str> {
        str::from_utf8(self.bytes).ok()
    }
}

static FORMAT_1_NAMES: &[&str; 258] = &[
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
    use crate::binary::write::WriteBuffer;
    use crate::subset::TestSubsetGlyphs;

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

    fn build_post_with_unused_names() -> Result<Vec<u8>, WriteError> {
        // Build a post table with unused name entries
        let mut w = WriteBuffer::new();
        let header = Header {
            version: 0x00020000,
            italic_angle: 0,
            underline_position: 0,
            underline_thickness: 0,
            is_fixed_pitch: 0,
            min_mem_type_42: 0,
            max_mem_type_42: 0,
            min_mem_type_1: 0,
            max_mem_type_1: 0,
        };

        Header::write(&mut w, &header)?;

        let num_glyphs = 10u16;
        U16Be::write(&mut w, num_glyphs)?;

        // Write name indexes that have unused names between each used entry
        U16Be::write(&mut w, 0u16)?; // .notdef
        for i in 0..(num_glyphs - 1) {
            U16Be::write(&mut w, i * 2 + 258)?;
        }
        // Write the names
        for i in 1..num_glyphs {
            // Write a real entry
            let name = format!("gid{}", i);
            let string = PascalString {
                bytes: name.as_bytes(),
            };
            PascalString::write(&mut w, &string)?;

            // Then the unused one in between
            let name = format!("unused{}", i);
            let string = PascalString {
                bytes: name.as_bytes(),
            };
            PascalString::write(&mut w, &string)?;
        }

        Ok(w.into_inner())
    }

    #[test]
    fn unused_glyph_names() {
        let post_data = build_post_with_unused_names().expect("unable to build post table");
        let post = ReadScope::new(&post_data)
            .read::<PostTable<'_>>()
            .expect("unable to parse post table");
        let num_glyphs = post.opt_sub_table.as_ref().unwrap().glyph_name_index.len() as u16;
        for i in 0..num_glyphs {
            let expected = if i == 0 {
                String::from(".notdef")
            } else {
                format!("gid{}", i)
            };
            assert_eq!(post.glyph_name(i).unwrap().unwrap(), &expected);
        }
    }

    fn header(version: i32) -> Header {
        Header {
            version,
            italic_angle: 1,
            underline_position: -2,
            underline_thickness: 3,
            is_fixed_pitch: 0,
            min_mem_type_42: 0,
            max_mem_type_42: 0,
            min_mem_type_1: 0,
            max_mem_type_1: 0,
        }
    }

    /// Build a version 2.0 table with the name indexes `glyph_name_index` and the names `names`.
    fn post_version_2(glyph_name_index: &[u16], names: &[&str]) -> Vec<u8> {
        let mut w = WriteBuffer::new();
        Header::write(&mut w, &header(0x00020000)).unwrap();
        U16Be::write(&mut w, glyph_name_index.len() as u16).unwrap();
        for &index in glyph_name_index {
            U16Be::write(&mut w, index).unwrap();
        }
        for name in names {
            let string = PascalString {
                bytes: name.as_bytes(),
            };
            PascalString::write(&mut w, &string).unwrap();
        }
        w.into_inner()
    }

    /// Subset the `post` table `data` to the glyphs `old_ids` and write the result.
    fn subset_data(data: &[u8], old_ids: &[u16]) -> Vec<u8> {
        let post = ReadScope::new(data).read::<PostTable<'_>>().unwrap();
        let subset = post.subset(&TestSubsetGlyphs::new(old_ids));
        let mut w = WriteBuffer::new();
        SubsetPostTable::write(&mut w, &subset).unwrap();
        w.into_inner()
    }

    fn glyph_names(data: &[u8], num_glyphs: u16) -> Vec<String> {
        let post = ReadScope::new(data).read::<PostTable<'_>>().unwrap();
        (0..num_glyphs)
            .map(|glyph_id| post.glyph_name(glyph_id).unwrap().unwrap().to_string())
            .collect()
    }

    #[test]
    fn subset_version_2_keeps_names_of_retained_glyphs() {
        // Glyph 0 .notdef, 1 space (standard), 2 foo, 3 bar, 4 baz
        let data = post_version_2(&[0, 3, 258, 259, 260], &["foo", "bar", "baz"]);

        // The new glyphs are baz, .notdef, space, foo. The name bar is dropped.
        let subset = subset_data(&data, &[4, 0, 1, 2]);
        let post = ReadScope::new(&subset).read::<PostTable<'_>>().unwrap();
        assert_eq!(post.header.version, 0x00020000);
        assert_eq!(post.header.italic_angle, 1);
        assert_eq!(post.header.underline_position, -2);
        assert_eq!(post.header.underline_thickness, 3);
        let sub_table = post.opt_sub_table.as_ref().unwrap();
        assert_eq!(sub_table.glyph_name_index.len(), 4);
        assert_eq!(sub_table.names.len(), 2);
        assert_eq!(glyph_names(&subset, 4), ["baz", ".notdef", "space", "foo"]);
    }

    #[test]
    fn subset_version_2_stores_repeated_names_once() {
        // Glyphs 1 and 3 refer to the same name by separate indexes, as do 2 and 4
        let data = post_version_2(&[0, 258, 259, 260, 261], &["foo", "qux", "foo", "qux"]);

        let subset = subset_data(&data, &[1, 2, 3, 4]);
        let post = ReadScope::new(&subset).read::<PostTable<'_>>().unwrap();
        assert_eq!(post.opt_sub_table.as_ref().unwrap().names.len(), 2);
        assert_eq!(glyph_names(&subset, 4), ["foo", "qux", "foo", "qux"]);
    }

    #[test]
    fn subset_version_2_uses_standard_indexes_for_standard_names() {
        // Glyph 1 is named "A" by a stored name, instead of by its standard index
        let data = post_version_2(&[0, 258, 259], &["A", "other"]);

        let subset = subset_data(&data, &[1, 2]);
        let post = ReadScope::new(&subset).read::<PostTable<'_>>().unwrap();
        let sub_table = post.opt_sub_table.as_ref().unwrap();
        assert_eq!(sub_table.glyph_name_index.iter().next(), Some(36));
        assert_eq!(sub_table.names.len(), 1);
        assert_eq!(glyph_names(&subset, 2), ["A", "other"]);
    }

    #[test]
    fn subset_version_2_names_unlisted_glyphs_notdef() {
        // The table lists 2 glyphs, and the glyph 5 of the font has no entry
        let data = post_version_2(&[0, 3], &[]);

        let subset = subset_data(&data, &[1, 5]);
        assert_eq!(glyph_names(&subset, 2), ["space", ".notdef"]);
    }

    #[test]
    fn subset_version_1_is_written_as_version_2() {
        let mut w = WriteBuffer::new();
        Header::write(&mut w, &header(0x00010000)).unwrap();
        let data = w.into_inner();

        // Glyph 3 is "space", glyph 36 "A", and glyph 300 is past the standard names
        let subset = subset_data(&data, &[36, 3, 300]);
        let post = ReadScope::new(&subset).read::<PostTable<'_>>().unwrap();
        assert_eq!(post.header.version, 0x00020000);
        assert!(post.opt_sub_table.as_ref().unwrap().names.is_empty());
        assert_eq!(glyph_names(&subset, 3), ["A", "space", ".notdef"]);
    }

    #[test]
    fn subset_version_3_has_no_names() {
        let mut w = WriteBuffer::new();
        Header::write(&mut w, &header(0x00030000)).unwrap();
        let data = w.into_inner();

        let subset = subset_data(&data, &[0, 1]);
        assert_eq!(subset.len(), 32);
        let post = ReadScope::new(&subset).read::<PostTable<'_>>().unwrap();
        assert_eq!(post.header.version, 0x00030000);
        assert!(post.opt_sub_table.is_none());
    }
}
