#![deny(missing_docs)]

//! `LTSH` table parsing, subsetting, and writing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/ltsh>

use std::borrow::Cow;

use crate::binary::read::{ReadBinary, ReadCtxt};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::U16Be;
use crate::error::{ParseError, WriteError};
use crate::subset::SubsetGlyphs;

/// `LTSH` Linear Threshold Table
///
/// Holds the pixel size from which the advance width of each glyph scales linearly.
#[derive(Debug, Clone)]
pub struct LtshTable<'a> {
    /// The vertical pixel size (ppem) at and above which the advance width of the glyph scales
    /// linearly, indexed by glyph id.
    pub y_pixels: Cow<'a, [u8]>,
}

impl ReadBinary for LtshTable<'_> {
    type HostType<'a> = LtshTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let version = ctxt.read_u16be()?;
        ctxt.check_version(version == 0)?;
        let num_glyphs = ctxt.read_u16be()?;
        let y_pixels = ctxt.read_slice(usize::from(num_glyphs))?;

        Ok(LtshTable {
            y_pixels: Cow::Borrowed(y_pixels),
        })
    }
}

impl WriteBinary<&Self> for LtshTable<'_> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &Self) -> Result<(), WriteError> {
        U16Be::write(ctxt, 0u16)?; // version
        U16Be::write(ctxt, u16::try_from(table.y_pixels.len())?)?;
        ctxt.write_bytes(&table.y_pixels)
    }
}

impl LtshTable<'_> {
    /// Create an `LTSH` table for a subset of the glyphs in this font.
    ///
    /// `glyphs` is the subset of the glyphs in this font.
    pub(crate) fn subset<'b>(
        &self,
        glyphs: &impl SubsetGlyphs,
    ) -> Result<LtshTable<'b>, ParseError> {
        let y_pixels = (0..glyphs.len())
            .map(|new_id| {
                let old_id = glyphs.old_id(u16::try_from(new_id).ok()?);
                self.y_pixels.get(usize::from(old_id)).copied()
            })
            .collect::<Option<Vec<_>>>()
            .ok_or(ParseError::BadIndex)?;

        Ok(LtshTable {
            y_pixels: Cow::Owned(y_pixels),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;
    use crate::binary::write::WriteBuffer;
    use crate::subset::TestSubsetGlyphs;

    #[test]
    fn write_then_read() {
        let table = LtshTable {
            y_pixels: Cow::Borrowed(&[1, 8, 255, 0]),
        };
        let mut w = WriteBuffer::new();
        LtshTable::write(&mut w, &table).unwrap();
        let data = w.into_inner();
        assert_eq!(data, [0, 0, 0, 4, 1, 8, 255, 0]);

        let read = ReadScope::new(&data).read::<LtshTable<'_>>().unwrap();
        assert_eq!(&*read.y_pixels, &[1, 8, 255, 0]);
    }

    #[test]
    fn read_empty() {
        let read = ReadScope::new(&[0, 0, 0, 0])
            .read::<LtshTable<'_>>()
            .unwrap();
        assert!(read.y_pixels.is_empty());
    }

    #[test]
    fn read_truncated() {
        assert!(ReadScope::new(&[0, 0, 0, 3, 1, 2])
            .read::<LtshTable<'_>>()
            .is_err());
    }

    #[test]
    fn read_unknown_version() {
        assert!(ReadScope::new(&[0, 1, 0, 0])
            .read::<LtshTable<'_>>()
            .is_err());
    }

    #[test]
    fn subset_keeps_values_of_retained_glyphs_in_new_order() {
        let table = LtshTable {
            y_pixels: Cow::Borrowed(&[1, 2, 3, 4, 5]),
        };
        let subset = table.subset(&TestSubsetGlyphs::new(&[4, 0, 2])).unwrap();
        assert_eq!(&*subset.y_pixels, &[5, 1, 3]);

        let subset = table.subset(&TestSubsetGlyphs::new(&[])).unwrap();
        assert!(subset.y_pixels.is_empty());
    }

    #[test]
    fn subset_glyph_outside_table_is_an_error() {
        let table = LtshTable {
            y_pixels: Cow::Borrowed(&[1, 2, 3]),
        };
        assert!(table.subset(&TestSubsetGlyphs::new(&[0, 3])).is_err());
    }

    #[test]
    fn subset_then_write() {
        let table = LtshTable {
            y_pixels: Cow::Borrowed(&[1, 2, 3, 4]),
        };
        let subset = table.subset(&TestSubsetGlyphs::new(&[3, 1])).unwrap();
        let mut w = WriteBuffer::new();
        LtshTable::write(&mut w, &subset).unwrap();
        assert_eq!(w.bytes(), [0, 0, 0, 2, 4, 2]);
    }
}
