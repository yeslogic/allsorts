#![deny(missing_docs)]

//! `LTSH` table parsing and writing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/ltsh>

use std::borrow::Cow;

use crate::binary::read::{ReadBinary, ReadCtxt};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::U16Be;
use crate::error::{ParseError, WriteError};

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;
    use crate::binary::write::WriteBuffer;

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
}
