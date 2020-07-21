#![deny(missing_docs)]

//! Standard Bitmap Graphics Table (`sbix`).
//!
//! References:
//!
//! * [Microsoft](https://docs.microsoft.com/en-us/typography/opentype/spec/sbix)
//! * [Apple](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6sbix.html)

use std::convert::TryFrom;

use crate::binary::read::{CheckIndex, ReadArray, ReadBinaryDep, ReadCtxt, ReadScope};
use crate::binary::U32Be;
use crate::error::ParseError;

/// `sbix` table containing bitmaps.
pub struct Sbix<'a> {
    /// Drawing flags.
    ///
    /// * Bit 0: Set to 1.
    /// * Bit 1: Draw outlines.
    /// * Bits 2 to 15: reserved (set to 0).
    pub flags: u16,
    /// Bitmap data for different sizes.
    pub strikes: Vec<SbixStrike<'a>>,
}

/// A single strike (ppem/ppi) combination.
pub struct SbixStrike<'a> {
    scope: ReadScope<'a>,
    /// The PPEM size for which this strike was designed.
    pub ppem: u16,
    /// The device pixel density (in pixels-per-inch) for which this strike was designed.
    pub ppi: u16,
    /// Offset from the beginning of the strike data header to bitmap data for an individual glyph.
    glyph_data_offsets: ReadArray<'a, U32Be>,
}

/// An `sbix` bitmap glyph.
pub struct SbixGlyph<'a> {
    /// The horizontal (x-axis) offset from the left edge of the graphic to the glyph’s origin.
    ///
    /// That is, the x-coordinate of the point on the baseline at the left edge of the glyph.
    pub origin_offset_x: u16,
    /// The vertical (y-axis) offset from the bottom edge of the graphic to the glyph’s origin.
    ///
    /// That is, the y-coordinate of the point on the baseline at the left edge of the glyph.
    pub origin_offset_y: u16,
    /// Indicates the format of the embedded graphic data.
    ///
    /// One of `jpg `, `png ` or `tiff`, or the special format `dupe`.
    pub graphic_type: u32,
    /// The actual embedded graphic data.
    pub data: &'a [u8],
}

impl<'a> ReadBinaryDep<'a> for Sbix<'a> {
    type Args = usize; // num_glyphs
    type HostType = Self;

    /// Read the `sbix` table.
    ///
    /// `num_glyphs` should be read from the `maxp` table.
    fn read_dep(ctxt: &mut ReadCtxt<'a>, num_glyphs: usize) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let version = ctxt.read_u16be()?;
        ctxt.check(version == 1)?;
        let flags = ctxt.read_u16be()?;
        let num_strikes = usize::try_from(ctxt.read_u32be()?)?;
        let strike_offsets = ctxt.read_array::<U32Be>(num_strikes)?;
        let strikes = strike_offsets
            .iter()
            .map(|offset| {
                let offset = usize::try_from(offset)?;
                scope.offset(offset).read_dep::<SbixStrike<'_>>(num_glyphs)
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Sbix { flags, strikes })
    }
}

impl<'a> ReadBinaryDep<'a> for SbixStrike<'a> {
    type Args = usize; // num_glyphs
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, num_glyphs: usize) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let ppem = ctxt.read_u16be()?;
        let ppi = ctxt.read_u16be()?;
        // The glyph_data_offset array includes offsets for every glyph ID, plus one extra.
        // — https://docs.microsoft.com/en-us/typography/opentype/spec/sbix#strikes
        let glyph_data_offsets = ctxt.read_array::<U32Be>(num_glyphs + 1)?;

        Ok(SbixStrike {
            scope,
            ppem,
            ppi,
            glyph_data_offsets,
        })
    }
}

impl<'a> ReadBinaryDep<'a> for SbixGlyph<'a> {
    type Args = usize; // data_len
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, length: usize) -> Result<Self, ParseError> {
        let origin_offset_x = ctxt.read_u16be()?;
        let origin_offset_y = ctxt.read_u16be()?;
        let graphic_type = ctxt.read_u32be()?;
        // The length of the glyph data is the length of the glyph minus the preceding
        // header fields:
        // * u16 origin_offset_x  = 2
        // * u16 origin_offset_y  = 2
        // * u32 graphic_type     = 4
        // TOTAL:                 = 8
        let data = ctxt.read_slice(length - 8)?;

        Ok(SbixGlyph {
            origin_offset_x,
            origin_offset_y,
            graphic_type,
            data,
        })
    }
}

impl<'a> SbixStrike<'a> {
    /// Read a glyph from this strike specified by `glyph_index`.
    pub fn read_glyph(&self, glyph_index: u16) -> Result<Option<SbixGlyph<'a>>, ParseError> {
        let glyph_index = usize::from(glyph_index);
        // The length of the bitmap data for each glyph is variable, and can be determined from the
        // difference between two consecutive offsets. Hence, the length of data for glyph N is
        // glyph_data_offset[N+1] - glyph_data_offset[N].
        // — https://docs.microsoft.com/en-us/typography/opentype/spec/sbix#strikes
        self.glyph_data_offsets.check_index(glyph_index)?;
        self.glyph_data_offsets.check_index(glyph_index + 1)?;
        let offset = usize::try_from(self.glyph_data_offsets.get_item(glyph_index))?;
        let end = usize::try_from(self.glyph_data_offsets.get_item(glyph_index + 1))?;
        match end.checked_sub(offset) {
            Some(0) => {
                // If this is zero, there is no bitmap data for that glyph in this strike.
                // — https://docs.microsoft.com/en-us/typography/opentype/spec/sbix#strikes
                Ok(None)
            }
            Some(length) => {
                let data = self.scope.offset_length(offset, length)?;
                data.read_dep::<SbixGlyph<'_>>(length).map(Some)
            }
            None => Err(ParseError::BadOffset),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::binary::read::ReadScope;
    use crate::fontfile::FontFile;
    use crate::tables::{FontTableProvider, MaxpTable};
    use crate::tag;

    use crate::tests::read_fixture;

    #[test]
    fn test_read_sbix() {
        let buffer = read_fixture("tests/fonts/woff1/chromacheck-sbix.woff");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontFile<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let maxp_data = table_provider
            .read_table_data(tag::MAXP)
            .expect("unable to read maxp table data");
        let maxp = ReadScope::new(&maxp_data).read::<MaxpTable>().unwrap();
        let sbix_data = table_provider
            .read_table_data(tag::SBIX)
            .expect("unable to read sbix table data");
        let sbix = ReadScope::new(&sbix_data)
            .read_dep::<Sbix<'_>>(usize::try_from(maxp.num_glyphs).unwrap())
            .unwrap();

        // Header
        assert_eq!(sbix.flags, 1);
        assert_eq!(sbix.strikes.len(), 1);

        // Strikes
        let strike = &sbix.strikes[0];
        assert_eq!(strike.ppem, 300);
        assert_eq!(strike.ppi, 72);

        // Glyphs
        let glyphs = (0..maxp.num_glyphs)
            .map(|glyph_index| strike.read_glyph(glyph_index))
            .collect::<Result<Vec<_>, _>>()
            .expect("unable to read glyph");

        assert_eq!(glyphs.len(), 2);
        assert!(glyphs[0].is_none());
        if let Some(ref glyph) = glyphs[1] {
            assert_eq!(glyph.origin_offset_x, 0);
            assert_eq!(glyph.origin_offset_y, 0);
            assert_eq!(glyph.graphic_type, tag::from_string("png ").unwrap());
            assert_eq!(glyph.data.len(), 224);
            assert_eq!(*glyph.data.last().unwrap(), 0x82);
        } else {
            panic!("expected Some(SbixGlyph) got None");
        }
    }
}
