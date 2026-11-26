#![deny(missing_docs)]

//! Standard Bitmap Graphics Table (`sbix`) parsing, subsetting, and writing.
//!
//! References:
//!
//! * [Microsoft](https://docs.microsoft.com/en-us/typography/opentype/spec/sbix)
//! * [Apple](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6sbix.html)

use super::{
    BitDepth, Bitmap, BitmapGlyph, EncapsulatedBitmap, EncapsulatedFormat, Metrics, OriginOffset,
};
use crate::binary::read::{ReadArray, ReadBinaryDep, ReadCtxt, ReadScope, ReadUnchecked};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::{I16Be, U16Be, U32Be};
use crate::error::{ParseError, WriteError};
use crate::subset::SubsetGlyphs;
use crate::tag;
use crate::SafeFrom;

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
    pub origin_offset_x: i16,
    /// The vertical (y-axis) offset from the bottom edge of the graphic to the glyph’s origin.
    ///
    /// That is, the y-coordinate of the point on the baseline at the left edge of the glyph.
    pub origin_offset_y: i16,
    /// Indicates the format of the embedded graphic data.
    ///
    /// One of `jpg `, `png ` or `tiff`; or the special formats `dupe` or `flip`.
    pub graphic_type: u32,
    /// The actual embedded graphic data.
    pub data: &'a [u8],
}

impl ReadBinaryDep for Sbix<'_> {
    type Args<'a> = usize; // num_glyphs
    type HostType<'a> = Sbix<'a>;

    /// Read the `sbix` table.
    ///
    /// `num_glyphs` should be read from the `maxp` table.
    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        num_glyphs: usize,
    ) -> Result<Self::HostType<'a>, ParseError> {
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

impl ReadBinaryDep for SbixStrike<'_> {
    type Args<'a> = usize; // num_glyphs
    type HostType<'a> = SbixStrike<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        num_glyphs: usize,
    ) -> Result<Self::HostType<'a>, ParseError> {
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

impl ReadBinaryDep for SbixGlyph<'_> {
    type Args<'a> = usize; // data_len
    type HostType<'a> = SbixGlyph<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        length: usize,
    ) -> Result<Self::HostType<'a>, ParseError> {
        let origin_offset_x = ctxt.read_i16be()?;
        let origin_offset_y = ctxt.read_i16be()?;
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

impl<'a> Sbix<'a> {
    /// Find a strike matching the desired parameters
    pub fn find_strike(
        &self,
        glyph_index: u16,
        target_ppem: u16,
        _max_bit_depth: BitDepth,
    ) -> Option<&SbixStrike<'a>> {
        // A strike does not need to include data for every glyph, and does not need to include
        // data for the same set of glyphs as other strikes. If the application is using bitmap
        // data to draw text and there is bitmap data for a glyph in any strike, then the glyph
        // must be drawn using a bitmap from some strike.
        //
        // If the exact size is not available, implementations may choose a bitmap based on the
        // closest available larger size, or the closest available integer-multiple larger size, or
        // on some other basis. The only cases in which a glyph is not drawn using a bitmap are if
        // the application has not requested that text be drawn using bitmap data or if there is no
        // bitmap data for the glyph in any strike.
        //
        // — https://docs.microsoft.com/en-us/typography/opentype/spec/sbix#strikes
        let target_ppem = i32::from(target_ppem);
        let candidates = self
            .strikes
            .iter()
            .filter(|strike| strike.contains_glyph(glyph_index));

        let mut best = None;
        for strike in candidates {
            let strike_ppem = i32::from(strike.ppem);
            let difference = strike_ppem - target_ppem;
            match best {
                Some((current_best_difference, _)) => {
                    if super::bigger_or_closer_to_zero(strike_ppem, current_best_difference) {
                        best = Some((difference, strike))
                    }
                }
                None => best = Some((difference, strike)),
            }
        }

        best.map(|(_, strike)| strike)
    }

    /// Create an `sbix` table for a subset of the glyphs in this font.
    ///
    /// `glyphs` is the subset of the glyphs in this font. The flags and every strike are
    /// retained, and each strike holds the record of each retained glyph. `dupe` and `flip`
    /// records are updated to refer to the new id of the glyph whose bitmap they use, which must be
    /// in `glyphs`: when subsetting a font, the glyphs that [`Sbix::reference_glyph_ids`] returns
    /// are added to the subset font.
    ///
    /// Returns an error if a record of a retained glyph can't be read, or if it is a `dupe` or
    /// `flip` record whose data is not a glyph id of this font or whose glyph is not in `glyphs`.
    pub(crate) fn subset(&self, glyphs: &impl SubsetGlyphs) -> Result<owned::Sbix, ParseError> {
        let strikes = self
            .strikes
            .iter()
            .map(|strike| {
                let strike_glyphs = (0..glyphs.len())
                    .map(|new_id| {
                        let old_id = glyphs.old_id(u16::try_from(new_id)?);
                        strike.subset_glyph(old_id, glyphs)
                    })
                    .collect::<Result<Vec<_>, ParseError>>()?;
                Ok(owned::SbixStrike {
                    ppem: strike.ppem,
                    ppi: strike.ppi,
                    glyphs: strike_glyphs,
                })
            })
            .collect::<Result<Vec<_>, ParseError>>()?;

        Ok(owned::Sbix {
            flags: self.flags,
            strikes,
        })
    }

    /// The glyphs whose bitmaps the `dupe` and `flip` records of the glyph `glyph_id` use, in
    /// strike order.
    ///
    /// Yields an error for a record that can't be read or a `dupe` or `flip` record whose data is
    /// not a glyph id of this font, as [`Sbix::subset`] does.
    pub(crate) fn reference_glyph_ids(
        &self,
        glyph_id: u16,
    ) -> impl Iterator<Item = Result<u16, ParseError>> + use<'_, 'a> {
        self.strikes
            .iter()
            .filter_map(move |strike| match strike.read_glyph(glyph_id) {
                Ok(Some(glyph)) => strike.reference_glyph_id(&glyph).transpose(),
                Ok(None) => None,
                Err(err) => Some(Err(err)),
            })
    }
}

impl<'a> SbixStrike<'a> {
    /// Read a glyph from this strike specified by `glyph_index`.
    pub fn read_glyph(&self, glyph_index: u16) -> Result<Option<SbixGlyph<'a>>, ParseError> {
        let (offset, end) = self.glyph_offset_end(glyph_index)?;
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

    fn glyph_offset_end(&self, glyph_index: u16) -> Result<(usize, usize), ParseError> {
        // The length of the bitmap data for each glyph is variable, and can be determined from the
        // difference between two consecutive offsets. Hence, the length of data for glyph N is
        // glyph_data_offset[N+1] - glyph_data_offset[N].
        // — https://docs.microsoft.com/en-us/typography/opentype/spec/sbix#strikes
        let glyph_index = usize::from(glyph_index);
        let offset = self
            .glyph_data_offsets
            .get_item(glyph_index)
            .map(SafeFrom::safe_from)
            .ok_or(ParseError::BadIndex)?;
        let end = self
            .glyph_data_offsets
            .get_item(glyph_index + 1)
            .map(SafeFrom::safe_from)
            .ok_or(ParseError::BadIndex)?;
        Ok((offset, end))
    }

    fn contains_glyph(&self, glyph_index: u16) -> bool {
        self.glyph_offset_end(glyph_index)
            .map(|(offset, end)| end.saturating_sub(offset) > 0)
            .unwrap_or(false)
    }

    /// Copy the bitmap of the glyph `glyph_index` for the subset font `glyphs`.
    fn subset_glyph(
        &self,
        glyph_index: u16,
        glyphs: &impl SubsetGlyphs,
    ) -> Result<Option<owned::SbixGlyph>, ParseError> {
        let Some(glyph) = self.read_glyph(glyph_index)? else {
            return Ok(None);
        };
        let data = match self.reference_glyph_id(&glyph)? {
            Some(glyph_id) => {
                let new_id = glyphs.try_new_id(glyph_id).ok_or(ParseError::BadIndex)?;
                Box::from(new_id.to_be_bytes())
            }
            None => Box::from(glyph.data),
        };

        Ok(Some(owned::SbixGlyph {
            origin_offset_x: glyph.origin_offset_x,
            origin_offset_y: glyph.origin_offset_y,
            graphic_type: glyph.graphic_type,
            data,
        }))
    }

    /// The id of the glyph whose bitmap `glyph` of this strike uses, if it is a `dupe` or `flip`
    /// glyph.
    ///
    /// Returns an error if the data of a `dupe` or `flip` glyph is not a glyph id of this font.
    fn reference_glyph_id(&self, glyph: &SbixGlyph<'_>) -> Result<Option<u16>, ParseError> {
        // The special graphicType of 'dupe' indicates that the data field contains a uint16,
        // big-endian glyph ID.
        // — https://learn.microsoft.com/en-us/typography/opentype/spec/sbix#glyph-data
        //
        // The data of the (Apple-specific) 'flip' graphicType is also a glyph ID, whose bitmap is
        // used flipped horizontally.
        if glyph.graphic_type != tag::DUPE && glyph.graphic_type != tag::FLIP {
            return Ok(None);
        }
        let glyph_id = ReadScope::new(glyph.data).read::<U16Be>()?;
        // There is an offset for each glyph plus one extra
        if usize::from(glyph_id) + 1 < self.glyph_data_offsets.len() {
            Ok(Some(glyph_id))
        } else {
            Err(ParseError::BadIndex)
        }
    }
}

impl<'a> From<(&SbixStrike<'a>, &SbixGlyph<'a>, u16, bool)> for BitmapGlyph {
    fn from(
        (strike, glyph, bitmap_id, should_flip_hori): (&SbixStrike<'a>, &SbixGlyph<'a>, u16, bool),
    ) -> Self {
        let encapsulated = EncapsulatedBitmap {
            format: EncapsulatedFormat::from(glyph.graphic_type),
            data: Box::from(glyph.data),
        };
        BitmapGlyph {
            bitmap: Bitmap::Encapsulated(encapsulated),
            bitmap_id,
            metrics: Metrics::HmtxVmtx(OriginOffset::from(glyph)),
            ppem_x: Some(strike.ppem),
            ppem_y: Some(strike.ppem),
            should_flip_hori,
        }
    }
}

impl From<u32> for EncapsulatedFormat {
    fn from(tag: u32) -> Self {
        match tag {
            tag::JPG => EncapsulatedFormat::Jpeg,
            tag::PNG => EncapsulatedFormat::Png,
            tag::TIFF => EncapsulatedFormat::Tiff,
            _ => EncapsulatedFormat::Other(tag),
        }
    }
}

impl From<&SbixGlyph<'_>> for OriginOffset {
    fn from(glyph: &SbixGlyph<'_>) -> Self {
        OriginOffset {
            x: glyph.origin_offset_x,
            y: glyph.origin_offset_y,
        }
    }
}

impl WriteBinary<&Self> for owned::Sbix {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &Self) -> Result<(), WriteError> {
        let start = ctxt.bytes_written();
        U16Be::write(ctxt, 1u16)?; // version
        U16Be::write(ctxt, table.flags)?;
        U32Be::write(ctxt, u32::try_from(table.strikes.len())?)?;
        // Offsets from the beginning of the table to each strike
        let strike_offsets = ctxt.placeholder_array::<U32Be, u32>(table.strikes.len())?;
        for (strike, placeholder) in table.strikes.iter().zip(strike_offsets) {
            let offset = u32::try_from(ctxt.bytes_written() - start)?;
            ctxt.write_placeholder(placeholder, offset)?;
            owned::SbixStrike::write(ctxt, strike)?;
        }

        Ok(())
    }
}

impl WriteBinary<&Self> for owned::SbixStrike {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, strike: &Self) -> Result<(), WriteError> {
        U16Be::write(ctxt, strike.ppem)?;
        U16Be::write(ctxt, strike.ppi)?;
        // Offsets from the beginning of the strike to the data of each glyph, plus one extra so
        // that the length of the data of each glyph is the difference between consecutive offsets
        let mut offset = 2 * U16Be::SIZE + (strike.glyphs.len() + 1) * U32Be::SIZE;
        U32Be::write(ctxt, u32::try_from(offset)?)?;
        for glyph in &strike.glyphs {
            offset += glyph.as_ref().map_or(0, owned::SbixGlyph::size);
            U32Be::write(ctxt, u32::try_from(offset)?)?;
        }
        for glyph in strike.glyphs.iter().flatten() {
            owned::SbixGlyph::write(ctxt, glyph)?;
        }

        Ok(())
    }
}

impl WriteBinary<&Self> for owned::SbixGlyph {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, glyph: &Self) -> Result<(), WriteError> {
        I16Be::write(ctxt, glyph.origin_offset_x)?;
        I16Be::write(ctxt, glyph.origin_offset_y)?;
        U32Be::write(ctxt, glyph.graphic_type)?;
        ctxt.write_bytes(&glyph.data)
    }
}

impl owned::SbixGlyph {
    /// The size of this glyph in bytes.
    fn size(&self) -> usize {
        2 * I16Be::SIZE + U32Be::SIZE + self.data.len()
    }
}

pub(crate) mod owned {
    //! Owned `sbix` table, as produced by [`Sbix::subset`](super::Sbix::subset).

    /// `sbix` table containing bitmaps (owned version).
    pub struct Sbix {
        /// Drawing flags.
        pub flags: u16,
        /// Bitmap data for different sizes.
        pub strikes: Vec<SbixStrike>,
    }

    /// A single strike (ppem/ppi) combination (owned version).
    pub struct SbixStrike {
        /// The PPEM size for which this strike was designed.
        pub ppem: u16,
        /// The device pixel density (in pixels-per-inch) for which this strike was designed.
        pub ppi: u16,
        /// The bitmap of each glyph, indexed by glyph id, or `None` if the glyph has no bitmap in
        /// this strike.
        pub glyphs: Vec<Option<SbixGlyph>>,
    }

    /// An `sbix` bitmap glyph (owned version).
    pub struct SbixGlyph {
        /// The horizontal (x-axis) offset from the left edge of the graphic to the glyph’s origin.
        pub origin_offset_x: i16,
        /// The vertical (y-axis) offset from the bottom edge of the graphic to the glyph’s origin.
        pub origin_offset_y: i16,
        /// Indicates the format of the embedded graphic data.
        pub graphic_type: u32,
        /// The actual embedded graphic data.
        pub data: Box<[u8]>,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::binary::read::ReadScope;
    use crate::binary::write::WriteBuffer;
    use crate::font_data::FontData;
    use crate::subset::TestSubsetGlyphs;
    use crate::tables::{FontTableProvider, MaxpTable};
    use crate::tag;

    use crate::tests::read_fixture;

    #[test]
    fn test_read_sbix() {
        let buffer = read_fixture("tests/fonts/woff1/chromacheck-sbix.woff");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
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

        // Strikes
        assert_eq!(sbix.strikes.len(), 1);
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
            assert_eq!(glyph.graphic_type, tag::PNG);
            assert_eq!(glyph.data.len(), 224);
            assert_eq!(*glyph.data.last().unwrap(), 0x82);
        } else {
            panic!("expected Some(SbixGlyph) got None");
        }
    }

    fn glyph(graphic_type: u32, data: &[u8]) -> Option<owned::SbixGlyph> {
        Some(owned::SbixGlyph {
            origin_offset_x: -3,
            origin_offset_y: 7,
            graphic_type,
            data: data.into(),
        })
    }

    /// Strike with 5 glyphs: none, png, dupe of 1, flip of 4, png.
    fn strike(ppem: u16, png_byte: u8) -> owned::SbixStrike {
        owned::SbixStrike {
            ppem,
            ppi: 72,
            glyphs: vec![
                None,
                glyph(tag::PNG, &[png_byte, 1]),
                glyph(tag::DUPE, &[0, 1]),
                glyph(tag::FLIP, &[0, 4]),
                glyph(tag::PNG, &[png_byte, 4, 4]),
            ],
        }
    }

    fn sbix_data(sbix: &owned::Sbix) -> Vec<u8> {
        let mut w = WriteBuffer::new();
        owned::Sbix::write(&mut w, sbix).unwrap();
        w.into_inner()
    }

    /// A table with two strikes of 5 glyphs each.
    fn test_sbix_data() -> Vec<u8> {
        sbix_data(&owned::Sbix {
            flags: 1,
            strikes: vec![strike(20, 0xA0), strike(40, 0xB0)],
        })
    }

    fn read_sbix(data: &[u8], num_glyphs: usize) -> Sbix<'_> {
        ReadScope::new(data)
            .read_dep::<Sbix<'_>>(num_glyphs)
            .unwrap()
    }

    /// Subset `data` (of 5 glyphs) to `old_ids` and write the result.
    fn subset_data(data: &[u8], old_ids: &[u16]) -> Vec<u8> {
        let subset = read_sbix(data, 5)
            .subset(&TestSubsetGlyphs::new(old_ids))
            .unwrap();
        sbix_data(&subset)
    }

    fn glyph_data(strike: &SbixStrike<'_>, glyph_id: u16) -> Option<(u32, Vec<u8>)> {
        let glyph = strike.read_glyph(glyph_id).unwrap()?;
        assert_eq!((glyph.origin_offset_x, glyph.origin_offset_y), (-3, 7));
        Some((glyph.graphic_type, glyph.data.to_vec()))
    }

    #[test]
    fn write_layout() {
        let data = sbix_data(&owned::Sbix {
            flags: 3,
            strikes: vec![owned::SbixStrike {
                ppem: 20,
                ppi: 72,
                glyphs: vec![None, glyph(tag::PNG, &[9, 8, 7])],
            }],
        });

        let mut expected = vec![0, 1, 0, 3, 0, 0, 0, 1, 0, 0, 0, 12];
        expected.extend([0, 20, 0, 72]);
        // Offsets of the 2 glyphs and the end, from the start of the strike
        expected.extend([0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 27]);
        expected.extend([0xFF, 0xFD, 0, 7]);
        expected.extend(tag::PNG.to_be_bytes());
        expected.extend([9, 8, 7]);
        assert_eq!(data, expected);

        let sbix = read_sbix(&data, 2);
        assert_eq!(sbix.flags, 3);
        assert!(sbix.strikes[0].read_glyph(0).unwrap().is_none());
        assert_eq!(
            glyph_data(&sbix.strikes[0], 1),
            Some((tag::PNG, vec![9, 8, 7]))
        );
    }

    #[test]
    fn subset_renumbers_glyphs_and_points_dupe_and_flip_at_new_ids() {
        let data = test_sbix_data();

        // Glyph 3 (flip of 4) is first, then 4, 2 (dupe of 1), 1 and 0
        let subset = subset_data(&data, &[3, 4, 2, 1, 0]);
        let sbix = read_sbix(&subset, 5);
        assert_eq!(sbix.flags, 1);
        assert_eq!(sbix.strikes.len(), 2);
        for (strike, (ppem, png_byte)) in sbix.strikes.iter().zip([(20, 0xA0), (40, 0xB0)]) {
            assert_eq!((strike.ppem, strike.ppi), (ppem, 72));
            assert_eq!(glyph_data(strike, 0), Some((tag::FLIP, vec![0, 1])));
            assert_eq!(
                glyph_data(strike, 1),
                Some((tag::PNG, vec![png_byte, 4, 4]))
            );
            assert_eq!(glyph_data(strike, 2), Some((tag::DUPE, vec![0, 3])));
            assert_eq!(glyph_data(strike, 3), Some((tag::PNG, vec![png_byte, 1])));
            assert_eq!(glyph_data(strike, 4), None);
        }
    }

    #[test]
    fn subset_drops_glyphs_that_are_not_retained() {
        let data = test_sbix_data();

        let subset = subset_data(&data, &[4, 1]);
        let sbix = read_sbix(&subset, 2);
        assert_eq!(sbix.strikes.len(), 2);
        assert_eq!(
            glyph_data(&sbix.strikes[1], 0),
            Some((tag::PNG, vec![0xB0, 4, 4]))
        );
        assert_eq!(
            glyph_data(&sbix.strikes[1], 1),
            Some((tag::PNG, vec![0xB0, 1]))
        );
        assert_eq!(sbix.strikes[1].glyph_data_offsets.len(), 3);
    }

    #[test]
    fn subset_with_a_reference_outside_the_subset_is_an_error() {
        let data = test_sbix_data();
        let sbix = read_sbix(&data, 5);

        // Glyph 2 is a dupe of glyph 1, glyph 3 is a flip of glyph 4
        assert!(sbix.subset(&TestSubsetGlyphs::new(&[2])).is_err());
        assert!(sbix.subset(&TestSubsetGlyphs::new(&[3, 1])).is_err());
        assert!(sbix.subset(&TestSubsetGlyphs::new(&[2, 1])).is_ok());
    }

    #[test]
    fn subset_with_a_reference_that_is_not_a_glyph_is_an_error() {
        let mut bad_strike = strike(20, 0);
        bad_strike.glyphs[2] = glyph(tag::DUPE, &[0, 5]);
        let data = sbix_data(&owned::Sbix {
            flags: 0,
            strikes: vec![bad_strike],
        });
        let sbix = read_sbix(&data, 5);

        assert!(sbix.subset(&TestSubsetGlyphs::new(&[2, 1, 4])).is_err());
        assert!(sbix.reference_glyph_ids(2).next().unwrap().is_err());
        // The glyphs without a reference are not affected
        assert!(sbix.subset(&TestSubsetGlyphs::new(&[1, 4])).is_ok());
    }

    #[test]
    fn reference_glyph_ids_lists_references_of_each_strike() {
        let mut other_strike = strike(40, 0xB0);
        other_strike.glyphs[2] = glyph(tag::DUPE, &[0, 4]);
        let data = sbix_data(&owned::Sbix {
            flags: 0,
            strikes: vec![strike(20, 0xA0), other_strike],
        });
        let sbix = read_sbix(&data, 5);

        let references = |glyph_id| {
            sbix.reference_glyph_ids(glyph_id)
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        };
        assert_eq!(references(2), vec![1, 4]);
        assert_eq!(references(3), vec![4, 4]);
        // No bitmap, or a bitmap that is not a reference
        assert!(references(0).is_empty());
        assert!(references(1).is_empty());
        assert!(references(4).is_empty());
    }
}
