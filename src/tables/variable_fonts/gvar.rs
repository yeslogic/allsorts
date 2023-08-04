#![deny(missing_docs)]

//! `gvar` Glyph Variations Table
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/gvar>

use crate::binary::read::{ReadBinary, ReadCtxt, ReadScope, ReadUnchecked};
use crate::binary::{U16Be, U32Be};
use crate::error::ParseError;
use crate::tables::loca::LocaOffsets;
use crate::tables::F2Dot14;
use crate::SafeFrom;

/// `gvar` Glyph Variations Table
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#gvar-header>
pub struct Gvar<'a> {
    /// Major version number of the glyph variations table.
    major_version: u16,
    /// Minor version number of the glyph variations table.
    minor_version: u16,
    /// The number of variation axes for this font.
    ///
    /// This must be the same number as axisCount in
    /// the 'fvar' table.
    axis_count: u16,
    /// The number of shared tuple records.
    ///
    /// Shared tuple records can be referenced within glyph
    /// variation data tables for multiple glyphs, as opposed to other tuple records stored
    /// directly within a glyph variation data table.
    shared_tuple_count: u16,
    /// Scope containing data for the shared tuple records.
    shared_tuples_scope: ReadScope<'a>,
    /// The number of glyphs in this font.
    ///
    /// This must match the number of glyphs stored elsewhere in
    /// the font.
    glyph_count: u16,
    /// Bit-field that gives the format of the offset array that follows.
    ///
    /// If bit 0 is clear, the
    /// offsets are u16; if bit 0 is set, the offsets are uint32.
    flags: u16,
    /// Scope containing the data for the array of GlyphVariationData tables.
    glyph_variation_data_array_scope: ReadScope<'a>,
    /// Offsets from the start of the GlyphVariationData array to each GlyphVariationData table.
    glyph_variation_data_offsets: LocaOffsets<'a>, // [glyphCount + 1] : Offset16 or Offset32 ,
}

impl ReadBinary for Gvar<'_> {
    type HostType<'a> = Gvar<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let major_version = ctxt.read_u16be()?;
        ctxt.check_version(major_version == 1)?;
        let minor_version = ctxt.read_u16be()?;
        let axis_count = ctxt.read_u16be()?;
        let shared_tuple_count = ctxt.read_u16be()?;
        let shared_tuples_offset = ctxt.read_u32be()?;
        let glyph_count = ctxt.read_u16be()?;
        let flags = ctxt.read_u16be()?;
        let glyph_variation_data_array_offset = ctxt.read_u32be()?;
        // If bit 0 is clear, the offsets are uint16; if bit 0 is set, the offsets are uint32.
        let glyph_variation_data_offsets = if flags & 1 == 1 {
            // The actual local offset is stored. The value of n is numGlyphs + 1.
            LocaOffsets::Long(ctxt.read_array::<U32Be>(usize::from(glyph_count) + 1)?)
        } else {
            // The actual local offset divided by 2 is stored. The value of n is numGlyphs + 1.
            LocaOffsets::Short(ctxt.read_array::<U16Be>(usize::from(glyph_count) + 1)?)
        };

        // Store the shared tuples
        let shared_tuples_len =
            usize::from(shared_tuple_count) * usize::from(axis_count) * F2Dot14::SIZE;
        let shared_tuples_scope =
            scope.offset_length(usize::safe_from(shared_tuples_offset), shared_tuples_len)?;

        // Read the glyph variation data
        if glyph_variation_data_offsets.len() < 2 {
            return Err(ParseError::BadIndex);
        }
        // NOTE(unwrap): Safe due to check above
        let glyph_variation_data_array_scope = scope.offset_length(
            usize::safe_from(glyph_variation_data_array_offset),
            usize::safe_from(glyph_variation_data_offsets.last().unwrap()),
        )?;

        Ok(Gvar {
            major_version,
            minor_version,
            axis_count,
            shared_tuple_count,
            shared_tuples_scope,
            glyph_count,
            flags,
            glyph_variation_data_array_scope,
            glyph_variation_data_offsets,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;
    use crate::font_data::FontData;
    use crate::tables::FontTableProvider;
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    fn gvar() {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let stat_data = table_provider
            .read_table_data(tag::GVAR)
            .expect("unable to read fvar table data");
        let gvar = ReadScope::new(&stat_data).read::<Gvar<'_>>().unwrap();
        assert_eq!(gvar.major_version, 1);
        assert_eq!(gvar.minor_version, 0);
        assert_eq!(gvar.axis_count, 3);
        assert_eq!(gvar.shared_tuple_count, 15);
        assert_eq!(gvar.shared_tuples_scope.data().len(), 90);
        assert_eq!(gvar.glyph_count, 4);
        assert_eq!(gvar.flags, 0);
        assert_eq!(gvar.glyph_variation_data_array_scope.data().len(), 3028);
        assert_eq!(gvar.glyph_variation_data_offsets.len(), 5);
    }
}
