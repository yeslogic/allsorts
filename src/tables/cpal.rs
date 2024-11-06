#![deny(missing_docs)]

//! `CPAL` table parsing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/cpal>

use crate::binary::read::{ReadArray, ReadBinary, ReadCtxt, ReadFrom};
use crate::binary::{U16Be, U32Be, U8};
use crate::error::ParseError;
use crate::SafeFrom;

/// `CPAL` — Color Palette Table
pub struct CpalTable<'a> {
    /// Table version number.
    version: u16,
    /// Number of palette entries in each palette.
    num_palette_entries: u16,
    /// Color records for all palettes.
    color_records_array: ReadArray<'a, ColorRecord>,
    /// Index of each palette’s first color record in the combined color record array.
    color_record_indices: ReadArray<'a, U16Be>,
    /// Palette Types Array.
    palette_types_array: Option<ReadArray<'a, U32Be>>,
    /// Palette Labels Array.
    palette_labels_array: Option<ReadArray<'a, U16Be>>,
    /// Palette Entry Labels Array.
    palette_entry_labels_array: Option<ReadArray<'a, U16Be>>,
}

impl ReadBinary for CpalTable<'_> {
    type HostType<'a> = CpalTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let start = ctxt.scope();
        let version = ctxt.read_u16be()?;
        let num_palette_entries = ctxt.read_u16be()?;
        let num_palettes = ctxt.read_u16be()?;
        // A minimum of one palette must be provided in the CPAL table if the table is present.
        ctxt.check(num_palettes > 0)?;
        let num_color_records = ctxt.read_u16be()?;
        let color_records_array_offset = ctxt.read_u32be()?;
        // Multiple colorRecordIndices may refer to the same color record, in which case multiple
        // palettes would use the same color records
        let color_record_indices = ctxt.read_array(usize::from(num_palettes))?;
        let color_records_array = start
            .offset(usize::safe_from(color_records_array_offset))
            .ctxt()
            .read_array(usize::from(num_color_records))?;

        let (
            palette_types_array_offset,
            palette_labels_array_offset,
            palette_entry_labels_array_offset,
        ) = if version == 1 {
            let palette_types_array_offset = ctxt.read_u32be()?;
            let palette_labels_array_offset = ctxt.read_u32be()?;
            let palette_entry_labels_array_offset = ctxt.read_u32be()?;
            (
                palette_types_array_offset,
                palette_labels_array_offset,
                palette_entry_labels_array_offset,
            )
        } else {
            (0, 0, 0)
        };

        let palette_types_array = (palette_types_array_offset > 0)
            .then(|| {
                start
                    .offset(usize::safe_from(palette_types_array_offset))
                    .ctxt()
                    .read_array(usize::from(num_palettes))
            })
            .transpose()?;
        let palette_labels_array = (palette_labels_array_offset > 0)
            .then(|| {
                start
                    .offset(usize::safe_from(palette_labels_array_offset))
                    .ctxt()
                    .read_array(usize::from(num_palettes))
            })
            .transpose()?;
        let palette_entry_labels_array = (palette_entry_labels_array_offset > 0)
            .then(|| {
                start
                    .offset(usize::safe_from(palette_entry_labels_array_offset))
                    .ctxt()
                    .read_array(usize::from(num_palette_entries))
            })
            .transpose()?;

        Ok(CpalTable {
            version,
            num_palette_entries,
            color_records_array,
            color_record_indices,
            palette_types_array,
            palette_labels_array,
            palette_entry_labels_array,
        })
    }
}

/// A BGRA color record.
#[derive(Debug, Copy, Clone)]
pub struct ColorRecord {
    /// Blue value (B0).
    pub blue: u8,
    /// Green value (B1).
    pub green: u8,
    /// Red value (B2).
    pub red: u8,
    /// Alpha value (B3).
    pub alpha: u8,
}

impl ReadFrom for ColorRecord {
    type ReadType = (U8, U8, U8, U8);

    fn read_from((blue, green, red, alpha): (u8, u8, u8, u8)) -> Self {
        ColorRecord {
            blue,
            green,
            red,
            alpha,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        binary::read::ReadScope,
        tables::{FontTableProvider, OpenTypeFont},
        tag,
        tests::read_fixture,
    };

    #[test]
    fn test_read_cpal_v1_variable() {
        let buffer = read_fixture(
            "tests/fonts/colr/SixtyfourConvergence-Regular-VariableFont_BLED,SCAN,XELA,YELA.ttf",
        );
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let table_provider = otf.table_provider(0).expect("error reading font file");

        let cpal_data = table_provider
            .read_table_data(tag::CPAL)
            .expect("unable to read CPAL data");
        let cpal = ReadScope::new(&cpal_data)
            .read::<CpalTable<'_>>()
            .expect("unable to parse CPAL table");

        assert_eq!(cpal.version, 1);
        assert_eq!(cpal.num_palette_entries, 6);
        assert_eq!(cpal.color_records_array.len(), 12);
        assert_eq!(cpal.color_record_indices.len(), 2);
        assert_eq!(
            cpal.palette_types_array.as_ref().map(|map| map.len()),
            Some(2)
        );
        assert!(cpal.palette_labels_array.is_none());
        assert!(cpal.palette_entry_labels_array.is_none());
    }
}
