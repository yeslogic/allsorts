#![deny(missing_docs)]

//! `hdmx` table parsing and writing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/hdmx>

use std::borrow::Cow;

use crate::binary::read::{ReadBinaryDep, ReadCtxt};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::{long_align, U16Be, U32Be, U8};
use crate::error::{ParseError, WriteError};
use crate::size;

/// `hdmx` Horizontal Device Metrics Table
///
/// Holds the advance widths of the glyphs in pixels for a number of pixel sizes.
#[derive(Debug, Clone)]
pub struct HdmxTable<'a> {
    /// The device records, one per pixel size.
    pub records: Vec<DeviceRecord<'a>>,
}

/// The advance widths of the glyphs at one pixel size.
#[derive(Debug, Clone)]
pub struct DeviceRecord<'a> {
    /// The pixel size (ppem) of the widths.
    pub pixel_size: u8,
    /// The maximum of the widths.
    pub max_width: u8,
    /// The advance width in pixels of each glyph, indexed by glyph id.
    pub widths: Cow<'a, [u8]>,
}

impl ReadBinaryDep for HdmxTable<'_> {
    type Args<'a> = u16; // num_glyphs
    type HostType<'a> = HdmxTable<'a>;

    /// Read the `hdmx` table.
    ///
    /// `num_glyphs` should be read from the `maxp` table.
    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        num_glyphs: u16,
    ) -> Result<Self::HostType<'a>, ParseError> {
        let version = ctxt.read_u16be()?;
        ctxt.check_version(version == 0)?;
        let num_records = usize::from(ctxt.read_u16be()?);
        let size_device_record = usize::try_from(ctxt.read_u32be()?)?;
        let num_glyphs = usize::from(num_glyphs);
        ctxt.check(num_records == 0 || size_device_record >= 2 * size::U8 + num_glyphs)?;

        // The records are padded to `size_device_record` bytes. The padding of the last record is
        // not needed to read it so it is not required to be present.
        let scope = ctxt.scope();
        let records = (0..num_records)
            .map(|index| {
                let mut ctxt = scope.offset(index * size_device_record).ctxt();
                let pixel_size = ctxt.read_u8()?;
                let max_width = ctxt.read_u8()?;
                let widths = ctxt.read_slice(num_glyphs)?;
                Ok(DeviceRecord {
                    pixel_size,
                    max_width,
                    widths: Cow::Borrowed(widths),
                })
            })
            .collect::<Result<Vec<_>, ParseError>>()?;

        Ok(HdmxTable { records })
    }
}

impl WriteBinary<&Self> for HdmxTable<'_> {
    type Output = ();

    /// Write the `hdmx` table.
    ///
    /// The number of glyphs is taken from the records, which must all hold the same number of
    /// widths, otherwise `WriteError::BadValue` is returned. A table without records has no
    /// widths to count and is written as if it were for a font with no glyphs.
    fn write<C: WriteContext>(ctxt: &mut C, table: &Self) -> Result<(), WriteError> {
        let num_glyphs = table
            .records
            .first()
            .map_or(0, |record| record.widths.len());
        // Each record is padded to a multiple of 4 bytes
        let size_device_record = long_align(2 * size::U8 + num_glyphs);

        U16Be::write(ctxt, 0u16)?; // version
        U16Be::write(ctxt, u16::try_from(table.records.len())?)?; // numRecords
        U32Be::write(ctxt, u32::try_from(size_device_record)?)?; // sizeDeviceRecord
        for record in &table.records {
            if record.widths.len() != num_glyphs {
                return Err(WriteError::BadValue);
            }
            U8::write(ctxt, record.pixel_size)?;
            U8::write(ctxt, record.max_width)?;
            ctxt.write_bytes(&record.widths)?;
            ctxt.write_zeros(size_device_record - 2 * size::U8 - num_glyphs)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;
    use crate::binary::write::WriteBuffer;

    fn table<'a>() -> HdmxTable<'a> {
        HdmxTable {
            records: vec![
                DeviceRecord {
                    pixel_size: 12,
                    max_width: 9,
                    widths: Cow::Borrowed(&[5, 9, 7]),
                },
                DeviceRecord {
                    pixel_size: 24,
                    max_width: 20,
                    widths: Cow::Borrowed(&[11, 20, 15]),
                },
            ],
        }
    }

    #[test]
    fn write_then_read() {
        let mut w = WriteBuffer::new();
        HdmxTable::write(&mut w, &table()).unwrap();
        let data = w.into_inner();

        // Header of 8 bytes, then 2 records of 2 + 3 bytes padded to 8
        assert_eq!(data.len(), 8 + 2 * 8);
        assert_eq!(&data[..8], &[0, 0, 0, 2, 0, 0, 0, 8]);
        assert_eq!(&data[8..16], &[12, 9, 5, 9, 7, 0, 0, 0]);

        let read = ReadScope::new(&data).read_dep::<HdmxTable<'_>>(3).unwrap();
        assert_eq!(read.records.len(), 2);
        assert_eq!(read.records[0].pixel_size, 12);
        assert_eq!(read.records[0].max_width, 9);
        assert_eq!(&*read.records[0].widths, &[5, 9, 7]);
        assert_eq!(read.records[1].pixel_size, 24);
        assert_eq!(read.records[1].max_width, 20);
        assert_eq!(&*read.records[1].widths, &[11, 20, 15]);
    }

    #[test]
    fn read_without_padding_of_last_record() {
        let mut w = WriteBuffer::new();
        HdmxTable::write(&mut w, &table()).unwrap();
        let mut data = w.into_inner();
        data.truncate(data.len() - 3);

        let read = ReadScope::new(&data).read_dep::<HdmxTable<'_>>(3).unwrap();
        assert_eq!(&*read.records[1].widths, &[11, 20, 15]);
    }

    #[test]
    fn read_truncated_record() {
        let mut w = WriteBuffer::new();
        HdmxTable::write(&mut w, &table()).unwrap();
        let mut data = w.into_inner();
        data.truncate(data.len() - 4);

        assert!(ReadScope::new(&data).read_dep::<HdmxTable<'_>>(3).is_err());
    }

    #[test]
    fn write_records_with_different_numbers_of_widths() {
        let mut table = table();
        table.records[1].widths = Cow::Borrowed(&[11, 20, 15, 1]);
        let mut w = WriteBuffer::new();
        assert!(matches!(
            HdmxTable::write(&mut w, &table),
            Err(WriteError::BadValue)
        ));
    }

    #[test]
    fn write_pads_records_to_four_bytes() {
        // Each record is 2 bytes of header and one byte per glyph, padded to a multiple of 4
        for (num_glyphs, record_size) in [(0, 4), (1, 4), (2, 4), (3, 8), (6, 8), (7, 12), (18, 20)]
        {
            let table = HdmxTable {
                records: vec![DeviceRecord {
                    pixel_size: 9,
                    max_width: 1,
                    widths: Cow::Owned(vec![1; num_glyphs]),
                }],
            };
            let mut w = WriteBuffer::new();
            HdmxTable::write(&mut w, &table).unwrap();
            let data = w.into_inner();

            assert_eq!(data.len(), 8 + record_size, "{} glyphs", num_glyphs);
            let read = ReadScope::new(&data)
                .read_dep::<HdmxTable<'_>>(num_glyphs as u16)
                .unwrap();
            assert_eq!(&*read.records[0].widths, &*table.records[0].widths);
        }
    }

    #[test]
    fn write_without_records() {
        let table = HdmxTable { records: vec![] };
        let mut w = WriteBuffer::new();
        HdmxTable::write(&mut w, &table).unwrap();
        let data = w.into_inner();
        // version 0, no records, each record would be 4 bytes
        assert_eq!(data, &[0, 0, 0, 0, 0, 0, 0, 4]);

        let read = ReadScope::new(&data).read_dep::<HdmxTable<'_>>(3).unwrap();
        assert!(read.records.is_empty());
    }
}
