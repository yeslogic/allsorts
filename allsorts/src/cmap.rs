//! Implements the [cmap](cmap specification) table
//!
//! [cmap specification]: https://www.microsoft.com/typography/otspec/cmap.htm

use byteorder::{BigEndian, ReadBytesExt};
use std::io::{self, Read};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PlatformId(pub u16);

impl PlatformId {
    pub const UNICODE: PlatformId = PlatformId(0);
    pub const MACINTOSH: PlatformId = PlatformId(1);
    pub const WINDOWS: PlatformId = PlatformId(3);
    pub const CUSTOM: PlatformId = PlatformId(4);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EncodingId(pub u16);

impl EncodingId {
    pub const WINDOWS_SYMBOL: EncodingId = EncodingId(0);
    pub const WINDOWS_UNICODE_BMP_UCS2: EncodingId = EncodingId(1);
    pub const WINDOWS_SHIFT_JIS: EncodingId = EncodingId(2);
    pub const WINDOWS_PRC: EncodingId = EncodingId(3);
    pub const WINDOWS_BIG5: EncodingId = EncodingId(4);
    pub const WINDOWS_WANSUNG: EncodingId = EncodingId(5);
    pub const WINDOWS_JOHAB: EncodingId = EncodingId(6);
    // pub const WINDOWS_RESERVED: EncodingId = EncodingId(7);
    // pub const WINDOWS_RESERVED: EncodingId = EncodingId(8);
    // pub const WINDOWS_RESERVED: EncodingId = EncodingId(9);
    pub const WINDOWS_UNICODE_UCS4: EncodingId = EncodingId(10);

    pub const MACINTOSH_APPLE_ROMAN: EncodingId = EncodingId(0);
    pub const MACINTOSH_UNICODE_UCS4: EncodingId = EncodingId(4);
}

/// A table that defines the mappings of achacter codes to the glyph indices used in the font.
///
/// Multiple encoding schemes may be supported via the `encoding_records`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CMap {
    pub version: u16,
    pub encoding_records: Vec<EncodingRecord>,
}

impl CMap {
    pub fn decode<R: Read + ?Sized>(reader: &mut R) -> io::Result<CMap> {
        let version = reader.read_u16::<BigEndian>()?;
        if version != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Expected cmap version to be 0",
            ));
        }

        let num_tables = reader.read_u16::<BigEndian>()?;
        let encoding_records = (0..num_tables)
            .map(|_| EncodingRecord::decode(reader))
            .collect::<io::Result<_>>()?;

        Ok(CMap {
            version,
            encoding_records,
        })
    }

    /// Find the first encoding id and subtable offset for the given `platform_id`
    pub fn find_subtable_for_platform(&self, platform_id: PlatformId) -> Option<(EncodingId, u32)> {
        self.encoding_records
            .iter()
            .find(|record| record.platform_id == platform_id)
            .map(|record| (record.encoding_id, record.subtable_offset))
    }

    /// Find the first subtable offset for the given `platform_id` and `encoding_id`
    pub fn find_subtable(&self, platform_id: PlatformId, encoding_id: EncodingId) -> Option<u32> {
        self.encoding_records
            .iter()
            .find(|record| {
                record.platform_id == platform_id && record.encoding_id == encoding_id
            })
            .map(|record| record.subtable_offset)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EncodingRecord {
    pub platform_id: PlatformId,
    pub encoding_id: EncodingId,
    pub subtable_offset: u32,
}

impl EncodingRecord {
    pub fn decode<R: Read + ?Sized>(reader: &mut R) -> io::Result<EncodingRecord> {
        Ok(EncodingRecord {
            platform_id: PlatformId(reader.read_u16::<BigEndian>()?),
            encoding_id: EncodingId(reader.read_u16::<BigEndian>()?),
            subtable_offset: reader.read_u32::<BigEndian>()?,
        })
    }
}

#[cfg(test)]
mod test {
    use byteorder::{BigEndian, WriteBytesExt};
    use std::io::Cursor;

    use super::*;

    #[test]
    fn empty_data() {
        let mut cursor = Cursor::new(Vec::new());

        assert!(CMap::decode(&mut cursor).is_err());
    }

    #[test]
    fn missing_length() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version

        let mut cursor = Cursor::new(data);
        assert!(CMap::decode(&mut cursor).is_err());
    }

    #[test]
    fn invalid_version() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(1).unwrap(); // version
        data.write_u16::<BigEndian>(0).unwrap(); // num_tables

        let mut cursor = Cursor::new(data);
        assert!(CMap::decode(&mut cursor).is_err());
    }

    #[test]
    fn empty_subtables() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version
        data.write_u16::<BigEndian>(0).unwrap(); // num_tables

        let mut cursor = Cursor::new(data);
        let cmap = CMap::decode(&mut cursor).unwrap();
        assert_eq!(cmap.version, 0);
        assert_eq!(cmap.encoding_records, vec![]);
    }

    #[test]
    fn one_encoding_record() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version
        data.write_u16::<BigEndian>(1).unwrap(); // num_tables
        // encoding_record 0
        data.write_u16::<BigEndian>(3).unwrap(); // platform_id
        data.write_u16::<BigEndian>(10).unwrap(); // encoding_id
        data.write_u32::<BigEndian>(256).unwrap(); // subtable_offset

        let mut cursor = Cursor::new(data);
        let cmap = CMap::decode(&mut cursor).unwrap();
        assert_eq!(cmap.version, 0);
        assert_eq!(
            cmap.encoding_records,
            vec![
                EncodingRecord {
                    platform_id: PlatformId::WINDOWS,
                    encoding_id: EncodingId::WINDOWS_UNICODE_UCS4,
                    subtable_offset: 256,
                },
            ]
        );
    }

    #[test]
    fn two_encoding_records() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version
        data.write_u16::<BigEndian>(2).unwrap(); // num_tables
        // encoding_record 0
        data.write_u16::<BigEndian>(3).unwrap(); // platform_id
        data.write_u16::<BigEndian>(10).unwrap(); // encoding_id
        data.write_u32::<BigEndian>(256).unwrap(); // subtable_offset
        // encoding_record 1
        data.write_u16::<BigEndian>(1).unwrap(); // platform_id
        data.write_u16::<BigEndian>(0).unwrap(); // encoding_id
        data.write_u32::<BigEndian>(513).unwrap(); // subtable_offset

        let mut cursor = Cursor::new(data);
        let cmap = CMap::decode(&mut cursor).unwrap();
        assert_eq!(cmap.version, 0);
        assert_eq!(
            cmap.encoding_records,
            vec![
                EncodingRecord {
                    platform_id: PlatformId::WINDOWS,
                    encoding_id: EncodingId::WINDOWS_UNICODE_UCS4,
                    subtable_offset: 256,
                },
                EncodingRecord {
                    platform_id: PlatformId::MACINTOSH,
                    encoding_id: EncodingId::MACINTOSH_APPLE_ROMAN,
                    subtable_offset: 513,
                },
            ]
        );
    }

    #[test]
    fn length_too_large() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version
        data.write_u16::<BigEndian>(3).unwrap(); // num_tables
        // encoding_record 0
        data.write_u16::<BigEndian>(3).unwrap(); // platform_id
        data.write_u16::<BigEndian>(10).unwrap(); // encoding_id
        data.write_u32::<BigEndian>(256).unwrap(); // subtable_offset
        // encoding_record 1
        data.write_u16::<BigEndian>(1).unwrap(); // platform_id
        data.write_u16::<BigEndian>(0).unwrap(); // encoding_id
        data.write_u32::<BigEndian>(513).unwrap(); // subtable_offset

        let mut cursor = Cursor::new(data);
        assert!(CMap::decode(&mut cursor).is_err());
    }

    #[test]
    fn eof_in_encoding_record() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version
        data.write_u16::<BigEndian>(3).unwrap(); // num_tables
        // encoding_record 0
        data.write_u16::<BigEndian>(3).unwrap(); // platform_id
        data.write_u16::<BigEndian>(10).unwrap(); // encoding_id
        data.write_u32::<BigEndian>(256).unwrap(); // subtable_offset
        // encoding_record 1
        data.write_u16::<BigEndian>(1).unwrap(); // platform_id
        data.write_u16::<BigEndian>(0).unwrap(); // encoding_id
        data.write_u16::<BigEndian>(0).unwrap(); // subtable_offset

        let mut cursor = Cursor::new(data);
        assert!(CMap::decode(&mut cursor).is_err());
    }
}
