#[cfg(test)]
extern crate byteorder;

include!(concat!(env!("OUT_DIR"), "/otf.rs"));

pub mod platform_id {
    pub const UNICODE: u16 = 0;
    pub const MACINTOSH: u16 = 1;
    pub const WINDOWS: u16 = 3;
    pub const CUSTOM: u16 = 4;
}

pub mod encoding_id {
    pub const WINDOWS_SYMBOL: u16 = 0;
    pub const WINDOWS_UNICODE_BMP_UCS2: u16 = 1;
    pub const WINDOWS_SHIFT_JIS: u16 = 2;
    pub const WINDOWS_PRC: u16 = 3;
    pub const WINDOWS_BIG5: u16 = 4;
    pub const WINDOWS_WANSUNG: u16 = 5;
    pub const WINDOWS_JOHAB: u16 = 6;
    // pub const WINDOWS_RESERVED: u16 = 7;
    // pub const WINDOWS_RESERVED: u16 = 8;
    // pub const WINDOWS_RESERVED: u16 = 9;
    pub const WINDOWS_UNICODE_UCS4: u16 = 10;

    pub const MACINTOSH_APPLE_ROMAN: u16 = 0;
    pub const MACINTOSH_UNICODE_UCS4: u16 = 4;
}

impl CMap {
    /// Find the first encoding id and subtable offset for the given `platform_id`
    pub fn find_subtable_for_platform(&self, platform_id: u16) -> Option<(u16, Offset32)> {
        self.encoding_records
            .iter()
            .find(|record| record.platform_id == platform_id)
            .map(|record| (record.encoding_id, record.subtable_offset.clone()))
    }

    /// Find the first subtable offset for the given `platform_id` and `encoding_id`
    pub fn find_subtable(&self, platform_id: u16, encoding_id: u16) -> Option<Offset32> {
        self.encoding_records
            .iter()
            .find(|record| record.platform_id == platform_id && record.encoding_id == encoding_id)
            .map(|record| record.subtable_offset.clone())
    }
}

#[cfg(test)]
mod test {
    use byteorder::{BigEndian, WriteBytesExt};
    use ddl_util::FromBinary;
    use std::io::Cursor;

    use super::*;

    #[test]
    fn empty_data() {
        let mut cursor = Cursor::new(Vec::new());

        assert!(CMap::from_binary(&mut cursor).is_err());
    }

    #[test]
    fn missing_length() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version

        let mut cursor = Cursor::new(data);
        assert!(CMap::from_binary(&mut cursor).is_err());
    }

    #[test]
    #[ignore]
    fn invalid_version() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(1).unwrap(); // version
        data.write_u16::<BigEndian>(0).unwrap(); // num_tables

        let mut cursor = Cursor::new(data);
        assert!(CMap::from_binary(&mut cursor).is_err());
    }

    #[test]
    fn empty_subtables() {
        let mut data = Vec::new();

        data.write_u16::<BigEndian>(0).unwrap(); // version
        data.write_u16::<BigEndian>(0).unwrap(); // num_tables

        let mut cursor = Cursor::new(data);
        let cmap = CMap::from_binary(&mut cursor).unwrap();
        assert_eq!(cmap.version, 0);
        assert_eq!(cmap.num_tables, 0);
        assert_eq!(cmap.encoding_records.len(), cmap.num_tables as usize);
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
        let cmap = CMap::from_binary(&mut cursor).unwrap();
        assert_eq!(cmap.version, 0);
        assert_eq!(cmap.num_tables, 1);
        assert_eq!(cmap.encoding_records.len(), cmap.num_tables as usize);
        assert_eq!(cmap.encoding_records[0].platform_id, platform_id::WINDOWS);
        assert_eq!(
            cmap.encoding_records[0].encoding_id,
            encoding_id::WINDOWS_UNICODE_UCS4
        );
        assert_eq!(cmap.encoding_records[0].subtable_offset.address, 256);
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
        let cmap = CMap::from_binary(&mut cursor).unwrap();
        assert_eq!(cmap.version, 0);
        assert_eq!(cmap.num_tables, 2);
        assert_eq!(cmap.encoding_records.len(), cmap.num_tables as usize);
        assert_eq!(cmap.encoding_records[0].platform_id, platform_id::WINDOWS);
        assert_eq!(
            cmap.encoding_records[0].encoding_id,
            encoding_id::WINDOWS_UNICODE_UCS4
        );
        assert_eq!(cmap.encoding_records[0].subtable_offset.address, 256);
        assert_eq!(cmap.encoding_records[1].platform_id, platform_id::MACINTOSH);
        assert_eq!(
            cmap.encoding_records[1].encoding_id,
            encoding_id::MACINTOSH_APPLE_ROMAN
        );
        assert_eq!(cmap.encoding_records[1].subtable_offset.address, 513);
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
        assert!(CMap::from_binary(&mut cursor).is_err());
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
        assert!(CMap::from_binary(&mut cursor).is_err());
    }
}
