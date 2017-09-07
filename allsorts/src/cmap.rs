use std::io::{self, Read};
use byteorder::{BigEndian, ReadBytesExt};

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

pub struct Cmap {
    pub version: u16,
    pub encoding_records: Vec<EncodingRecord>,
}

impl Cmap {
    pub fn decode<R: Read + ?Sized>(reader: &mut R) -> io::Result<Cmap> {
        let version = reader.read_u16::<BigEndian>()?;
        let num_tables = reader.read_u16::<BigEndian>()?;
        let encoding_records = (0..num_tables)
            .map(|_| EncodingRecord::decode(reader))
            .collect::<io::Result<_>>()?;

        Ok(Cmap {
            version,
            encoding_records,
        })
    }
}

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
