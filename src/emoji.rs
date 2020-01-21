use std::convert::TryFrom;

use crate::binary::read::{
    ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFixedSizeDep, ReadFrom, ReadScope,
};
use crate::binary::{U16Be, U32Be, I8, U8};
use crate::error::ParseError;
use crate::size;

/// `CBLC` — Color Bitmap Location Table
struct CBLCTable<'a> {
    pub major_version: u16,
    pub minor_version: u16,
    pub bitmap_sizes: ReadArray<'a, BitmapSize<'a>>,
}

/// `CBDT` — Color Bitmap Data Table
struct CBDTTable<'a> {
    pub major_version: u16,
    pub minor_version: u16,
    pub data: ReadScope<'a>,
}

pub enum GlyphBitmapData<'a> {
    Format1 {
        small_metrics: SmallGlyphMetrics,
        data: &'a [u8],
    },
    Format2 {
        small_metrics: SmallGlyphMetrics,
        data: &'a [u8],
    },
    // Format3 (obsolete, not in OpenType spec)
    // Format4 (not supported by OpenType, Apple specific)
    Format5 {
        data: &'a [u8],
    },
    Format6 {
        big_metrics: BigGlyphMetrics,
        data: &'a [u8],
    },
    Format7 {
        small_metrics: SmallGlyphMetrics,
        data: &'a [u8],
    },
    Format8 {
        small_metrics: SmallGlyphMetrics,
        components: ReadArray<'a, EbdtComponent>,
    },
    Format9 {
        big_metrics: BigGlyphMetrics,
        components: ReadArray<'a, EbdtComponent>,
    },
    // 10-16 are not defined
    Format17 {
        small_metrics: SmallGlyphMetrics,
        /// Raw PNG data
        data: &'a [u8],
    },
    Format18 {
        big_metrics: BigGlyphMetrics,
        /// Raw PNG data
        data: &'a [u8],
    },
    Format19 {
        /// Raw PNG data
        data: &'a [u8],
    },
}

pub struct EbdtComponent {
    /// Component glyph ID
    pub glyph_id: u16,
    /// Position of component left
    pub x_offset: i8,
    /// Position of component top
    pub y_offset: i8,
}

struct BitmapSize<'a> {
    /// Number of bytes in corresponding index subtables and array.
    pub index_tables_size: u32,
    /// There is an index subtable for each range or format change.
    pub number_of_index_sub_tables: u32,
    /// Line metrics for text rendered horizontally.
    pub hori: SbitLineMetrics,
    /// Line metrics for text rendered vertically.
    pub vert: SbitLineMetrics,
    /// Lowest glyph index for this size.
    pub start_glyph_index: u16,
    /// Highest glyph index for this size.
    pub end_glyph_index: u16,
    /// Horizontal pixels per em.
    pub ppem_x: u8,
    /// Vertical pixels per em.
    pub ppem_y: u8,
    /// Bit depth.
    ///
    /// In addition to already defined bitDepth values 1, 2, 4, and 8 supported by existing
    /// implementations, the value of 32 is used to identify color bitmaps with 8 bit per pixel
    /// RGBA channels.
    pub bit_depth: u8,
    /// Vertical or horizontal (see the Bitmap Flags section of the EBLC table).
    pub flags: i8,
    /// Index sub-table records.
    #[allow(dead_code)]
    index_sub_table_records: ReadArray<'a, IndexSubTableRecord>,
    /// Index sub-tables, one for each record.
    #[allow(dead_code)]
    index_sub_tables: Vec<IndexSubTable<'a>>,
}

pub struct SbitLineMetrics {
    pub ascender: i8,
    pub descender: i8,
    pub width_max: u8,
    pub caret_slope_numerator: i8,
    pub caret_slope_denominator: i8,
    pub caret_offset: i8,
    pub min_origin_sb: i8,
    pub min_advance_sb: i8,
    pub max_before_bl: i8,
    pub min_after_bl: i8,
    pub pad1: i8,
    pub pad2: i8,
}

pub struct IndexSubTableRecord {
    /// First glyph ID of this range.
    pub first_glyph_index: u16,
    /// Last glyph ID of this range (inclusive).
    pub last_glyph_index: u16,
    // Add to indexSubTableArrayOffset to get offset from beginning of EBLC.
    additional_offset_to_index_sub_table: u32,
}

pub enum IndexSubTable<'a> {
    Format1 {
        /// Format of EBDT image data.
        image_format: ImageFormat,
        /// Offset to image data in EBDT table.
        image_data_offset: u32,
        offsets: ReadArray<'a, U32Be>,
    },
    Format2 {
        /// Format of EBDT image data.
        image_format: ImageFormat,
        /// Offset to image data in EBDT table.
        image_data_offset: u32,
        image_size: u32,
        big_metrics: BigGlyphMetrics,
    },
    Format3 {
        /// Format of EBDT image data.
        image_format: ImageFormat,
        /// Offset to image data in EBDT table.
        image_data_offset: u32,
        offsets: ReadArray<'a, U16Be>,
    },
    Format4 {
        /// Format of EBDT image data.
        image_format: ImageFormat,
        /// Offset to image data in EBDT table.
        image_data_offset: u32,
        num_glyphs: u32,
        glyph_array: ReadArray<'a, GlyphOffsetPair>,
    },
    Format5 {
        /// Format of EBDT image data.
        image_format: ImageFormat,
        /// Offset to image data in EBDT table.
        image_data_offset: u32,
        /// All glyphs have the same data size.
        image_size: u32,
        /// All glyphs have the same metrics.
        big_metrics: BigGlyphMetrics,
        /// Array length.
        num_glyphs: u32,
        /// One per glyph, sorted by glyph ID.
        glyph_id_array: ReadArray<'a, U16Be>,
    },
}

/// Valid image formats
pub enum ImageFormat {
    Format1,
    Format2,
    Format5,
    Format6,
    Format7,
    Format8,
    Format9,
    Format17,
    Format18,
    Format19,
}

pub struct SmallGlyphMetrics {
    pub height: u8,
    pub width: u8,
    pub bearing_x: i8,
    pub bearing_y: i8,
    pub advance: u8,
}

pub struct BigGlyphMetrics {
    pub height: u8,
    pub width: u8,
    pub hori_bearing_x: i8,
    pub hori_bearing_y: i8,
    pub hori_advance: u8,
    pub vert_bearing_x: i8,
    pub vert_bearing_y: i8,
    pub vert_advance: u8,
}

pub struct GlyphOffsetPair {
    /// Glyph ID of glyph present.
    pub glyph_id: u16,
    /// Location in EBDT.
    pub offset: u16,
}

impl<'a> ReadBinary<'a> for CBLCTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let table = ctxt.scope();

        let major_version = ctxt.read_u16be()?;
        // version 2 is EBLT, version 3 is CBLC, 3 is backward compatible but defines additional
        // formats and bit depth.
        ctxt.check_version(major_version >= 2 && major_version <= 3)?;
        let minor_version = ctxt.read_u16be()?;
        let num_sizes = ctxt.read_u32be()?;
        let bitmap_sizes = ctxt.read_array_dep(usize::try_from(num_sizes)?, table)?;

        Ok(CBLCTable {
            major_version,
            minor_version,
            bitmap_sizes,
        })
    }
}

impl<'a> ReadBinary<'a> for CBDTTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        // The locators in the CBLC table are relative to the start of the CBDT table.
        // So we hold on to a scope at the start of the table for later use.
        let data = ctxt.scope();
        let major_version = ctxt.read_u16be()?;
        // version 2 is EBLT, version 3 is CBLC, 3 is backward compatible but defines additional
        // formats and bit depth.
        ctxt.check_version(major_version >= 2 && major_version <= 3)?;
        let minor_version = ctxt.read_u16be()?;
        Ok(CBDTTable {
            major_version,
            minor_version,
            data,
        })
    }
}

impl<'a> ReadBinaryDep<'a> for BitmapSize<'a> {
    type HostType = Self;
    type Args = ReadScope<'a>;

    fn read_dep(
        ctxt: &mut ReadCtxt<'a>,
        /* TODO: rename */ scope: Self::Args,
    ) -> Result<Self, ParseError> {
        let index_sub_table_array_offset = usize::try_from(ctxt.read_u32be()?)?;
        let index_tables_size = ctxt.read_u32be()?;
        let number_of_index_sub_tables = ctxt.read_u32be()?;
        let _color_ref = ctxt.read_u32be()?; // Not used; set to 0.
        let hori = ctxt.read::<SbitLineMetrics>()?;
        let vert = ctxt.read::<SbitLineMetrics>()?;
        let start_glyph_index = ctxt.read_u16be()?;
        let end_glyph_index = ctxt.read_u16be()?;
        let ppem_x = ctxt.read_u8()?;
        let ppem_y = ctxt.read_u8()?;
        let bit_depth = ctxt.read_u8()?;
        let flags = ctxt.read_i8()?;

        // Read the index sub tables
        let index_sub_table_records: ReadArray<'_, IndexSubTableRecord> = scope
            .offset(index_sub_table_array_offset)
            .ctxt()
            .read_array::<IndexSubTableRecord>(usize::try_from(number_of_index_sub_tables)?)?;
        let mut index_sub_tables = Vec::with_capacity(usize::try_from(index_tables_size)?);
        for index_sub_table_record in index_sub_table_records.iter() {
            let offset = index_sub_table_array_offset
                .checked_add(usize::try_from(
                    index_sub_table_record.additional_offset_to_index_sub_table,
                )?)
                .ok_or(ParseError::BadOffset)?;
            // Read the index sub table
            let index_sub_table = scope
                .offset(offset)
                .ctxt()
                .read_dep::<IndexSubTable<'_>>((start_glyph_index, end_glyph_index))?;
            index_sub_tables.push(index_sub_table);
        }

        Ok(BitmapSize {
            index_tables_size,
            number_of_index_sub_tables,
            hori,
            vert,
            start_glyph_index,
            end_glyph_index,
            ppem_x,
            ppem_y,
            bit_depth,
            flags,
            index_sub_table_records,
            index_sub_tables,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for BitmapSize<'a> {
    fn size(_scope: Self::Args) -> usize {
        (4 * size::U32) + (2 * SbitLineMetrics::size(())) + (2 * size::U16) + 4
    }
}

impl<'a> ReadBinary<'a> for SbitLineMetrics {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let ascender = ctxt.read_i8()?;
        let descender = ctxt.read_i8()?;
        let width_max = ctxt.read_u8()?;
        let caret_slope_numerator = ctxt.read_i8()?;
        let caret_slope_denominator = ctxt.read_i8()?;
        let caret_offset = ctxt.read_i8()?;
        let min_origin_sb = ctxt.read_i8()?;
        let min_advance_sb = ctxt.read_i8()?;
        let max_before_bl = ctxt.read_i8()?;
        let min_after_bl = ctxt.read_i8()?;
        let pad1 = ctxt.read_i8()?;
        let pad2 = ctxt.read_i8()?;

        Ok(SbitLineMetrics {
            ascender,
            descender,
            width_max,
            caret_slope_numerator,
            caret_slope_denominator,
            caret_offset,
            min_origin_sb,
            min_advance_sb,
            max_before_bl,
            min_after_bl,
            pad1,
            pad2,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for SbitLineMetrics {
    fn size(_scope: Self::Args) -> usize {
        // 12 fields, all 1 byte
        12
    }
}

impl<'a> ReadFrom<'a> for IndexSubTableRecord {
    type ReadType = (U16Be, U16Be, U32Be);

    fn from(
        (first_glyph_index, last_glyph_index, additional_offset_to_index_sub_table): (
            u16,
            u16,
            u32,
        ),
    ) -> Self {
        IndexSubTableRecord {
            first_glyph_index,
            last_glyph_index,
            additional_offset_to_index_sub_table,
        }
    }
}

impl<'a> ReadBinaryDep<'a> for IndexSubTable<'a> {
    type HostType = Self;
    type Args = (u16, u16);

    fn read_dep(
        ctxt: &mut ReadCtxt<'a>,
        (first_glyph_index, last_glyph_index): (u16, u16),
    ) -> Result<Self, ParseError> {
        let index_format = ctxt.read_u16be()?;
        let image_format = ImageFormat::try_from(ctxt.read_u16be()?)?;
        let image_data_offset = ctxt.read_u32be()?;

        match index_format {
            1 => {
                // +1 for last_glyph_index being inclusive,
                // +1 for there being an extra record at the end
                let offsets = ctxt.read_array::<U32Be>(usize::from(
                    last_glyph_index - first_glyph_index + 1 + 1,
                ))?;
                Ok(IndexSubTable::Format1 {
                    image_format,
                    image_data_offset,
                    offsets,
                })
            }
            2 => {
                let image_size = ctxt.read_u32be()?;
                let big_metrics = ctxt.read::<BigGlyphMetrics>()?;
                Ok(IndexSubTable::Format2 {
                    image_format,
                    image_data_offset,
                    image_size,
                    big_metrics,
                })
            }
            3 => {
                // +1 for last_glyph_index being inclusive,
                // +1 for there being an extra record at the end
                let offsets = ctxt.read_array::<U16Be>(usize::from(
                    last_glyph_index - first_glyph_index + 1 + 1,
                ))?;
                Ok(IndexSubTable::Format3 {
                    image_format,
                    image_data_offset,
                    offsets,
                })
            }
            4 => {
                let num_glyphs = ctxt.read_u32be()?;
                let glyph_array =
                    ctxt.read_array::<GlyphOffsetPair>(usize::try_from(num_glyphs + 1)?)?;
                Ok(IndexSubTable::Format4 {
                    image_format,
                    image_data_offset,
                    num_glyphs,
                    glyph_array,
                })
            }
            5 => {
                let image_size = ctxt.read_u32be()?;
                let big_metrics = ctxt.read::<BigGlyphMetrics>()?;
                let num_glyphs = ctxt.read_u32be()?;
                let glyph_id_array = ctxt.read_array::<U16Be>(usize::try_from(num_glyphs)?)?;
                Ok(IndexSubTable::Format5 {
                    image_format,
                    image_data_offset,
                    image_size,
                    big_metrics,
                    num_glyphs,
                    glyph_id_array,
                })
            }
            _ => Err(ParseError::BadValue),
        }
    }
}

impl<'a> ReadFrom<'a> for SmallGlyphMetrics {
    type ReadType = ((U8, U8), (I8, I8, U8));

    fn from(((height, width), (bearing_x, bearing_y, advance)): ((u8, u8), (i8, i8, u8))) -> Self {
        SmallGlyphMetrics {
            height,
            width,
            bearing_x,
            bearing_y,
            advance,
        }
    }
}

impl<'a> ReadBinary<'a> for BigGlyphMetrics {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let height = ctxt.read_u8()?;
        let width = ctxt.read_u8()?;
        let hori_bearing_x = ctxt.read_i8()?;
        let hori_bearing_y = ctxt.read_i8()?;
        let hori_advance = ctxt.read_u8()?;
        let vert_bearing_x = ctxt.read_i8()?;
        let vert_bearing_y = ctxt.read_i8()?;
        let vert_advance = ctxt.read_u8()?;

        Ok(BigGlyphMetrics {
            height,
            width,
            hori_bearing_x,
            hori_bearing_y,
            hori_advance,
            vert_bearing_x,
            vert_bearing_y,
            vert_advance,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for BigGlyphMetrics {
    fn size(_scope: Self::Args) -> usize {
        // 8 fields, all 1 byte
        8
    }
}

impl<'a> ReadFrom<'a> for GlyphOffsetPair {
    type ReadType = (U16Be, U16Be);

    fn from((glyph_id, offset): (u16, u16)) -> Self {
        GlyphOffsetPair { glyph_id, offset }
    }
}

impl<'a> ReadFrom<'a> for EbdtComponent {
    type ReadType = (U16Be, I8, I8);

    fn from((glyph_id, x_offset, y_offset): (u16, i8, i8)) -> Self {
        EbdtComponent {
            glyph_id,
            x_offset,
            y_offset,
        }
    }
}

impl TryFrom<u16> for ImageFormat {
    type Error = ParseError;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(ImageFormat::Format1),
            2 => Ok(ImageFormat::Format2),
            5 => Ok(ImageFormat::Format5),
            6 => Ok(ImageFormat::Format6),
            7 => Ok(ImageFormat::Format7),
            8 => Ok(ImageFormat::Format8),
            9 => Ok(ImageFormat::Format9),
            17 => Ok(ImageFormat::Format17),
            18 => Ok(ImageFormat::Format18),
            19 => Ok(ImageFormat::Format19),
            _ => Err(ParseError::BadValue),
        }
    }
}
