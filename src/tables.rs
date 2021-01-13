//! OpenType font table parsing and writing.

pub mod cmap;
pub mod glyf;
pub mod loca;
pub mod os2;
pub mod svg;

use crate::binary::read::{
    CheckIndex, ReadArray, ReadArrayCow, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope,
};
use crate::binary::write::{Placeholder, WriteBinary, WriteContext};
use crate::binary::{I16Be, I32Be, I64Be, U16Be, U32Be};
use crate::error::{ParseError, WriteError};
use crate::size;
use crate::tag;

use std::borrow::Cow;
use std::convert::TryFrom;

/// Magic value identifying a CFF font (`OTTO`)
pub const CFF_MAGIC: u32 = tag::OTTO;

/// Magic number identifying TrueType 1.0
///
/// The version number 1.0 as a 16.16 fixed-point value, indicating TrueType glyph data.
pub const TTF_MAGIC: u32 = 0x00010000;

/// Magic value identifying a TrueType font collection `ttcf`
pub const TTCF_MAGIC: u32 = tag::TTCF;

/// 32-bit signed fixed-point number (16.16)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Fixed(i32);

/// Date represented in number of seconds since 12:00 midnight, January 1, 1904
///
/// The value is represented as a signed 64-bit integer.
type LongDateTime = i64;

pub trait FontTableProvider {
    /// Return data for the specified table if present
    fn table_data<'a>(&'a self, tag: u32) -> Result<Option<Cow<'a, [u8]>>, ParseError>;

    fn has_table<'a>(&'a self, tag: u32) -> bool;

    fn read_table_data<'a>(&'a self, tag: u32) -> Result<Cow<'a, [u8]>, ParseError> {
        self.table_data(tag)?.ok_or(ParseError::MissingValue)
    }
}

/// The F2DOT14 format consists of a signed, 2’s complement integer and an unsigned fraction.
///
/// To compute the actual value, take the integer and add the fraction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct F2Dot14(u16);

/// The size of the offsets in the `loca` table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/loca>
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IndexToLocFormat {
    /// Offsets are 16-bit. The actual local offset divided by 2 is stored.
    Short,
    /// Offsets are 32-bit. The actual local offset is stored.
    Long,
}

pub struct OpenTypeFont<'a> {
    pub scope: ReadScope<'a>,
    pub data: OpenTypeData<'a>,
}

/// An OpenTypeFont containing a single font or a collection of fonts
pub enum OpenTypeData<'a> {
    Single(OffsetTable<'a>),
    Collection(TTCHeader<'a>),
}

/// TrueType collection header
pub struct TTCHeader<'a> {
    pub major_version: u16,
    pub minor_version: u16,
    pub offset_tables: ReadArray<'a, U32Be>,
    // TODO add digital signature fields
}

/// OpenType Offset Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
#[derive(Clone)]
pub struct OffsetTable<'a> {
    pub sfnt_version: u32,
    pub search_range: u16,
    pub entry_selector: u16,
    pub range_shift: u16,
    pub table_records: ReadArray<'a, TableRecord>,
}

pub struct OffsetTableFontProvider<'a> {
    scope: ReadScope<'a>,
    offset_table: Cow<'a, OffsetTable<'a>>,
}

/// An entry in the Offset Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Hash)]
pub struct TableRecord {
    pub table_tag: u32,
    pub checksum: u32,
    pub offset: u32,
    pub length: u32,
}

/// `head` table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/head>
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct HeadTable {
    pub major_version: u16,
    pub minor_version: u16,
    pub font_revision: Fixed,
    pub check_sum_adjustment: u32,
    pub magic_number: u32,
    pub flags: u16,
    pub units_per_em: u16,
    pub created: LongDateTime,
    pub modified: LongDateTime,
    pub x_min: i16,
    pub y_min: i16,
    pub x_max: i16,
    pub y_max: i16,
    pub mac_style: u16,
    pub lowest_rec_ppem: u16,
    pub font_direction_hint: i16,
    pub index_to_loc_format: IndexToLocFormat,
    pub glyph_data_format: i16,
}

/// `hhea` horizontal header table
///
/// > This table contains information for horizontal layout.
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/hhea>
///
/// This struct is also used for the `vhea` table.
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct HheaTable {
    pub ascender: i16,
    pub descender: i16,
    pub line_gap: i16,
    pub advance_width_max: u16,
    pub min_left_side_bearing: i16,
    pub min_right_side_bearing: i16,
    pub x_max_extent: i16,
    pub caret_slope_rise: i16,
    pub caret_slope_run: i16,
    pub caret_offset: i16,
    pub num_h_metrics: u16,
}

/// `hmtx` horizontal metrics table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx>
///
/// This struct is also used for `vmtx` table.
#[derive(Debug)]
pub struct HmtxTable<'a> {
    pub h_metrics: ReadArrayCow<'a, LongHorMetric>,
    pub left_side_bearings: ReadArrayCow<'a, I16Be>,
}

/// A `longHorMetric` record in the `hmtx` table.
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx>
///
/// This struct is also used for LongVerMetric `vmtx` table.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct LongHorMetric {
    pub advance_width: u16,
    pub lsb: i16,
}

/// maxp - Maximum profile
///
/// This table establishes the memory requirements for this font. Fonts with CFF data must use
/// Version 0.5 of this table, specifying only the numGlyphs field. Fonts with TrueType outlines
/// must use Version 1.0 of this table, where all data is required.
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/maxp>
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct MaxpTable {
    pub num_glyphs: u16,
    /// Extra fields, present if maxp table is version 1.0, absent if version 0.5.
    pub version1_sub_table: Option<MaxpVersion1SubTable>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct MaxpVersion1SubTable {
    /// Maximum points in a non-composite glyph.
    pub max_points: u16,
    /// Maximum contours in a non-composite glyph.
    pub max_contours: u16,
    /// Maximum points in a composite glyph.
    pub max_composite_points: u16,
    /// Maximum contours in a composite glyph.
    pub max_composite_contours: u16,
    /// 1 if instructions do not use the twilight zone (Z0), or 2 if instructions do use Z0; should
    /// be set to 2 in most cases.
    pub max_zones: u16,
    /// Maximum points used in Z0.
    pub max_twilight_points: u16,
    /// Number of Storage Area locations.
    pub max_storage: u16,
    /// Number of FDEFs, equal to the highest function number + 1.
    pub max_function_defs: u16,
    /// Number of IDEFs.
    pub max_instruction_defs: u16,
    /// Maximum stack depth across Font Program ('fpgm' table), CVT Program ('prep' table) and all
    /// glyph instructions (in the 'glyf' table).
    pub max_stack_elements: u16,
    /// Maximum byte count for glyph instructions.
    pub max_size_of_instructions: u16,
    /// Maximum number of components referenced at “top level” for any composite glyph.
    pub max_component_elements: u16,
    /// Maximum levels of recursion; 1 for simple components.
    pub max_component_depth: u16,
}

/// `name` table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/name>
pub struct NameTable<'a> {
    pub string_storage: ReadScope<'a>,
    pub name_records: ReadArray<'a, NameRecord>,
    pub opt_langtag_records: Option<ReadArray<'a, LangTagRecord>>,
}

/// Record within the `name` table
pub struct NameRecord {
    pub platform_id: u16,
    pub encoding_id: u16,
    pub language_id: u16,
    pub name_id: u16,
    pub length: u16,
    pub offset: u16,
}

/// Language-tag record within the `name` table
pub struct LangTagRecord {
    pub length: u16,
    pub offset: u16,
}

impl<'a> OpenTypeFont<'a> {
    pub fn table_provider(
        &'a self,
        index: usize,
    ) -> Result<OffsetTableFontProvider<'a>, ParseError> {
        match &self.data {
            OpenTypeData::Single(offset_table) => Ok(OffsetTableFontProvider {
                offset_table: Cow::Borrowed(offset_table),
                scope: self.scope.clone(),
            }),
            OpenTypeData::Collection(ttc) => ttc
                .offset_tables
                .check_index(index)
                .and_then(|()| ttc.offset_tables.read_item(index))
                .and_then(|offset| usize::try_from(offset).map_err(ParseError::from))
                .and_then(|offset| self.scope.offset(offset).read::<OffsetTable<'_>>())
                .map(|offset_table| OffsetTableFontProvider {
                    offset_table: Cow::Owned(offset_table),
                    scope: self.scope.clone(),
                }),
        }
    }
}

impl<'a> ReadBinary<'a> for OpenTypeFont<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let mut peek = ctxt.clone();
        let magic = peek.read_u32be()?;
        match magic {
            TTF_MAGIC | CFF_MAGIC => {
                let offset_table = ctxt.read::<OffsetTable<'_>>()?;
                let font = OpenTypeData::Single(offset_table);
                Ok(OpenTypeFont { scope, data: font })
            }
            TTCF_MAGIC => {
                let ttc_header = ctxt.read::<TTCHeader<'_>>()?;
                let font = OpenTypeData::Collection(ttc_header);
                Ok(OpenTypeFont { scope, data: font })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a> ReadBinary<'a> for TTCHeader<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let ttc_tag = ctxt.read_u32be()?;
        match ttc_tag {
            TTCF_MAGIC => {
                let major_version = ctxt.read_u16be()?;
                let minor_version = ctxt.read_u16be()?;
                ctxt.check(major_version == 1 || major_version == 2)?;
                let num_fonts = usize::try_from(ctxt.read_u32be()?)?;
                let offset_tables = ctxt.read_array::<U32Be>(num_fonts)?;
                // TODO read digital signature fields in TTCHeader version 2
                Ok(TTCHeader {
                    major_version,
                    minor_version,
                    offset_tables,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a> ReadBinary<'a> for OffsetTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let sfnt_version = ctxt.read_u32be()?;
        match sfnt_version {
            TTF_MAGIC | CFF_MAGIC => {
                let num_tables = ctxt.read_u16be()?;
                let search_range = ctxt.read_u16be()?;
                let entry_selector = ctxt.read_u16be()?;
                let range_shift = ctxt.read_u16be()?;
                let table_records = ctxt.read_array::<TableRecord>(usize::from(num_tables))?;
                Ok(OffsetTable {
                    sfnt_version,
                    search_range,
                    entry_selector,
                    range_shift,
                    table_records,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a> FontTableProvider for OffsetTableFontProvider<'a> {
    fn table_data<'b>(&'b self, tag: u32) -> Result<Option<Cow<'b, [u8]>>, ParseError> {
        self.offset_table
            .read_table(&self.scope, tag)
            .map(|scope| scope.map(|scope| Cow::Borrowed(scope.data())))
    }

    fn has_table<'b>(&'b self, tag: u32) -> bool {
        self.offset_table.find_table_record(tag).is_some()
    }
}

impl<'a> ReadFrom<'a> for TableRecord {
    type ReadType = ((U32Be, U32Be), (U32Be, U32Be));
    fn from(((table_tag, checksum), (offset, length)): ((u32, u32), (u32, u32))) -> Self {
        TableRecord {
            table_tag,
            checksum,
            offset,
            length,
        }
    }
}

impl WriteBinary<&Self> for TableRecord {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &TableRecord) -> Result<(), WriteError> {
        U32Be::write(ctxt, table.table_tag)?;
        U32Be::write(ctxt, table.checksum)?;
        U32Be::write(ctxt, table.offset)?;
        U32Be::write(ctxt, table.length)?;

        Ok(())
    }
}

impl<'a> OffsetTable<'a> {
    pub fn find_table_record(&self, tag: u32) -> Option<TableRecord> {
        for table_record in &self.table_records {
            if table_record.table_tag == tag {
                return Some(table_record);
            }
        }
        None
    }

    pub fn read_table(
        &self,
        scope: &ReadScope<'a>,
        tag: u32,
    ) -> Result<Option<ReadScope<'a>>, ParseError> {
        if let Some(table_record) = self.find_table_record(tag) {
            let table = table_record.read_table(&scope)?;
            Ok(Some(table))
        } else {
            Ok(None)
        }
    }
}

impl TableRecord {
    pub const SIZE: usize = 4 * size::U32;

    pub fn read_table<'a>(&self, scope: &ReadScope<'a>) -> Result<ReadScope<'a>, ParseError> {
        let offset = usize::try_from(self.offset)?;
        let length = usize::try_from(self.length)?;
        scope.offset_length(offset, length)
    }
}

impl<'a> ReadBinary<'a> for HeadTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let major_version = ctxt.read::<U16Be>()?;
        let minor_version = ctxt.read::<U16Be>()?;
        let font_revision = ctxt.read::<Fixed>()?;
        let check_sum_adjustment = ctxt.read::<U32Be>()?;
        let magic_number = ctxt.read::<U32Be>()?;
        ctxt.check(magic_number == 0x5F0F3CF5)?;
        let flags = ctxt.read::<U16Be>()?;
        let units_per_em = ctxt.read::<U16Be>()?;
        let created = ctxt.read::<I64Be>()?;
        let modified = ctxt.read::<I64Be>()?;
        let x_min = ctxt.read::<I16Be>()?;
        let y_min = ctxt.read::<I16Be>()?;
        let x_max = ctxt.read::<I16Be>()?;
        let y_max = ctxt.read::<I16Be>()?;
        let mac_style = ctxt.read::<U16Be>()?;
        let lowest_rec_ppem = ctxt.read::<U16Be>()?;
        let font_direction_hint = ctxt.read::<I16Be>()?;
        let index_to_loc_format = ctxt.read::<IndexToLocFormat>()?;
        let glyph_data_format = ctxt.read::<I16Be>()?;

        Ok(HeadTable {
            major_version,
            minor_version,
            font_revision,
            check_sum_adjustment,
            magic_number,
            flags,
            units_per_em,
            created,
            modified,
            x_min,
            y_min,
            x_max,
            y_max,
            mac_style,
            lowest_rec_ppem,
            font_direction_hint,
            index_to_loc_format,
            glyph_data_format,
        })
    }
}

impl<'a> WriteBinary<&Self> for HeadTable {
    type Output = Placeholder<U32Be, u32>;

    /// Writes the table to the `WriteContext` and returns a placeholder to the `check_sum_adjustment` field.
    ///
    /// The `check_sum_adjustment` field requires special handling to calculate. See:
    /// https://docs.microsoft.com/en-us/typography/opentype/spec/head
    fn write<C: WriteContext>(ctxt: &mut C, table: &HeadTable) -> Result<Self::Output, WriteError> {
        U16Be::write(ctxt, table.major_version)?;
        U16Be::write(ctxt, table.minor_version)?;
        Fixed::write(ctxt, table.font_revision)?;
        let check_sum_adjustment = ctxt.placeholder()?;
        U32Be::write(ctxt, table.magic_number)?;
        U16Be::write(ctxt, table.flags)?;
        U16Be::write(ctxt, table.units_per_em)?;
        I64Be::write(ctxt, table.created)?;
        I64Be::write(ctxt, table.modified)?;
        I16Be::write(ctxt, table.x_min)?;
        I16Be::write(ctxt, table.y_min)?;
        I16Be::write(ctxt, table.x_max)?;
        I16Be::write(ctxt, table.y_max)?;
        U16Be::write(ctxt, table.mac_style)?;
        U16Be::write(ctxt, table.lowest_rec_ppem)?;
        I16Be::write(ctxt, table.font_direction_hint)?;
        IndexToLocFormat::write(ctxt, table.index_to_loc_format)?;
        I16Be::write(ctxt, table.glyph_data_format)?;

        Ok(check_sum_adjustment)
    }
}

impl HeadTable {
    // macStyle:
    // Bit 0: Bold (if set to 1);
    // Bit 1: Italic (if set to 1)
    // Bit 2: Underline (if set to 1)
    // Bit 3: Outline (if set to 1)
    // Bit 4: Shadow (if set to 1)
    // Bit 5: Condensed (if set to 1)
    // Bit 6: Extended (if set to 1)
    // Bits 7–15: Reserved (set to 0).
    // https://docs.microsoft.com/en-us/typography/opentype/spec/head
    pub fn is_bold(&self) -> bool {
        self.mac_style & 1 != 0
    }

    pub fn is_italic(&self) -> bool {
        self.mac_style & 2 != 0
    }
}

impl<'a> ReadBinary<'a> for HheaTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let major_version = ctxt.read_u16be()?;
        let _minor_version = ctxt.read_u16be()?;
        ctxt.check(major_version == 1)?;
        let ascender = ctxt.read_i16be()?;
        let descender = ctxt.read_i16be()?;
        let line_gap = ctxt.read_i16be()?;
        let advance_width_max = ctxt.read_u16be()?;
        let min_left_side_bearing = ctxt.read_i16be()?;
        let min_right_side_bearing = ctxt.read_i16be()?;
        let x_max_extent = ctxt.read_i16be()?;
        let caret_slope_rise = ctxt.read_i16be()?;
        let caret_slope_run = ctxt.read_i16be()?;
        let caret_offset = ctxt.read_i16be()?;
        let _reserved1 = ctxt.read_i16be()?;
        let _reserved2 = ctxt.read_i16be()?;
        let _reserved3 = ctxt.read_i16be()?;
        let _reserved4 = ctxt.read_i16be()?;
        let metric_data_format = ctxt.read_i16be()?;
        ctxt.check(metric_data_format == 0)?;
        let num_h_metrics = ctxt.read_u16be()?;

        Ok(HheaTable {
            ascender,
            descender,
            line_gap,
            advance_width_max,
            min_left_side_bearing,
            min_right_side_bearing,
            x_max_extent,
            caret_slope_rise,
            caret_slope_run,
            caret_offset,
            num_h_metrics,
        })
    }
}

impl<'a> WriteBinary<&Self> for HheaTable {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &HheaTable) -> Result<(), WriteError> {
        U16Be::write(ctxt, 1u16)?; // major_version
        U16Be::write(ctxt, 0u16)?; // minor_version

        I16Be::write(ctxt, table.ascender)?;
        I16Be::write(ctxt, table.descender)?;
        I16Be::write(ctxt, table.line_gap)?;
        U16Be::write(ctxt, table.advance_width_max)?;
        I16Be::write(ctxt, table.min_left_side_bearing)?;
        I16Be::write(ctxt, table.min_right_side_bearing)?;
        I16Be::write(ctxt, table.x_max_extent)?;
        I16Be::write(ctxt, table.caret_slope_rise)?;
        I16Be::write(ctxt, table.caret_slope_run)?;
        I16Be::write(ctxt, table.caret_offset)?;

        I16Be::write(ctxt, 0i16)?; // reserved
        I16Be::write(ctxt, 0i16)?; // reserved
        I16Be::write(ctxt, 0i16)?; // reserved
        I16Be::write(ctxt, 0i16)?; // reserved

        I16Be::write(ctxt, 0i16)?; // metric_data_format

        U16Be::write(ctxt, table.num_h_metrics)?;

        Ok(())
    }
}

impl<'a> ReadBinaryDep<'a> for HmtxTable<'a> {
    type Args = (usize, usize); // num_glyphs, num_h_metrics
    type HostType = Self;

    fn read_dep(
        ctxt: &mut ReadCtxt<'a>,
        (num_glyphs, num_h_metrics): (usize, usize),
    ) -> Result<Self, ParseError> {
        let h_metrics = ctxt.read_array::<LongHorMetric>(num_h_metrics)?;
        let left_side_bearings =
            ctxt.read_array::<I16Be>(num_glyphs.saturating_sub(num_h_metrics))?;
        Ok(HmtxTable {
            h_metrics: ReadArrayCow::Borrowed(h_metrics),
            left_side_bearings: ReadArrayCow::Borrowed(left_side_bearings),
        })
    }
}

impl<'a> WriteBinary<&Self> for HmtxTable<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &HmtxTable<'a>) -> Result<(), WriteError> {
        ReadArrayCow::write(ctxt, &table.h_metrics)?;
        ReadArrayCow::write(ctxt, &table.left_side_bearings)?;

        Ok(())
    }
}

impl<'a> HmtxTable<'a> {
    pub fn horizontal_advance(&self, glyph_id: u16, num_h_metrics: u16) -> Result<u16, ParseError> {
        // As an optimization, the number of records can be less than the number of glyphs, in
        // which case the advance width value of the last record applies to all remaining glyph
        // IDs. -- https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx
        let index = if glyph_id < num_h_metrics {
            usize::from(glyph_id)
        } else {
            usize::from(num_h_metrics.checked_sub(1).ok_or(ParseError::BadIndex)?)
        };

        self.h_metrics
            .check_index(index)
            .and_then(|_| self.h_metrics.read_item(index))
            .map(|long_hor_metric| long_hor_metric.advance_width)
    }
}

impl<'a> ReadFrom<'a> for LongHorMetric {
    type ReadType = (U16Be, I16Be);
    fn from((advance_width, lsb): (u16, i16)) -> Self {
        LongHorMetric { advance_width, lsb }
    }
}

impl<'a> WriteBinary for LongHorMetric {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, metric: LongHorMetric) -> Result<(), WriteError> {
        U16Be::write(ctxt, metric.advance_width)?;
        I16Be::write(ctxt, metric.lsb)?;

        Ok(())
    }
}

impl<'a> ReadBinary<'a> for MaxpTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let version = ctxt.read_u32be()?;
        let num_glyphs = ctxt.read_u16be()?;
        let sub_table = if version == 0x00010000 {
            Some(ctxt.read::<MaxpVersion1SubTable>()?)
        } else {
            None
        };
        Ok(MaxpTable {
            num_glyphs,
            version1_sub_table: sub_table,
        })
    }
}

impl<'a> WriteBinary<&Self> for MaxpTable {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: &MaxpTable) -> Result<(), WriteError> {
        if let Some(sub_table) = &table.version1_sub_table {
            U32Be::write(ctxt, 0x00010000u32)?; // version 1.0
            U16Be::write(ctxt, table.num_glyphs)?;
            MaxpVersion1SubTable::write(ctxt, sub_table)?;
        } else {
            U32Be::write(ctxt, 0x00005000u32)?; // version 0.5
            U16Be::write(ctxt, table.num_glyphs)?;
        }
        Ok(())
    }
}

impl<'a> ReadBinary<'a> for MaxpVersion1SubTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let max_points = ctxt.read_u16be()?;
        let max_contours = ctxt.read_u16be()?;
        let max_composite_points = ctxt.read_u16be()?;
        let max_composite_contours = ctxt.read_u16be()?;
        let max_zones = ctxt.read_u16be()?;
        let max_twilight_points = ctxt.read_u16be()?;
        let max_storage = ctxt.read_u16be()?;
        let max_function_defs = ctxt.read_u16be()?;
        let max_instruction_defs = ctxt.read_u16be()?;
        let max_stack_elements = ctxt.read_u16be()?;
        let max_size_of_instructions = ctxt.read_u16be()?;
        let max_component_elements = ctxt.read_u16be()?;
        let max_component_depth = ctxt.read_u16be()?;

        Ok(MaxpVersion1SubTable {
            max_points,
            max_contours,
            max_composite_points,
            max_composite_contours,
            max_zones,
            max_twilight_points,
            max_storage,
            max_function_defs,
            max_instruction_defs,
            max_stack_elements,
            max_size_of_instructions,
            max_component_elements,
            max_component_depth,
        })
    }
}

impl<'a> WriteBinary<&Self> for MaxpVersion1SubTable {
    type Output = ();

    fn write<C: WriteContext>(
        ctxt: &mut C,
        table: &MaxpVersion1SubTable,
    ) -> Result<(), WriteError> {
        U16Be::write(ctxt, table.max_points)?;
        U16Be::write(ctxt, table.max_contours)?;
        U16Be::write(ctxt, table.max_composite_points)?;
        U16Be::write(ctxt, table.max_composite_contours)?;
        U16Be::write(ctxt, table.max_zones)?;
        U16Be::write(ctxt, table.max_twilight_points)?;
        U16Be::write(ctxt, table.max_storage)?;
        U16Be::write(ctxt, table.max_function_defs)?;
        U16Be::write(ctxt, table.max_instruction_defs)?;
        U16Be::write(ctxt, table.max_stack_elements)?;
        U16Be::write(ctxt, table.max_size_of_instructions)?;
        U16Be::write(ctxt, table.max_component_elements)?;
        U16Be::write(ctxt, table.max_component_depth)?;

        Ok(())
    }
}

impl<'a> ReadBinary<'a> for NameTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();

        let format = ctxt.read_u16be()?;
        ctxt.check(format <= 1)?;
        let count = usize::from(ctxt.read_u16be()?);
        let string_offset = usize::from(ctxt.read_u16be()?);
        let string_storage = scope.offset(string_offset);
        let name_records = ctxt.read_array::<NameRecord>(count)?;
        let opt_langtag_records = if format > 0 {
            let langtag_count = usize::from(ctxt.read_u16be()?);
            let langtag_records = ctxt.read_array::<LangTagRecord>(langtag_count)?;
            Some(langtag_records)
        } else {
            None
        };

        Ok(NameTable {
            string_storage,
            name_records,
            opt_langtag_records,
        })
    }
}

impl<'a> WriteBinary<&Self> for NameTable<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, name: &NameTable<'a>) -> Result<(), WriteError> {
        let format = name.opt_langtag_records.as_ref().map_or(0u16, |_| 1);
        U16Be::write(ctxt, format)?;
        U16Be::write(ctxt, u16::try_from(name.name_records.len())?)?; // count
        let string_offset = ctxt.placeholder::<U16Be, _>()?;
        <&ReadArray<'a, _>>::write(ctxt, &name.name_records)?;

        if let Some(lang_tag_records) = &name.opt_langtag_records {
            U16Be::write(ctxt, u16::try_from(lang_tag_records.len())?)?; // lang_tag_count
            <&ReadArray<'a, _>>::write(ctxt, lang_tag_records)?;
        }

        ctxt.write_placeholder(string_offset, u16::try_from(ctxt.bytes_written())?)?;
        ctxt.write_bytes(name.string_storage.data())?;

        Ok(())
    }
}

impl<'a> ReadFrom<'a> for NameRecord {
    type ReadType = ((U16Be, U16Be, U16Be), (U16Be, U16Be, U16Be));
    fn from(
        ((platform_id, encoding_id, language_id), (name_id, length, offset)): (
            (u16, u16, u16),
            (u16, u16, u16),
        ),
    ) -> Self {
        NameRecord {
            platform_id,
            encoding_id,
            language_id,
            name_id,
            length,
            offset,
        }
    }
}

impl<'a> WriteBinary for NameRecord {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, record: NameRecord) -> Result<(), WriteError> {
        U16Be::write(ctxt, record.platform_id)?;
        U16Be::write(ctxt, record.encoding_id)?;
        U16Be::write(ctxt, record.language_id)?;
        U16Be::write(ctxt, record.name_id)?;
        U16Be::write(ctxt, record.length)?;
        U16Be::write(ctxt, record.offset)?;

        Ok(())
    }
}

impl<'a> ReadFrom<'a> for LangTagRecord {
    type ReadType = (U16Be, U16Be);
    fn from((length, offset): (u16, u16)) -> Self {
        LangTagRecord { length, offset }
    }
}

impl<'a> WriteBinary for LangTagRecord {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, record: LangTagRecord) -> Result<(), WriteError> {
        U16Be::write(ctxt, record.length)?;
        U16Be::write(ctxt, record.offset)?;

        Ok(())
    }
}

impl<'a> ReadFrom<'a> for F2Dot14 {
    type ReadType = U16Be;

    fn from(value: u16) -> Self {
        F2Dot14(value)
    }
}

impl WriteBinary for F2Dot14 {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, val: Self) -> Result<(), WriteError> {
        U16Be::write(ctxt, val.0)
    }
}

impl<'a> ReadBinary<'a> for IndexToLocFormat {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let index_to_loc_format = ctxt.read_i16be()?;

        match index_to_loc_format {
            0 => Ok(IndexToLocFormat::Short),
            1 => Ok(IndexToLocFormat::Long),
            _ => Err(ParseError::BadValue),
        }
    }
}

impl WriteBinary for IndexToLocFormat {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, index_to_loc_format: Self) -> Result<(), WriteError> {
        match index_to_loc_format {
            IndexToLocFormat::Short => I16Be::write(ctxt, 0i16),
            IndexToLocFormat::Long => I16Be::write(ctxt, 1i16),
        }
    }
}

impl Fixed {
    pub fn new(value: i32) -> Fixed {
        Fixed(value)
    }
}

impl<'a> ReadFrom<'a> for Fixed {
    type ReadType = I32Be;

    fn from(value: i32) -> Self {
        Fixed(value)
    }
}

impl WriteBinary for Fixed {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, val: Self) -> Result<(), WriteError> {
        I32Be::write(ctxt, val.0)
    }
}

impl From<Fixed> for f32 {
    fn from(value: Fixed) -> f32 {
        (f64::from(value.0) / 65536.0) as f32
    }
}

impl F2Dot14 {
    pub fn new(value: u16) -> Self {
        F2Dot14(value)
    }
}

impl From<F2Dot14> for f32 {
    fn from(value: F2Dot14) -> Self {
        // The F2DOT14 format consists of a signed, 2’s complement integer and an unsigned fraction.
        let int: i8 = match value.0 >> 14 {
            0b00 => 0,
            0b01 => 1,
            0b10 => -2,
            0b11 => -1,
            _ => unreachable!(),
        };
        let fraction = value.0 & 0x3FFF;
        f32::from(int) + (f32::from(fraction) / 16384.)
    }
}

impl<T: FontTableProvider> FontTableProvider for Box<T> {
    fn table_data<'a>(&'a self, tag: u32) -> Result<Option<Cow<'a, [u8]>>, ParseError> {
        self.as_ref().table_data(tag)
    }

    fn has_table<'a>(&'a self, tag: u32) -> bool {
        self.as_ref().has_table(tag)
    }
}

#[cfg(test)]
mod tests {
    use super::{HeadTable, HmtxTable, NameTable};
    use crate::binary::read::ReadScope;
    use crate::binary::write::{WriteBinary, WriteBuffer, WriteContext};
    use crate::tables::{F2Dot14, Fixed};

    #[test]
    fn test_write_head_table() {
        // Read a head table in, then write it back out and compare it
        let head_data = include_bytes!("../tests/fonts/opentype/head.bin");
        let head = ReadScope::new(head_data).read::<HeadTable>().unwrap();
        let checksum_adjustment = head.check_sum_adjustment;

        let mut ctxt = WriteBuffer::new();
        let placeholder = HeadTable::write(&mut ctxt, &head).unwrap();
        ctxt.write_placeholder(placeholder, checksum_adjustment)
            .unwrap();

        assert_eq!(ctxt.bytes(), &head_data[..]);
    }

    #[test]
    fn test_write_hmtx_table() {
        // Read a hmtx table in, then write it back out and compare it
        let hmtx_data = include_bytes!("../tests/fonts/opentype/hmtx.bin");
        let num_glyphs = 1264;
        let num_h_metrics = 1264;
        let hmtx = ReadScope::new(hmtx_data)
            .read_dep::<HmtxTable<'_>>((num_glyphs, num_h_metrics))
            .unwrap();

        let mut ctxt = WriteBuffer::new();
        HmtxTable::write(&mut ctxt, &hmtx).unwrap();

        assert_eq!(ctxt.bytes(), &hmtx_data[..]);
    }

    #[test]
    fn test_write_name_table() {
        // Read a name table in, then write it back out and compare it
        let name_data = include_bytes!("../tests/fonts/opentype/name.bin");
        let name = ReadScope::new(name_data).read::<NameTable<'_>>().unwrap();

        let mut ctxt = WriteBuffer::new();
        NameTable::write(&mut ctxt, &name).unwrap();

        assert_eq!(ctxt.bytes(), &name_data[..]);
    }

    #[test]
    fn f32_from_f2dot14() {
        // Examples from https://docs.microsoft.com/en-us/typography/opentype/spec/otff#data-types
        assert_close(f32::from(F2Dot14(0x7fff)), 1.999939);
        assert_close(f32::from(F2Dot14(0x7000)), 1.75);
        assert_close(f32::from(F2Dot14(0x0001)), 0.000061);
        assert_close(f32::from(F2Dot14(0x0000)), 0.0);
        assert_close(f32::from(F2Dot14(0xffff)), -0.000061);
        assert_close(f32::from(F2Dot14(0x8000)), -2.0);
    }

    #[test]
    fn f32_from_fixed() {
        assert_close(f32::from(Fixed(0x7fff_0000)), 32767.);
        assert_close(f32::from(Fixed(0x7000_0001)), 28672.0001);
        assert_close(f32::from(Fixed(0x0001_0000)), 1.0);
        assert_close(f32::from(Fixed(0x0000_0000)), 0.0);
        assert_close(
            f32::from(Fixed(i32::from_be_bytes([0xff; 4]))),
            -0.000015259,
        );
        assert_close(f32::from(Fixed(0x7fff_ffff)), 32768.0);
    }

    fn assert_close(actual: f32, expected: f32) {
        assert!(
            (actual - expected).abs() < std::f32::EPSILON,
            "{:?} != {:?} ± {}",
            actual,
            expected,
            std::f32::EPSILON
        );
    }
}
