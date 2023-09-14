#![deny(missing_docs)]

//! Font subsetting.

use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::fmt;
use std::num::Wrapping;

use itertools::Itertools;

use crate::binary::read::{ReadArrayCow, ReadScope};
use crate::binary::write::{Placeholder, WriteBinary};
use crate::binary::write::{WriteBinaryDep, WriteBuffer, WriteContext};
use crate::binary::{long_align, U16Be, U32Be};
use crate::cff::{CFFError, CFF};
use crate::error::{ParseError, ReadWriteError, WriteError};
use crate::post::PostTable;
use crate::tables::cmap::subset::{CmapStrategy, CmapTarget, MappingsToKeep, NewIds, OldIds};
use crate::tables::cmap::{owned, EncodingId, PlatformId};
use crate::tables::glyf::GlyfTable;
use crate::tables::loca::{self, LocaTable};
use crate::tables::{
    self, cmap, FontTableProvider, HeadTable, HheaTable, HmtxTable, IndexToLocFormat, MaxpTable,
    TableRecord,
};
use crate::{checksum, tag};

/// Error type returned from subsetting.
#[derive(Debug)]
pub enum SubsetError {
    /// An error occurred reading or parsing data.
    Parse(ParseError),
    /// An error occurred serializing data.
    Write(WriteError),
    /// An error occurred when interpreting CFF CharStrings
    CFF(CFFError),
}

pub(crate) trait SubsetGlyphs {
    /// The number of glyphs in this collection
    fn len(&self) -> usize;

    /// Return the old glyph id for the supplied new glyph id
    fn old_id(&self, new_id: u16) -> u16;

    /// Return the new glyph id for the supplied old glyph id
    fn new_id(&self, old_id: u16) -> u16;
}

struct FontBuilder {
    sfnt_version: u32,
    tables: BTreeMap<u32, WriteBuffer>,
}

struct FontBuilderWithHead {
    inner: FontBuilder,
    check_sum_adjustment: Placeholder<U32Be, u32>,
    index_to_loc_format: IndexToLocFormat,
}

struct TaggedBuffer {
    tag: u32,
    buffer: WriteBuffer,
}

struct OrderedTables {
    tables: Vec<TaggedBuffer>,
    checksum: Wrapping<u32>,
}

/// Subset this font so that it only contains the glyphs with the supplied `glyph_ids`.
///
/// `glyph_ids` requirements:
///
/// * Glyph id 0, corresponding to the `.notdef` glyph must always be present.
/// * There must be no duplicate glyph ids.
///
/// If either of these requirements are not upheld this function will return
/// `ParseError::BadValue`.
pub fn subset(
    provider: &impl FontTableProvider,
    glyph_ids: &[u16],
) -> Result<Vec<u8>, SubsetError> {
    let mappings_to_keep = MappingsToKeep::new(provider, glyph_ids, CmapTarget::Unrestricted)?;
    if provider.has_table(tag::CFF) {
        subset_cff(provider, glyph_ids, mappings_to_keep, true)
    } else {
        subset_ttf(
            provider,
            glyph_ids,
            CmapStrategy::Generate(mappings_to_keep),
        )
        .map_err(SubsetError::from)
    }
}

/// Subset a TTF font.
///
/// If `mappings_to_keep` is `None` a `cmap` table in the subset font will be omitted.
/// Otherwise it will be used to build a new `cmap` table.
fn subset_ttf(
    provider: &impl FontTableProvider,
    glyph_ids: &[u16],
    cmap_strategy: CmapStrategy,
) -> Result<Vec<u8>, ReadWriteError> {
    let head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
    let mut maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
    let loca_data = provider.read_table_data(tag::LOCA)?;
    let loca = ReadScope::new(&loca_data)
        .read_dep::<LocaTable<'_>>((usize::from(maxp.num_glyphs), head.index_to_loc_format))?;
    let glyf_data = provider.read_table_data(tag::GLYF)?;
    let glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
    let mut hhea = ReadScope::new(&provider.read_table_data(tag::HHEA)?).read::<HheaTable>()?;
    let hmtx_data = provider.read_table_data(tag::HMTX)?;
    let hmtx = ReadScope::new(&hmtx_data).read_dep::<HmtxTable<'_>>((
        usize::from(maxp.num_glyphs),
        usize::from(hhea.num_h_metrics),
    ))?;

    // Build a new post table with version set to 3, which does not contain any additional
    // PostScript data
    let post_data = provider.read_table_data(tag::POST)?;
    let mut post = ReadScope::new(&post_data).read::<PostTable<'_>>()?;
    post.header.version = 0x00030000; // version 3.0
    post.opt_sub_table = None;

    // Subset the glyphs
    let subset_glyphs = glyf.subset(glyph_ids)?;

    // Build a new cmap table
    let cmap = match cmap_strategy {
        CmapStrategy::Generate(mappings_to_keep) => {
            let mappings_to_keep = mappings_to_keep.update_to_new_ids(&subset_glyphs);
            Some(create_cmap_table(&mappings_to_keep)?)
        }
        CmapStrategy::MacRomanSupplied(cmap) => {
            Some(create_cmap_table_from_cmap_array(glyph_ids, cmap)?)
        }
        CmapStrategy::Omit => None,
    };

    // Build new maxp table
    let num_glyphs = u16::try_from(subset_glyphs.len()).map_err(ParseError::from)?;
    maxp.num_glyphs = num_glyphs;

    // Build new hhea table
    let num_h_metrics = usize::from(hhea.num_h_metrics);
    hhea.num_h_metrics = num_glyphs;

    // Build new hmtx table
    let hmtx = create_hmtx_table(&hmtx, num_h_metrics, &subset_glyphs)?;

    // Extract the new glyf table now that we're done with subset_glyphs
    let glyf = GlyfTable::from(subset_glyphs);

    // Get the remaining tables
    let cvt = provider.table_data(tag::CVT)?;
    let fpgm = provider.table_data(tag::FPGM)?;
    let name = provider.table_data(tag::NAME)?;
    let prep = provider.table_data(tag::PREP)?;

    // Build the new font
    let mut builder = FontBuilder::new(0x00010000_u32);
    if let Some(cmap) = cmap {
        builder.add_table::<_, cmap::owned::Cmap>(tag::CMAP, cmap, ())?;
    }
    if let Some(cvt) = cvt {
        builder.add_table::<_, ReadScope<'_>>(tag::CVT, ReadScope::new(&cvt), ())?;
    }
    if let Some(fpgm) = fpgm {
        builder.add_table::<_, ReadScope<'_>>(tag::FPGM, ReadScope::new(&fpgm), ())?;
    }
    builder.add_table::<_, HheaTable>(tag::HHEA, &hhea, ())?;
    builder.add_table::<_, HmtxTable<'_>>(tag::HMTX, &hmtx, ())?;
    builder.add_table::<_, MaxpTable>(tag::MAXP, &maxp, ())?;
    if let Some(name) = name {
        builder.add_table::<_, ReadScope<'_>>(tag::NAME, ReadScope::new(&name), ())?;
    }
    builder.add_table::<_, PostTable<'_>>(tag::POST, &post, ())?;
    if let Some(prep) = prep {
        builder.add_table::<_, ReadScope<'_>>(tag::PREP, ReadScope::new(&prep), ())?;
    }
    let mut builder = builder.add_head_table(&head)?;
    builder.add_glyf_table(glyf)?;
    builder.data()
}

fn subset_cff(
    provider: &impl FontTableProvider,
    glyph_ids: &[u16],
    mappings_to_keep: MappingsToKeep<OldIds>,
    convert_cff_to_cid_if_more_than_255_glyphs: bool,
) -> Result<Vec<u8>, SubsetError> {
    let cff_data = provider.read_table_data(tag::CFF)?;
    let scope = ReadScope::new(&cff_data);
    let cff: CFF<'_> = scope.read::<CFF<'_>>()?;
    if cff.name_index.count != 1 || cff.fonts.len() != 1 {
        return Err(SubsetError::from(ParseError::BadIndex));
    }

    let head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
    let mut maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
    let mut hhea = ReadScope::new(&provider.read_table_data(tag::HHEA)?).read::<HheaTable>()?;
    let hmtx_data = provider.read_table_data(tag::HMTX)?;
    let hmtx = ReadScope::new(&hmtx_data).read_dep::<HmtxTable<'_>>((
        usize::from(maxp.num_glyphs),
        usize::from(hhea.num_h_metrics),
    ))?;

    // Build a new post table with version set to 3, which does not contain any additional
    // PostScript data
    let post_data = provider.read_table_data(tag::POST)?;
    let mut post = ReadScope::new(&post_data).read::<PostTable<'_>>()?;
    post.header.version = 0x00030000; // version 3.0
    post.opt_sub_table = None;

    // Build the new CFF table
    let cff_subset = cff.subset(glyph_ids, convert_cff_to_cid_if_more_than_255_glyphs)?;
    let mappings_to_keep = mappings_to_keep.update_to_new_ids(&cff_subset);

    // Build a new cmap table
    let cmap = create_cmap_table(&mappings_to_keep)?;

    // Build new maxp table
    let num_glyphs = u16::try_from(cff_subset.len()).map_err(ParseError::from)?;
    maxp.num_glyphs = num_glyphs;

    // Build new hhea table
    let num_h_metrics = usize::from(hhea.num_h_metrics);
    hhea.num_h_metrics = num_glyphs;

    // Build new hmtx table
    let hmtx = create_hmtx_table(&hmtx, num_h_metrics, &cff_subset)?;

    // Get the remaining tables
    let cvt = provider.table_data(tag::CVT)?;
    let fpgm = provider.table_data(tag::FPGM)?;
    let name = provider.table_data(tag::NAME)?;
    let prep = provider.table_data(tag::PREP)?;
    let os_2 = provider.read_table_data(tag::OS_2)?;

    // Build the new font
    let mut builder = FontBuilder::new(tag::OTTO);
    builder.add_table::<_, cmap::owned::Cmap>(tag::CMAP, cmap, ())?;
    if let Some(cvt) = cvt {
        builder.add_table::<_, ReadScope<'_>>(tag::CVT, ReadScope::new(&cvt), ())?;
    }
    if let Some(fpgm) = fpgm {
        builder.add_table::<_, ReadScope<'_>>(tag::FPGM, ReadScope::new(&fpgm), ())?;
    }
    builder.add_table::<_, HheaTable>(tag::HHEA, &hhea, ())?;
    builder.add_table::<_, HmtxTable<'_>>(tag::HMTX, &hmtx, ())?;
    builder.add_table::<_, MaxpTable>(tag::MAXP, &maxp, ())?;
    if let Some(name) = name {
        builder.add_table::<_, ReadScope<'_>>(tag::NAME, ReadScope::new(&name), ())?;
    }
    builder.add_table::<_, ReadScope<'_>>(tag::OS_2, ReadScope::new(&os_2), ())?;
    builder.add_table::<_, PostTable<'_>>(tag::POST, &post, ())?;
    if let Some(prep) = prep {
        builder.add_table::<_, ReadScope<'_>>(tag::PREP, ReadScope::new(&prep), ())?;
    }

    // Extract the new CFF table now that we're done with cff_subset
    let cff = CFF::from(cff_subset);
    builder.add_table::<_, CFF<'_>>(tag::CFF, &cff, ())?;
    let builder = builder.add_head_table(&head)?;
    builder.data().map_err(SubsetError::from)
}

fn create_cmap_table(
    mappings_to_keep: &MappingsToKeep<NewIds>,
) -> Result<owned::Cmap, ReadWriteError> {
    let encoding_record = owned::EncodingRecord::from_mappings(mappings_to_keep)?;
    Ok(owned::Cmap {
        encoding_records: vec![encoding_record],
    })
}

fn create_cmap_table_from_cmap_array(
    glyph_ids: &[u16],
    cmap: Box<[u8; 256]>,
) -> Result<owned::Cmap, ReadWriteError> {
    use cmap::owned::{Cmap, CmapSubtable, EncodingRecord};

    if glyph_ids.len() > 256 {
        return Err(ReadWriteError::Write(WriteError::BadValue));
    }

    Ok(Cmap {
        encoding_records: vec![EncodingRecord {
            platform_id: PlatformId::MACINTOSH,
            encoding_id: EncodingId::MACINTOSH_APPLE_ROMAN,
            sub_table: CmapSubtable::Format0 {
                language: 0, // the subtable is language independent
                glyph_id_array: cmap,
            },
        }],
    })
}

/// Construct a complete font from the supplied provider and tags.
pub fn whole_font<F: FontTableProvider>(
    provider: &F,
    tags: &[u32],
) -> Result<Vec<u8>, ReadWriteError> {
    let head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
    let maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;

    let sfnt_version = tags
        .iter()
        .position(|&tag| tag == tag::CFF)
        .map(|_| tables::CFF_MAGIC)
        .unwrap_or(tables::TTF_MAGIC);
    let mut builder = FontBuilder::new(sfnt_version);
    let mut wants_glyf = false;
    for &tag in tags {
        match tag {
            tag::GLYF => wants_glyf = true,
            tag::HEAD | tag::MAXP | tag::LOCA => (),
            _ => {
                builder.add_table::<_, ReadScope<'_>>(
                    tag,
                    ReadScope::new(&provider.read_table_data(tag)?),
                    (),
                )?;
            }
        }
    }
    // maxp and head are required for the font to be usable, so they're always added.
    builder.add_table::<_, MaxpTable>(tag::MAXP, &maxp, ())?;
    let mut builder_with_head = builder.add_head_table(&head)?;

    // Add glyf and loca if requested, glyf implies loca. They may not be requested in the case of
    // a CFF font, or CBDT/CBLC font.
    if wants_glyf {
        let loca_data = provider.read_table_data(tag::LOCA)?;
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((usize::from(maxp.num_glyphs), head.index_to_loc_format))?;
        let glyf_data = provider.read_table_data(tag::GLYF)?;
        let glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
        builder_with_head.add_glyf_table(glyf)?;
    }
    builder_with_head.data()
}

fn create_hmtx_table<'b>(
    hmtx: &HmtxTable<'_>,
    num_h_metrics: usize,
    subset_glyphs: &impl SubsetGlyphs,
) -> Result<HmtxTable<'b>, ReadWriteError> {
    let mut h_metrics = Vec::with_capacity(num_h_metrics);

    for glyph_id in 0..subset_glyphs.len() {
        // Cast is safe as glyph indexes are 16-bit values
        let old_id = usize::from(subset_glyphs.old_id(glyph_id as u16));

        if old_id < num_h_metrics {
            h_metrics.push(hmtx.h_metrics.read_item(old_id)?);
        } else {
            // As an optimization, the number of records can be less than the number of glyphs, in which case the
            // advance width value of the last record applies to all remaining glyph IDs.
            // https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx
            let mut metric = hmtx.h_metrics.read_item(num_h_metrics - 1)?;
            metric.lsb = hmtx.left_side_bearings.read_item(old_id - num_h_metrics)?;
            h_metrics.push(metric);
        }
    }

    Ok(HmtxTable {
        h_metrics: ReadArrayCow::Owned(h_metrics),
        left_side_bearings: ReadArrayCow::Owned(vec![]),
    })
}

impl FontBuilder {
    pub fn new(sfnt_version: u32) -> Self {
        FontBuilder {
            sfnt_version,
            tables: BTreeMap::new(),
        }
    }

    pub fn add_table<HostType, T: WriteBinaryDep<HostType>>(
        &mut self,
        tag: u32,
        table: HostType,
        args: T::Args,
    ) -> Result<T::Output, ReadWriteError> {
        assert_ne!(tag, tag::HEAD, "head table must use add_head_table");
        assert_ne!(tag, tag::GLYF, "glyf table must use add_glyf_table");

        self.add_table_inner::<HostType, T>(tag, table, args)
    }

    fn add_table_inner<HostType, T: WriteBinaryDep<HostType>>(
        &mut self,
        tag: u32,
        table: HostType,
        args: T::Args,
    ) -> Result<T::Output, ReadWriteError> {
        let mut buffer = WriteBuffer::new();
        let output = T::write_dep(&mut buffer, table, args)?;
        self.tables.insert(tag, buffer);

        Ok(output)
    }

    pub fn add_head_table(
        mut self,
        table: &HeadTable,
    ) -> Result<FontBuilderWithHead, ReadWriteError> {
        let placeholder = self.add_table_inner::<_, HeadTable>(tag::HEAD, table, ())?;

        Ok(FontBuilderWithHead {
            inner: self,
            check_sum_adjustment: placeholder,
            index_to_loc_format: table.index_to_loc_format,
        })
    }
}

impl FontBuilderWithHead {
    pub fn add_glyf_table(&mut self, table: GlyfTable<'_>) -> Result<(), ReadWriteError> {
        let loca = self.inner.add_table_inner::<_, GlyfTable<'_>>(
            tag::GLYF,
            table,
            self.index_to_loc_format,
        )?;
        self.inner.add_table_inner::<_, loca::owned::LocaTable>(
            tag::LOCA,
            loca,
            self.index_to_loc_format,
        )?;

        Ok(())
    }

    /// Returns a `Vec<u8>` containing the built font
    pub fn data(mut self) -> Result<Vec<u8>, ReadWriteError> {
        let mut font = WriteBuffer::new();

        self.write_offset_table(&mut font)?;
        let table_offset =
            long_align(self.inner.tables.len() * TableRecord::SIZE + font.bytes_written());

        // Add tables in desired order
        let mut ordered_tables = self.write_table_directory(&mut font)?;

        // pad
        let length = font.bytes_written();
        let padded_length = long_align(length);
        assert_eq!(
            padded_length, table_offset,
            "offset after writing table directory is not at expected position"
        );
        font.write_zeros(padded_length - length)?;

        // Fill in check_sum_adjustment in the head table. the magic number comes from the OpenType spec.
        let headers_checksum = checksum::table_checksum(font.bytes())?;
        let checksum = Wrapping(0xB1B0AFBA) - (headers_checksum + ordered_tables.checksum);

        // Write out the font tables
        let mut placeholder = Some(self.check_sum_adjustment);
        for TaggedBuffer { tag, buffer } in ordered_tables.tables.iter_mut() {
            if *tag == tag::HEAD {
                buffer.write_placeholder(placeholder.take().unwrap(), checksum.0)?;
            }
            font.write_bytes(buffer.bytes())?;
        }

        Ok(font.into_inner())
    }

    fn write_offset_table(&self, font: &mut WriteBuffer) -> Result<(), WriteError> {
        let num_tables = u16::try_from(self.inner.tables.len())?;
        let n = max_power_of_2(num_tables);
        let search_range = (1 << n) * 16;
        let entry_selector = n;
        let range_shift = num_tables * 16 - search_range;

        U32Be::write(font, self.inner.sfnt_version)?;
        U16Be::write(font, num_tables)?;
        U16Be::write(font, search_range)?;
        U16Be::write(font, entry_selector)?;
        U16Be::write(font, range_shift)?;

        Ok(())
    }

    fn write_table_directory(
        &mut self,
        font: &mut WriteBuffer,
    ) -> Result<OrderedTables, ReadWriteError> {
        let mut tables = Vec::with_capacity(self.inner.tables.len());
        let mut checksum = Wrapping(0);
        let mut table_offset =
            long_align(self.inner.tables.len() * TableRecord::SIZE + font.bytes_written());

        let tags = self.inner.tables.keys().cloned().collect_vec();
        for tag in tags {
            if let Some(mut table) = self.inner.tables.remove(&tag) {
                let length = table.len();
                let padded_length = long_align(length);
                table.write_zeros(padded_length - length)?;

                let table_checksum = checksum::table_checksum(table.bytes())?;
                checksum += table_checksum;

                let record = TableRecord {
                    table_tag: tag,
                    checksum: table_checksum.0,
                    offset: u32::try_from(table_offset).map_err(WriteError::from)?,
                    length: u32::try_from(length).map_err(WriteError::from)?,
                };

                table_offset += padded_length;
                TableRecord::write(font, &record)?;
                tables.push(TaggedBuffer { tag, buffer: table });
            }
        }

        Ok(OrderedTables { tables, checksum })
    }
}

/// Calculate the maximum power of 2 that is <= num
fn max_power_of_2(num: u16) -> u16 {
    15u16.saturating_sub(num.leading_zeros() as u16)
}

/// Prince specific subsetting behaviour.
///
/// prince::subset will produce a bare CFF table in the case of an input CFF font.
#[cfg(feature = "prince")]
pub mod prince {
    use super::{
        tag, FontTableProvider, MappingsToKeep, ParseError, ReadScope, SubsetError, WriteBinary,
        WriteBuffer, CFF,
    };
    use crate::tables::cmap::subset::{CmapStrategy, CmapTarget};
    use std::ffi::c_int;

    /// This enum describes the desired cmap generation and maps to the `cmap_target` type in Prince
    #[derive(Debug, Clone)]
    pub enum PrinceCmapTarget {
        /// Build a suitable cmap table
        Unrestricted,
        /// Build a Mac Roman cmap table
        MacRoman,
        /// Omit the cmap table entirely
        Omit,
        /// Use the supplied array as a Mac Roman cmap table
        MacRomanCmap(Box<[u8; 256]>),
    }

    impl PrinceCmapTarget {
        /// Build a new cmap from a `cmap_target` tag
        pub fn new(tag: c_int, cmap: Option<Box<[u8; 256]>>) -> Self {
            // NOTE: These tags should be kept in sync with the `cmap_target` type in Prince.
            match (tag, cmap) {
                (1, _) => PrinceCmapTarget::Unrestricted,
                (2, _) => PrinceCmapTarget::MacRoman,
                (3, _) => PrinceCmapTarget::Omit,
                (4, Some(cmap)) => PrinceCmapTarget::MacRomanCmap(cmap),
                _ => panic!("invalid value for PrinceCmapTarget: {}", tag),
            }
        }
    }

    /// Subset this font so that it only contains the glyphs with the supplied `glyph_ids`.
    ///
    /// Returns just the CFF table in the case of a CFF font, not a complete OpenType font.
    pub fn subset(
        provider: &impl FontTableProvider,
        glyph_ids: &[u16],
        cmap_target: PrinceCmapTarget,
        convert_cff_to_cid_if_more_than_255_glyphs: bool,
    ) -> Result<Vec<u8>, SubsetError> {
        if provider.has_table(tag::CFF) {
            subset_cff_table(
                provider,
                glyph_ids,
                convert_cff_to_cid_if_more_than_255_glyphs,
            )
        } else {
            let cmap_strategy = match cmap_target {
                PrinceCmapTarget::Unrestricted => {
                    let mappings_to_keep =
                        MappingsToKeep::new(provider, glyph_ids, CmapTarget::Unrestricted)?;
                    CmapStrategy::Generate(mappings_to_keep)
                }
                PrinceCmapTarget::MacRoman => {
                    let mappings_to_keep =
                        MappingsToKeep::new(provider, glyph_ids, CmapTarget::MacRoman)?;
                    CmapStrategy::Generate(mappings_to_keep)
                }
                PrinceCmapTarget::Omit => CmapStrategy::Omit,
                PrinceCmapTarget::MacRomanCmap(cmap) => CmapStrategy::MacRomanSupplied(cmap),
            };
            super::subset_ttf(provider, glyph_ids, cmap_strategy).map_err(SubsetError::from)
        }
    }

    /// Subset the CFF table and discard the rest
    ///
    /// Useful for PDF because a CFF table can be embedded directly without the need to wrap it in
    /// an OTF.
    fn subset_cff_table(
        provider: &impl FontTableProvider,
        glyph_ids: &[u16],
        convert_cff_to_cid_if_more_than_255_glyphs: bool,
    ) -> Result<Vec<u8>, SubsetError> {
        let cff_data = provider.read_table_data(tag::CFF)?;
        let scope = ReadScope::new(&cff_data);
        let cff: CFF<'_> = scope.read::<CFF<'_>>()?;
        if cff.name_index.count != 1 || cff.fonts.len() != 1 {
            return Err(SubsetError::from(ParseError::BadIndex));
        }

        // Build the new CFF table
        let cff = cff
            .subset(glyph_ids, convert_cff_to_cid_if_more_than_255_glyphs)?
            .into();

        let mut buffer = WriteBuffer::new();
        CFF::write(&mut buffer, &cff)?;

        Ok(buffer.into_inner())
    }
}

impl From<ParseError> for SubsetError {
    fn from(err: ParseError) -> SubsetError {
        SubsetError::Parse(err)
    }
}

impl From<WriteError> for SubsetError {
    fn from(err: WriteError) -> SubsetError {
        SubsetError::Write(err)
    }
}

impl From<CFFError> for SubsetError {
    fn from(err: CFFError) -> SubsetError {
        SubsetError::CFF(err)
    }
}

impl From<ReadWriteError> for SubsetError {
    fn from(err: ReadWriteError) -> SubsetError {
        match err {
            ReadWriteError::Read(err) => SubsetError::Parse(err),
            ReadWriteError::Write(err) => SubsetError::Write(err),
        }
    }
}

impl fmt::Display for SubsetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SubsetError::Parse(err) => write!(f, "subset: parse error: {}", err),
            SubsetError::Write(err) => write!(f, "subset: write error: {}", err),
            SubsetError::CFF(err) => write!(f, "subset: CFF error: {}", err),
        }
    }
}

impl std::error::Error for SubsetError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font_data::FontData;
    use crate::tables::cmap::CmapSubtable;
    use crate::tables::glyf::GlyphData;
    use crate::tables::glyf::{
        BoundingBox, CompositeGlyph, CompositeGlyphArgument, CompositeGlyphFlag, GlyfRecord, Glyph,
        Point, SimpleGlyph, SimpleGlyphFlag,
    };
    use crate::tables::{LongHorMetric, OpenTypeData, OpenTypeFont};
    use crate::tag::DisplayTag;
    use crate::tests::read_fixture;
    use crate::Font;

    use std::collections::HashSet;

    macro_rules! read_table {
        ($file:ident, $scope:expr, $tag:path, $t:ty) => {
            $file
                .read_table(&$scope, $tag)
                .expect("error reading table")
                .expect("no table found")
                .read::<$t>()
                .expect("unable to parse")
        };
        ($file:ident, $scope:expr, $tag:path, $t:ty, $args:expr) => {
            $file
                .read_table(&$scope, $tag)
                .expect("error reading table")
                .expect("no table found")
                .read_dep::<$t>($args)
                .expect("unable to parse")
        };
    }

    #[test]
    fn create_glyf_and_hmtx() {
        let buffer = read_fixture("tests/fonts/opentype/SFNT-TTF-Composite.ttf");
        let fontfile = ReadScope::new(&buffer)
            .read::<OpenTypeFont<'_>>()
            .expect("error reading OpenTypeFile");
        let font = match fontfile.data {
            OpenTypeData::Single(font) => font,
            OpenTypeData::Collection(_) => unreachable!(),
        };
        let head = read_table!(font, fontfile.scope, tag::HEAD, HeadTable);
        let maxp = read_table!(font, fontfile.scope, tag::MAXP, MaxpTable);
        let hhea = read_table!(font, fontfile.scope, tag::HHEA, HheaTable);
        let loca = read_table!(
            font,
            fontfile.scope,
            tag::LOCA,
            LocaTable<'_>,
            (usize::from(maxp.num_glyphs), head.index_to_loc_format)
        );
        let glyf = read_table!(font, fontfile.scope, tag::GLYF, GlyfTable<'_>, &loca);
        let hmtx = read_table!(
            font,
            fontfile.scope,
            tag::HMTX,
            HmtxTable<'_>,
            (
                usize::from(maxp.num_glyphs),
                usize::from(hhea.num_h_metrics),
            )
        );

        // 0 - .notdef
        // 2 - composite
        // 4 - simple
        let glyph_ids = [0, 2, 4];
        let subset_glyphs = glyf.subset(&glyph_ids).unwrap();
        let expected_glyf = GlyfTable {
            records: vec![
                GlyfRecord::Empty,
                GlyfRecord::Parsed(Glyph {
                    number_of_contours: -1,
                    bounding_box: BoundingBox {
                        x_min: 205,
                        x_max: 4514,
                        y_min: 0,
                        y_max: 1434,
                    },
                    data: GlyphData::Composite {
                        glyphs: vec![
                            CompositeGlyph {
                                flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                                    | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                                    | CompositeGlyphFlag::ROUND_XY_TO_GRID
                                    | CompositeGlyphFlag::MORE_COMPONENTS
                                    | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                                glyph_index: 3,
                                argument1: CompositeGlyphArgument::I16(3453),
                                argument2: CompositeGlyphArgument::I16(0),
                                scale: None,
                            },
                            CompositeGlyph {
                                flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                                    | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                                    | CompositeGlyphFlag::ROUND_XY_TO_GRID
                                    | CompositeGlyphFlag::MORE_COMPONENTS
                                    | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                                glyph_index: 4,
                                argument1: CompositeGlyphArgument::I16(2773),
                                argument2: CompositeGlyphArgument::I16(0),
                                scale: None,
                            },
                            CompositeGlyph {
                                flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                                    | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                                    | CompositeGlyphFlag::ROUND_XY_TO_GRID
                                    | CompositeGlyphFlag::MORE_COMPONENTS
                                    | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                                glyph_index: 5,
                                argument1: CompositeGlyphArgument::I16(1182),
                                argument2: CompositeGlyphArgument::I16(0),
                                scale: None,
                            },
                            CompositeGlyph {
                                flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                                    | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                                    | CompositeGlyphFlag::ROUND_XY_TO_GRID
                                    | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                                glyph_index: 2,
                                argument1: CompositeGlyphArgument::I16(205),
                                argument2: CompositeGlyphArgument::I16(0),
                                scale: None,
                            },
                        ],
                        instructions: &[],
                    },
                }),
                GlyfRecord::Parsed(Glyph {
                    number_of_contours: 1,
                    bounding_box: BoundingBox {
                        x_min: 0,
                        x_max: 1073,
                        y_min: 0,
                        y_max: 1434,
                    },
                    data: GlyphData::Simple(SimpleGlyph {
                        end_pts_of_contours: vec![9],
                        instructions: &[],
                        coordinates: vec![
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(0, 1434),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(1073, 1434),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(1073, 1098),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(485, 1098),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(485, 831),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(987, 831),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(987, 500),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(485, 500),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(485, 0),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(0, 0),
                            ),
                        ],
                    }),
                }),
                GlyfRecord::Parsed(Glyph {
                    number_of_contours: 1,
                    bounding_box: BoundingBox {
                        x_min: 0,
                        x_max: 1061,
                        y_min: 0,
                        y_max: 1434,
                    },
                    data: GlyphData::Simple(SimpleGlyph {
                        end_pts_of_contours: vec![5],
                        instructions: &[],
                        coordinates: vec![
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(0, 1434),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(485, 1434),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(485, 369),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(1061, 369),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(1061, 0),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(0, 0),
                            ),
                        ],
                    }),
                }),
                GlyfRecord::Parsed(Glyph {
                    number_of_contours: 1,
                    bounding_box: BoundingBox {
                        x_min: 0,
                        x_max: 485,
                        y_min: 0,
                        y_max: 1434,
                    },
                    data: GlyphData::Simple(SimpleGlyph {
                        end_pts_of_contours: vec![3],
                        instructions: &[],
                        coordinates: vec![
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(0, 1434),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(485, 1434),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(485, 0),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(0, 0),
                            ),
                        ],
                    }),
                }),
                GlyfRecord::Parsed(Glyph {
                    number_of_contours: 2,
                    bounding_box: BoundingBox {
                        x_min: 0,
                        x_max: 1478,
                        y_min: 0,
                        y_max: 1434,
                    },
                    data: GlyphData::Simple(SimpleGlyph {
                        end_pts_of_contours: vec![7, 10],
                        instructions: &[],
                        coordinates: vec![
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(0, 0),
                            ),
                            (SimpleGlyphFlag::ON_CURVE_POINT, Point(436, 1434)),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(1042, 1434),
                            ),
                            (SimpleGlyphFlag::ON_CURVE_POINT, Point(1478, 0)),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(975, 0),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_SHORT_VECTOR
                                    | SimpleGlyphFlag::Y_SHORT_VECTOR
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(909, 244),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(493, 244),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_SHORT_VECTOR
                                    | SimpleGlyphFlag::Y_SHORT_VECTOR,
                                Point(430, 0),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_SHORT_VECTOR
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                                Point(579, 565),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT
                                    | SimpleGlyphFlag::X_SHORT_VECTOR
                                    | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
                                    | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                                Point(825, 565),
                            ),
                            (
                                SimpleGlyphFlag::ON_CURVE_POINT | SimpleGlyphFlag::X_SHORT_VECTOR,
                                Point(702, 1032),
                            ),
                        ],
                    }),
                }),
            ],
        };

        let num_h_metrics = usize::from(hhea.num_h_metrics);
        let hmtx = create_hmtx_table(&hmtx, num_h_metrics, &subset_glyphs).unwrap();

        let mut glyf: GlyfTable<'_> = subset_glyphs.into();
        glyf.records.iter_mut().for_each(|rec| rec.parse().unwrap());
        assert_eq!(glyf, expected_glyf);

        let expected = vec![
            LongHorMetric {
                advance_width: 1536,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 4719,
                lsb: 205,
            },
            LongHorMetric {
                advance_width: 0,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 0,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 0,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 0,
                lsb: 0,
            },
        ];

        assert_eq!(hmtx.h_metrics.iter().collect::<Vec<_>>(), expected);
        assert_eq!(hmtx.left_side_bearings.iter().collect::<Vec<_>>(), vec![]);
    }

    #[test]
    fn font_builder() {
        // Test that reading a font in, adding all its tables and writing it out equals the
        // original font
        let buffer = read_fixture("tests/fonts/opentype/test-font.ttf");
        let fontfile = ReadScope::new(&buffer)
            .read::<OpenTypeFont<'_>>()
            .expect("error reading OpenTypeFile");
        let font = match fontfile.data {
            OpenTypeData::Single(font) => font,
            OpenTypeData::Collection(_) => unreachable!(),
        };
        let head = read_table!(font, fontfile.scope, tag::HEAD, HeadTable);
        let maxp = read_table!(font, fontfile.scope, tag::MAXP, MaxpTable);
        let hhea = read_table!(font, fontfile.scope, tag::HHEA, HheaTable);
        let loca = read_table!(
            font,
            fontfile.scope,
            tag::LOCA,
            LocaTable<'_>,
            (usize::from(maxp.num_glyphs), head.index_to_loc_format)
        );
        let glyf = read_table!(font, fontfile.scope, tag::GLYF, GlyfTable<'_>, &loca);
        let hmtx = read_table!(
            font,
            fontfile.scope,
            tag::HMTX,
            HmtxTable<'_>,
            (
                usize::from(maxp.num_glyphs),
                usize::from(hhea.num_h_metrics),
            )
        );

        let mut builder = FontBuilder::new(tables::TTF_MAGIC);
        builder
            .add_table::<_, HheaTable>(tag::HHEA, &hhea, ())
            .unwrap();
        builder
            .add_table::<_, HmtxTable<'_>>(tag::HMTX, &hmtx, ())
            .unwrap();
        builder
            .add_table::<_, MaxpTable>(tag::MAXP, &maxp, ())
            .unwrap();

        let tables_added = [
            tag::HEAD,
            tag::GLYF,
            tag::HHEA,
            tag::HMTX,
            tag::MAXP,
            tag::LOCA,
        ]
        .iter()
        .collect::<HashSet<&u32>>();
        for record in font.table_records.iter() {
            if tables_added.contains(&record.table_tag) {
                continue;
            }

            let table = font
                .read_table(&fontfile.scope, record.table_tag)
                .unwrap()
                .unwrap();
            builder
                .add_table::<_, ReadScope<'_>>(record.table_tag, table, ())
                .unwrap();
        }

        let mut builder = builder.add_head_table(&head).unwrap();
        builder.add_glyf_table(glyf).unwrap();
        let data = builder.data().unwrap();

        let new_fontfile = ReadScope::new(&data)
            .read::<OpenTypeFont<'_>>()
            .expect("error reading new OpenTypeFile");
        let new_font = match new_fontfile.data {
            OpenTypeData::Single(font) => font,
            OpenTypeData::Collection(_) => unreachable!(),
        };

        assert_eq!(new_font.table_records.len(), font.table_records.len());
        for record in font.table_records.iter() {
            match record.table_tag {
                tag::GLYF | tag::LOCA => {
                    // TODO: check content of glyf and loca
                    // glyf differs because we don't do anything fancy with points at the moment
                    // and always write them out as i16 values.
                    // loca differs because glyf differs
                    continue;
                }
                tag => {
                    let new_record = new_font.find_table_record(record.table_tag).unwrap();
                    let tag = DisplayTag(tag);
                    assert_eq!((tag, new_record.checksum), (tag, record.checksum));
                }
            }
        }
    }

    #[test]
    fn invalid_glyph_id() {
        // Test to ensure that invalid glyph ids don't panic when subsetting
        let buffer = read_fixture("tests/fonts/opentype/Klei.otf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let mut glyph_ids = [0, 9999];

        match subset(&opentype_file.table_provider(0).unwrap(), &mut glyph_ids) {
            Err(SubsetError::Parse(ParseError::BadIndex)) => {}
            err => panic!(
                "expected SubsetError::Parse(ParseError::BadIndex) got {:?}",
                err
            ),
        }
    }

    #[test]
    fn empty_mappings_to_keep() {
        // Test to ensure that an empty mappings to keep doesn't panic when subsetting
        let buffer = read_fixture("tests/fonts/opentype/SourceCodePro-Regular.otf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        // glyph 118 is not Unicode, so does not end up in the mappings to keep
        let mut glyph_ids = [0, 118];
        let subset_font_data =
            subset(&opentype_file.table_provider(0).unwrap(), &mut glyph_ids).unwrap();

        let opentype_file = ReadScope::new(&subset_font_data)
            .read::<OpenTypeFont<'_>>()
            .unwrap();
        let font = Font::new(opentype_file.table_provider(0).unwrap())
            .unwrap()
            .unwrap();
        let cmap = ReadScope::new(font.cmap_subtable_data())
            .read::<CmapSubtable<'_>>()
            .unwrap();

        // If mappings_to_keep is empty a mac roman cmap sub-table is created, which doesn't
        // care that it's empty.
        if let CmapSubtable::Format0 { glyph_id_array, .. } = cmap {
            assert!(glyph_id_array.iter().all(|x| x == 0));
        } else {
            panic!("expected cmap sub-table format 0");
        }
    }

    #[test]
    fn ttf_mappings_to_keep_is_none() {
        // Test that when subsetting a TTF font with mappings_to_keep set to None the cmap table is
        // omitted from the subset font.
        let buffer = read_fixture("tests/fonts/opentype/test-font.ttf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let mut glyph_ids = [0, 2];
        let subset_font_data = subset_ttf(
            &opentype_file.table_provider(0).unwrap(),
            &mut glyph_ids,
            CmapStrategy::Omit,
        )
        .unwrap();

        let opentype_file = ReadScope::new(&subset_font_data)
            .read::<OpenTypeFont<'_>>()
            .unwrap();
        let table_provider = opentype_file.table_provider(0).unwrap();
        assert!(!table_provider.has_table(tag::CMAP));
    }

    // This test ensures we can call whole_font on a font without a `glyf` table (E.g. CFF).
    #[test]
    fn test_whole_font() {
        let buffer = read_fixture("tests/fonts/opentype/Klei.otf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to read FontFile");
        let provider = font_file
            .table_provider(0)
            .expect("unable to get FontTableProvider");
        let tags = [
            tag::CFF,
            tag::GDEF,
            tag::GPOS,
            tag::GSUB,
            tag::OS_2,
            tag::CMAP,
            tag::HEAD,
            tag::HHEA,
            tag::HMTX,
            tag::MAXP,
            tag::NAME,
            tag::POST,
        ];
        assert!(whole_font(&provider, &tags).is_ok());
    }

    #[test]
    fn test_max_power_of_2() {
        assert_eq!(max_power_of_2(0), 0);
        assert_eq!(max_power_of_2(1), 0);
        assert_eq!(max_power_of_2(2), 1);
        assert_eq!(max_power_of_2(4), 2);
        assert_eq!(max_power_of_2(8), 3);
        assert_eq!(max_power_of_2(16), 4);
        assert_eq!(max_power_of_2(49), 5);
        assert_eq!(max_power_of_2(std::u16::MAX), 15);
    }
}
