#![deny(missing_docs)]

//! Font subsetting.

use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::iter;
use std::num::Wrapping;

use itertools::Itertools;

use crate::binary::read::{ReadArrayCow, ReadScope};
use crate::binary::write::{Placeholder, WriteBinary};
use crate::binary::write::{WriteBinaryDep, WriteBuffer, WriteContext};
use crate::binary::{long_align, U16Be, U32Be};
use crate::cff::CFF;
use crate::error::{ParseError, ReadWriteError, WriteError};
use crate::font::Encoding;
use crate::macroman::{char_to_macroman, is_macroman};
use crate::post::PostTable;
use crate::tables::cmap::owned::{CmapSubtableFormat12, CmapSubtableFormat4};
use crate::tables::cmap::{owned, Cmap, CmapSubtable, SequentialMapGroup};
use crate::tables::glyf::GlyfTable;
use crate::tables::loca::{self, LocaTable};
use crate::tables::{
    self, cmap, FontTableProvider, HeadTable, HheaTable, HmtxTable, IndexToLocFormat, MaxpTable,
    TableRecord,
};
use crate::{checksum, tag};

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
/// For TTF fonts the first entry must always be 0, corresponding to the `.notdef` glyph. If this
/// is not the case this function will return `WriteError::BadValue`.
pub fn subset(
    provider: &impl FontTableProvider,
    glyph_ids: &[u16],
) -> Result<Vec<u8>, ReadWriteError> {
    if provider.has_table(tag::CFF) {
        subset_cff(provider, glyph_ids, None, true)
    } else {
        subset_ttf(provider, glyph_ids, None)
    }
}

fn subset_ttf(
    provider: &impl FontTableProvider,
    glyph_ids: &[u16],
    cmap0: Option<Box<[u8; 256]>>,
) -> Result<Vec<u8>, ReadWriteError> {
    if glyph_ids.get(0) != Some(&0) {
        // glyph index 0 is the .notdef glyph, the fallback, it must always be first
        return Err(ReadWriteError::Write(WriteError::BadValue));
    }

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

    // Build new maxp table
    let num_glyphs = u16::try_from(subset_glyphs.len()).map_err(ParseError::from)?;
    maxp.num_glyphs = num_glyphs;

    // Build new hhea table
    let num_h_metrics = usize::from(hhea.num_h_metrics);
    hhea.num_h_metrics = num_glyphs;

    // Build new hmtx table
    let hmtx = create_hmtx_table(&hmtx, num_h_metrics, &subset_glyphs)?;

    // Extract the new glyf table now that we're done with subset_glyphs
    let glyf = GlyfTable::from(subset_glyphs.clone()); // FIXME: clone

    // Get the remaining tables
    let cvt = provider.table_data(tag::CVT)?;
    let fpgm = provider.table_data(tag::FPGM)?;
    let name = provider.table_data(tag::NAME)?;
    let prep = provider.table_data(tag::PREP)?;

    // Build the new font
    let mut builder = FontBuilder::new(0x00010000_u32);
    // TODO: Move this before the build phase
    if let Some(cmap0) = cmap0 {
        // Build a new cmap table
        let cmap = create_cmap_table(glyph_ids, cmap0)?;
        builder.add_table::<_, cmap::owned::Cmap>(tag::CMAP, cmap, ())?;
    } else {
        let cmap_data = provider.read_table_data(tag::CMAP)?;
        let cmap0 = ReadScope::new(&cmap_data).read::<Cmap<'_>>()?;
        let (encoding, cmap_subtable) =
            crate::font::read_cmap_subtable(&cmap0)?.ok_or(ParseError::MissingValue)?;
        let cmap = create_real_cmap_table(glyph_ids, &subset_glyphs, encoding, cmap_subtable)?;
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
    cmap0: Option<Box<[u8; 256]>>,
    convert_cff_to_cid_if_more_than_255_glyphs: bool,
) -> Result<Vec<u8>, ReadWriteError> {
    let cff_data = provider.read_table_data(tag::CFF)?;
    let scope = ReadScope::new(&cff_data);
    let cff: CFF<'_> = scope.read::<CFF<'_>>()?;
    if cff.name_index.count != 1 || cff.fonts.len() != 1 {
        return Err(ReadWriteError::from(ParseError::BadIndex));
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

    // Build new maxp table
    let num_glyphs = u16::try_from(cff_subset.len()).map_err(ParseError::from)?; // FIXME: is .len() correct here
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
    // TODO: Move this before the build phase
    if let Some(cmap0) = cmap0 {
        // Build a new cmap table
        let cmap = create_cmap_table(glyph_ids, cmap0)?;
        builder.add_table::<_, cmap::owned::Cmap>(tag::CMAP, cmap, ())?;
    } else {
        let cmap_data = provider.read_table_data(tag::CMAP)?;
        let cmap0 = ReadScope::new(&cmap_data).read::<Cmap<'_>>()?;
        let (encoding, cmap_subtable) =
            crate::font::read_cmap_subtable(&cmap0)?.ok_or(ParseError::MissingValue)?;
        let cmap = create_real_cmap_table(glyph_ids, &cff_subset, encoding, cmap_subtable)?;
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
    builder.add_table::<_, ReadScope<'_>>(tag::OS_2, ReadScope::new(&os_2), ())?;
    builder.add_table::<_, PostTable<'_>>(tag::POST, &post, ())?;
    if let Some(prep) = prep {
        builder.add_table::<_, ReadScope<'_>>(tag::PREP, ReadScope::new(&prep), ())?;
    }

    // Extract the new CFF table now that we're done with cff_subset
    let cff = CFF::from(cff_subset);
    builder.add_table::<_, CFF<'_>>(tag::CFF, &cff, ())?;
    let builder = builder.add_head_table(&head)?;
    builder.data()
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

fn create_cmap_table(
    glyph_ids: &[u16],
    cmap0: Box<[u8; 256]>,
) -> Result<cmap::owned::Cmap, ReadWriteError> {
    use cmap::owned::{Cmap, CmapSubtable, EncodingRecord};

    if glyph_ids.len() > 256 {
        return Err(ReadWriteError::Write(WriteError::BadValue));
    }

    Ok(Cmap {
        encoding_records: vec![EncodingRecord {
            platform_id: 1, // Macintosh platform
            encoding_id: 0, // Roman
            sub_table: CmapSubtable::Format0 {
                language: 0, // the subtable is language independent
                glyph_id_array: cmap0,
            },
        }],
    })
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
enum CharExistence {
    /// Can be encoded in [MacRoman](https://en.wikipedia.org/wiki/Mac_OS_Roman)
    MacRoman,
    /// Unicode Plane 0
    BasicMultilingualPlane,
    /// Unicode Plane 1 onwards
    AstralPlane,
    /// Exists outside Unicode
    DivinePlane,
}

// TODO: rename, ascention happens in the mapping_fn callback now
fn unicode_maybe_ascend(ch: char) -> CharExistence {
    match ch {
        ch if is_macroman(ch) => CharExistence::MacRoman,
        ch if ch <= '\u{FFFF}' => CharExistence::BasicMultilingualPlane,
        _ => CharExistence::AstralPlane,
    }
}

fn create_real_cmap_table(
    glyph_ids: &[u16],
    subset_glyphs: &impl SubsetGlyphs,
    encoding: Encoding,
    sub_table: CmapSubtable<'_>,
) -> Result<cmap::owned::Cmap, ReadWriteError> {
    let mut mappings_to_keep = BTreeMap::new();
    let mut plane;
    let maybe_ascend = match encoding {
        Encoding::Unicode => {
            plane = CharExistence::MacRoman;
            Some(unicode_maybe_ascend)
        }
        Encoding::Symbol => {
            plane = CharExistence::DivinePlane;
            None
        }
        Encoding::AppleRoman => {
            plane = CharExistence::MacRoman;
            None
        }
        Encoding::Big5 => todo!(),
    };
    sub_table.mappings_fn(|ch, gid| {
        // Skip mappings that point at .notdef (glyph id zero)
        if gid != 0 && glyph_ids.contains(&gid) {
            if let Some(maybe_ascend) = maybe_ascend {
                let existence = maybe_ascend(std::char::from_u32(ch).unwrap_or(' '));
                if existence > plane {
                    plane = existence;
                }
            }
            // TODO: Test if switching to a Set is worth it for this
            let new_id = subset_glyphs.new_id(gid);
            mappings_to_keep.insert(ch, new_id);
        }
    })?;

    // Ok now build the new sub-table
    let encoding_record = match plane {
        // TODO: Ensure there are fewer than 256 mappings
        CharExistence::MacRoman => {
            // The language field must be set to zero for all 'cmap' subtables whose platform IDs are other than Macintosh (platform ID 1). For 'cmap' subtables whose platform IDs are Macintosh, set this field to the Macintosh language ID of the 'cmap' subtable plus one, or to zero if the 'cmap' subtable is not language-specific. For example, a Mac OS Turkish 'cmap' subtable must set this field to 18, since the Macintosh language ID for Turkish is 17. A Mac OS Roman 'cmap' subtable must set this field to 0, since Mac OS Roman is not a language-specific encoding.
            let mut glyph_id_array = [0; 256];

            // If the encoding was not already mac roman then we need to convert from the source encoding
            match encoding {
                Encoding::Unicode => {
                    for (ch, gid) in mappings_to_keep.iter() {
                        let ch = std::char::from_u32(*ch).ok_or(ParseError::BadValue)?;
                        let ch_mac = char_to_macroman(ch).unwrap(); // should not panic as we verified all chars with `is_macroman` earlier
                                                                    // Cast is safe as we determined that all chars are valid in Mac Roman
                        glyph_id_array[usize::from(ch_mac)] = *gid as u8;
                    }
                }
                Encoding::Symbol => {}
                Encoding::AppleRoman => {
                    for (ch, gid) in mappings_to_keep.iter() {
                        // Cast is safe as we determined that all chars are valid in Mac Roman
                        glyph_id_array[*ch as usize] = *gid as u8;
                    }
                }
                Encoding::Big5 => {}
            }

            let sub_table = owned::CmapSubtable::Format0 {
                language: 0,
                glyph_id_array: Box::new(glyph_id_array),
            };
            owned::EncodingRecord {
                platform_id: 1, // Macintosh
                encoding_id: 0, // Roman
                sub_table,
            }
        }
        CharExistence::BasicMultilingualPlane => {
            // The language field must be set to zero for all 'cmap' subtables whose platform IDs are other than Macintosh (platform ID 1).
            let sub_table = cmap::owned::CmapSubtable::Format4(CmapSubtableFormat4::from_mappings(
                &mappings_to_keep,
            ));
            owned::EncodingRecord {
                platform_id: 0, // Unicode
                encoding_id: 3, // Unicode 2.0 and onwards semantics, Unicode BMP only
                sub_table,
            }
        }
        CharExistence::AstralPlane => {
            let sub_table = cmap::owned::CmapSubtable::Format12(
                CmapSubtableFormat12::from_mappings(&mappings_to_keep),
            );
            owned::EncodingRecord {
                platform_id: 0, // Unicode
                encoding_id: 4, // Unicode 2.0 and onwards semantics, Unicode full repertoire
                sub_table,
            }
        }
        CharExistence::DivinePlane => {
            let sub_table = cmap::owned::CmapSubtable::Format4(CmapSubtableFormat4::from_mappings(
                &mappings_to_keep,
            ));
            owned::EncodingRecord {
                platform_id: 3, // Windows
                encoding_id: 0, // Symbol
                sub_table,
            }
        }
    };

    // TODO: Now we need to set the encoding and other bits of the cmap table
    Ok(owned::Cmap {
        encoding_records: vec![encoding_record],
    })
}

#[derive(Debug)]
struct CmapSubtableFormat4Segment<'a> {
    start: u32,
    end: u32,
    glyph_ids: &'a mut Vec<u16>,
    consecutive_glyph_ids: bool,
}

impl<'a> CmapSubtableFormat4Segment<'a> {
    fn new(start: u32, gid: u16, glyph_ids: &'a mut Vec<u16>) -> Self {
        glyph_ids.clear();
        glyph_ids.push(gid);
        CmapSubtableFormat4Segment {
            start,
            end: start,
            glyph_ids,
            consecutive_glyph_ids: true,
        }
    }

    fn add(&mut self, ch: u32, gid: u16) -> bool {
        let gap = ch - self.start - 1; // -1 because the next consecutive character introduces no gap
        let should_remain_compact = self.consecutive_glyph_ids && self.glyph_ids.len() >= 4;

        if gap > 0 && should_remain_compact {
            // Each new segment costs 8 bytes so if the gap will introduce a non-consecutive glyph
            // id and the current segment contains 4 of more entries it's better to start a new
            // segment and allow this one to continue to use the compact representation.
            false
        } else if gap < 4 {
            // Each gap entry is two bytes in the glyph id array, if the gap is less than four
            // characters then it's worth adding to this segment, otherwise it's better to create
            // a new segment (which costs 8 bytes).

            // Gaps need to be mapped to .notdef (glyph id 0)
            // let gap = ch - self.start;
            if gap == 0 {
                let prev = self.glyph_ids.last().copied().unwrap(); // NOTE(unwrap): glyph_ids is never empty
                self.consecutive_glyph_ids &= (prev + 1) == gid;
            } else {
                self.glyph_ids.extend(iter::repeat(0).take(gap as usize));
                self.consecutive_glyph_ids = false; // if there's a gap then the glyph ids can't be consecutive
            }
            self.glyph_ids.push(gid);
            self.end = ch;
            true
        } else {
            false
        }
    }
}

impl CmapSubtableFormat4 {
    fn from_mappings(mappings: &BTreeMap<u32, u16>) -> owned::CmapSubtableFormat4 {
        // group the mappings into contiguous ranges, there can be holes in the ranges

        let mut table = owned::CmapSubtableFormat4 {
            language: 0,
            end_codes: Vec::new(),
            start_codes: Vec::new(),
            id_deltas: Vec::new(),
            id_range_offsets: Vec::new(),
            glyph_id_array: Vec::new(),
        };

        let mut glyph_ids = Vec::new();
        let (&start, &gid) = mappings.iter().next().unwrap(); // TODO: unwrap is safe because..? mappings can't be empty?
        let mut segment = CmapSubtableFormat4Segment::new(start, gid, &mut glyph_ids);
        let mut id_range_offset_fixup_indices = Vec::new();
        // TODO: document skip(1), because we pulled start already
        for (&ch, &gid) in mappings.iter().skip(1) {
            if !segment.add(ch, gid) {
                table.add_segment(segment, &mut id_range_offset_fixup_indices);
                segment = CmapSubtableFormat4Segment::new(ch, gid, &mut glyph_ids);
            }
        }

        // Add final range
        table.add_segment(segment, &mut id_range_offset_fixup_indices);

        // final start code and endCode values must be 0xFFFF. This segment need not contain any valid
        // mappings. (It can just map the single character code 0xFFFF to missingGlyph).
        // However, the segment must be present.
        segment = CmapSubtableFormat4Segment::new(0xFFFF, 0, &mut glyph_ids);
        table.add_segment(segment, &mut id_range_offset_fixup_indices);

        // Fix up the id_range_offsets now that all segments have been added
        for index in id_range_offset_fixup_indices {
            let id_range_offset = &mut table.id_range_offsets[index];
            // FIXME: usize might not be the appropriate size for this calculation
            *id_range_offset =
                (2 * (table.end_codes.len() + usize::from(*id_range_offset) - index)) as u16;
            println!(
                "consecutive_glyph_ids, id range offset = {}",
                *id_range_offset
            );
        }

        table
    }

    fn add_segment(
        &mut self,
        segment: CmapSubtableFormat4Segment<'_>,
        id_range_offset_fixups: &mut Vec<usize>,
    ) {
        // println!("{} -> {}", segment.start, segment.end);
        dbg!(&segment);
        self.start_codes.push(segment.start as u16);
        self.end_codes.push(segment.end as u16);

        // determine what needs to be added to id deltas, etc.

        // if the segment contains contiguous range of glyph ids then we can just store
        // an id delta for the entire range.
        if segment.consecutive_glyph_ids {
            // NOTE(unwrap): safe as segments will always contain at least one char->glyph mapping
            let first_glyph_id = *segment.glyph_ids.first().unwrap();

            // NOTE: casting start to i32 is safe as format 4 can only hold Unicode BMP chars,
            // which are 16-bit values. Casting the result to i16 is safe because the calculation
            // is modulo 0x10000 (65536), which limits the value to ±0x10000.
            self.id_deltas
                .push((i32::from(first_glyph_id) - segment.start as i32 % 0x10000) as i16);
            self.id_range_offsets.push(0);
            println!(
                "consecutive_glyph_ids, id delta = {}",
                self.id_deltas.last().unwrap()
            )
        } else {
            eprintln!("non-consecutive_glyph_ids");
            // Glyph ids are not consecutive so store them in the glyph id array via id range offsets
            self.id_deltas.push(0);
            // NOTE: The id range offset value will be fixed up in a later pass
            id_range_offset_fixups.push(self.id_range_offsets.len());
            self.id_range_offsets.push(self.glyph_id_array.len() as u16); // TODO: NOTE(cast)
            self.glyph_id_array.extend_from_slice(&segment.glyph_ids);
        }
    }
}

impl CmapSubtableFormat12 {
    fn from_mappings(mappings: &BTreeMap<u32, u16>) -> owned::CmapSubtableFormat12 {
        let (&start, &gid) = mappings.iter().next().unwrap(); // TODO: unwrap is safe because..? mappings can't be empty?
        let mut segment = SequentialMapGroup {
            start_char_code: start,
            end_char_code: start,
            start_glyph_id: u32::from(gid),
        };
        let mut segments = Vec::new();
        let mut prev_gid = gid;

        // TODO: document skip(1), because we pulled start already
        for (&ch, &gid) in mappings.iter().skip(1) {
            if ch == segment.end_char_code + 1 && gid == prev_gid + 1 {
                segment.end_char_code += 1
            } else {
                segments.push(segment);
                segment = SequentialMapGroup {
                    start_char_code: ch,
                    end_char_code: ch,
                    start_glyph_id: u32::from(gid),
                };
            }
            prev_gid = gid;
        }
        segments.push(segment);

        dbg!(&segments);
        CmapSubtableFormat12 {
            language: 0,
            groups: segments,
        }
    }
}

fn create_hmtx_table<'b>(
    hmtx: &HmtxTable<'_>,
    num_h_metrics: usize,
    subset_glyphs: &impl SubsetGlyphs,
) -> Result<HmtxTable<'b>, ReadWriteError> {
    let mut h_metrics = Vec::with_capacity(num_h_metrics);

    for glyph_id in 0..subset_glyphs.len() {
        let old_id = usize::from(subset_glyphs.old_id(glyph_id as u16)); // FIXME: cast comment/change

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
        let placeholder = self.add_table_inner::<_, HeadTable>(tag::HEAD, &table, ())?;

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
                tables.push(TaggedBuffer {
                    tag: tag,
                    buffer: table,
                });
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
        tag, FontTableProvider, ParseError, ReadScope, ReadWriteError, WriteBinary, WriteBuffer,
        CFF,
    };

    /// Subset this font so that it only contains the glyphs with the supplied `glyph_ids`.
    ///
    /// Returns just the CFF table in the case of a CFF font, not a complete OpenType font.
    pub fn subset(
        provider: &impl FontTableProvider,
        glyph_ids: &[u16],
        cmap0: Option<Box<[u8; 256]>>,
        convert_cff_to_cid_if_more_than_255_glyphs: bool,
    ) -> Result<Vec<u8>, ReadWriteError> {
        if provider.has_table(tag::CFF) {
            subset_cff_table(
                provider,
                glyph_ids,
                cmap0,
                convert_cff_to_cid_if_more_than_255_glyphs,
            )
        } else {
            super::subset_ttf(provider, glyph_ids, cmap0)
        }
    }

    /// Subset the CFF table and discard the rest
    ///
    /// Useful for PDF because a CFF table can be embedded directly without the need to wrap it in
    /// an OTF.
    fn subset_cff_table(
        provider: &impl FontTableProvider,
        glyph_ids: &[u16],
        _cmap0: Option<Box<[u8; 256]>>,
        convert_cff_to_cid_if_more_than_255_glyphs: bool,
    ) -> Result<Vec<u8>, ReadWriteError> {
        let cff_data = provider.read_table_data(tag::CFF)?;
        let scope = ReadScope::new(&cff_data);
        let cff: CFF<'_> = scope.read::<CFF<'_>>()?;
        if cff.name_index.count != 1 || cff.fonts.len() != 1 {
            return Err(ReadWriteError::from(ParseError::BadIndex));
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font_data::FontData;
    use crate::tables::glyf::GlyphData;
    use crate::tables::glyf::{
        BoundingBox, CompositeGlyph, CompositeGlyphArgument, CompositeGlyphFlag, GlyfRecord, Glyph,
        Point, SimpleGlyph, SimpleGlyphFlag,
    };
    use crate::tables::{LongHorMetric, OpenTypeData, OpenTypeFont};
    use crate::tag::DisplayTag;
    use crate::tests::read_fixture;

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
                        flags: vec![
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                        ],
                        coordinates: vec![
                            Point(0, 1434),
                            Point(1073, 1434),
                            Point(1073, 1098),
                            Point(485, 1098),
                            Point(485, 831),
                            Point(987, 831),
                            Point(987, 500),
                            Point(485, 500),
                            Point(485, 0),
                            Point(0, 0),
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
                        flags: vec![
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                        ],
                        coordinates: vec![
                            Point(0, 1434),
                            Point(485, 1434),
                            Point(485, 369),
                            Point(1061, 369),
                            Point(1061, 0),
                            Point(0, 0),
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
                        flags: vec![
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                        ],
                        coordinates: vec![
                            Point(0, 1434),
                            Point(485, 1434),
                            Point(485, 0),
                            Point(0, 0),
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
                        flags: vec![
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_SHORT_VECTOR
                                | SimpleGlyphFlag::Y_SHORT_VECTOR
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_SHORT_VECTOR
                                | SimpleGlyphFlag::Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_SHORT_VECTOR
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT
                                | SimpleGlyphFlag::X_SHORT_VECTOR
                                | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
                                | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                            SimpleGlyphFlag::ON_CURVE_POINT | SimpleGlyphFlag::X_SHORT_VECTOR,
                        ],
                        coordinates: vec![
                            Point(0, 0),
                            Point(436, 1434),
                            Point(1042, 1434),
                            Point(1478, 0),
                            Point(975, 0),
                            Point(909, 244),
                            Point(493, 244),
                            Point(430, 0),
                            Point(579, 565),
                            Point(825, 565),
                            Point(702, 1032),
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
    #[cfg(feature = "prince")]
    fn invalid_glyph_id() {
        // Test to ensure that invalid glyph ids don't panic when subsetting
        let buffer = read_fixture("../../../tests/data/fonts/HardGothicNormal.ttf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let glyph_ids = [0, 9999];

        match subset(&opentype_file.table_provider(0).unwrap(), &glyph_ids) {
            Err(ReadWriteError::Read(ParseError::BadIndex)) => {}
            _ => panic!("expected ReadWriteError::Read(ParseError::BadIndex) got somthing else"),
        }
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

    #[test]
    fn test_unicode_maybe_ascend() {
        assert_eq!(unicode_maybe_ascend('a'), CharExistence::MacRoman);
        assert_eq!(
            unicode_maybe_ascend('ռ'),
            CharExistence::BasicMultilingualPlane
        );
        assert_eq!(unicode_maybe_ascend('🦀'), CharExistence::AstralPlane);
    }

    #[test]
    fn test_format4_subtable() {
        let mappings = vec![
            ('a' as u32, 1),
            ('b' as u32, 2),
            ('i' as u32, 4),
            ('j' as u32, 3),
        ]
        .into_iter()
        .collect();
        CmapSubtableFormat4::from_mappings(&mappings);
    }

    #[test]
    fn test_format12_subtable() {
        let mappings = vec![
            ('a' as u32, 1),
            ('b' as u32, 2),
            ('🦀' as u32, 3),
            ('🦁' as u32, 4),
        ]
        .into_iter()
        .collect();
        CmapSubtableFormat12::from_mappings(&mappings);
    }
}
