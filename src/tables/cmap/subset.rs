use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::iter;
use std::marker::PhantomData;

use rustc_hash::FxHashSet;

use crate::big5::big5_to_unicode;
use crate::binary::read::ReadScope;
use crate::error::ParseError;
use crate::font::Encoding;
use crate::macroman::{char_to_macroman, is_macroman, macroman_to_char};
use crate::subset::SubsetGlyphs;
use crate::tables::cmap::{owned, Cmap, EncodingId, PlatformId, SequentialMapGroup};
use crate::tables::{cmap, FontTableProvider};
use crate::tag;

pub struct MappingsToKeep<T> {
    mappings: BTreeMap<Character, u16>,
    plane: CharExistence,
    _ids: PhantomData<T>,
}

pub enum OldIds {}
pub enum NewIds {}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum CharExistence {
    /// Can be encoded in [MacRoman](https://en.wikipedia.org/wiki/Mac_OS_Roman)
    MacRoman = 1,
    /// Unicode Plane 0
    BasicMultilingualPlane = 2,
    /// Unicode Plane 1 onwards
    AstralPlane = 3,
    /// Exists outside Unicode
    DivinePlane = 4,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
enum Character {
    Unicode(char),
    Symbol(u32),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CmapTarget {
    Unrestricted,
    MacRoman,
}

#[derive(Debug)]
struct CmapSubtableFormat4Segment<'a> {
    start: u32,
    end: u32,
    glyph_ids: &'a mut Vec<u16>,
    consecutive_glyph_ids: bool,
}

impl Character {
    fn new(ch: u32, encoding: Encoding) -> Option<Self> {
        match encoding {
            Encoding::Unicode => std::char::from_u32(ch).map(Character::Unicode),
            Encoding::Symbol => Some(Character::Symbol(ch)),
            Encoding::AppleRoman => macroman_to_char(ch as u8).map(Character::Unicode),
            Encoding::Big5 => u16::try_from(ch)
                .ok()
                .and_then(big5_to_unicode)
                .map(Character::Unicode),
        }
    }

    fn existence(self) -> CharExistence {
        match self {
            Character::Unicode(ch) if is_macroman(ch) => CharExistence::MacRoman,
            Character::Unicode(ch) if ch <= '\u{FFFF}' => CharExistence::BasicMultilingualPlane,
            Character::Unicode(_) => CharExistence::AstralPlane,
            Character::Symbol(_) => CharExistence::DivinePlane,
        }
    }

    fn as_u32(self) -> u32 {
        match self {
            Character::Unicode(ch) => ch as u32,
            Character::Symbol(ch) => ch,
        }
    }
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
        // -1 because the next consecutive character introduces no gap
        let gap = ch.saturating_sub(self.end).saturating_sub(1);
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
            if gap == 0 {
                // NOTE(unwrap): glyph_ids is never empty
                let prev = self.glyph_ids.last().copied().unwrap();
                self.consecutive_glyph_ids &= (prev + 1) == gid;
            } else {
                self.glyph_ids.extend(iter::repeat(0).take(gap as usize));
                // if there's a gap then the glyph ids can't be consecutive
                self.consecutive_glyph_ids = false;
            }
            self.glyph_ids.push(gid);
            self.end = ch;
            true
        } else {
            false
        }
    }
}

impl owned::CmapSubtableFormat4 {
    fn from_mappings(
        mappings: &MappingsToKeep<NewIds>,
    ) -> Result<owned::CmapSubtableFormat4, ParseError> {
        let mut table = owned::CmapSubtableFormat4 {
            language: 0,
            end_codes: Vec::new(),
            start_codes: Vec::new(),
            id_deltas: Vec::new(),
            id_range_offsets: Vec::new(),
            glyph_id_array: Vec::new(),
        };

        // Group the mappings into contiguous ranges, there can be holes in the ranges
        let mut glyph_ids = Vec::new();
        let mut id_range_offset_fixup_indices = Vec::new();
        // NOTE(unwrap): safe as mappings is non-empty
        let (start, gid) = mappings.iter().next().unwrap();
        let mut segment = CmapSubtableFormat4Segment::new(start.as_u32(), gid, &mut glyph_ids);
        for (ch, gid) in mappings.iter().skip(1) {
            if !segment.add(ch.as_u32(), gid) {
                table.add_segment(segment, &mut id_range_offset_fixup_indices);
                segment = CmapSubtableFormat4Segment::new(ch.as_u32(), gid, &mut glyph_ids);
            }
        }

        // Add final range
        table.add_segment(segment, &mut id_range_offset_fixup_indices);

        // Final start code and endCode values must be 0xFFFF. This segment need not contain any
        // valid mappings. (It can just map the single character code 0xFFFF to missingGlyph).
        // However, the segment must be present.
        //
        // — https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values
        segment = CmapSubtableFormat4Segment::new(0xFFFF, 0, &mut glyph_ids);
        table.add_segment(segment, &mut id_range_offset_fixup_indices);

        // Fix up the id_range_offsets now that all segments have been added
        let num_segments = table.end_codes.len();
        for index in id_range_offset_fixup_indices {
            let id_range_offset = &mut table.id_range_offsets[index];
            let count = num_segments + usize::from(*id_range_offset) - index;
            // ×2 because we need to skip over `count` 16-bit values
            *id_range_offset = u16::try_from(2 * count).map_err(|_| ParseError::LimitExceeded)?;
        }

        Ok(table)
    }

    fn add_segment(
        &mut self,
        segment: CmapSubtableFormat4Segment<'_>,
        id_range_offset_fixups: &mut Vec<usize>,
    ) {
        self.start_codes.push(segment.start as u16);
        self.end_codes.push(segment.end as u16);

        // If the segment contains contiguous range of glyph ids then we can just store
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
        } else {
            // Glyph ids are not consecutive so store them in the glyph id array via id range
            // offsets
            self.id_deltas.push(0);
            // NOTE: The id range offset value will be fixed up in a later pass
            id_range_offset_fixups.push(self.id_range_offsets.len());
            // NOTE: casting should be safe as num_glyphs in a font is u16
            self.id_range_offsets.push(self.glyph_id_array.len() as u16);
            self.glyph_id_array.extend_from_slice(&segment.glyph_ids);
        }
    }
}

impl owned::CmapSubtableFormat12 {
    fn from_mappings(mappings: &MappingsToKeep<NewIds>) -> owned::CmapSubtableFormat12 {
        // NOTE(unwrap): safe as mappings is non-empty
        let (start, gid) = mappings.iter().next().unwrap();
        let mut segment = SequentialMapGroup {
            start_char_code: start.as_u32(),
            end_char_code: start.as_u32(),
            start_glyph_id: u32::from(gid),
        };
        let mut segments = Vec::new();
        let mut prev_gid = gid;
        for (ch, gid) in mappings.iter().skip(1) {
            if ch.as_u32() == segment.end_char_code + 1 && gid == prev_gid + 1 {
                segment.end_char_code += 1
            } else {
                segments.push(segment);
                segment = SequentialMapGroup {
                    start_char_code: ch.as_u32(),
                    end_char_code: ch.as_u32(),
                    start_glyph_id: u32::from(gid),
                };
            }
            prev_gid = gid;
        }
        segments.push(segment);

        owned::CmapSubtableFormat12 {
            language: 0,
            groups: segments,
        }
    }
}

impl owned::EncodingRecord {
    pub fn from_mappings(mappings: &MappingsToKeep<NewIds>) -> Result<Self, ParseError> {
        match mappings.plane() {
            CharExistence::MacRoman => {
                // The language field must be set to zero for all 'cmap' subtables whose platform
                // IDs are other than Macintosh (platform ID 1). For 'cmap' subtables whose
                // platform IDs are Macintosh, set this field to the Macintosh language ID of the
                // 'cmap' subtable plus one, or to zero if the 'cmap' subtable is not
                // language-specific. For example, a Mac OS Turkish 'cmap' subtable must set this
                // field to 18, since the Macintosh language ID for Turkish is 17. A Mac OS Roman
                // 'cmap' subtable must set this field to 0, since Mac OS Roman is not a
                // language-specific encoding.
                //
                // — https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#use-of-the-language-field-in-cmap-subtables
                let mut glyph_id_array = [0; 256];
                for (ch, gid) in mappings.iter() {
                    let ch_mac = match ch {
                        // NOTE(unwrap): Safe as we verified all chars with `is_macroman` earlier
                        Character::Unicode(unicode) => {
                            usize::from(char_to_macroman(unicode).unwrap())
                        }
                        Character::Symbol(_) => unreachable!("symbol in mac roman"),
                    };
                    // Cast is safe as we determined that all chars are valid in Mac Roman
                    glyph_id_array[ch_mac] = gid as u8;
                }
                let sub_table = owned::CmapSubtable::Format0 {
                    language: 0,
                    glyph_id_array: Box::new(glyph_id_array),
                };
                Ok(owned::EncodingRecord {
                    platform_id: PlatformId::MACINTOSH,
                    encoding_id: EncodingId::MACINTOSH_APPLE_ROMAN,
                    sub_table,
                })
            }
            CharExistence::BasicMultilingualPlane => {
                let sub_table = cmap::owned::CmapSubtable::Format4(
                    owned::CmapSubtableFormat4::from_mappings(mappings)?,
                );
                Ok(owned::EncodingRecord {
                    platform_id: PlatformId::UNICODE,
                    encoding_id: EncodingId::UNICODE_BMP,
                    sub_table,
                })
            }
            CharExistence::AstralPlane => {
                let sub_table = cmap::owned::CmapSubtable::Format12(
                    owned::CmapSubtableFormat12::from_mappings(mappings),
                );
                Ok(owned::EncodingRecord {
                    platform_id: PlatformId::UNICODE,
                    encoding_id: EncodingId::UNICODE_FULL,
                    sub_table,
                })
            }
            CharExistence::DivinePlane => {
                let sub_table = cmap::owned::CmapSubtable::Format4(
                    owned::CmapSubtableFormat4::from_mappings(mappings)?,
                );
                Ok(owned::EncodingRecord {
                    platform_id: PlatformId::WINDOWS,
                    encoding_id: EncodingId::WINDOWS_SYMBOL,
                    sub_table,
                })
            }
        }
    }
}

impl<T> MappingsToKeep<T> {
    fn iter<'a>(&'a self) -> impl Iterator<Item = (Character, u16)> + 'a {
        self.mappings.iter().map(|(&ch, &gid)| (ch, gid))
    }

    fn plane(&self) -> CharExistence {
        self.plane
    }
}

impl MappingsToKeep<OldIds> {
    pub(crate) fn new(
        provider: &impl FontTableProvider,
        glyph_ids: &[u16],
        target: CmapTarget,
    ) -> Result<Self, ParseError> {
        let cmap_data = provider.read_table_data(tag::CMAP)?;
        let cmap0 = ReadScope::new(&cmap_data).read::<Cmap<'_>>()?;
        let (encoding, cmap_sub_table) =
            crate::font::read_cmap_subtable(&cmap0)?.ok_or(ParseError::MissingValue)?;

        // Collect cmap mappings for the selected glyph ids
        let mut mappings_to_keep = BTreeMap::new();
        let mut plane = CharExistence::MacRoman;

        // Process all the mappings and select the ones we want to keep
        cmap_sub_table.mappings_fn(|ch, gid| {
            if gid != 0 && glyph_ids.contains(&gid) {
                // We want to keep this mapping, determine the plane it lives on
                let output_char = match Character::new(ch, encoding) {
                    Some(ch) => ch,
                    None => return,
                };
                match target {
                    CmapTarget::MacRoman => {
                        // Only keep if it's MacRoman compatible
                        if output_char.existence() <= CharExistence::MacRoman {
                            mappings_to_keep.insert(output_char, gid);
                        }
                    }
                    CmapTarget::Unrestricted => {
                        if output_char.existence() > plane {
                            plane = output_char.existence();
                        }
                        mappings_to_keep.insert(output_char, gid);
                    }
                }
            }
        })?;

        if mappings_to_keep.len() <= usize::from(u16::MAX) {
            Ok(MappingsToKeep {
                mappings: mappings_to_keep,
                plane,
                _ids: PhantomData,
            })
        } else {
            Err(ParseError::LimitExceeded)
        }
    }

    /// Reorder glyph_ids into character order
    ///
    /// As a side effect of this duplicate glyph ids will be filtered out.
    ///
    /// By subsetting in character order the cmap tables have a better chance of being compact.
    /// Particularly format 4 and 12, which can map ranges of characters to ranges of glyph ids
    /// efficiently.
    pub(crate) fn reorder_glyph_ids(&self, glyph_ids: &[u16]) -> Vec<u16> {
        let mut present = FxHashSet::with_capacity_and_hasher(glyph_ids.len(), Default::default());
        let mut remaining = glyph_ids.iter().copied().collect::<FxHashSet<_>>();
        let mut reordered_glyph_ids = Vec::with_capacity(glyph_ids.len());

        // .notdef goes first
        present.insert(0);
        remaining.remove(&0);
        reordered_glyph_ids.push(0);

        // add glyphs that map to a character in character order
        for (_ch, &gid) in &self.mappings {
            if present.insert(gid) {
                reordered_glyph_ids.push(gid);
                remaining.remove(&gid);
            }
        }

        // add any remaining glyphs
        reordered_glyph_ids.extend(remaining.iter());
        reordered_glyph_ids
    }

    /// Update the glyph ids to be ids in the new subset font
    pub(crate) fn update_to_new_ids(
        mut self,
        subset_glyphs: &impl SubsetGlyphs,
    ) -> MappingsToKeep<NewIds> {
        self.mappings
            .iter_mut()
            .for_each(|(_ch, gid)| *gid = subset_glyphs.new_id(*gid));
        MappingsToKeep {
            mappings: self.mappings,
            plane: self.plane,
            _ids: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tables::cmap::{CmapSubtable, CmapSubtableFormat4};
    use crate::tables::OpenTypeFont;
    use crate::tests::read_fixture;
    use crate::Font;

    #[test]
    fn test_character_existence() {
        assert_eq!(Character::Unicode('a').existence(), CharExistence::MacRoman);
        assert_eq!(
            Character::Unicode('ռ').existence(),
            CharExistence::BasicMultilingualPlane
        );
        assert_eq!(
            Character::Unicode('🦀').existence(),
            CharExistence::AstralPlane
        );
    }

    #[test]
    fn test_format4_subtable() {
        let mappings = MappingsToKeep {
            mappings: vec![
                (Character::Unicode('a'), 1),
                (Character::Unicode('b'), 2),
                (Character::Unicode('i'), 4),
                (Character::Unicode('j'), 3),
            ]
            .into_iter()
            .collect(),
            plane: CharExistence::MacRoman,
            _ids: PhantomData,
        };
        let sub_table = owned::CmapSubtableFormat4::from_mappings(&mappings).unwrap();
        let expected = owned::CmapSubtableFormat4 {
            language: 0,
            start_codes: vec![97, 105, 0xFFFF],
            end_codes: vec![98, 106, 0xFFFF],
            id_deltas: vec![-96, 0, 1],
            id_range_offsets: vec![0, 4, 0],
            glyph_id_array: vec![4, 3],
        };
        assert_eq!(sub_table, expected);
    }

    #[test]
    fn test_format12_subtable() {
        let mappings = MappingsToKeep {
            mappings: vec![
                (Character::Unicode('a'), 1),
                (Character::Unicode('b'), 2),
                (Character::Unicode('🦀'), 3),
                (Character::Unicode('🦁'), 4),
            ]
            .into_iter()
            .collect(),
            plane: CharExistence::AstralPlane,
            _ids: PhantomData,
        };
        let sub_table = owned::CmapSubtableFormat12::from_mappings(&mappings);
        let expected = owned::CmapSubtableFormat12 {
            language: 0,
            groups: vec![
                SequentialMapGroup {
                    start_char_code: 97,
                    end_char_code: 98,
                    start_glyph_id: 1,
                },
                SequentialMapGroup {
                    start_char_code: 129408,
                    end_char_code: 129409,
                    start_glyph_id: 3,
                },
            ],
        };
        assert_eq!(sub_table, expected);
    }

    #[test]
    fn test_subtable_optimal_order() {
        let buffer = read_fixture("tests/fonts/opentype/Amaranth-Regular.ttf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        // This set of glyphs was collected by running...
        // ./bin/prince tests/adhoc/australia.html
        // where the wiki2.css file had been modified to use the font above
        // the subset method was changed to print the glyph ids it was called with
        // [
        //     ' ', '\"', '$', '%', '(', ')', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7',
        //     '8', '9', ':', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
        //     'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'Z', '[', ']', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
        //     'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '°', '²',
        //     'æ', 'é', 'ɹ', 'Τ', '—', '’', '′', 'ﬀ', 'ﬂ', 'ﬃ',
        // ]
        //
        // These glyph ids correspond to the characters above but have been shuffled
        let mut glyph_ids = [
            0, 37, 21, 10, 67, 13, 96, 8, 216, 14, 49, 29, 95, 63, 25, 73, 60, 54, 110, 6, 43, 72,
            3, 26, 48, 17, 38, 44, 135, 51, 53, 27, 88, 69, 20, 74, 12, 114, 40, 47, 64, 84, 61,
            118, 46, 28, 33, 9, 15, 19, 191, 66, 11, 217, 79, 42, 62, 22, 133, 68, 50, 57, 5, 18,
            58, 39, 36, 4, 7, 94, 35, 34, 41, 65, 24, 30, 23, 32, 45, 119, 16,
        ];
        let subset_font_data =
            crate::subset::subset(&opentype_file.table_provider(0).unwrap(), &mut glyph_ids)
                .unwrap();

        let opentype_file = ReadScope::new(&subset_font_data)
            .read::<OpenTypeFont<'_>>()
            .unwrap();
        let font = Font::new(opentype_file.table_provider(0).unwrap())
            .unwrap()
            .unwrap();

        let cmap_data = font.cmap_subtable_data();
        // Before implementing the ordering optimisation the table was 526 bytes
        assert_eq!(cmap_data.len(), 214);
        let cmap = ReadScope::new(cmap_data)
            .read::<CmapSubtable<'_>>()
            .unwrap();
        if let CmapSubtable::Format4(CmapSubtableFormat4 { end_codes, .. }) = cmap {
            // Before implementing the ordering optimisation there were 23 entries
            assert_eq!(end_codes.len(), 8);
        } else {
            panic!("expected cmap sub-table format 4");
        }
    }
}
