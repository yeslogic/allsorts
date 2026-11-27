#![deny(missing_docs)]

//! `kern` table parsing, subsetting, and writing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/kern>

use tinyvec::TinyVec;

use crate::{
    binary::{
        read::{
            ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope, ReadUnchecked,
        },
        write::{WriteBinary, WriteContext},
        I16Be, U16Be, U8,
    },
    context::Glyph,
    error::{ParseError, WriteError},
    glyph_position::TextDirection,
    gpos::{Info, Placement},
    scripts::horizontal_text_direction,
    subset::{self, SubsetGlyphs},
};

use super::aat::{
    VecTable, CLASS_CODE_DELETED, CLASS_CODE_EOT, CLASS_CODE_OOB, DELETED_GLYPH, MAX_OPS,
};
use owned::KernPairsSubtable;

/// Version 0 `coverage` bit: the sub-table has horizontal data.
const COVERAGE_HORIZONTAL: u16 = 1;
/// Version 0 `coverage` bit: the sub-table has minimum values rather than kerning values.
const COVERAGE_MINIMUM: u16 = 1 << 1;
/// Version 0 `coverage` bit: kerning is perpendicular to the flow of the text.
const COVERAGE_CROSS_STREAM: u16 = 1 << 2;
/// Version 0 `coverage` bit: the value replaces the accumulated value.
const COVERAGE_OVERRIDE: u16 = 1 << 3;

/// `kern` Kerning Table.
#[derive(Clone, Copy)]
pub struct KernTable<'a> {
    version: KernTableVersion,
    /// Number of subtables in the kerning table.
    table_count: u32,
    data: &'a [u8],
}

#[derive(Clone, Copy)]
enum KernTableVersion {
    /// Version of the kerning table, as defined in OpenType.
    KernTableVersion0,
    /// Version of the kerning table, as defined in Apple Advanced Typography.
    /// Contains extensions that are not supported in OpenType.
    KernTableVersion1,
}

/// Kerning data.
enum KernData<'a> {
    /// Format 0 kerning data (pairs).
    Format0(KernFormat0<'a>),
    /// Format 1 kerning data (state table).
    Format1(KernFormat1<'a>),
    /// Format 2 kerning data (2D array).
    Format2(KernFormat2<'a>),
    /// Format 3 kerning data (2D array).
    Format3(KernFormat3<'a>),
}

/// Format 0 kerning data (pairs).
struct KernFormat0<'a> {
    /// Array of KernPair records.
    kern_pairs: ReadArray<'a, KernPair>, // [nPairs]: KernPair,
}

/// Format 1 kerning data (state table).
struct KernFormat1<'a> {
    class_table: ClassTableFormat1<'a>,
    state_array: StateArray<'a>,
    entry_table: VecTable<ContextualEntry>,
    value_table: ValueTable<'a>,
}

struct StateArray<'a> {
    state_size: u16,
    offset: u16,
    states: Vec<ReadArray<'a, U8>>,
}

/// Format 2 kerning data (2D array).
struct KernFormat2<'a> {
    left_table: ClassTableFormat2<'a>,
    right_table: ClassTableFormat2<'a>,
    kerning_array: &'a [u8], // ReadArray<'a, I16Be>,
}

/// Format 3 kerning data (2D array).
struct KernFormat3<'a> {
    kern_value_table: ReadArray<'a, I16Be>,
    left_class_table: ReadArray<'a, U8>,
    right_class_table: ReadArray<'a, U8>,
    right_class_count: usize,
    kern_index_table: ReadArray<'a, U8>,
}

/// Kerning value for glyph pair.
#[derive(Clone, Copy)]
struct KernPair {
    /// The glyph index for the left-hand glyph in the kerning pair.
    left: u16,
    /// The glyph index for the right-hand glyph in the kerning pair.
    right: u16,
    /// The kerning value for the above pair, in font design units. If this value is greater than
    /// zero, the characters will be moved apart. If this value is less than zero, the character
    /// will be moved closer together.
    value: i16,
}

/// Format 1 glyph class table.
struct ClassTableFormat1<'a> {
    /// Glyph index of the first glyph in the class table.
    first_glyph: u16,
    /// The class codes (indexed by glyph index minus first_glyph).
    class_array: ReadArray<'a, U8>,
}

/// Format 2 glyph class table.
struct ClassTableFormat2<'a> {
    /// First glyph in class range.
    first_glyph: u16,
    values: ReadArray<'a, U16Be>,
}

/// Sub-table within `kern` table.
struct KernSubtable<'a> {
    coverage: KernCoverage,
    data: KernData<'a>,
}

enum KernCoverage {
    KernCoverageVersion0(u16),
    KernCoverageVersion1(u16),
}

impl ReadBinary for KernTable<'_> {
    type HostType<'a> = KernTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let version = match ctxt.read_u16be()? {
            0 => KernTableVersion::KernTableVersion0,
            1 => {
                let _padding = ctxt.read_u16be()?;
                KernTableVersion::KernTableVersion1
            }
            _ => return Err(ParseError::BadVersion),
        };
        let table_count = match version {
            KernTableVersion::KernTableVersion0 => u32::from(ctxt.read_u16be()?),
            KernTableVersion::KernTableVersion1 => ctxt.read_u32be()?,
        };
        let data = ctxt.scope().data();
        let kern = KernTable {
            version,
            table_count,
            data,
        };

        // Validate the sub-tables can be read.
        // Note that the sub-table `length` field can't be trusted as the very widely used
        // OpenSans font has an invalid value for this field. Avoid its use where possible.
        // https://github.com/fonttools/fonttools/issues/314#issuecomment-118116527
        kern.sub_tables().try_for_each(|table| table.map(drop))?;

        Ok(kern)
    }
}

impl<'a> KernTable<'a> {
    /// Iterate over the sub-tables of this `kern` table.
    fn sub_tables(&self) -> impl Iterator<Item = Result<KernSubtable<'a>, ParseError>> + 'a {
        let mut ctxt = ReadScope::new(self.data).ctxt();
        let version = self.version;

        (0..self.table_count).map(move |_| {
            let start = ctxt.scope();
            let (length, sub_table) = match version {
                KernTableVersion::KernTableVersion0 => {
                    let _version = ctxt.read_u16be()?;
                    let length = usize::from(ctxt.read_u16be()?);
                    let coverage = ctxt.read_u16be()?;
                    let format = coverage >> 8;
                    let data = match format {
                        0 => Self::read_format0(&mut ctxt).map(KernData::Format0)?,
                        2 => Self::read_format2(&mut ctxt, start, length).map(KernData::Format2)?,
                        _ => return Err(ParseError::BadValue),
                    };
                    let coverage = KernCoverage::KernCoverageVersion0(coverage);
                    (length, KernSubtable { coverage, data })
                }
                KernTableVersion::KernTableVersion1 => {
                    let length = usize::try_from(ctxt.read_u32be()?)?;
                    let coverage = ctxt.read_u16be()?;
                    let _tuple_index = ctxt.read_u16be()?;
                    let format = coverage & 0x00FF;
                    let data = match format {
                        0 => Self::read_format0(&mut ctxt).map(KernData::Format0)?,
                        1 => Self::read_format1(&mut ctxt, length).map(KernData::Format1)?,
                        2 => Self::read_format2(&mut ctxt, start, length).map(KernData::Format2)?,
                        3 => Self::read_format3(&mut ctxt, length).map(KernData::Format3)?,
                        _ => return Err(ParseError::BadValue),
                    };
                    let coverage = KernCoverage::KernCoverageVersion1(coverage);
                    (length, KernSubtable { coverage, data })
                }
            };

            // A sub-table ends `length` bytes after its start, which skips data read via offsets
            // and any padding. A `length` shorter than the data read is not trusted: the 16-bit
            // `length` of a version 0 format 0 sub-table with more than 10920 pairs wraps, so the
            // next sub-table starts at the end of the data read instead.
            let read_length = start.data().len() - ctxt.scope().data().len();
            ctxt = start.offset(length.max(read_length)).ctxt();

            Ok(sub_table)
        })
    }

    // Format 0 is the only sub-table format supported by Windows.
    fn read_format0(ctxt: &mut ReadCtxt<'a>) -> Result<KernFormat0<'a>, ParseError> {
        let n_pairs = ctxt.read_u16be()?;
        let _search_range = ctxt.read_u16be()?;
        let _entry_selector = ctxt.read_u16be()?;
        let _range_shift = ctxt.read_u16be()?;
        let kern_pairs = ctxt.read_array(usize::from(n_pairs))?; // [nPairs]: KernPair,

        Ok(KernFormat0 { kern_pairs })
    }

    fn read_format1(ctxt: &mut ReadCtxt<'a>, length: usize) -> Result<KernFormat1<'a>, ParseError> {
        // Per the spec, state table offsets are from the beginning of the state table, and _not_
        // from the beginning of the subtable (like in format 2).
        let sub_body_length = length.checked_sub(8).ok_or(ParseError::BadEof)?;
        let mut sub_ctxt = ctxt.read_scope(sub_body_length)?.ctxt();
        let start = sub_ctxt.scope();

        // StateHeader
        let state_size = sub_ctxt.read_u16be()?;
        let class_table_offset = sub_ctxt.read_u16be()?;
        let state_array_offset = sub_ctxt.read_u16be()?;
        let entry_table_offset = sub_ctxt.read_u16be()?;

        let value_table_offset = sub_ctxt.read_u16be()?;

        let class_table = start
            .offset(usize::from(class_table_offset))
            .read::<ClassTableFormat1<'_>>()?;

        // The _non-extended_ AAT state tables seem to differ from their extended counterpart,
        // in that the `new_state` value in the entry table is not a state number (like in `morx`),
        // but an offset from the beginning of the state table to the new state.
        //
        // We handle this by storing the `state_size` and `state_array_offset`, then using it to
        // compute the state number. It is probably easier to just store the state array as a
        // 1-dimensional array, but this approach is consistent with `morx` and makes it simpler to
        // generalise the state table methods (future work).
        let state_array = start
            .offset(usize::from(state_array_offset))
            .read_dep::<StateArray<'_>>((state_size, state_array_offset))?;

        let entry_table = start
            .offset(usize::from(entry_table_offset))
            .read::<VecTable<ContextualEntry>>()?;

        // Likewise, the `value_offset` value in the entry table is an offset from the beginning
        // of the state table.
        let value_table = ValueTable {
            offset: value_table_offset,
            values: start.offset(usize::from(value_table_offset)).data(),
        };

        Ok(KernFormat1 {
            class_table,
            state_array,
            entry_table,
            value_table,
        })
    }

    fn read_format2(
        ctxt: &mut ReadCtxt<'a>,
        start: ReadScope<'a>,
        length: usize,
    ) -> Result<KernFormat2<'a>, ParseError> {
        let _row_width = ctxt.read_u16be()?;
        let left_class_offset = ctxt.read_u16be()?;
        let right_class_offset = ctxt.read_u16be()?;
        let kerning_array_offset = usize::from(ctxt.read_u16be()?);

        let left_table = start
            .offset(usize::from(left_class_offset))
            .read::<ClassTableFormat2<'_>>()?;
        let right_table = start
            .offset(usize::from(right_class_offset))
            .read::<ClassTableFormat2<'_>>()?;
        // The kerning array is a 2-dimensional array of kerning values, with each row in the array
        // representing one left-hand glyph class, and each column representing one right-hand glyph
        // class.
        //
        // In order to compute the size of the kerning array without the (possibly unreliable)
        // subtable `length` field, we need to multiply the number of left-hand classes by the
        // number of right-hand classes. The `row_width` field presumably gives us the number of
        // right-hand classes, but there isn't a way to obtain the number of left-hand classes
        // without scanning the left-hand class table for the largest class number.
        //
        // As such, use the subtable `length` field. There appear to be _very_ few fonts in the
        // wild that use format 2 any way.
        let kerning_array_length = length
            .checked_sub(kerning_array_offset)
            .ok_or(ParseError::BadEof)?;
        let kerning_array = start
            .offset(kerning_array_offset)
            .ctxt()
            .read_slice(kerning_array_length)?;

        Ok(KernFormat2 {
            left_table,
            right_table,
            kerning_array,
        })
    }

    fn read_format3(ctxt: &mut ReadCtxt<'a>, length: usize) -> Result<KernFormat3<'a>, ParseError> {
        // Assume that `length` can be trusted. Subtable format 3 is specific to `kern` version 1;
        // unlike version 0, it stores its subtable length as a uint32. As such, the issues that
        // affect version 0 (see comment on OpenSans above) shouldn't occur.
        //
        // Use `length` to establish a sub-context from which the format 3 sub-subtables are read.
        // This is to guard against a situation where `length` > sum_length_of_subsubtables, which
        // occurs in Apple's Skia font and causes a mis-read of subsequent sub-tables.
        let sub_body_length = length.checked_sub(8).ok_or(ParseError::BadEof)?;
        let mut sub_ctxt = ctxt.read_scope(sub_body_length)?.ctxt();

        let glyph_count = usize::from(sub_ctxt.read_u16be()?);
        let kern_value_count = usize::from(sub_ctxt.read_u8()?);
        let left_class_count = usize::from(sub_ctxt.read_u8()?);
        let right_class_count = usize::from(sub_ctxt.read_u8()?);
        let _flags = sub_ctxt.read_u8()?;

        let kern_value_table = sub_ctxt.read_array(kern_value_count)?;
        let left_class_table = sub_ctxt.read_array(glyph_count)?;
        let right_class_table = sub_ctxt.read_array(glyph_count)?;
        let kern_index_table = sub_ctxt.read_array(left_class_count * right_class_count)?;

        Ok(KernFormat3 {
            kern_value_table,
            left_class_table,
            right_class_table,
            right_class_count,
            kern_index_table,
        })
    }

    /// Create an owned version of this `kern` table.
    pub fn to_owned(&self) -> owned::KernTable {
        owned::KernTable {
            version: self.version,
            table_count: self.table_count,
            data: Box::from(self.data),
        }
    }

    /// Create a `kern` table for a subset of the glyphs in this font.
    ///
    /// `glyphs` is the subset of the glyphs in this font. Pairs that refer to glyphs absent from
    /// `glyphs` are dropped; the remaining pairs are renumbered and sorted as the format requires.
    ///
    /// Format 0 (glyph pair) sub-tables are retained, in their original order. Sub-tables left
    /// without any pairs are omitted, and `None` is returned if no sub-tables remain.
    ///
    /// The result is always a version 0 table. The format 0 sub-tables of an Apple version 1.0
    /// table are converted to the version 0 encoding, which has the same pair layout: the
    /// horizontal and cross-stream bits are carried over to their version 0 positions.
    ///
    /// Sub-tables that can't be carried over make this return `ParseError::NotImplemented`.
    /// These are sub-tables in formats 1, 2 and 3, and sub-tables of a version 1.0 table that
    /// have the variation bit set.
    pub(crate) fn subset(
        &self,
        glyphs: &impl SubsetGlyphs,
    ) -> Result<Option<owned::KernPairsTable>, ParseError> {
        let mut sub_tables = Vec::new();
        for sub_table in self.sub_tables() {
            let sub_table = sub_table?;
            let (KernData::Format0(format0), Some(coverage)) =
                (&sub_table.data, sub_table.version0_coverage())
            else {
                return Err(ParseError::NotImplemented);
            };

            let mut pairs = format0
                .kern_pairs
                .iter()
                .filter_map(|pair| {
                    let left = glyphs.try_new_id(pair.left)?;
                    let right = glyphs.try_new_id(pair.right)?;
                    Some(KernPair {
                        left,
                        right,
                        value: pair.value,
                    })
                })
                .collect::<Vec<_>>();
            if pairs.is_empty() {
                continue;
            }
            pairs.sort_by_key(KernPair::search_key);
            sub_tables.push(KernPairsSubtable { coverage, pairs });
        }

        Ok((!sub_tables.is_empty()).then_some(owned::KernPairsTable { sub_tables }))
    }
}

impl WriteBinary for owned::KernPairsTable {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: Self) -> Result<(), WriteError> {
        U16Be::write(ctxt, 0u16)?; // version
        U16Be::write(ctxt, u16::try_from(table.sub_tables.len())?)?;
        ctxt.write_vec::<KernPairsSubtable, _>(table.sub_tables)?;

        Ok(())
    }
}

impl WriteBinary for KernPairsSubtable {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, table: Self) -> Result<(), WriteError> {
        let n_pairs = u16::try_from(table.pairs.len())?;
        let entry_selector = subset::max_power_of_2(n_pairs);
        let search_range = (1 << entry_selector) * KernPair::SIZE;
        let range_shift = (table.pairs.len() * KernPair::SIZE).saturating_sub(search_range);

        let start = ctxt.bytes_written();
        U16Be::write(ctxt, 0u16)?; // version
        let length = ctxt.placeholder::<U16Be, _>()?;
        U16Be::write(ctxt, table.coverage)?;
        U16Be::write(ctxt, n_pairs)?;
        // The 16-bit `length` field can't hold the length of a sub-table with more than 10920
        // pairs, nor `searchRange` its value with more than 16383 pairs. As fontTools does, they
        // are truncated to 16 bits then, and `rangeShift` is capped at 0xFFFF. The kern table of
        // OpenSans has a sub-table whose length is truncated. `KernTable::sub_tables` doesn't
        // trust a length shorter than the data read.
        U16Be::write(ctxt, search_range as u16)?;
        U16Be::write(ctxt, entry_selector)?;
        U16Be::write(ctxt, u16::try_from(range_shift).unwrap_or(u16::MAX))?;
        ctxt.write_vec::<KernPair, _>(table.pairs)?;
        ctxt.write_placeholder(length, (ctxt.bytes_written() - start) as u16)?;

        Ok(())
    }
}

impl<'a> From<&'a owned::KernTable> for KernTable<'a> {
    fn from(kern: &'a owned::KernTable) -> Self {
        KernTable {
            version: kern.version,
            table_count: kern.table_count,
            data: &kern.data,
        }
    }
}

impl KernSubtable<'_> {
    /// True if table has horizontal data, false if vertical.
    fn is_horizontal(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & COVERAGE_HORIZONTAL != 0,
            KernCoverage::KernCoverageVersion1(c) => c & 0x8000 == 0,
        }
    }

    /// If true the table has minimum values, otherwise the table has kerning values.
    fn is_minimum(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & COVERAGE_MINIMUM != 0,
            KernCoverage::KernCoverageVersion1(_) => false,
        }
    }

    /// Is kerning is perpendicular to the flow of the text.
    fn is_cross_stream(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & COVERAGE_CROSS_STREAM != 0,
            KernCoverage::KernCoverageVersion1(c) => c & 0x4000 != 0,
        }
    }

    /// True if the value in this table should replace the value currently being accumulated.
    fn is_override(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & COVERAGE_OVERRIDE != 0,
            KernCoverage::KernCoverageVersion1(_) => false,
        }
    }

    /// True if table has variation kerning values.
    fn has_variation(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(_) => false,
            KernCoverage::KernCoverageVersion1(c) => c & 0x2000 != 0,
        }
    }

    /// The `coverage` of this sub-table in the version 0 encoding.
    ///
    /// Returns `None` if the sub-table has properties that the version 0 encoding can't express.
    fn version0_coverage(&self) -> Option<u16> {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => Some(c),
            KernCoverage::KernCoverageVersion1(_) => {
                if self.has_variation() {
                    return None;
                }
                let mut coverage = 0;
                if self.is_horizontal() {
                    coverage |= COVERAGE_HORIZONTAL;
                }
                if self.is_cross_stream() {
                    coverage |= COVERAGE_CROSS_STREAM;
                }
                Some(coverage)
            }
        }
    }
}

impl KernPair {
    fn search_key(&self) -> u32 {
        (u32::from(self.left) << 16) | u32::from(self.right)
    }
}

impl ReadFrom for KernPair {
    type ReadType = (U16Be, U16Be, I16Be);

    fn read_from((left, right, value): (u16, u16, i16)) -> Self {
        KernPair { left, right, value }
    }
}

impl WriteBinary for KernPair {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, pair: KernPair) -> Result<(), WriteError> {
        U16Be::write(ctxt, pair.left)?;
        U16Be::write(ctxt, pair.right)?;
        I16Be::write(ctxt, pair.value)?;

        Ok(())
    }
}

impl ReadBinaryDep for StateArray<'_> {
    type Args<'a> = (u16, u16);
    type HostType<'a> = StateArray<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        (state_size, offset): (u16, u16),
    ) -> Result<Self::HostType<'a>, ParseError> {
        let mut state_array: Vec<ReadArray<'a, U8>> = Vec::new();

        loop {
            match ctxt.read_array::<U8>(usize::from(state_size)) {
                Ok(array) => state_array.push(array),
                Err(ParseError::BadEof) => break,
                Err(err) => return Err(err),
            }
        }

        Ok(StateArray {
            state_size,
            offset,
            states: state_array,
        })
    }
}

impl ReadBinary for ClassTableFormat1<'_> {
    type HostType<'a> = ClassTableFormat1<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let first_glyph = ctxt.read_u16be()?;
        let n_glyphs = ctxt.read_u16be()?;
        let class_array = ctxt.read_array(usize::from(n_glyphs))?;

        Ok(ClassTableFormat1 {
            first_glyph,
            class_array,
        })
    }
}

impl ReadBinary for ClassTableFormat2<'_> {
    type HostType<'a> = ClassTableFormat2<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let first_glyph = ctxt.read_u16be()?;
        let n_glyphs = ctxt.read_u16be()?;
        let values = ctxt.read_array(usize::from(n_glyphs))?;

        Ok(ClassTableFormat2 {
            first_glyph,
            values,
        })
    }
}

struct ContextualEntry {
    new_state: u16,
    flags: u16,
}

impl ContextualEntry {
    /// Push: if set, push this glyph on the kerning stack.
    fn push(&self) -> bool {
        self.flags & 0x8000 != 0
    }

    /// If set, don't advance to the next glyph before going to the new state.
    fn dont_advance(&self) -> bool {
        self.flags & 0x4000 != 0
    }

    /// ValueOffset: byte offset from the beginning of the subtable to the value table for the
    /// glyphs on the kerning stack.
    fn value_offset(&self) -> u16 {
        self.flags & 0x3FFF
    }
}

impl ReadFrom for ContextualEntry {
    type ReadType = (U16Be, U16Be);

    fn read_from((new_state, flags): (u16, u16)) -> Self {
        ContextualEntry { new_state, flags }
    }
}

struct ValueTable<'a> {
    offset: u16,
    values: &'a [u8],
}

struct ContextualContext<'a> {
    max_ops: isize,
    infos: &'a mut [Info],
    new_state: u16,
    stack: TinyVec<[usize; 8]>,
}

impl<'a> ContextualContext<'a> {
    fn new(infos: &'a mut [Info], new_state: u16) -> ContextualContext<'a> {
        ContextualContext {
            max_ops: MAX_OPS,
            infos,
            new_state,
            stack: TinyVec::new(),
        }
    }

    fn process(
        &mut self,
        is_cross_stream: bool,
        subtable: &KernFormat1<'_>,
    ) -> Result<(), ParseError> {
        let mut i = 0;
        while i <= self.infos.len() {
            let class = match self.infos.get(i) {
                Some(info) => {
                    let glyph_id = info.get_glyph_index();
                    if glyph_id == DELETED_GLYPH {
                        CLASS_CODE_DELETED
                    } else {
                        subtable
                            .class_table
                            .get(glyph_id)
                            .map(u16::from)
                            .unwrap_or(CLASS_CODE_OOB)
                    }
                }
                None => CLASS_CODE_EOT,
            };

            let entry_table_index = subtable
                .state_array
                .get(class, self.new_state)
                .ok_or(ParseError::BadIndex)?;

            let entry = subtable
                .entry_table
                .0
                .get(usize::from(entry_table_index))
                .ok_or(ParseError::BadIndex)?;

            self.new_state = entry.new_state;

            if entry.push() {
                if class == CLASS_CODE_EOT {
                    // `i` points to one past the buffer, so don't push it.
                } else if self.stack.last() == Some(&i) {
                    // When DONT_ADVANCE == true, avoid pushing the same index twice.
                } else {
                    self.stack.push(i)
                }
            }

            let mut value_offset = entry.value_offset();
            if value_offset != 0 {
                'stack: loop {
                    let value = subtable
                        .value_table
                        .get(value_offset)
                        .ok_or(ParseError::BadIndex)?;

                    let popped_i = match self.stack.pop() {
                        Some(popped_i) => popped_i,
                        None => break 'stack, // Stack underflow.
                    };

                    let info = &mut self.infos[popped_i];
                    let kerning = value.kerning();
                    if is_cross_stream {
                        // Not in the spec but in the example at the bottom of the page.
                        // Seems to be a special flag that resets cross-stream kerning.
                        if kerning == -0x8000 {
                            info.reset_cross_stream = true;
                            info.placement = match info.placement {
                                Placement::Distance(dx, _dy) => Placement::Distance(dx, 0),
                                _ => Placement::None,
                            }
                        } else if !info.reset_cross_stream {
                            info.placement.combine_distance(0, i32::from(kerning));
                        }
                    } else {
                        info.kerning += kerning;
                        info.placement.combine_distance(i32::from(kerning), 0);
                    }

                    if value.end_of_list() {
                        break 'stack;
                    }

                    value_offset = value_offset
                        .checked_add(u16::try_from(size_of::<i16>())?)
                        .ok_or(ParseError::BadIndex)?;
                }
            }

            if class == CLASS_CODE_EOT {
                break;
            }

            self.max_ops -= 1;
            if !entry.dont_advance() || self.max_ops <= 0 {
                i += 1;
            }
        }

        Ok(())
    }
}

impl KernData<'_> {
    /// Lookup the kerning for a pair of glyphs. Not applicable to format 1.
    fn lookup(&self, left: u16, right: u16) -> Option<i16> {
        match self {
            KernData::Format0(x) => {
                // The KernPair records must be ordered by combining the left and right values to
                // form an unsigned 32-bit integer (left as the high-order word), then ordering
                // records numerically using these combined values.
                let needle = (u32::from(left) << 16) | u32::from(right);
                x.kern_pairs
                    .binary_search_by(|pair| pair.search_key().cmp(&needle))
                    .ok()
                    .and_then(|index| x.kern_pairs.get_item(index))
                    .map(|pair| pair.value)
            }
            KernData::Format1(_) => None,
            KernData::Format2(x) => {
                // Get the class of the left/right glyphs, then lookup the kerning value
                let left_class = x.left_table.get(left)?;
                let right_class = x.right_table.get(right)?;

                // The values in the right class table are stored pre-multiplied by the number of
                // bytes in a single kerning value, and the values in the left class table are
                // stored pre-multiplied by the number of bytes in one row. This eliminates a need
                // to multiply the row and column values together to determine the location of the
                // kerning value.
                ReadScope::new(x.kerning_array)
                    .offset(usize::from(left_class) + usize::from(right_class))
                    .read::<I16Be>()
                    .ok()
            }
            KernData::Format3(x) => {
                // Suppose you have two glyphs, L and R, and you wish to determine the kerning
                // value. You can do so using this pseudo-expression:
                // value = kernValue[kernIndex[leftClass[L] * rightClassCount + rightClass[R]]].
                let left_class = x.left_class_table.get_item(usize::from(left))?;
                let right_class = x.right_class_table.get_item(usize::from(right))?;
                let kern_index = x.kern_index_table.get_item(
                    usize::from(left_class) * x.right_class_count + usize::from(right_class),
                )?;
                x.kern_value_table.get_item(usize::from(kern_index))
            }
        }
    }

    /// Apply state-table-based kerning to an entire glyph buffer. Only applicable to format 1.
    fn apply_format_1(&self, is_cross_stream: bool, infos: &mut [Info]) -> Result<(), ParseError> {
        match self {
            KernData::Format1(x) => {
                let mut context = ContextualContext::new(infos, x.state_array.offset);
                context.process(is_cross_stream, x)
            }
            _ => Ok(()),
        }
    }

    /// Indicates a format 1 subtable.
    fn is_format_1(&self) -> bool {
        matches!(self, KernData::Format1(_))
    }
}

/// Apply kerning to an array of positioned glyphs.
pub fn apply(
    kern: &KernTable<'_>,
    script_tag: u32,
    infos: &mut [Info],
) -> Result<bool, ParseError> {
    let mut has_cross_stream = false;

    if infos.is_empty() {
        return Ok(has_cross_stream);
    }

    let reverse_infos = horizontal_text_direction(script_tag) == TextDirection::RightToLeft;
    if reverse_infos {
        infos.reverse();
    }

    for sub_table in kern.sub_tables() {
        let sub_table = sub_table?;
        apply_sub_table(&sub_table, infos)?;
        has_cross_stream |= sub_table.is_cross_stream();
    }

    if has_cross_stream {
        accumulate_cross_stream_offsets(infos);
    }

    if reverse_infos {
        infos.reverse();
    }

    Ok(has_cross_stream)
}

fn apply_sub_table(sub_table: &KernSubtable<'_>, infos: &mut [Info]) -> Result<(), ParseError> {
    let kern_data = &sub_table.data;
    if kern_data.is_format_1() {
        if !sub_table.is_horizontal() {
            // TODO: Support vertical kerning.
            return Ok(());
        }

        kern_data.apply_format_1(sub_table.is_cross_stream(), infos)
    } else {
        if !sub_table.is_horizontal() || sub_table.is_cross_stream() {
            // TODO: Support vertical kerning; cross-stream kerning.
            return Ok(());
        }

        let mut iter = infos.iter_mut();
        let Some(mut left) = iter.next() else {
            return Ok(());
        };

        for right in iter {
            if let Some(value) = kern_data.lookup(left.get_glyph_index(), right.get_glyph_index()) {
                left.kerning = if sub_table.is_override() {
                    value
                } else if sub_table.is_minimum() {
                    left.kerning.min(value)
                } else {
                    left.kerning + value
                }
            }
            left = right;
        }

        Ok(())
    }
}

fn accumulate_cross_stream_offsets(infos: &mut [Info]) {
    let mut iter = infos.iter_mut();
    let Some(mut left) = iter.next() else {
        return;
    };

    for right in iter {
        if let Placement::Distance(_dx, dy) = left.placement {
            if !right.reset_cross_stream {
                right.placement.combine_distance(0, dy);
            }
        }
        left = right;
    }
}

impl ClassTableFormat1<'_> {
    fn get(&self, glyph_id: u16) -> Option<u8> {
        let index = glyph_id.checked_sub(self.first_glyph).map(usize::from)?;
        self.class_array.get_item(index)
    }
}

impl ClassTableFormat2<'_> {
    fn get(&self, glyph_id: u16) -> Option<u16> {
        let index = glyph_id.checked_sub(self.first_glyph).map(usize::from)?;
        self.values.get_item(index)
    }
}

impl StateArray<'_> {
    fn get(&self, class: u16, new_state: u16) -> Option<u8> {
        let row_index = new_state.checked_sub(self.offset)? / self.state_size;

        self.states
            .get(usize::from(row_index))
            .and_then(|s| s.get_item(usize::from(class)))
    }
}

impl ValueTable<'_> {
    fn get(&self, value_offset: u16) -> Option<Value> {
        let index = value_offset.checked_sub(self.offset).map(usize::from)?;

        ReadScope::new(self.values)
            .offset(index)
            .read::<I16Be>()
            .map(Value)
            .ok()
    }
}

struct Value(i16);

impl Value {
    /// From Apple's `kern` spec: The end of the list is marked by an odd value whose exact
    /// interpretation is determined by the `coverage` field in the subtable header.
    ///
    /// There is nothing of that sort in the `coverage` field, but the example in the spec implies
    /// that this "odd value" is the kerning value's low bit.
    fn end_of_list(&self) -> bool {
        self.0 & 1 == 1
    }

    /// The kerning value, which is the raw value with the low bit masked away.
    fn kerning(&self) -> i16 {
        self.0 & !1
    }
}

/// Version of `kern` table that holds owned data
pub mod owned {
    use super::{KernPair, KernTableVersion};

    /// Version 0 `kern` table holding only format 0 (glyph pair) sub-tables, as produced by
    /// [`KernTable::subset`](super::KernTable::subset).
    pub(crate) struct KernPairsTable {
        pub(super) sub_tables: Vec<KernPairsSubtable>,
    }

    /// A format 0 `kern` sub-table: a list of kerning pairs sorted by left, then right glyph id.
    pub(crate) struct KernPairsSubtable {
        /// The `coverage` field in the version 0 encoding.
        pub(super) coverage: u16,
        pub(super) pairs: Vec<KernPair>,
    }

    /// `kern` Kerning Table (owned version).
    pub struct KernTable {
        pub(super) version: KernTableVersion,
        /// Number of subtables in the kerning table.
        pub(super) table_count: u32,
        pub(super) data: Box<[u8]>,
    }

    impl KernTable {
        /// Creates an instance of [KernTable][super::KernTable] that borrows its data from this table.
        pub fn as_borrowed(&self) -> super::KernTable<'_> {
            super::KernTable {
                version: self.version,
                table_count: self.table_count,
                data: &self.data[..],
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        binary::{write::WriteBuffer, U32Be},
        subset::TestSubsetGlyphs,
        tables::{FontTableProvider, OpenTypeFont},
        tag,
        tests::read_fixture,
    };

    use super::*;

    #[test]
    fn parse() {
        let font_buffer = read_fixture("tests/fonts/opentype/OpenSans-Regular.ttf");
        let otf = ReadScope::new(&font_buffer)
            .read::<OpenTypeFont<'_>>()
            .unwrap();
        let table_provider = otf.table_provider(0).expect("error reading font file");

        let kern_data = table_provider
            .read_table_data(tag::KERN)
            .expect("unable to read kern data");
        let kern = ReadScope::new(&kern_data)
            .read::<KernTable<'_>>()
            .expect("unable to parse kern table");

        let subtables = kern
            .sub_tables()
            .collect::<Result<Vec<_>, _>>()
            .expect("error iterating sub-tables");

        assert_eq!(subtables.len(), 1);
        let sub_table = &subtables[0];
        assert!(sub_table.is_horizontal());
        assert!(!sub_table.is_minimum());
        let sub_table_data = &sub_table.data;
        // 'W' and 'A'
        let kerning = sub_table_data.lookup(58, 36);
        assert_eq!(kerning, Some(-82));
    }

    /// Size of the fields of a format 0 sub-table that follow the sub-table header: `nPairs`,
    /// `searchRange`, `entrySelector` and `rangeShift`.
    const FORMAT0_BODY_HEADER_SIZE: usize = 4 * U16Be::SIZE;
    /// Size of the header of a version 0, format 0 sub-table: `version`, `length` and `coverage`,
    /// then the fields of the body.
    const FORMAT0_HEADER_SIZE: usize = 3 * U16Be::SIZE + FORMAT0_BODY_HEADER_SIZE;
    /// Size of the header of a version 0 table: `version` and `nTables`.
    const VERSION0_HEADER_SIZE: usize = 2 * U16Be::SIZE;
    /// Size of the header of a version 1 sub-table: `length`, `coverage` and `tupleIndex`.
    const VERSION1_SUB_TABLE_HEADER_SIZE: usize = U32Be::SIZE + 2 * U16Be::SIZE;
    /// Version 1 `coverage` bit: the sub-table is vertical.
    const V1_VERTICAL: u16 = 0x8000;
    /// Version 1 `coverage` bit: kerning is perpendicular to the flow of the text.
    const V1_CROSS_STREAM: u16 = 0x4000;
    /// Version 1 `coverage` bit: the sub-table has variation data.
    const V1_VARIATION: u16 = 0x2000;

    /// A kerning pair as left glyph, right glyph and value.
    pub(crate) type PairTuple = (u16, u16, i16);

    fn kern_pairs(pairs: &[PairTuple]) -> Vec<KernPair> {
        pairs
            .iter()
            .map(|&(left, right, value)| KernPair { left, right, value })
            .collect()
    }

    /// Build a version 0 `kern` table of format 0 sub-tables with the given coverage and pairs,
    /// using the writer.
    pub(crate) fn version0_table(sub_tables: &[(u16, &[PairTuple])]) -> Vec<u8> {
        let table = owned::KernPairsTable {
            sub_tables: sub_tables
                .iter()
                .map(|&(coverage, pairs)| KernPairsSubtable {
                    coverage,
                    pairs: kern_pairs(pairs),
                })
                .collect(),
        };
        let mut w = WriteBuffer::new();
        owned::KernPairsTable::write(&mut w, table).unwrap();
        w.into_inner()
    }

    /// Build a version 0 format 0 sub-table whose `length` field is `length`, followed by
    /// `padding` zero bytes. The writer can't produce this as it never adds padding.
    fn padded_format0_sub_table(length: u16, pairs: &[PairTuple], padding: usize) -> Vec<u8> {
        let mut w = WriteBuffer::new();
        U16Be::write(&mut w, 0u16).unwrap(); // version
        U16Be::write(&mut w, length).unwrap();
        U16Be::write(&mut w, COVERAGE_HORIZONTAL).unwrap();
        U16Be::write(&mut w, u16::try_from(pairs.len()).unwrap()).unwrap(); // nPairs

        // searchRange, entrySelector and rangeShift are not used when reading
        w.write_zeros(3 * U16Be::SIZE).unwrap();
        w.write_iter::<KernPair, _>(kern_pairs(pairs).into_iter())
            .unwrap();
        w.write_zeros(padding).unwrap();
        w.into_inner()
    }

    /// Build a version 1 `kern` table of format 0 sub-tables with the given coverage and pairs.
    fn version1_table(sub_tables: &[(u16, &[PairTuple])]) -> Vec<u8> {
        let mut w = WriteBuffer::new();
        U16Be::write(&mut w, 1u16).unwrap(); // version, the high half of Fixed 1.0
        U16Be::write(&mut w, 0u16).unwrap(); // the low half of the version
        U32Be::write(&mut w, u32::try_from(sub_tables.len()).unwrap()).unwrap(); // nTables
        for &(coverage, pairs) in sub_tables {
            let length = VERSION1_SUB_TABLE_HEADER_SIZE
                + FORMAT0_BODY_HEADER_SIZE
                + pairs.len() * KernPair::SIZE;
            U32Be::write(&mut w, u32::try_from(length).unwrap()).unwrap();
            U16Be::write(&mut w, coverage).unwrap(); // the format is 0 in the low byte
            U16Be::write(&mut w, 0u16).unwrap(); // tupleIndex
            U16Be::write(&mut w, u16::try_from(pairs.len()).unwrap()).unwrap(); // nPairs
            w.write_zeros(3 * U16Be::SIZE).unwrap(); // search fields, not used when reading
            w.write_iter::<KernPair, _>(kern_pairs(pairs).into_iter())
                .unwrap();
        }
        w.into_inner()
    }

    /// Build a version 0 table holding one format 2 sub-table, with one class on each side.
    fn format2_table() -> Vec<u8> {
        // Format 2 header: version, length, coverage, rowWidth, then the offsets of the left and
        // right class tables and of the kerning array
        const HEADER_SIZE: usize = 7 * U16Be::SIZE;
        // A class table holds firstGlyph, nGlyphs and one class value
        const CLASS_TABLE_SIZE: usize = 3 * U16Be::SIZE;
        // The offsets are from the start of the sub-table
        let left_offset = HEADER_SIZE;
        let right_offset = left_offset + CLASS_TABLE_SIZE;
        let array_offset = right_offset + CLASS_TABLE_SIZE;
        let length = array_offset + I16Be::SIZE;

        let mut w = WriteBuffer::new();
        U16Be::write(&mut w, 0u16).unwrap(); // table version
        U16Be::write(&mut w, 1u16).unwrap(); // nTables
        U16Be::write(&mut w, 0u16).unwrap(); // sub-table version
        U16Be::write(&mut w, u16::try_from(length).unwrap()).unwrap();
        U16Be::write(&mut w, (2u16 << 8) | COVERAGE_HORIZONTAL).unwrap(); // format 2
        U16Be::write(&mut w, u16::try_from(I16Be::SIZE).unwrap()).unwrap(); // rowWidth
        U16Be::write(&mut w, u16::try_from(left_offset).unwrap()).unwrap();
        U16Be::write(&mut w, u16::try_from(right_offset).unwrap()).unwrap();
        U16Be::write(&mut w, u16::try_from(array_offset).unwrap()).unwrap();
        for _ in 0..2 {
            U16Be::write(&mut w, 1u16).unwrap(); // firstGlyph
            U16Be::write(&mut w, 1u16).unwrap(); // nGlyphs
            U16Be::write(&mut w, 0u16).unwrap(); // class value of the glyph
        }
        I16Be::write(&mut w, -5i16).unwrap(); // the kerning value
        w.into_inner()
    }

    fn pairs_of_sub_tables(data: &[u8]) -> Vec<Vec<PairTuple>> {
        let kern = ReadScope::new(data).read::<KernTable<'_>>().unwrap();
        kern.sub_tables()
            .map(|sub_table| match sub_table.unwrap().data {
                KernData::Format0(format0) => format0
                    .kern_pairs
                    .iter()
                    .map(|pair| (pair.left, pair.right, pair.value))
                    .collect(),
                _ => panic!("expected a format 0 sub-table"),
            })
            .collect()
    }

    #[test]
    fn sub_table_after_padded_sub_table() {
        // The length of the first sub-table includes 4 bytes of padding after its pair
        let padding = 4;
        let first_pairs = [(1, 2, -10)];
        let second_pairs = [(3, 4, 20)];
        let first = padded_format0_sub_table(
            u16::try_from(FORMAT0_HEADER_SIZE + KernPair::SIZE + padding).unwrap(),
            &first_pairs,
            padding,
        );
        let second = padded_format0_sub_table(
            u16::try_from(FORMAT0_HEADER_SIZE + KernPair::SIZE).unwrap(),
            &second_pairs,
            0,
        );
        let mut w = WriteBuffer::new();
        U16Be::write(&mut w, 0u16).unwrap(); // version
        U16Be::write(&mut w, 2u16).unwrap(); // nTables
        w.write_bytes(&first).unwrap();
        w.write_bytes(&second).unwrap();

        assert_eq!(
            pairs_of_sub_tables(&w.into_inner()),
            vec![first_pairs.to_vec(), second_pairs.to_vec()]
        );
    }

    #[test]
    fn sub_table_after_sub_table_with_wrapped_length() {
        // A sub-table with 10923 pairs is 65552 bytes long, which wraps to 16 in the 16-bit length
        // that the writer produces
        let pairs = (0..10923u16).map(|i| (i, i, 1)).collect::<Vec<_>>();
        let data = version0_table(&[
            (COVERAGE_HORIZONTAL, &pairs),
            (COVERAGE_HORIZONTAL, &[(3, 4, 20)]),
        ]);

        let sub_tables = pairs_of_sub_tables(&data);
        assert_eq!(sub_tables.len(), 2);
        assert_eq!(sub_tables[0], pairs);
        assert_eq!(sub_tables[1], vec![(3, 4, 20)]);
    }

    /// Subset the `kern` table `data` to the glyphs `old_ids` and write it. Returns `None` if the
    /// subset table has no sub-tables.
    fn subset_data(data: &[u8], old_ids: &[u16]) -> Option<Vec<u8>> {
        let kern = ReadScope::new(data).read::<KernTable<'_>>().unwrap();
        let subset = kern.subset(&TestSubsetGlyphs::new(old_ids)).unwrap()?;
        let mut w = WriteBuffer::new();
        owned::KernPairsTable::write(&mut w, subset).unwrap();
        Some(w.into_inner())
    }

    /// The coverage and pairs of each sub-table of the version 0 `kern` table `data`.
    pub(crate) fn coverage_and_pairs(data: &[u8]) -> Vec<(u16, Vec<PairTuple>)> {
        let kern = ReadScope::new(data).read::<KernTable<'_>>().unwrap();
        assert!(matches!(kern.version, KernTableVersion::KernTableVersion0));
        kern.sub_tables()
            .map(|sub_table| {
                let sub_table = sub_table.unwrap();
                let coverage = sub_table.version0_coverage().unwrap();
                match sub_table.data {
                    KernData::Format0(format0) => (
                        coverage,
                        format0
                            .kern_pairs
                            .iter()
                            .map(|pair| (pair.left, pair.right, pair.value))
                            .collect(),
                    ),
                    _ => panic!("expected a format 0 sub-table"),
                }
            })
            .collect()
    }

    #[test]
    fn subset_renumbers_drops_and_sorts_pairs() {
        let data = version0_table(&[(
            COVERAGE_HORIZONTAL,
            &[(1, 2, -10), (2, 3, 5), (5, 1, 7), (5, 2, 8), (6, 6, 1)],
        )]);

        // 5 -> 0, 2 -> 1, 1 -> 2. The pairs with glyphs 3 and 6 are dropped, the others are sorted
        // by their new ids.
        let subset = subset_data(&data, &[5, 2, 1]).unwrap();
        assert_eq!(
            coverage_and_pairs(&subset),
            vec![(COVERAGE_HORIZONTAL, vec![(0, 1, 8), (0, 2, 7), (2, 1, -10)])]
        );
    }

    #[test]
    fn subset_omits_empty_sub_tables_and_keeps_order() {
        let override_coverage = COVERAGE_HORIZONTAL | COVERAGE_OVERRIDE;
        let data = version0_table(&[
            (COVERAGE_HORIZONTAL, &[(1, 2, 10)]),
            (COVERAGE_HORIZONTAL, &[(3, 4, 20)]),
            (override_coverage, &[(2, 1, 30), (1, 1, 40)]),
        ]);

        // The second sub-table has no pair between retained glyphs
        let subset = subset_data(&data, &[1, 2]).unwrap();
        assert_eq!(
            coverage_and_pairs(&subset),
            vec![
                (COVERAGE_HORIZONTAL, vec![(0, 1, 10)]),
                (override_coverage, vec![(0, 0, 40), (1, 0, 30)]),
            ]
        );
    }

    #[test]
    fn subset_without_pairs_has_no_table() {
        let data = version0_table(&[
            (COVERAGE_HORIZONTAL, &[(1, 2, 10)]),
            (COVERAGE_HORIZONTAL, &[(3, 4, 20)]),
        ]);

        // Only one glyph of any pair is retained
        assert!(subset_data(&data, &[1, 3]).is_none());
        assert!(subset_data(&data, &[]).is_none());
    }

    #[test]
    fn subset_writes_version_0_header_and_search_fields() {
        let pairs = [(0, 1, 1), (0, 2, 2), (1, 2, 3), (2, 0, 4)];
        let data = version0_table(&[(COVERAGE_HORIZONTAL, &pairs)]);
        let subset = subset_data(&data, &[0, 1, 2]).unwrap();

        let mut expected = WriteBuffer::new();
        U16Be::write(&mut expected, 0u16).unwrap(); // table version
        U16Be::write(&mut expected, 1u16).unwrap(); // number of sub-tables
        U16Be::write(&mut expected, 0u16).unwrap(); // sub-table version
        let length = FORMAT0_HEADER_SIZE + pairs.len() * KernPair::SIZE;
        U16Be::write(&mut expected, u16::try_from(length).unwrap()).unwrap();
        U16Be::write(&mut expected, COVERAGE_HORIZONTAL).unwrap();
        U16Be::write(&mut expected, u16::try_from(pairs.len()).unwrap()).unwrap(); // nPairs

        // 4 pairs is a power of 2, so the search range covers all of them
        let search_range = pairs.len() * KernPair::SIZE;
        U16Be::write(&mut expected, u16::try_from(search_range).unwrap()).unwrap();
        U16Be::write(&mut expected, 2u16).unwrap(); // entrySelector
        U16Be::write(&mut expected, 0u16).unwrap(); // rangeShift
        expected
            .write_iter::<KernPair, _>(kern_pairs(&pairs).into_iter())
            .unwrap();

        let expected = expected.into_inner();
        assert_eq!(subset.len(), VERSION0_HEADER_SIZE + length);
        assert_eq!(subset, expected);
    }

    #[test]
    fn subset_sub_table_with_wrapped_length_keeps_all_pairs() {
        // A sub-table with 10923 pairs is 65552 bytes long, which wraps to 16 in the 16-bit length
        let pairs = (0..10923u16).map(|i| (i, i, 1)).collect::<Vec<_>>();
        let data = version0_table(&[
            (COVERAGE_HORIZONTAL, &pairs),
            (COVERAGE_HORIZONTAL, &[(20000, 1, 20), (3, 4, 20)]),
        ]);
        let old_ids = (0..10923u16).collect::<Vec<_>>();

        let subset = subset_data(&data, &old_ids).unwrap();
        // The length field follows the table header and the sub-table version
        let length_offset = VERSION0_HEADER_SIZE + U16Be::SIZE;
        let length = u16::from_be_bytes([subset[length_offset], subset[length_offset + 1]]);
        let full_length = FORMAT0_HEADER_SIZE + pairs.len() * KernPair::SIZE;
        assert!(full_length > usize::from(u16::MAX));
        assert_eq!(length, full_length as u16);
        // The sub-table after the one with the wrapped length is still found
        let sub_tables = coverage_and_pairs(&subset);
        assert_eq!(sub_tables.len(), 2);
        assert_eq!(sub_tables[0].1, pairs);
        assert_eq!(sub_tables[1].1, vec![(3, 4, 20)]);
    }

    #[test]
    fn subset_of_large_table_keeps_every_pair() {
        // The single sub-table of Open Sans has 18694 pairs, so its length does not fit in 16 bits
        let buffer = read_fixture("tests/fonts/opentype/OpenSans-Regular.ttf");
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let provider = otf.table_provider(0).unwrap();
        let data = provider.read_table_data(tag::KERN).unwrap();
        let maxp = provider.read_table_data(tag::MAXP).unwrap();
        let num_glyphs = ReadScope::new(&maxp)
            .read::<crate::tables::MaxpTable>()
            .unwrap()
            .num_glyphs;
        let (coverage, source) = coverage_and_pairs(&data).remove(0);
        assert_eq!(source.len(), 18694);

        // All glyphs, in the original order and then in reverse (apart from .notdef)
        let forward = (0..num_glyphs).collect::<Vec<_>>();
        let reversed = std::iter::once(0)
            .chain((1..num_glyphs).rev())
            .collect::<Vec<_>>();
        for old_ids in [forward, reversed] {
            let new_id = |old: u16| old_ids.iter().position(|&id| id == old).unwrap() as u16;
            let mut expected = source
                .iter()
                .map(|&(left, right, value)| (new_id(left), new_id(right), value))
                .collect::<Vec<_>>();
            expected.sort_by_key(|&(left, right, _)| (left, right));

            let subset = subset_data(&data, &old_ids).unwrap();
            assert_eq!(coverage_and_pairs(&subset), vec![(coverage, expected)]);
        }
    }

    #[test]
    fn subset_version_1_format_0_sub_tables() {
        // Horizontal, vertical and cross-stream, and horizontal again
        let data = version1_table(&[
            (0, &[(1, 2, 9)]),
            (V1_VERTICAL | V1_CROSS_STREAM, &[(2, 1, 8)]),
            (0, &[(2, 2, 7)]),
        ]);

        // The result is a version 0 table (coverage_and_pairs checks), with the pairs renumbered
        // (1 -> 0, 2 -> 1) and the horizontal and cross-stream bits mapped to their version 0
        // positions.
        let subset = subset_data(&data, &[1, 2]).unwrap();
        assert_eq!(
            coverage_and_pairs(&subset),
            vec![
                (COVERAGE_HORIZONTAL, vec![(0, 1, 9)]),
                (COVERAGE_CROSS_STREAM, vec![(1, 0, 8)]),
                (COVERAGE_HORIZONTAL, vec![(1, 1, 7)]),
            ]
        );
    }

    #[test]
    fn subset_version_1_variation_sub_table_is_not_implemented() {
        let data = version1_table(&[(0, &[(1, 2, 9)]), (V1_VARIATION, &[(1, 2, 9)])]);
        let kern = ReadScope::new(&data).read::<KernTable<'_>>().unwrap();

        let result = kern.subset(&TestSubsetGlyphs::new(&[1, 2]));
        assert!(matches!(result, Err(ParseError::NotImplemented)));
    }

    #[test]
    fn subset_format_2_sub_table_is_not_implemented() {
        let data = format2_table();
        let kern = ReadScope::new(&data).read::<KernTable<'_>>().unwrap();

        let result = kern.subset(&TestSubsetGlyphs::new(&[0, 1]));
        assert!(matches!(result, Err(ParseError::NotImplemented)));
    }
}
