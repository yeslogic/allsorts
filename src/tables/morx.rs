//! Binary reading of the `morx` table.
use std::convert::TryInto;

use bitflags::bitflags;

use crate::binary::read::{ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom};
use crate::binary::{U16Be, U32Be, U64Be, U8};
use crate::error::ParseError;
use crate::size;
use crate::SafeFrom;

/// The extended glyph metamorphosis table.
///
/// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html>
#[derive(Debug)]
pub struct MorxTable<'a> {
    pub version: u16,
    pub chains: Vec<Chain<'a>>,
}

impl<'b> ReadBinary for MorxTable<'b> {
    type HostType<'a> = MorxTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let version = ctxt.read_u16be()?;
        // TODO: handle this:
        // If the 'morx' table version is 3 or greater, then the last subtable in the chain is
        // followed by a subtableGlyphCoverageArray.
        ctxt.check_version(version == 2 || version == 3)?;
        let _unused = ctxt.read_u16be()?;
        let n_chains = ctxt.read_u32be()?;
        let mut chains = Vec::with_capacity(usize::safe_from(n_chains));

        for _i in 0..n_chains {
            // Read the chain header to get the chain length
            let scope_hdr = ctxt.scope();
            let chain_header = scope_hdr.read::<ChainHeader>()?;
            let chain_length = usize::safe_from(chain_header.chain_length);

            // Get a scope of length "chain_length" to read the chain and advance to the correct
            // position in the buffer for reading the next chain, regardless whether the "Subtable
            // Glyph Coverage table" is present at the end of the chain.
            let chain_scope = ctxt.read_scope(chain_length)?;
            let chain = chain_scope.read::<Chain<'a>>()?;
            chains.push(chain);
        }

        Ok(MorxTable { version, chains })
    }
}

#[derive(Debug)]
pub struct ChainHeader {
    pub default_flags: u32,
    chain_length: u32,
    n_feature_entries: u32,
    n_subtables: u32,
}

impl ReadFrom for ChainHeader {
    type ReadType = (U32Be, U32Be, U32Be, U32Be);

    fn read_from(
        (default_flags, chain_length, n_feature_entries, n_subtables): (u32, u32, u32, u32),
    ) -> Self {
        ChainHeader {
            default_flags,
            chain_length,
            n_feature_entries,
            n_subtables,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Feature {
    pub feature_type: u16,
    pub feature_setting: u16,
    pub enable_flags: u32,
    pub disable_flags: u32,
}

impl ReadFrom for Feature {
    type ReadType = (U16Be, U16Be, U32Be, U32Be);

    fn read_from(
        (feature_type, feature_setting, enable_flags, disable_flags): (u16, u16, u32, u32),
    ) -> Self {
        Feature {
            feature_type,
            feature_setting,
            enable_flags,
            disable_flags,
        }
    }
}

#[derive(Debug)]
pub struct Chain<'a> {
    pub chain_header: ChainHeader,
    pub feature_array: ReadArray<'a, Feature>,
    pub subtables: Vec<Subtable<'a>>,
}

impl<'b> ReadBinary for Chain<'b> {
    type HostType<'a> = Chain<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let chain_header = ctxt.read::<ChainHeader>()?;
        let feature_array =
            ctxt.read_array::<Feature>(usize::safe_from(chain_header.n_feature_entries))?;
        let subtables = (0..chain_header.n_subtables)
            .map(|_i| ctxt.read::<Subtable<'a>>())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Chain {
            chain_header,
            feature_array,
            subtables,
        })
    }
}

#[derive(Debug)]
pub struct SubtableHeader {
    length: u32,
    pub coverage: u32,
    pub sub_feature_flags: u32,
}

impl ReadFrom for SubtableHeader {
    type ReadType = (U32Be, U32Be, U32Be);

    fn read_from((length, coverage, sub_feature_flags): (u32, u32, u32)) -> Self {
        SubtableHeader {
            length,
            coverage,
            sub_feature_flags,
        }
    }
}

#[derive(Debug)]
pub struct Subtable<'a> {
    pub subtable_header: SubtableHeader,
    pub subtable_body: SubtableType<'a>,
}

impl<'b> ReadBinary for Subtable<'b> {
    type HostType<'a> = Subtable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let subtable_header = ctxt.read::<SubtableHeader>()?;

        // 12 is the length of the subtable header that needs to be skipped.
        let subtable_body_length = subtable_header
            .length
            .checked_sub(12)
            .map(usize::safe_from)
            .ok_or(ParseError::BadEof)?;

        // Get a shorter scope from the ReadCtxt to read the subtable
        let subtable_scope = ctxt.read_scope(subtable_body_length)?;

        let subtable_body = match subtable_header.coverage & 0xFF {
            1 => SubtableType::Contextual {
                contextual_subtable: subtable_scope.read::<ContextualSubtable<'a>>()?,
            },
            2 => SubtableType::Ligature {
                ligature_subtable: subtable_scope.read::<LigatureSubtable<'a>>()?,
            },
            4 => SubtableType::NonContextual {
                noncontextual_subtable: subtable_scope.read::<NonContextualSubtable<'a>>()?,
            },
            0 | 5 => {
                // Read the subtable to a slice &'a[u8] if it is another type other than ligature,
                // contextual or noncontextual
                SubtableType::Other {
                    other_subtable: subtable_scope.data(),
                }
            }
            _ => {
                return Err(ParseError::BadValue);
            }
        };

        Ok(Subtable {
            subtable_header,
            subtable_body,
        })
    }
}

#[derive(Debug)]
pub enum SubtableType<'a> {
    Contextual {
        contextual_subtable: ContextualSubtable<'a>,
    },
    Ligature {
        ligature_subtable: LigatureSubtable<'a>,
    },
    NonContextual {
        noncontextual_subtable: NonContextualSubtable<'a>,
    },
    Other {
        other_subtable: &'a [u8],
    },
}

#[derive(Debug)]
pub struct STXheader {
    n_classes: u32,
    class_table_offset: u32,
    state_array_offset: u32,
    entry_table_offset: u32,
}

impl ReadFrom for STXheader {
    type ReadType = (U32Be, U32Be, U32Be, U32Be);

    fn read_from(
        (n_classes, class_table_offset, state_array_offset, entry_table_offset): (
            u32,
            u32,
            u32,
            u32,
        ),
    ) -> Self {
        STXheader {
            n_classes,
            class_table_offset,
            state_array_offset,
            entry_table_offset,
        }
    }
}

/// Contextual Glyph Substitution Subtable
#[derive(Debug)]
pub struct ContextualSubtable<'a> {
    _stx_header: STXheader,
    pub class_table: ClassLookupTable<'a>,
    pub state_array: StateArray<'a>,
    pub entry_table: ContextualEntryTable,
    pub substitution_subtables: Vec<ClassLookupTable<'a>>,
}

impl<'b> ReadBinary for ContextualSubtable<'b> {
    type HostType<'a> = ContextualSubtable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let subtable = ctxt.scope();

        let stx_header = ctxt.read::<STXheader>()?;
        let substitution_subtables_offset = ctxt.read_u32be()?;

        let class_table = subtable
            .offset(usize::safe_from(stx_header.class_table_offset))
            .read::<ClassLookupTable<'a>>()?;

        let state_array = subtable
            .offset(usize::safe_from(stx_header.state_array_offset))
            .read_dep::<StateArray<'a>>(NClasses(stx_header.n_classes))?;

        let entry_table = subtable
            .offset(usize::safe_from(stx_header.entry_table_offset))
            .read::<ContextualEntryTable>()?;

        let first_offset_to_subst_tables = subtable
            .offset(usize::safe_from(substitution_subtables_offset))
            .ctxt()
            .read_u32be()?;

        let offset_array_len = first_offset_to_subst_tables / 4;
        let mut subst_tables_ctxt = subtable
            .offset(usize::safe_from(substitution_subtables_offset))
            .ctxt();

        // TODO: Can we avoid building this temporary Vec?
        let mut offsets_to_subst_tables: Vec<u32> = Vec::new();
        for _i in 0..offset_array_len {
            let value = match subst_tables_ctxt.read_u32be() {
                Ok(val) => val,
                Err(_err) => break,
            };
            offsets_to_subst_tables.push(value);
        }

        // TODO: Can this be pre-allocated?
        let mut substitution_subtables: Vec<ClassLookupTable<'a>> = Vec::new();
        for offset in offsets_to_subst_tables.iter().map(|o| usize::safe_from(*o)) {
            let subst_subtable = match subtable
                .offset(usize::safe_from(substitution_subtables_offset))
                .offset(offset)
                .read::<ClassLookupTable<'a>>()
            {
                Ok(val) => val,
                Err(_err) => break,
            };
            substitution_subtables.push(subst_subtable);
        }

        Ok(ContextualSubtable {
            _stx_header: stx_header,
            class_table,
            state_array,
            entry_table,
            substitution_subtables,
        })
    }
}

/// Noncontextual Glyph Substitution Subtable
#[derive(Debug)]
pub struct NonContextualSubtable<'a> {
    pub lookup_table: ClassLookupTable<'a>,
}

impl<'b> ReadBinary for NonContextualSubtable<'b> {
    type HostType<'a> = NonContextualSubtable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let lookup_table = ctxt.read::<ClassLookupTable<'a>>()?;

        Ok(NonContextualSubtable { lookup_table })
    }
}

/// Ligature subtable
#[derive(Debug)]
pub struct LigatureSubtable<'a> {
    _stx_header: STXheader,
    pub class_table: ClassLookupTable<'a>,
    pub state_array: StateArray<'a>,
    pub entry_table: LigatureEntryTable,
    pub action_table: LigatureActionTable,
    pub component_table: ComponentTable<'a>,
    pub ligature_list: LigatureList<'a>,
}

impl<'b> ReadBinary for LigatureSubtable<'b> {
    type HostType<'a> = LigatureSubtable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let subtable = ctxt.scope();

        let stx_header = ctxt.read::<STXheader>()?;

        let lig_action_offset = ctxt.read_u32be()?;

        let component_offset = ctxt.read_u32be()?;

        let ligature_list_offset = ctxt.read_u32be()?;

        let class_table = subtable
            .offset(usize::safe_from(stx_header.class_table_offset))
            .read::<ClassLookupTable<'a>>()?;

        let state_array = subtable
            .offset(usize::safe_from(stx_header.state_array_offset))
            .read_dep::<StateArray<'a>>(NClasses(stx_header.n_classes))?;

        let entry_table = subtable
            .offset(usize::safe_from(stx_header.entry_table_offset))
            .read::<LigatureEntryTable>()?;

        let action_table = subtable
            .offset(usize::safe_from(lig_action_offset))
            .read::<LigatureActionTable>()?;

        let component_table = subtable
            .offset(usize::safe_from(component_offset))
            .read::<ComponentTable<'a>>()?;

        let ligature_list = subtable
            .offset(usize::safe_from(ligature_list_offset))
            .read::<LigatureList<'a>>()?;

        Ok(LigatureSubtable {
            _stx_header: stx_header,
            class_table,
            state_array,
            entry_table,
            action_table,
            component_table,
            ligature_list,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NClasses(u32);

#[derive(Debug)]
pub struct StateArray<'a>(Vec<ReadArray<'a, U16Be>>);

impl<'a> StateArray<'a> {
    pub fn get(&self, index: u16) -> Option<&ReadArray<'a, U16Be>> {
        self.0.get(usize::from(index))
    }
}

impl<'b> ReadBinaryDep for StateArray<'b> {
    type Args<'a> = NClasses;
    type HostType<'a> = StateArray<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        NClasses(n_classes): NClasses,
    ) -> Result<Self::HostType<'a>, ParseError> {
        // TODO: Can we pre-allocate this?
        let mut state_array: Vec<ReadArray<'a, U16Be>> = Vec::new();
        let state_row_len = usize::safe_from(n_classes);

        loop {
            let state_row = match ctxt.read_array::<U16Be>(state_row_len) {
                Ok(array) => array,
                Err(ParseError::BadEof) => break,
                Err(err) => return Err(err),
            };

            state_array.push(state_row);
        }

        Ok(StateArray(state_array))
    }
}

#[derive(Debug)]
pub struct ComponentTable<'a> {
    pub component_array: ReadArray<'a, U16Be>,
}

impl<'b> ReadBinary for ComponentTable<'b> {
    type HostType<'a> = ComponentTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let len_remaining = ctxt.scope().data().len();
        let component_array = ctxt.read_array::<U16Be>(len_remaining / size::U16)?;

        Ok(ComponentTable { component_array })
    }
}

#[derive(Debug)]
pub struct LigatureList<'a>(pub ReadArray<'a, U16Be>);

impl<'a> LigatureList<'a> {
    pub fn get(&self, index: u16) -> Option<u16> {
        let index = usize::from(index);
        if index < self.0.len() {
            Some(self.0.get_item(index))
        } else {
            None
        }
    }
}

impl<'b> ReadBinary for LigatureList<'b> {
    type HostType<'a> = LigatureList<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let len_remaining = ctxt.scope().data().len();
        let ligature_list = ctxt.read_array::<U16Be>(len_remaining / size::U16)?;

        Ok(LigatureList(ligature_list))
    }
}

#[derive(Debug)]
pub struct LookupTableHeader {
    pub format: u16,
    bin_srch_header: Option<BinSrchHeader>,
}

impl ReadBinary for LookupTableHeader {
    type HostType<'a> = Self;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let format = ctxt.read_u16be()?;

        let bin_srch_header = match format {
            2 | 4 | 6 => Some(ctxt.read::<BinSrchHeader>()?),
            0 | 8 | 10 => None,
            _ => return Err(ParseError::BadValue),
        };

        Ok(LookupTableHeader {
            format,
            bin_srch_header,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinSrchHeader {
    unit_size: u16,
    n_units: u16,
    _search_range: u16,
    _entry_selector: u16,
    _range_shift: u16,
}

impl ReadBinary for BinSrchHeader {
    type HostType<'a> = Self;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let unit_size = ctxt.read_u16be()?;
        let n_units = ctxt.read_u16be()?;
        let search_range = ctxt.read_u16be()?;
        let entry_selector = ctxt.read_u16be()?;
        let range_shift = ctxt.read_u16be()?;

        Ok(BinSrchHeader {
            unit_size,
            n_units,
            _search_range: search_range,
            _entry_selector: entry_selector,
            _range_shift: range_shift,
        })
    }
}

#[derive(Debug)]
pub enum LookupTable<'a> {
    /// Simple Array format 0
    Format0 { lookup_values: Vec<u16> }, // FIXME: ReadArray
    /// Segment Single format 2
    Format2 {
        lookup_segments: ReadArray<'a, LookupSegmentFmt2>,
    },
    /// Segment Array format 4
    Format4 {
        lookup_segments: Vec<LookupValuesFmt4<'a>>,
    },
    /// Single Table format 6
    Format6 {
        lookup_entries: ReadArray<'a, LookupSingleFmt6>,
    },
    /// Trimmed Array format 8
    Format8(LookupTableFormat8<'a>),
    /// Trimmed Array format 10
    Format10(LookupTableFormat10<'a>),
}

#[derive(Debug)]
pub struct LookupTableFormat8<'a> {
    first_glyph: u16,
    lookup_values: ReadArray<'a, U16Be>,
}

impl<'a> LookupTableFormat8<'a> {
    pub fn new(
        first_glyph: u16,
        lookup_values: ReadArray<'a, U16Be>,
    ) -> Option<LookupTableFormat8<'a>> {
        // Validate arguments
        let len = lookup_values.len().try_into().ok()?;
        let _last = first_glyph.checked_add(len)?;
        Some(LookupTableFormat8 {
            first_glyph,
            lookup_values,
        })
    }
    pub fn contains(&self, glyph: u16) -> bool {
        // (glyph >= *first_glyph) && (glyph <= (*first_glyph + *glyph_count - 1))

        // NOTE(cast): Safe due to validation in new
        let end = self.first_glyph + self.lookup_values.len() as u16;
        (self.first_glyph..end).contains(&glyph)
    }

    pub fn lookup(&self, glyph: u16) -> Option<u16> {
        self.contains(glyph).then(|| {
            self.lookup_values
                .get_item(usize::from(glyph - self.first_glyph))
        })
    }
}

// TODO: Format8 is basically this with a unit size of 2
#[derive(Debug)]
pub struct LookupTableFormat10<'a> {
    first_glyph: u16,
    lookup_values: UnitSize<'a>,
}

impl<'a> LookupTableFormat10<'a> {
    pub fn new(first_glyph: u16, lookup_values: UnitSize<'a>) -> Option<Self> {
        // Validate arguments
        let len = lookup_values.len().try_into().ok()?;
        let _last = first_glyph.checked_add(len)?;
        Some(LookupTableFormat10 {
            first_glyph,
            lookup_values,
        })
    }

    pub fn contains(&self, glyph: u16) -> bool {
        // (glyph >= *first_glyph) && (glyph <= (*first_glyph + *glyph_count - 1))

        // NOTE(cast): Safe due to validation in new
        let end = self.first_glyph + self.lookup_values.len() as u16;
        (self.first_glyph..end).contains(&glyph)
    }

    pub fn lookup(&self, glyph: u16) -> Option<u16> {
        self.contains(glyph).then(|| {
            let index = glyph - self.first_glyph;
            match &self.lookup_values {
                UnitSize::OneByte(one_byte_values) => {
                    u16::from(one_byte_values.get_item(usize::from(index)))
                }
                UnitSize::TwoByte(two_byte_values) => two_byte_values.get_item(usize::from(index)),
                // Note: ignore 4-byte and 8-byte lookup values for now
                UnitSize::FourByte { .. } | UnitSize::EightByte { .. } => {
                    todo!("handle 4 and 8-bit lookup values")
                }
            }
        })
    }
}

#[derive(Debug)]
pub enum UnitSize<'a> {
    OneByte(ReadArray<'a, U8>),
    TwoByte(ReadArray<'a, U16Be>),
    FourByte(ReadArray<'a, U32Be>),
    EightByte(ReadArray<'a, U64Be>),
}

impl UnitSize<'_> {
    pub fn len(&self) -> usize {
        match self {
            UnitSize::OneByte(array) => array.len(),
            UnitSize::TwoByte(array) => array.len(),
            UnitSize::FourByte(array) => array.len(),
            UnitSize::EightByte(array) => array.len(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LookupSegmentFmt2 {
    pub last_glyph: u16,
    pub first_glyph: u16,
    // FIXME: Assumption: lookup values are commonly u16. If not u16, pass an error.
    pub lookup_value: u16,
}

impl LookupSegmentFmt2 {
    pub fn contains(&self, glyph: u16) -> bool {
        (self.first_glyph..=self.last_glyph).contains(&glyph)
    }
}

impl ReadFrom for LookupSegmentFmt2 {
    type ReadType = (U16Be, U16Be, U16Be);

    fn read_from((last_glyph, first_glyph, lookup_value): (u16, u16, u16)) -> Self {
        LookupSegmentFmt2 {
            last_glyph,
            first_glyph,
            lookup_value,
        }
    }
}

#[derive(Debug)]
pub struct LookupSegmentFmt4 {
    last_glyph: u16,
    first_glyph: u16,
    offset: u16,
}

impl ReadFrom for LookupSegmentFmt4 {
    type ReadType = (U16Be, U16Be, U16Be);

    fn read_from((last_glyph, first_glyph, offset): (u16, u16, u16)) -> Self {
        LookupSegmentFmt4 {
            last_glyph,
            first_glyph,
            offset,
        }
    }
}

#[derive(Debug)]
pub struct LookupValuesFmt4<'a> {
    pub last_glyph: u16,
    pub first_glyph: u16,
    pub lookup_values: ReadArray<'a, U16Be>,
}

impl LookupValuesFmt4<'_> {
    pub fn contains(&self, glyph: u16) -> bool {
        (self.first_glyph..=self.last_glyph).contains(&glyph)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LookupSingleFmt6 {
    pub glyph: u16,
    // FIXME: Assumption: lookup values are commonly u16. If not u16, pass an error.
    pub lookup_value: u16,
}

impl ReadFrom for LookupSingleFmt6 {
    type ReadType = (U16Be, U16Be);

    fn read_from((glyph, lookup_value): (u16, u16)) -> Self {
        LookupSingleFmt6 {
            glyph,
            lookup_value,
        }
    }
}

#[derive(Debug)]
pub struct ClassLookupTable<'a> {
    pub lookup_table: LookupTable<'a>,
}

impl<'b> ReadBinary for ClassLookupTable<'b> {
    type HostType<'a> = ClassLookupTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let class_table = ctxt.scope();

        let lookup_header = ctxt.read::<LookupTableHeader>()?;
        match (lookup_header.format, lookup_header.bin_srch_header) {
            (0, None) => {
                // FIXME: It seems like there should be an entry per-glyph, so n_glyphs should be passed in
                // so this can be ReadArray
                let mut lookup_values = Vec::new();

                loop {
                    let lookup_value = match ctxt.read_u16be() {
                        Ok(val) => val,
                        Err(_err) => break,
                    };

                    lookup_values.push(lookup_value);
                }

                let lookup_table = LookupTable::Format0 { lookup_values };

                Ok(ClassLookupTable { lookup_table })
            }
            (2, Some(b_sch_header)) => {
                // FIXME: 6 is a minimum
                // The units for this binary search are of type LookupSegment, and always have a minimum length of 6.
                if b_sch_header.unit_size != 6 {
                    return Err(ParseError::BadValue);
                }

                let lookup_segments =
                    ctxt.read_array::<LookupSegmentFmt2>(usize::from(b_sch_header.n_units))?;
                let lookup_table = LookupTable::Format2 { lookup_segments };

                Ok(ClassLookupTable { lookup_table })
            }
            (4, Some(b_sch_header)) => {
                let mut lookup_segments: Vec<LookupValuesFmt4<'_>> =
                    Vec::with_capacity(usize::from(b_sch_header.n_units));

                for _i in 0..b_sch_header.n_units {
                    let segment = match ctxt.read::<LookupSegmentFmt4>() {
                        Ok(val) => val,
                        // FIXME: Why isn't this error returned?
                        Err(_err) => break,
                    };

                    // To guarantee that a binary search terminates, you must include one or more
                    // special "end of search table" values at the end of the data to be searched.
                    // The number of termination values that need to be included is table-specific.
                    // The value that indicates binary search termination is 0xFFFF.
                    if (segment.first_glyph == 0xFFFF) && (segment.last_glyph == 0xFFFF) {
                        // TODO: I think only the last_glyph needs to be 0xFFFF to terminate the search
                        break;
                    }

                    let mut read_ctxt = class_table.offset(usize::from(segment.offset)).ctxt();

                    let num_lookup_values = segment
                        .last_glyph
                        .checked_sub(segment.first_glyph)
                        .ok_or(ParseError::BadValue)?
                        .checked_add(1)
                        .ok_or(ParseError::BadValue)?;
                    let lookup_values =
                        read_ctxt.read_array::<U16Be>(usize::from(num_lookup_values))?;

                    let lookup_segment = LookupValuesFmt4 {
                        last_glyph: segment.last_glyph,
                        first_glyph: segment.first_glyph,
                        lookup_values,
                    };

                    lookup_segments.push(lookup_segment);
                }

                let lookup_table = LookupTable::Format4 { lookup_segments };

                Ok(ClassLookupTable { lookup_table })
            }
            (6, Some(b_sch_header)) => {
                // FIXME: 4 is a minimum
                // The units for this binary search are of type LookupSingle and always have a minimum length of 4.
                if b_sch_header.unit_size != 4 {
                    return Err(ParseError::BadValue);
                }

                let lookup_entries =
                    ctxt.read_array::<LookupSingleFmt6>(usize::from(b_sch_header.n_units))?;

                let lookup_table = LookupTable::Format6 { lookup_entries };

                Ok(ClassLookupTable { lookup_table })
            }
            (8, None) => {
                let first_glyph = ctxt.read_u16be()?;
                let glyph_count = ctxt.read_u16be()?;
                let lookup_values = ctxt.read_array::<U16Be>(usize::from(glyph_count))?;
                let lookup_table = LookupTableFormat8::new(first_glyph, lookup_values)
                    .ok_or(ParseError::BadValue)?;

                Ok(ClassLookupTable {
                    lookup_table: LookupTable::Format8(lookup_table),
                })
            }
            (10, None) => {
                // Size of a lookup unit for this lookup table in bytes. Allowed values are 1, 2, 4, and 8.
                let unit_size = ctxt.read_u16be()?;
                let first_glyph = ctxt.read_u16be()?;
                let glyph_count = ctxt.read_u16be().map(usize::from)?;

                let lookup_values = match unit_size {
                    1 => {
                        let lookup_values = ctxt.read_array::<U8>(glyph_count)?;
                        UnitSize::OneByte(lookup_values)
                    }
                    2 => {
                        let lookup_values = ctxt.read_array::<U16Be>(glyph_count)?;
                        UnitSize::TwoByte(lookup_values)
                    }
                    4 => {
                        let lookup_values = ctxt.read_array::<U32Be>(glyph_count)?;
                        UnitSize::FourByte(lookup_values)
                    }
                    8 => {
                        let lookup_values = ctxt.read_array::<U64Be>(glyph_count)?;
                        UnitSize::EightByte(lookup_values)
                    }
                    _ => return Err(ParseError::BadValue),
                };

                let lookup_table = LookupTableFormat10::new(first_glyph, lookup_values)
                    .ok_or(ParseError::BadValue)?;

                Ok(ClassLookupTable {
                    lookup_table: LookupTable::Format10(lookup_table),
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

#[derive(Debug)]
pub struct LigatureEntry {
    pub next_state_index: u16,
    pub entry_flags: u16,
    pub lig_action_index: u16,
}

impl ReadFrom for LigatureEntry {
    type ReadType = (U16Be, U16Be, U16Be);

    fn read_from((next_state_index, entry_flags, lig_action_index): (u16, u16, u16)) -> Self {
        LigatureEntry {
            next_state_index,
            entry_flags,
            lig_action_index,
        }
    }
}

#[derive(Debug)]
pub struct LigatureEntryTable {
    pub lig_entries: Vec<LigatureEntry>,
}

impl ReadBinary for LigatureEntryTable {
    type HostType<'a> = Self;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        // TODO: Can we determine the size of this up front?
        let mut entry_vec: Vec<LigatureEntry> = Vec::new();

        loop {
            let entry = match ctxt.read::<LigatureEntry>() {
                Ok(val) => val,
                Err(_err) => break,
            };

            entry_vec.push(entry);
        }

        Ok(LigatureEntryTable {
            lig_entries: entry_vec,
        })
    }
}

bitflags! {
    pub struct ContextualEntryFlags: u16 {
        /// If set, make the current glyph the marked glyph.
        const SET_MARK = 0x8000;
        /// If set, don't advance to the next glyph before going to the new state.
        const DONT_ADVANCE = 0x4000;
        // 0x3FFF 	reserved 	These bits are reserved and should be set to 0.
    }
}

#[derive(Debug)]
pub struct ContextualEntry {
    pub next_state: u16,
    pub flags: ContextualEntryFlags,
    pub mark_index: u16,
    pub current_index: u16,
}

impl ReadFrom for ContextualEntry {
    type ReadType = (U16Be, U16Be, U16Be, U16Be);

    fn read_from((next_state, flags, mark_index, current_index): (u16, u16, u16, u16)) -> Self {
        ContextualEntry {
            next_state,
            flags: ContextualEntryFlags::from_bits_truncate(flags),
            mark_index,
            current_index,
        }
    }
}

#[derive(Debug)]
pub struct ContextualEntryTable {
    pub contextual_entries: Vec<ContextualEntry>,
}

impl ReadBinary for ContextualEntryTable {
    type HostType<'a> = Self;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        // TODO: Can we determine the size of this up front?
        let mut entry_vec: Vec<ContextualEntry> = Vec::new();

        loop {
            let entry = match ctxt.read::<ContextualEntry>() {
                Ok(val) => val,
                Err(_err) => break,
            };

            entry_vec.push(entry);
        }

        Ok(ContextualEntryTable {
            contextual_entries: entry_vec,
        })
    }
}

#[derive(Debug)]
pub struct LigatureActionTable {
    pub actions: Vec<u32>,
}

impl ReadBinary for LigatureActionTable {
    type HostType<'a> = Self;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        // TODO: Can we determine the size of this up front?
        let mut action_vec: Vec<u32> = Vec::new();

        loop {
            let action = match ctxt.read_u32be() {
                Ok(val) => val,
                Err(_err) => break,
            };

            action_vec.push(action);
        }

        Ok(LigatureActionTable {
            actions: action_vec,
        })
    }
}
