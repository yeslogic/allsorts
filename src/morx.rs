use std::convert::TryFrom;

use crate::binary::read::{ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope};
use crate::binary::U16Be;
use crate::binary::U32Be;
use crate::binary::U8;
use crate::error::ParseError;
use crate::gsub::{GlyphOrigin, RawGlyph};
use crate::tinyvec::{tiny_vec, TinyVec};

//----------------------------------------------------------------------------------
#[derive(Debug)]
pub struct MorxHeader {
    version: u16,
    unused: u16,
    n_chains: u32,
}

impl<'a> ReadFrom<'a> for MorxHeader {
    type ReadType = (U16Be, U16Be, U32Be);

    fn from((version, unused, n_chains): (u16, u16, u32)) -> Self {
        MorxHeader {
            version,
            unused,
            n_chains,
        }
    }
}

#[derive(Debug)]
pub struct MorxTable {
    morx_header: MorxHeader,
    morx_chains: Vec<Chain>,
}

impl<'a> ReadBinary<'a> for MorxTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let morx_header = ctxt.read::<MorxHeader>()?;
        let mut chain_vec = Vec::new();

        for i in 0..morx_header.n_chains {
            //read the chain header to get the chain length
            let scope_hdr = ctxt.scope();
            let chain_header = scope_hdr.read::<ChainHeader>()?;
            let chain_length: usize = usize::try_from(chain_header.chain_length)?;

            //get a scope of length "chain_length" to read the chain and
            //advance to the correct position in the buffer for reading
            //the next chain, regardless whether the "Subtable Glyph Coverage table"
            //is present at the end of the chain.
            let chain_scope = ctxt.read_scope(chain_length)?;
            let chain = chain_scope.read::<Chain>()?;
            chain_vec.push(chain);
        }

        Ok(MorxTable {
            morx_header: morx_header,
            morx_chains: chain_vec,
        })
    }
}

//----------------------------------------------------------------------------------
#[derive(Debug)]
pub struct ChainHeader {
    default_flags: u32,
    chain_length: u32,
    n_feature_entries: u32,
    n_subtables: u32,
}

impl<'a> ReadFrom<'a> for ChainHeader {
    type ReadType = (U32Be, U32Be, U32Be, U32Be);

    fn from(
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

//----------------------------------------------------------------------------------
#[derive(Debug)]
pub struct Feature {
    feature_type: u16,
    feature_setting: u16,
    enable_flags: u32,
    disable_flags: u32,
}

impl<'a> ReadFrom<'a> for Feature {
    type ReadType = (U16Be, U16Be, U32Be, U32Be);

    fn from(
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

//----------------------------------------------------------------------------------
#[derive(Debug)]
pub struct Chain {
    chain_header: ChainHeader,
    feature_array: Vec<Feature>,
    subtables: Vec<Subtable>,
}

impl<'a> ReadBinary<'a> for Chain {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let chain_header = ctxt.read::<ChainHeader>()?;

        let features =
            ctxt.read_array::<Feature>(usize::try_from(chain_header.n_feature_entries)?)?;
        let feature_vec = features.to_vec();

        let mut subtable_vec = Vec::new();
        for i in 0..chain_header.n_subtables {
            let subtable = ctxt.read::<Subtable>()?;
            subtable_vec.push(subtable);
        }

        Ok(Chain {
            chain_header: chain_header,
            feature_array: feature_vec,
            subtables: subtable_vec,
        })
    }
}

//----------------------------------------------------------------------------------
#[derive(Debug)]
pub struct SubtableHeader {
    length: u32,
    coverage: u32,
    sub_feature_flags: u32,
}

impl<'a> ReadFrom<'a> for SubtableHeader {
    type ReadType = (U32Be, U32Be, U32Be);

    fn from((length, coverage, sub_feature_flags): (u32, u32, u32)) -> Self {
        SubtableHeader {
            length,
            coverage,
            sub_feature_flags,
        }
    }
}

//----------------------------------------------------------------------------------
#[derive(Debug)]
pub struct Subtable {
    subtable_header: SubtableHeader,
    subtable_body: SubtableType,
}

impl<'a> ReadBinary<'a> for Subtable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let subtable_header = ctxt.read::<SubtableHeader>()?;

        let subtable_body: SubtableType;
        let subtable_body_length: usize = usize::try_from(subtable_header.length - 12)?;
        match subtable_header.coverage & 0xFF {
            2 => {
                //Get a shorter scope from the context to read the ligature subtable
                let subtable_scope = ctxt.read_scope(subtable_body_length)?;
                let ligature_subtable = subtable_scope.read::<LigatureSubtable>()?;
                subtable_body = SubtableType::Ligature { ligature_subtable };
            }
            _ => {
                //read the subtable to a Vec<u8> if it is another type other than ligature
                let subtable_array = ctxt.read_array::<U8>(subtable_body_length)?;
                let subtable_vec = subtable_array.to_vec();

                subtable_body = SubtableType::Other {
                    other_subtable: subtable_vec,
                };
            }
        }

        Ok(Subtable {
            subtable_header: subtable_header,
            subtable_body: subtable_body,
        })
    }
}

/***************************************************************************************************
//--------------------------------------------------------------------------------------------------
#[derive(Debug)]
pub enum SubtableType {
    Rearragement {
        rearrangement_subtable: RearrangementSubtable,
    },
    Contextual {
        contextual_subtable: ContextualSubstitutionSubtable,
    },
    Ligature {
        ligature_subtable: LigatureSubtable,
    },
    NonContextual {
        noncontextual_subtable: NonContextualSubstitutionSubtable,
    },
    Insertion {
        insertion_subtable: InsertionSubtable,
    },
}

****************************************************************************************************/
//------------------------------------
#[derive(Debug)]
pub enum SubtableType {
    Ligature { ligature_subtable: LigatureSubtable },
    Other { other_subtable: Vec<u8> },
}

//----------------------------- Extended State Table Header ------------------------------------------

#[derive(Debug)]
pub struct STXheader {
    n_classes: u32,
    class_table_offset: u32,
    state_array_offset: u32,
    entry_table_offset: u32,
}

impl<'a> ReadFrom<'a> for STXheader {
    type ReadType = (U32Be, U32Be, U32Be, U32Be);

    fn from(
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

//------------------------------------ Subtables ------------------------------------------------

//Indic Rearrangement Subtable
#[derive(Debug)]
pub struct RearrangementSubtable {
    stx_header: STXheader,
    //subtable body here,
    //the rearrangement subtable is just a state table
}

//Contextual Glyph Substitution Subtable
#[derive(Debug)]
pub struct ContextualSubstitutionSubtable {
    stx_header: STXheader,
    substitution_subtables_offset: u32,
    //subtable body here
}

//Noncontextual Glyph Substitution Subtable
#[derive(Debug)]
pub struct NonContextualSubstitutionSubtable {
    stx_header: STXheader,
    //subtable body here
}

//glyph Insertion Subtable
#[derive(Debug)]
pub struct InsertionSubtable {
    stx_header: STXheader,
    //subtable body here
}

//Ligature subtable
#[derive(Debug)]
pub struct LigatureSubtable {
    stx_header: STXheader,
    lig_action_offset: u32,
    component_offset: u32,
    ligature_list_offset: u32,
    class_table: ClassLookupTable,
    state_array: StateArray,
    entry_table: LigatureEntryTable,
    action_table: LigatureActionTable,
    component_table: ComponentTable,
    ligature_list: LigatureList,
}

impl<'a> ReadBinary<'a> for LigatureSubtable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let subtable = ctxt.scope();

        let stx_header = ctxt.read::<STXheader>()?;

        let lig_action_offset = ctxt.read_u32be()?;

        let component_offset = ctxt.read_u32be()?;

        let ligature_list_offset = ctxt.read_u32be()?;

        //read the class lookup table
        let class_table = subtable
            .offset(usize::try_from(stx_header.class_table_offset)?)
            .read::<ClassLookupTable>()?;

        //read the state array:
        let state_array = subtable
            .offset(usize::try_from(stx_header.state_array_offset)?)
            .read_dep::<StateArray>(NClasses(stx_header.n_classes))?;

        //read the ligature entry table
        let entry_table = subtable
            .offset(usize::try_from(stx_header.entry_table_offset)?)
            .read::<LigatureEntryTable>()?;

        //read the ligature action table
        let action_table = subtable
            .offset(usize::try_from(lig_action_offset)?)
            .read::<LigatureActionTable>()?;

        //read the component table
        let component_table = subtable
            .offset(usize::try_from(component_offset)?)
            .read::<ComponentTable>()?;

        //read the ligature list
        let ligature_list = subtable
            .offset(usize::try_from(ligature_list_offset)?)
            .read::<LigatureList>()?;

        Ok(LigatureSubtable {
            stx_header,
            lig_action_offset,
            component_offset,
            ligature_list_offset,
            class_table,
            state_array,
            entry_table,
            action_table,
            component_table,
            ligature_list,
        })
    }
}

//--------------------------------- State Array ---------------------------------------------
#[derive(Debug, Clone, Copy)]
pub struct NClasses(u32);

#[derive(Debug)]
pub struct StateArray {
    state_array: Vec<Vec<u16>>,
}

impl<'a> ReadBinaryDep<'a> for StateArray {
    type Args = NClasses;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: NClasses) -> Result<Self, ParseError> {
        let n_classes = args;

        let mut state_array: Vec<Vec<u16>> = Vec::new();

        'outer: loop {
            let mut state_row: Vec<u16> = Vec::new();

            for i in 0..n_classes.0 {
                let value = match ctxt.read_u16be() {
                    Ok(val) => val,
                    Err(_err) => break 'outer,
                };
                state_row.push(value);
            }
            state_array.push(state_row);
        }

        Ok(StateArray { state_array })
    }
}

//-------------------------------- Component Table ----------------------------------------------
#[derive(Debug)]
pub struct ComponentTable {
    component_array: Vec<u16>,
}

impl<'a> ReadBinary<'a> for ComponentTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let mut component_array: Vec<u16> = Vec::new();

        loop {
            let component = match ctxt.read_u16be() {
                Ok(val) => val,
                Err(_err) => break,
            };
            component_array.push(component);
        }

        Ok(ComponentTable { component_array })
    }
}

//------------------------------ Ligature List ----------------------------------------------------
#[derive(Debug)]
pub struct LigatureList {
    ligature_list: Vec<u16>,
}

impl<'a> ReadBinary<'a> for LigatureList {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let mut ligature_list: Vec<u16> = Vec::new();

        loop {
            let ligature = match ctxt.read_u16be() {
                Ok(val) => val,
                Err(_err) => break,
            };
            ligature_list.push(ligature);
        }

        Ok(LigatureList { ligature_list })
    }
}

//---------------------------- Class Lookup Table ------------------------------------------------
#[derive(Debug)]
pub struct LookupTableHeader {
    format: u16,
    bin_srch_header: Option<BinSrchHeader>,
}

impl<'a> ReadBinary<'a> for LookupTableHeader {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let format = ctxt.read_u16be()?;
        let bin_srch_header: Option<BinSrchHeader>;

        match format {
            0 => {
                bin_srch_header = None;
            }
            2 => {
                bin_srch_header = Some(ctxt.read::<BinSrchHeader>()?);
            }
            4 => {
                bin_srch_header = Some(ctxt.read::<BinSrchHeader>()?);
            }
            6 => {
                bin_srch_header = Some(ctxt.read::<BinSrchHeader>()?);
            }
            8 => {
                bin_srch_header = None;
            }
            10 => {
                bin_srch_header = None;
            }
            _ => return Err(ParseError::BadVersion),
        }

        Ok(LookupTableHeader {
            format,
            bin_srch_header,
        })
    }
}

#[derive(Debug)]
pub struct BinSrchHeader {
    unit_size: u16,
    n_units: u16,
    search_range: u16,
    entry_selector: u16,
    range_shift: u16,
}

impl<'a> ReadBinary<'a> for BinSrchHeader {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let unit_size = ctxt.read_u16be()?;
        let n_units = ctxt.read_u16be()?;
        let search_range = ctxt.read_u16be()?;
        let entry_selector = ctxt.read_u16be()?;
        let range_shift = ctxt.read_u16be()?;

        Ok(BinSrchHeader {
            unit_size,
            n_units,
            search_range,
            entry_selector,
            range_shift,
        })
    }
}

#[derive(Debug)]
pub enum LookupTable {
    Format0 {
        lookup_values: Vec<u16>, //Simple Array format 0
    },
    Format2 {
        lookup_segments: Vec<LookupSegmentFmt2>, //Segment Single format 2
    },
    Format4 {
        lookup_segments: Vec<LookupValuesFmt4>, //Segment Array format 4
    },
    Format6 {
        lookup_entries: Vec<LookupSingleFmt6>, //Single Table format 6
    },
    Format8 {
        first_glyph: u16,
        glyph_count: u16,
        lookup_values: Vec<u16>, //Trimmed Array format 8
    },
    Format10 {
        unit_size: u16,
        first_glyph: u16,
        glyph_count: u16,
        lookup_values: UnitSize, //item size can be 1, 2, 4 or 8,  determined by unit_size.
    },
}

#[derive(Debug)]
pub enum UnitSize {
    OneByte { lookup_values: Vec<u8> },
    TwoByte { lookup_values: Vec<u16> },
    FourByte { lookup_values: Vec<u32> },
    EightByte { lookup_values: Vec<u64> },
}

#[derive(Debug)]
pub struct LookupSegmentFmt2 {
    last_glyph: u16,
    first_glyph: u16,
    lookup_value: u16, //Assumption: lookup values are commonly u16. If not u16, pass an error.
}

impl<'a> ReadFrom<'a> for LookupSegmentFmt2 {
    type ReadType = (U16Be, U16Be, U16Be);

    fn from((last_glyph, first_glyph, lookup_value): (u16, u16, u16)) -> Self {
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

impl<'a> ReadFrom<'a> for LookupSegmentFmt4 {
    type ReadType = (U16Be, U16Be, U16Be);

    fn from((last_glyph, first_glyph, offset): (u16, u16, u16)) -> Self {
        LookupSegmentFmt4 {
            last_glyph,
            first_glyph,
            offset,
        }
    }
}

#[derive(Debug)]
pub struct LookupValuesFmt4 {
    last_glyph: u16,
    first_glyph: u16,
    lookup_values: Vec<u16>,
}

#[derive(Debug)]
pub struct LookupSingleFmt6 {
    glyph: u16,
    lookup_value: u16, //Assumption: lookup values are commonly u16. If not u16, pass an error.
}

impl<'a> ReadFrom<'a> for LookupSingleFmt6 {
    type ReadType = (U16Be, U16Be);

    fn from((glyph, lookup_value): (u16, u16)) -> Self {
        LookupSingleFmt6 {
            glyph,
            lookup_value,
        }
    }
}

#[derive(Debug)]
pub struct ClassLookupTable {
    lookup_header: LookupTableHeader,
    lookup_table: LookupTable,
}

impl<'a> ReadBinary<'a> for ClassLookupTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let class_table = ctxt.scope();

        let lookup_header = ctxt.read::<LookupTableHeader>()?;

        match lookup_header.format {
            0 => {
                match lookup_header.bin_srch_header {
                    None => {
                        let mut lookup_values = Vec::new();

                        loop {
                            let lookup_value = ctxt.read_u16be()?;
                            if lookup_value == 0xFFFF {
                                break; //guard value at the end
                            }
                            lookup_values.push(lookup_value);
                        }

                        let lookup_table = LookupTable::Format0 { lookup_values };

                        return Ok(ClassLookupTable {
                            lookup_header,
                            lookup_table,
                        });
                    }
                    _ => return Err(ParseError::BadValue),
                }
            }
            2 => match lookup_header.bin_srch_header {
                Some(ref b_sch_header) => {
                    if b_sch_header.unit_size != 6 {
                        return Err(ParseError::BadValue);
                    }

                    let mut lookup_segments = Vec::new();

                    for i in 0..b_sch_header.n_units {
                        let lookup_segment = ctxt.read::<LookupSegmentFmt2>()?;
                        lookup_segments.push(lookup_segment);
                    }

                    let guard_segment = ctxt.read::<LookupSegmentFmt2>()?;

                    let lookup_table = LookupTable::Format2 { lookup_segments };

                    return Ok(ClassLookupTable {
                        lookup_header,
                        lookup_table,
                    });
                }
                None => return Err(ParseError::BadValue),
            },
            4 => match lookup_header.bin_srch_header {
                Some(ref b_sch_header) => {
                    let mut lookup_segments: Vec<LookupValuesFmt4> = Vec::new();

                    for i in 0..b_sch_header.n_units {
                        let segment = ctxt.read::<LookupSegmentFmt4>()?;

                        let offset_to_lookup_values = segment.offset;
                        let mut read_ctxt = class_table
                            .offset(usize::from(offset_to_lookup_values))
                            .ctxt();

                        let mut lookup_values: Vec<u16> = Vec::new();
                        for i in 0..(segment.last_glyph - segment.first_glyph + 1) {
                            let lookup_value = match read_ctxt.read_u16be() {
                                Ok(val) => val,
                                Err(_err) => return Err(ParseError::BadEof),
                            };

                            lookup_values.push(lookup_value);
                        }

                        let lookup_segment = LookupValuesFmt4 {
                            last_glyph: segment.last_glyph,
                            first_glyph: segment.first_glyph,
                            lookup_values: lookup_values,
                        };

                        lookup_segments.push(lookup_segment);
                    }

                    let guard_segment = ctxt.read::<LookupSegmentFmt4>()?;

                    let lookup_table = LookupTable::Format4 { lookup_segments };

                    return Ok(ClassLookupTable {
                        lookup_header,
                        lookup_table,
                    });
                }
                None => return Err(ParseError::BadValue),
            },
            6 => match lookup_header.bin_srch_header {
                Some(ref b_sch_header) => {
                    if b_sch_header.unit_size != 4 {
                        return Err(ParseError::BadValue);
                    }

                    let mut lookup_entries = Vec::new();

                    for i in 0..b_sch_header.n_units {
                        let lookup_entry = ctxt.read::<LookupSingleFmt6>()?;
                        lookup_entries.push(lookup_entry);
                    }

                    let guard_entry = ctxt.read::<LookupSingleFmt6>()?;

                    let lookup_table = LookupTable::Format6 { lookup_entries };

                    return Ok(ClassLookupTable {
                        lookup_header,
                        lookup_table,
                    });
                }
                None => return Err(ParseError::BadValue),
            },
            8 => match lookup_header.bin_srch_header {
                None => {
                    let first_glyph = ctxt.read_u16be()?;
                    let glyph_count = ctxt.read_u16be()?;

                    let mut lookup_values = Vec::new();

                    for i in 0..glyph_count {
                        let lookup_value = ctxt.read_u16be()?;
                        lookup_values.push(lookup_value);
                    }
                    let guard_value = ctxt.read_u16be()?;

                    let lookup_table = LookupTable::Format8 {
                        first_glyph,
                        glyph_count,
                        lookup_values,
                    };

                    return Ok(ClassLookupTable {
                        lookup_header,
                        lookup_table,
                    });
                }
                _ => return Err(ParseError::BadValue),
            },
            10 => match lookup_header.bin_srch_header {
                None => {
                    let unit_size = ctxt.read_u16be()?;
                    let first_glyph = ctxt.read_u16be()?;
                    let glyph_count = ctxt.read_u16be()?;

                    match unit_size {
                        1 => {
                            let mut lookup_value_vec = Vec::new();

                            for i in 0..glyph_count {
                                let lookup_value = ctxt.read_u8()?;
                                lookup_value_vec.push(lookup_value);
                            }
                            let guard_value = ctxt.read_u8()?;

                            let lookup_values = UnitSize::OneByte {
                                lookup_values: lookup_value_vec,
                            };

                            let lookup_table = LookupTable::Format10 {
                                unit_size,
                                first_glyph,
                                glyph_count,
                                lookup_values,
                            };

                            return Ok(ClassLookupTable {
                                lookup_header,
                                lookup_table,
                            });
                        }
                        2 => {
                            let mut lookup_value_vec = Vec::new();

                            for i in 0..glyph_count {
                                let lookup_value = ctxt.read_u16be()?;
                                lookup_value_vec.push(lookup_value);
                            }
                            let guard_value = ctxt.read_u16be()?;

                            let lookup_values = UnitSize::TwoByte {
                                lookup_values: lookup_value_vec,
                            };

                            let lookup_table = LookupTable::Format10 {
                                unit_size,
                                first_glyph,
                                glyph_count,
                                lookup_values,
                            };

                            return Ok(ClassLookupTable {
                                lookup_header,
                                lookup_table,
                            });
                        }
                        4 => {
                            let mut lookup_value_vec = Vec::new();

                            for i in 0..glyph_count {
                                let lookup_value = ctxt.read_u32be()?;
                                lookup_value_vec.push(lookup_value);
                            }
                            let guard_value = ctxt.read_u32be()?;

                            let lookup_values = UnitSize::FourByte {
                                lookup_values: lookup_value_vec,
                            };

                            let lookup_table = LookupTable::Format10 {
                                unit_size,
                                first_glyph,
                                glyph_count,
                                lookup_values,
                            };

                            return Ok(ClassLookupTable {
                                lookup_header,
                                lookup_table,
                            });
                        }
                        8 => {
                            let mut lookup_value_vec = Vec::new();

                            for i in 0..glyph_count {
                                let lookup_value = ctxt.read_u64be()?;
                                lookup_value_vec.push(lookup_value);
                            }
                            let guard_value = ctxt.read_u64be()?;

                            let lookup_values = UnitSize::EightByte {
                                lookup_values: lookup_value_vec,
                            };

                            let lookup_table = LookupTable::Format10 {
                                unit_size,
                                first_glyph,
                                glyph_count,
                                lookup_values,
                            };

                            return Ok(ClassLookupTable {
                                lookup_header,
                                lookup_table,
                            });
                        }
                        _ => return Err(ParseError::BadValue),
                    }
                }
                _ => return Err(ParseError::BadValue),
            },
            _ => return Err(ParseError::BadVersion),
        }
    }
}
//----------------------------------------------------------------------------------------

//----------------------------------- Entry Table -----------------------------------------
#[derive(Debug)]
pub struct LigatureEntry {
    next_state_index: u16,
    entry_flags: u16,
    lig_action_index: u16,
}

impl<'a> ReadFrom<'a> for LigatureEntry {
    type ReadType = (U16Be, U16Be, U16Be);

    fn from((next_state_index, entry_flags, lig_action_index): (u16, u16, u16)) -> Self {
        LigatureEntry {
            next_state_index,
            entry_flags,
            lig_action_index,
        }
    }
}

#[derive(Debug)]
pub struct LigatureEntryTable {
    lig_entries: Vec<LigatureEntry>,
}

impl<'a> ReadBinary<'a> for LigatureEntryTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
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

//--------------------------------- Ligature Action Table ---------------------------------------------
#[derive(Debug)]
pub struct LigatureActionTable {
    actions: Vec<u32>,
}

impl<'a> ReadBinary<'a> for LigatureActionTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
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

//-------------------------------------------------------------------------------------------------------
//------------------------------  Looking Up Class ------------------------------------------------------

pub fn glyph_class(glyph: u16, class_table: &ClassLookupTable) -> u16 {
    if glyph == 0xFFFF {
        return 2;
    }

    match class_table.lookup_header.format {
        0 => {
            match &class_table.lookup_table {
                LookupTable::Format0 { lookup_values } => {
                    if (glyph as usize) < lookup_values.len() {
                        return lookup_values[glyph as usize];
                    } else {
                        return 1; //out of bounds
                    }
                }
                _ => return 1, //Only Format0 is valid here.
            }
        }
        2 => {
            match &class_table.lookup_table {
                LookupTable::Format2 { lookup_segments } => {
                    for lookup_segment in lookup_segments {
                        if (glyph >= lookup_segment.first_glyph)
                            && (glyph <= lookup_segment.last_glyph)
                        {
                            return lookup_segment.lookup_value;
                        }
                    }
                    return 1; //out of bounds
                }
                _ => return 1, //Only Format2 is valid here.
            }
        }
        4 => {
            match &class_table.lookup_table {
                LookupTable::Format4 { lookup_segments } => {
                    for lookup_segment in lookup_segments {
                        if (glyph >= lookup_segment.first_glyph)
                            && (glyph <= lookup_segment.last_glyph)
                        {
                            return lookup_segment.lookup_values
                                [usize::from(glyph - lookup_segment.first_glyph)];
                        }
                    }
                    return 1; //out of bounds
                }
                _ => return 1, //Only Format4 is valid here.
            }
        }
        6 => {
            match &class_table.lookup_table {
                LookupTable::Format6 { lookup_entries } => {
                    for lookup_entry in lookup_entries {
                        if glyph == lookup_entry.glyph {
                            return lookup_entry.lookup_value;
                        }
                    }
                    return 1; //out of bounds
                }
                _ => return 1, //Only Format6 is valid here..
            }
        }
        8 => {
            match &class_table.lookup_table {
                LookupTable::Format8 {
                    first_glyph,
                    glyph_count,
                    lookup_values,
                } => {
                    if (glyph >= *first_glyph) && (glyph <= (*first_glyph + *glyph_count - 1)) {
                        return lookup_values[usize::from(glyph - *first_glyph)];
                    } else {
                        return 1; //out of bounds
                    }
                }
                _ => return 1, //Only Format8 is valid here.
            }
        }
        10 => {
            match &class_table.lookup_table {
                LookupTable::Format10 {
                    unit_size,
                    first_glyph,
                    glyph_count,
                    lookup_values,
                } => {
                    match lookup_values {
                        UnitSize::OneByte {
                            lookup_values: one_byte_values,
                        } => {
                            if (glyph >= *first_glyph)
                                && (glyph <= (*first_glyph + *glyph_count - 1))
                            {
                                return one_byte_values[usize::from(glyph - *first_glyph)] as u16;
                            } else {
                                return 1; //out of bounds
                            }
                        }
                        UnitSize::TwoByte {
                            lookup_values: two_byte_values,
                        } => {
                            if (glyph >= *first_glyph)
                                && (glyph <= (*first_glyph + *glyph_count - 1))
                            {
                                return two_byte_values[usize::from(glyph - *first_glyph)];
                            } else {
                                return 1; //out of bounds
                            }
                        }
                        _ => return 1, //Note: ignore 4-byte and 8-byte lookup values for now
                    }
                }
                _ => return 1, //Only Format10 is valid here.
            }
        }
        _ => return 1, //No more formats except the ones above
    }
}

//------------------------------  Processing Ligature subtable ------------------------------------------

pub struct LigatureSubstitution<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    component_stack: Vec<RawGlyph<()>>,
}

impl<'a> LigatureSubstitution<'a> {
    pub fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> LigatureSubstitution<'a> {
        LigatureSubstitution {
            glyphs: glyphs,
            next_state: 0,
            component_stack: Vec::new(),
        }
    }

    pub fn process_glyphs(
        &mut self,
        ligature_subtable: &LigatureSubtable,
    ) -> Result<(), ParseError> {
        const SET_COMPONENT: u16 = 0x8000;
        const DONT_ADVANCE: u16 = 0x4000;
        const PERFORM_ACTION: u16 = 0x2000;
        const LAST: u32 = 0x80000000;
        const STORE: u32 = 0x40000000;

        let mut i: usize = 0;
        let mut start_pos: usize = 0;
        let mut end_pos: usize = 0;

        //loop through glyphs:
        'glyphs: loop {
            let glyph: RawGlyph<()>;
            if i < self.glyphs.len() {
                glyph = self.glyphs[i].clone(); //length of glyphs might have changed due to substitution. So need to check.
            }
            //assume the length of the glyphs array is shorter or unchanged.
            else {
                break 'glyphs;
            }

            let class = glyph_class(glyph.glyph_index, &ligature_subtable.class_table);

            'glyph: loop {
                let index_to_entry_table = ligature_subtable.state_array.state_array
                    [usize::from(self.next_state)][usize::from(class)];

                let entry =
                    &ligature_subtable.entry_table.lig_entries[usize::from(index_to_entry_table)];

                self.next_state = entry.next_state_index;

                let entry_flags: u16 = entry.entry_flags;

                if entry_flags & SET_COMPONENT != 0 {
                    //Set Component: push this glyph onto the component stack
                    self.component_stack.push(glyph.clone());
                    if self.component_stack.len() == 1 {
                        start_pos = i; //mark the position in the buffer for the first glyph in a ligature group
                    }
                }

                if entry_flags & PERFORM_ACTION != 0 {
                    //Perform Action: use the ligActionIndex to process a ligature group

                    end_pos = i; //mark the position in the buffer for the last glyph in a ligature group
                    let mut action_index: usize = usize::from(entry.lig_action_index);
                    let mut index_to_ligature: u16 = 0;
                    let mut ligature: RawGlyph<()> = RawGlyph {
                        unicodes: tiny_vec![[char; 1]],
                        glyph_index: 0x0000,
                        liga_component_pos: 0,
                        glyph_origin: GlyphOrigin::Direct,
                        small_caps: false,
                        multi_subst_dup: false,
                        is_vert_alt: false,
                        fake_bold: false,
                        fake_italic: false,
                        extra_data: (),
                        variation: None,
                    };

                    //loop through stack
                    'stack: loop {
                        let glyph_popped: u16;

                        match self.component_stack.pop() {
                            Some(val) => {
                                glyph_popped = val.glyph_index;

                                let mut unicodes = val.unicodes;
                                unicodes.append(&mut ligature.unicodes);
                                ligature.unicodes = unicodes;

                                ligature.variation = val.variation;
                            }
                            None => return Err(ParseError::MissingValue),
                        };

                        let action: u32 = ligature_subtable.action_table.actions[action_index];
                        action_index += 1;

                        let mut offset = action & 0x3FFFFFFF; //take 30 bits

                        if offset & 0x20000000 != 0 {
                            offset |= 0xC0000000; //sign-extend it to 32 bits
                        }
                        let offset = offset as i32; //convert to signed integer

                        let index_to_component_table = glyph_popped as i32 + offset;

                        if index_to_component_table < 0 {
                            return Err(ParseError::BadValue);
                        }

                        let index_to_component_table: usize =
                            match usize::try_from(index_to_component_table) {
                                Ok(index) => index,
                                Err(_err) => return Err(ParseError::BadValue),
                            };

                        index_to_ligature += &ligature_subtable.component_table.component_array
                            [index_to_component_table];

                        if (action & LAST != 0) || (action & STORE != 0) {
                            //storage when LAST or STORE is seen

                            let ligature_glyph = ligature_subtable.ligature_list.ligature_list
                                [usize::from(index_to_ligature)];

                            ligature.glyph_index = ligature_glyph;

                            //Subsitute glyphs[start_pos..(end_pos+1)] with ligature
                            self.glyphs.drain(start_pos..(end_pos + 1));

                            self.glyphs.insert(start_pos, ligature.clone());
                            i -= (end_pos - start_pos); //make adjustment to i after substitution

                            //Push ligature onto stack, only when the next state is non-zero
                            if self.next_state != 0 {
                                self.component_stack.push(ligature.clone());
                            }

                            //"ligature" has been inserted at start_pos in glyphs array.
                            //And the next glyph in glyphs array will be processed.
                        }

                        if action & LAST != 0 {
                            //this is the last action, so exit the loop 'stack
                            break 'stack;
                        }
                    }
                    //end of loop 'stack
                }
                //end of PERFORM_ACTION

                if entry_flags & DONT_ADVANCE == 0 {
                    break 'glyph; //exit the loop 'glyph unless entry_flags says DONT_ADVANCE
                } else {
                    self.component_stack.clear(); //if the entry_flags does say DONT_ADVANCE, then keep looping with the same glyph.
                                                  //clear the stack
                }
            }
            //end of loop 'glyph

            i += 1; //advance to the next glyph
        }
        //end of loop 'glyphs

        Ok(())
    }
}

//------------------------------------ Apply ligatures to an array of glyphs ----------------------------------------------
pub fn apply(morx_table: &MorxTable, glyphs: &mut Vec<RawGlyph<()>>) -> Result<(), ParseError> {
    let mut liga_subst: LigatureSubstitution<'_> = LigatureSubstitution::new(glyphs);

    for chain in morx_table.morx_chains.iter() {
        for subtable in chain.subtables.iter() {
            if subtable.subtable_header.coverage & 0xFF == 2 {
                if let SubtableType::Ligature { ligature_subtable } = &subtable.subtable_body {
                    liga_subst.next_state = 0;
                    liga_subst.component_stack.clear();
                    liga_subst.process_glyphs(ligature_subtable)?;
                }
            }
        }
    }

    Ok(())
}

//------------------------------------ Ligature Substitution Test ----------------------------------------------------------
pub fn morx_ligature_test<'a>(scope: ReadScope<'a>) -> Result<(), ParseError> {
    let morx_table = scope.read::<MorxTable>()?;

    //string: "ptgffigpfl" (for Zapfino.ttf)
    //let mut glyphs: Vec<u16> = vec![585, 604, 541, 536, 536, 552, 541, 585, 536, 565];

    //string: "ptpfgffigpfl" (for Zapfino.ttf)
    //let mut glyphs: Vec<u16> = vec![585, 604, 585, 536, 541, 536, 536, 552, 541, 585, 536, 565];

    //string: "Zapfino" (for Zapfino.ttf)
    //let mut glyphs:  Vec<u16> = vec![104, 504, 585, 536, 552, 573, 580];

    //string: "ptgffigpfl" (for Ayuthaya.ttf)
    //let mut glyphs: Vec<u16> = vec![197, 201, 188, 187, 187, 190, 188, 197, 187, 193];

    //string: ""\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F467}"" (for emoji.ttf)
    //let mut glyphs:  Vec<u16> = vec![1062, 43, 1164, 43, 1056, 43, 1056];

    //string: "U+1F1E6 U+1F1FA" (for emoji.ttf)
    //let mut glyphs:  Vec<u16> = vec![16, 36];

    let glyph1: RawGlyph<()> = RawGlyph {
        unicodes: tiny_vec![[char; 1]],
        glyph_index: 16,
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Direct,
        small_caps: false,
        multi_subst_dup: false,
        is_vert_alt: false,
        fake_bold: false,
        fake_italic: false,
        extra_data: (),
        variation: None,
    };

    let glyph2: RawGlyph<()> = RawGlyph {
        unicodes: tiny_vec![[char; 1]],
        glyph_index: 36,
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Direct,
        small_caps: false,
        multi_subst_dup: false,
        is_vert_alt: false,
        fake_bold: false,
        fake_italic: false,
        extra_data: (),
        variation: None,
    };

    let mut glyphs: Vec<RawGlyph<()>> = vec![glyph1, glyph2];

    let mut liga_subst: LigatureSubstitution<'_> = LigatureSubstitution::new(&mut glyphs);

    let mut liga_subtable_no: u16 = 0;
    for chain in morx_table.morx_chains.iter() {
        for subtable in chain.subtables.iter() {
            if subtable.subtable_header.coverage & 0xFF == 2 {
                liga_subtable_no += 1;
                println!("Ligature subtable No: {}", liga_subtable_no);

                if let SubtableType::Ligature { ligature_subtable } = &subtable.subtable_body {
                    liga_subst.next_state = 0;
                    liga_subst.component_stack.clear();
                    liga_subst.process_glyphs(ligature_subtable)?;
                }
            }
        }
    }

    // println!("The glyphs array after ligature substitutions: {:?}", glyphs);

    Ok(())
}

//----------------------------------------------------------------------------------------------
