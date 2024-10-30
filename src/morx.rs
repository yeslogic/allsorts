use std::convert::TryFrom;

use crate::binary::read::{ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope};
use crate::binary::{U16Be, U32Be, U8};
use crate::error::ParseError;
use crate::gsub::{FeatureMask, Features, GlyphOrigin, RawGlyph};
use crate::size;
use crate::tinyvec::tiny_vec;

#[derive(Debug)]
pub struct MorxHeader {
    version: u16,
    n_chains: u32,
}

impl<'a> ReadFrom<'a> for MorxHeader {
    type ReadType = (U16Be, U16Be, U32Be);

    fn from((version, _unused, n_chains): (u16, u16, u32)) -> Self {
        MorxHeader { version, n_chains }
    }
}

#[derive(Debug)]
pub struct MorxTable<'a> {
    morx_header: MorxHeader,
    morx_chains: Vec<Chain<'a>>,
}

impl<'a> ReadBinary<'a> for MorxTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let morx_header = ctxt.read::<MorxHeader>()?;
        let mut chain_vec = Vec::new();

        for _i in 0..morx_header.n_chains {
            //read the chain header to get the chain length
            let scope_hdr = ctxt.scope();
            let chain_header = scope_hdr.read::<ChainHeader>()?;
            let chain_length: usize = usize::try_from(chain_header.chain_length)?;

            //get a scope of length "chain_length" to read the chain and
            //advance to the correct position in the buffer for reading
            //the next chain, regardless whether the "Subtable Glyph Coverage table"
            //is present at the end of the chain.
            let chain_scope = ctxt.read_scope(chain_length)?;
            let chain = chain_scope.read::<Chain<'a>>()?;
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
#[derive(Debug, Clone, Copy)]
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
pub struct Chain<'a> {
    chain_header: ChainHeader,
    feature_array: ReadArray<'a, Feature>,
    subtables: Vec<Subtable<'a>>,
}

impl<'a> ReadBinary<'a> for Chain<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let chain_header = ctxt.read::<ChainHeader>()?;

        let features =
            ctxt.read_array::<Feature>(usize::try_from(chain_header.n_feature_entries)?)?;

        let mut subtable_vec = Vec::new();
        for _i in 0..chain_header.n_subtables {
            let subtable = ctxt.read::<Subtable<'a>>()?;
            subtable_vec.push(subtable);
        }

        Ok(Chain {
            chain_header: chain_header,
            feature_array: features,
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
pub struct Subtable<'a> {
    subtable_header: SubtableHeader,
    subtable_body: SubtableType<'a>,
}

impl<'a> ReadBinary<'a> for Subtable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let subtable_header = ctxt.read::<SubtableHeader>()?;

        let subtable_body: SubtableType<'a>;
        let subtable_body_length = usize::try_from(subtable_header.length - 12)?;
        //12 is the length of the subtable header that needs to be skipped.

        //Get a shorter scope from the ReadCtxt to read the subtable
        let subtable_scope = ctxt.read_scope(subtable_body_length)?;

        match subtable_header.coverage & 0xFF {
            1 => {
                let contextual_subtable = subtable_scope.read::<ContextualSubtable<'a>>()?;
                subtable_body = SubtableType::Contextual {
                    contextual_subtable,
                };
            }
            2 => {
                let ligature_subtable = subtable_scope.read::<LigatureSubtable<'a>>()?;
                subtable_body = SubtableType::Ligature { ligature_subtable };
            }
            4 => {
                let noncontextual_subtable = subtable_scope.read::<NonContextualSubtable<'a>>()?;
                subtable_body = SubtableType::NonContextual {
                    noncontextual_subtable,
                };
            }
            0 | 5 => {
                //read the subtable to a slice &'a[u8] if it is another type other than ligature, contextual or noncontextual
                let subtable_data = subtable_scope.data();

                subtable_body = SubtableType::Other {
                    other_subtable: subtable_data,
                };
            }
            _ => {
                return Err(ParseError::BadValue);
            }
        }

        Ok(Subtable {
            subtable_header,
            subtable_body,
        })
    }
}

//-------------------------------------------------------------------------------------------------
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

//Contextual Glyph Substitution Subtable
#[derive(Debug)]
pub struct ContextualSubtable<'a> {
    stx_header: STXheader,
    class_table: ClassLookupTable<'a>,
    state_array: StateArray<'a>,
    entry_table: ContextualEntryTable,
    substitution_subtables: Vec<ClassLookupTable<'a>>,
}

impl<'a> ReadBinary<'a> for ContextualSubtable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let subtable = ctxt.scope();

        let stx_header = ctxt.read::<STXheader>()?;
        let substitution_subtables_offset = ctxt.read_u32be()?;

        //read the class lookup table
        let class_table = subtable
            .offset(usize::try_from(stx_header.class_table_offset)?)
            .read::<ClassLookupTable<'a>>()?;

        //read the state array:
        let state_array = subtable
            .offset(usize::try_from(stx_header.state_array_offset)?)
            .read_dep::<StateArray<'a>>(NClasses(stx_header.n_classes))?;

        //read the contextual entry table
        let entry_table = subtable
            .offset(usize::try_from(stx_header.entry_table_offset)?)
            .read::<ContextualEntryTable>()?;

        let first_offset_to_subst_tables = subtable
            .offset(usize::try_from(substitution_subtables_offset)?)
            .ctxt()
            .read_u32be()?;

        let offset_array_len = first_offset_to_subst_tables / 4;
        let mut subst_tables_ctxt = subtable
            .offset(usize::try_from(substitution_subtables_offset)?)
            .ctxt();
        let mut offsets_to_subst_tables: Vec<u32> = Vec::new();

        for _i in 0..offset_array_len {
            let value = match subst_tables_ctxt.read_u32be() {
                Ok(val) => val,
                Err(_err) => break,
            };
            offsets_to_subst_tables.push(value);
        }

        let mut substitution_subtables: Vec<ClassLookupTable<'a>> = Vec::new();

        for &offset in offsets_to_subst_tables.iter() {
            let subst_subtable = match subtable
                .offset(usize::try_from(substitution_subtables_offset)?)
                .offset(usize::try_from(offset)?)
                .read::<ClassLookupTable<'a>>()
            {
                Ok(val) => val,
                Err(_err) => break,
            };
            substitution_subtables.push(subst_subtable);
        }

        Ok(ContextualSubtable {
            stx_header,
            class_table,
            state_array,
            entry_table,
            substitution_subtables,
        })
    }
}

//Noncontextual Glyph Substitution Subtable
#[derive(Debug)]
pub struct NonContextualSubtable<'a> {
    lookup_table: ClassLookupTable<'a>,
}

impl<'a> ReadBinary<'a> for NonContextualSubtable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let lookup_table = ctxt.read::<ClassLookupTable<'a>>()?;

        Ok(NonContextualSubtable { lookup_table })
    }
}

//Ligature subtable
#[derive(Debug)]
pub struct LigatureSubtable<'a> {
    stx_header: STXheader,
    class_table: ClassLookupTable<'a>,
    state_array: StateArray<'a>,
    entry_table: LigatureEntryTable,
    action_table: LigatureActionTable,
    component_table: ComponentTable<'a>,
    ligature_list: LigatureList<'a>,
}

impl<'a> ReadBinary<'a> for LigatureSubtable<'a> {
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
            .read::<ClassLookupTable<'a>>()?;

        //read the state array:
        let state_array = subtable
            .offset(usize::try_from(stx_header.state_array_offset)?)
            .read_dep::<StateArray<'a>>(NClasses(stx_header.n_classes))?;

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
            .read::<ComponentTable<'a>>()?;

        //read the ligature list
        let ligature_list = subtable
            .offset(usize::try_from(ligature_list_offset)?)
            .read::<LigatureList<'a>>()?;

        Ok(LigatureSubtable {
            stx_header,
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
pub struct StateArray<'a> {
    state_array: Vec<ReadArray<'a, U16Be>>,
}

impl<'a> ReadBinaryDep<'a> for StateArray<'a> {
    type Args = NClasses;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: NClasses) -> Result<Self, ParseError> {
        let n_classes = args;

        let mut state_array: Vec<ReadArray<'a, U16Be>> = Vec::new();
        let state_row_len = usize::try_from(n_classes.0)?;

        loop {
            let state_row = match ctxt.read_array::<U16Be>(state_row_len) {
                Ok(array) => array,
                Err(ParseError::BadEof) => break,
                Err(err) => return Err(err),
            };

            state_array.push(state_row);
        }

        Ok(StateArray { state_array })
    }
}

//-------------------------------- Component Table ----------------------------------------------
#[derive(Debug)]
pub struct ComponentTable<'a> {
    component_array: ReadArray<'a, U16Be>,
}

impl<'a> ReadBinary<'a> for ComponentTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let len_remaining = ctxt.scope().data().len();
        let component_array = ctxt.read_array::<U16Be>(len_remaining / size::U16)?;

        Ok(ComponentTable { component_array })
    }
}

//------------------------------ Ligature List ----------------------------------------------------
#[derive(Debug)]
pub struct LigatureList<'a> {
    ligature_list: ReadArray<'a, U16Be>,
}

impl<'a> ReadBinary<'a> for LigatureList<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let len_remaining = ctxt.scope().data().len();
        let ligature_list = ctxt.read_array::<U16Be>(len_remaining / size::U16)?;

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

        bin_srch_header = match format {
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
pub enum LookupTable<'a> {
    Format0 {
        lookup_values: Vec<u16>, //Simple Array format 0
    },
    Format2 {
        lookup_segments: ReadArray<'a, LookupSegmentFmt2>, //Segment Single format 2
    },
    Format4 {
        lookup_segments: Vec<LookupValuesFmt4<'a>>, //Segment Array format 4
    },
    Format6 {
        lookup_entries: ReadArray<'a, LookupSingleFmt6>, //Single Table format 6
    },
    Format8 {
        first_glyph: u16,
        glyph_count: u16,
        lookup_values: ReadArray<'a, U16Be>, //Trimmed Array format 8
    },
    Format10 {
        first_glyph: u16,
        glyph_count: u16,
        lookup_values: UnitSize<'a>, //item size can be 1, 2, 4 or 8,  determined by unit_size.
    },
}

#[derive(Debug)]
pub enum UnitSize<'a> {
    OneByte { lookup_values: ReadArray<'a, U8> },
    TwoByte { lookup_values: ReadArray<'a, U16Be> },
    FourByte { lookup_values: ReadArray<'a, U32Be> },
    EightByte { lookup_values: Vec<u64> },
}

#[derive(Debug, Copy, Clone)]
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
pub struct LookupValuesFmt4<'a> {
    last_glyph: u16,
    first_glyph: u16,
    lookup_values: ReadArray<'a, U16Be>,
}

#[derive(Debug, Copy, Clone)]
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
pub struct ClassLookupTable<'a> {
    lookup_header: LookupTableHeader,
    lookup_table: LookupTable<'a>,
}

impl<'a> ReadBinary<'a> for ClassLookupTable<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let class_table = ctxt.scope();

        let lookup_header = ctxt.read::<LookupTableHeader>()?;
        match lookup_header.format {
            0 => match lookup_header.bin_srch_header {
                None => {
                    let mut lookup_values = Vec::new();

                    loop {
                        let lookup_value = match ctxt.read_u16be() {
                            Ok(val) => val,
                            Err(_err) => break,
                        };

                        lookup_values.push(lookup_value);
                    }

                    let lookup_table = LookupTable::Format0 { lookup_values };

                    return Ok(ClassLookupTable {
                        lookup_header,
                        lookup_table,
                    });
                }
                _ => return Err(ParseError::BadValue),
            },
            2 => match lookup_header.bin_srch_header {
                Some(ref b_sch_header) => {
                    if b_sch_header.unit_size != 6 {
                        return Err(ParseError::BadValue);
                    }

                    let lookup_segments =
                        ctxt.read_array::<LookupSegmentFmt2>(usize::from(b_sch_header.n_units))?;
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
                    let mut lookup_segments: Vec<LookupValuesFmt4<'_>> = Vec::new();

                    for _i in 0..b_sch_header.n_units {
                        let segment = match ctxt.read::<LookupSegmentFmt4>() {
                            Ok(val) => val,
                            Err(_err) => break,
                        };

                        let offset_to_lookup_values = segment.offset;
                        if (segment.first_glyph == 0xFFFF) && (segment.last_glyph == 0xFFFF) {
                            break;
                        }

                        let mut read_ctxt = class_table
                            .offset(usize::from(offset_to_lookup_values))
                            .ctxt();

                        let lookup_values = read_ctxt.read_array::<U16Be>(usize::from(
                            segment.last_glyph - segment.first_glyph + 1,
                        ))?;

                        let lookup_segment = LookupValuesFmt4 {
                            last_glyph: segment.last_glyph,
                            first_glyph: segment.first_glyph,
                            lookup_values: lookup_values,
                        };

                        lookup_segments.push(lookup_segment);
                    }

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

                    let lookup_entries =
                        ctxt.read_array::<LookupSingleFmt6>(usize::from(b_sch_header.n_units))?;

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

                    let lookup_values = ctxt.read_array::<U16Be>(usize::from(glyph_count))?;

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
                            let lookup_value_array =
                                ctxt.read_array::<U8>(usize::from(glyph_count))?;
                            let lookup_values = UnitSize::OneByte {
                                lookup_values: lookup_value_array,
                            };

                            let lookup_table = LookupTable::Format10 {
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
                            let lookup_value_array =
                                ctxt.read_array::<U16Be>(usize::from(glyph_count))?;
                            let lookup_values = UnitSize::TwoByte {
                                lookup_values: lookup_value_array,
                            };

                            let lookup_table = LookupTable::Format10 {
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
                            let lookup_value_array =
                                ctxt.read_array::<U32Be>(usize::from(glyph_count))?;
                            let lookup_values = UnitSize::FourByte {
                                lookup_values: lookup_value_array,
                            };
                            let lookup_table = LookupTable::Format10 {
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

                            for _i in 0..glyph_count {
                                let lookup_value = ctxt.read_u64be()?;
                                lookup_value_vec.push(lookup_value);
                            }

                            let lookup_values = UnitSize::EightByte {
                                lookup_values: lookup_value_vec,
                            };

                            let lookup_table = LookupTable::Format10 {
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

//----------------------------------- Ligature Entry Table -----------------------------------------
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

//----------------------------------- Contextual Entry Table ------------------------------------------
#[derive(Debug)]
pub struct ContextualEntry {
    next_state: u16,
    flags: u16,
    mark_index: u16,
    current_index: u16,
}

impl<'a> ReadFrom<'a> for ContextualEntry {
    type ReadType = (U16Be, U16Be, U16Be, U16Be);

    fn from((next_state, flags, mark_index, current_index): (u16, u16, u16, u16)) -> Self {
        ContextualEntry {
            next_state,
            flags,
            mark_index,
            current_index,
        }
    }
}

#[derive(Debug)]
pub struct ContextualEntryTable {
    contextual_entries: Vec<ContextualEntry>,
}

impl<'a> ReadBinary<'a> for ContextualEntryTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
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

//------------------------------ Lookup function --------------------------------------------------------
pub fn lookup<'a>(glyph: u16, lookup_table: &ClassLookupTable<'a>) -> Option<u16> {
    if glyph == 0xFFFF {
        return Some(0xFFFF);
    }

    match lookup_table.lookup_header.format {
        0 => {
            match &lookup_table.lookup_table {
                LookupTable::Format0 { lookup_values } => {
                    return lookup_values.get(usize::from(glyph)).copied();
                }
                _ => return None, //Only Format0 is valid here.
            }
        }
        2 => {
            match &lookup_table.lookup_table {
                LookupTable::Format2 { lookup_segments } => {
                    for lookup_segment in lookup_segments {
                        if (glyph >= lookup_segment.first_glyph)
                            && (glyph <= lookup_segment.last_glyph)
                        {
                            return Some(lookup_segment.lookup_value);
                        }
                    }
                    return None; //out of bounds
                }
                _ => return None, //Only Format2 is valid here.
            }
        }
        4 => {
            match &lookup_table.lookup_table {
                LookupTable::Format4 { lookup_segments } => {
                    for lookup_segment in lookup_segments {
                        if (glyph >= lookup_segment.first_glyph)
                            && (glyph <= lookup_segment.last_glyph)
                        {
                            if ((glyph - lookup_segment.first_glyph) as usize)
                                < lookup_segment.lookup_values.len()
                            {
                                match lookup_segment
                                    .lookup_values
                                    .read_item(usize::from(glyph - lookup_segment.first_glyph))
                                {
                                    Ok(val) => return Some(val as u16),
                                    Err(_err) => return None,
                                }
                            }
                        }
                    }
                    return None; //out of bounds
                }
                _ => return None, //Only Format4 is valid here.
            }
        }
        6 => {
            match &lookup_table.lookup_table {
                LookupTable::Format6 { lookup_entries } => {
                    for lookup_entry in lookup_entries {
                        if glyph == lookup_entry.glyph {
                            return Some(lookup_entry.lookup_value);
                        }
                    }
                    return None; //out of bounds
                }
                _ => return None, //Only Format6 is valid here..
            }
        }
        8 => {
            match &lookup_table.lookup_table {
                LookupTable::Format8 {
                    first_glyph,
                    glyph_count,
                    lookup_values,
                } => {
                    if (glyph >= *first_glyph) && (glyph <= (*first_glyph + *glyph_count - 1)) {
                        match lookup_values.read_item(usize::from(glyph - *first_glyph)) {
                            Ok(val) => return Some(val as u16),
                            Err(_err) => return None,
                        }
                    } else {
                        return None; //out of bounds
                    }
                }
                _ => return None, //Only Format8 is valid here.
            }
        }
        10 => {
            match &lookup_table.lookup_table {
                LookupTable::Format10 {
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
                                match one_byte_values.read_item(usize::from(glyph - *first_glyph)) {
                                    Ok(val) => return Some(val as u16),
                                    Err(_err) => return None,
                                }
                            } else {
                                return None; //out of bounds
                            }
                        }
                        UnitSize::TwoByte {
                            lookup_values: two_byte_values,
                        } => {
                            if (glyph >= *first_glyph)
                                && (glyph <= (*first_glyph + *glyph_count - 1))
                            {
                                match two_byte_values.read_item(usize::from(glyph - *first_glyph)) {
                                    Ok(val) => return Some(val as u16),
                                    Err(_err) => return None,
                                }
                            } else {
                                return None; //out of bounds
                            }
                        }
                        _ => return None, //Note: ignore 4-byte and 8-byte lookup values for now
                    }
                }
                _ => return None, //Only Format10 is valid here.
            }
        }
        _ => return None, //No more formats except the ones above
    }
}

//------------------------------  Looking Up Class ------------------------------------------------------

pub fn glyph_class<'a>(glyph: u16, class_table: &ClassLookupTable<'a>) -> u16 {
    match lookup(glyph, class_table) {
        None => {
            return 1;
        }
        Some(val) => {
            if val == 0xFFFF {
                return 2;
            } else {
                return val;
            }
        }
    }
}

//--------------------------------- Contextual Substitution ---------------------------------------------
pub struct ContextualSubstitution<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    mark: Option<(usize, u16)>, //record marked glyph and its position: (position, mark_glyph)
}

impl<'a> ContextualSubstitution<'a> {
    pub fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> ContextualSubstitution<'a> {
        ContextualSubstitution {
            glyphs: glyphs,
            next_state: 0,
            mark: None,
        }
    }

    pub fn process_glyphs<'b>(
        &mut self,
        contextual_subtable: &ContextualSubtable<'b>,
    ) -> Result<(), ParseError> {
        const SET_MARK: u16 = 0x8000;
        const DONT_ADVANCE: u16 = 0x4000;
        let mut old_glyph: u16;
        let mut new_glyph: u16;

        //loop through glyphs:
        for i in 0..self.glyphs.len() {
            let current_glyph: u16 = self.glyphs[i].glyph_index;
            old_glyph = self.glyphs[i].glyph_index;
            new_glyph = self.glyphs[i].glyph_index;

            let mut class = glyph_class(current_glyph, &contextual_subtable.class_table);

            'glyph: loop {
                let index_to_entry_table;
                let entry;

                if let Some(state_row) = contextual_subtable
                    .state_array
                    .state_array
                    .get(usize::from(self.next_state))
                {
                    index_to_entry_table = state_row.read_item(usize::from(class))?;
                } else {
                    return Err(ParseError::BadIndex);
                }

                if let Some(contxt_entry) = &contextual_subtable
                    .entry_table
                    .contextual_entries
                    .get(usize::from(index_to_entry_table))
                {
                    entry = contxt_entry.clone();
                } else {
                    return Err(ParseError::BadIndex);
                }

                self.next_state = entry.next_state;

                //if there is a marked glyph on record and the entry is providing a mark_index to the
                //substitution table for it, then make the substitution for the marked glyph.
                if entry.mark_index != 0xFFFF {
                    if let Some((mark_pos, mark_glyph)) = self.mark {
                        if let Some(mark_glyph_subst) = lookup(
                            mark_glyph,
                            &contextual_subtable.substitution_subtables
                                [usize::from(entry.mark_index)],
                        ) {
                            self.glyphs[mark_pos].glyph_index = mark_glyph_subst;
                            self.glyphs[mark_pos].glyph_origin = GlyphOrigin::Direct;
                        }
                    }
                }

                //if the entry is providing a current_index to the substitution table for the current glyph,
                //then make the substitution for the current glyph
                if entry.current_index != 0xFFFF {
                    if let Some(current_glyph_subst) = lookup(
                        current_glyph,
                        &contextual_subtable.substitution_subtables
                            [usize::from(entry.current_index)],
                    ) {
                        self.glyphs[i].glyph_index = current_glyph_subst;
                        self.glyphs[i].glyph_origin = GlyphOrigin::Direct;
                        new_glyph = current_glyph_subst;
                    }
                }

                //if entry.flags says SET_MARK, then make the current glyph the marked glyph.
                if entry.flags & SET_MARK != 0 {
                    self.mark = Some((i, self.glyphs[i].glyph_index));
                }

                //exit the loop 'glyph unless entry.flags says DONT_ADVANCE.
                if entry.flags & DONT_ADVANCE == 0 {
                    break 'glyph;
                }

                //if the entry.flags says DONT_ADVANCE, then keep looping in loop 'glyph.
                //but the class may have to be re-calculated if the current glyph has been substituted.
                if new_glyph != old_glyph {
                    class = glyph_class(new_glyph, &contextual_subtable.class_table);
                    old_glyph = new_glyph;
                }
            }
            //end of loop 'glyph
        }

        Ok(())
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

    pub fn process_glyphs<'b>(
        &mut self,
        ligature_subtable: &LigatureSubtable<'b>,
    ) -> Result<(), ParseError> {
        const SET_COMPONENT: u16 = 0x8000;
        const DONT_ADVANCE: u16 = 0x4000;
        const PERFORM_ACTION: u16 = 0x2000;
        const LAST: u32 = 0x80000000;
        const STORE: u32 = 0x40000000;

        let mut i: usize = 0;
        let mut start_pos: usize = 0;
        let mut end_pos: usize;

        //loop through glyphs:
        while let Some(glyph) = self.glyphs.get(i) {
            let glyph = glyph.clone();
            let class = glyph_class(glyph.glyph_index, &ligature_subtable.class_table);

            'glyph: loop {
                let index_to_entry_table;
                let entry;

                if let Some(state_row) = ligature_subtable
                    .state_array
                    .state_array
                    .get(usize::from(self.next_state))
                {
                    index_to_entry_table = state_row.read_item(usize::from(class))?;
                } else {
                    return Err(ParseError::BadIndex);
                }

                if let Some(lig_entry) = &ligature_subtable
                    .entry_table
                    .lig_entries
                    .get(usize::from(index_to_entry_table))
                {
                    entry = lig_entry.clone();
                } else {
                    return Err(ParseError::BadIndex);
                }

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

                        let index_to_components = glyph_popped as i32 + offset;

                        if index_to_components < 0 {
                            return Err(ParseError::BadValue);
                        }

                        let index_to_component_table: usize =
                            match usize::try_from(index_to_components) {
                                Ok(index) => index,
                                Err(_err) => return Err(ParseError::BadValue),
                            };

                        index_to_ligature += &ligature_subtable
                            .component_table
                            .component_array
                            .read_item(index_to_component_table)?;

                        if (action & LAST != 0) || (action & STORE != 0) {
                            //storage when LAST or STORE is seen

                            //let ligature_glyph = ligature_subtable.ligature_list.ligature_list
                            //[usize::from(index_to_ligature)];
                            let ligature_glyph = ligature_subtable
                                .ligature_list
                                .ligature_list
                                .read_item(usize::from(index_to_ligature))?;

                            ligature.glyph_index = ligature_glyph;

                            //Subsitute glyphs[start_pos..(end_pos+1)] with ligature
                            self.glyphs.drain(start_pos..(end_pos + 1));

                            self.glyphs.insert(start_pos, ligature.clone());
                            i -= end_pos - start_pos; //make adjustment to i after substitution

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

//------------------------------------ NonContextual Lookup ---------------------------------------------------------------
//This function looks up and returns the noncontexutal substitute of glyph.
//It returns 0xFFFF for a glyph index out of bounds of the lookup value array indices.
pub fn noncontextual_lookup<'a>(glyph: u16, lookup_table: &ClassLookupTable<'a>) -> u16 {
    match lookup(glyph, lookup_table) {
        None => {
            return 0xFFFF;
        }
        Some(val) => {
            return val;
        }
    }
}

//------------------------------------ NonContextual Substiution ----------------------------------------------------------
pub fn noncontextual_substitution<'a>(
    glyphs: &mut Vec<RawGlyph<()>>,
    noncontextual_subtable: &NonContextualSubtable<'a>,
) -> Result<(), ParseError> {
    let mut glyph: u16;
    let mut subst: u16;
    for i in 0..glyphs.len() {
        glyph = glyphs[i].glyph_index;

        subst = noncontextual_lookup(glyph, &noncontextual_subtable.lookup_table);

        if (subst != 0xFFFF) && (subst != glyph) {
            glyphs[i].glyph_index = subst;
            glyphs[i].glyph_origin = GlyphOrigin::Direct;
        }
    }
    Ok(())
}

//------------------------------------ Apply ligatures to an array of glyphs ----------------------------------------------
pub fn apply<'a>(
    morx_table: &MorxTable<'a>,
    glyphs: &mut Vec<RawGlyph<()>>,
    features: &Features,
) -> Result<(), ParseError> {
    for chain in morx_table.morx_chains.iter() {
        let subfeatureflags: u32 = subfeatureflags(chain, features)?;
        for subtable in chain.subtables.iter() {
            if subfeatureflags & subtable.subtable_header.sub_feature_flags != 0 {
                match subtable.subtable_header.coverage & 0xFF {
                    1 => {
                        let mut contextual_subst: ContextualSubstitution<'_> =
                            ContextualSubstitution::new(glyphs);

                        if let SubtableType::Contextual {
                            contextual_subtable,
                        } = &subtable.subtable_body
                        {
                            contextual_subst.next_state = 0;
                            contextual_subst.process_glyphs(contextual_subtable)?;
                        } else {
                            return Err(ParseError::BadValue);
                        }
                    }
                    2 => {
                        let mut liga_subst: LigatureSubstitution<'_> =
                            LigatureSubstitution::new(glyphs);

                        if let SubtableType::Ligature { ligature_subtable } =
                            &subtable.subtable_body
                        {
                            liga_subst.next_state = 0;
                            liga_subst.component_stack.clear();
                            liga_subst.process_glyphs(ligature_subtable)?;
                        } else {
                            return Err(ParseError::BadValue);
                        }
                    }
                    4 => {
                        if let SubtableType::NonContextual {
                            noncontextual_subtable,
                        } = &subtable.subtable_body
                        {
                            noncontextual_substitution(glyphs, noncontextual_subtable)?;
                        } else {
                            return Err(ParseError::BadValue);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    Ok(())
}

//------------------------------------ Process requested feature list -----------------------------------------------------
pub fn subfeatureflags<'a>(chain: &Chain<'a>, features: &Features) -> Result<u32, ParseError> {
    //Feature type:
    const LIGATURE_TYPE: u16 = 1;
    //Feature selectors:
    const COMMON_LIGATURES_ON: u16 = 2;
    const COMMON_LIGATURES_OFF: u16 = 3;
    const CONTEXTUAL_LIGATURES_ON: u16 = 18;
    const CONTEXTUAL_LIGATURES_OFF: u16 = 19;
    const HISTORICAL_LIGATURES_ON: u16 = 20;
    const HISTORICAL_LIGATURES_OFF: u16 = 21;
    //--------------------------------------
    //Feature type:
    const NUMBER_CASE_TYPE: u16 = 21;
    //Feature selectors:
    const OLD_STYLE_NUMBERS: u16 = 0;
    const LINING_NUMBERS: u16 = 1;
    //---------------------------------------
    //Feature type:
    const NUMBER_SPACING_TYPE: u16 = 6;
    //Feature selectors:
    const TABULAR_NUMBERS: u16 = 0;
    const PROPORTIONAL_NUMBERS: u16 = 1;
    //---------------------------------------
    //Feature type:
    const FRACTION_TYPE: u16 = 11;
    //Feature selectors:
    const NO_FRACTIONS: u16 = 0;
    const FRACTIONS_STACKED: u16 = 1;
    const FRACTIONS_DIAGONAL: u16 = 2;
    //---------------------------------------
    //Feature type:
    const VERTICAL_POSITION_TYPE: u16 = 10;
    //Feature selectors:
    const ORDINALS: u16 = 3;
    //---------------------------------------
    //Feature type:
    const TYPOGRAPHIC_EXTRAS_TYPE: u16 = 14;
    //Feature selectors:
    const SLASHED_ZERO_ON: u16 = 4;
    const SLASHED_ZERO_OFF: u16 = 5;
    //---------------------------------------
    //Feature type:
    const LOWERCASE_TYPE: u16 = 37;
    //Feature selectors:
    const LOWERCASE_SMALL_CAPS: u16 = 1;
    //---------------------------------------
    //Feature type:
    const UPPERCASE_TYPE: u16 = 38;
    //Feature selectors:
    const UPPERCASE_SMALL_CAPS: u16 = 1;
    //---------------------------------------

    let mut subfeature_flags = chain.chain_header.default_flags;

    for entry in chain.feature_array.iter() {
        match features {
            Features::Custom(_features_list) => {
                return Ok(subfeature_flags);
            }
            Features::Mask(feature_mask) => {
                let apply = match (entry.feature_type, entry.feature_setting) {
                    (NUMBER_CASE_TYPE, LINING_NUMBERS) => feature_mask.contains(FeatureMask::LNUM),
                    (NUMBER_CASE_TYPE, OLD_STYLE_NUMBERS) => {
                        feature_mask.contains(FeatureMask::ONUM)
                    }
                    (NUMBER_SPACING_TYPE, PROPORTIONAL_NUMBERS) => {
                        feature_mask.contains(FeatureMask::PNUM)
                    }
                    (NUMBER_SPACING_TYPE, TABULAR_NUMBERS) => {
                        feature_mask.contains(FeatureMask::TNUM)
                    }
                    (FRACTION_TYPE, FRACTIONS_DIAGONAL) => feature_mask.contains(FeatureMask::FRAC),
                    (FRACTION_TYPE, FRACTIONS_STACKED) => feature_mask.contains(FeatureMask::AFRC),
                    (FRACTION_TYPE, NO_FRACTIONS) => {
                        !feature_mask.contains(FeatureMask::FRAC)
                            && !feature_mask.contains(FeatureMask::AFRC)
                    }
                    (VERTICAL_POSITION_TYPE, ORDINALS) => feature_mask.contains(FeatureMask::ORDN),
                    (TYPOGRAPHIC_EXTRAS_TYPE, SLASHED_ZERO_ON) => {
                        feature_mask.contains(FeatureMask::ZERO)
                    }
                    (TYPOGRAPHIC_EXTRAS_TYPE, SLASHED_ZERO_OFF) => {
                        !feature_mask.contains(FeatureMask::ZERO)
                    }
                    (LOWERCASE_TYPE, LOWERCASE_SMALL_CAPS) => {
                        feature_mask.contains(FeatureMask::SMCP)
                            || feature_mask.contains(FeatureMask::C2SC)
                    }
                    (UPPERCASE_TYPE, UPPERCASE_SMALL_CAPS) => {
                        feature_mask.contains(FeatureMask::C2SC)
                    }
                    (LIGATURE_TYPE, COMMON_LIGATURES_ON) => {
                        feature_mask.contains(FeatureMask::LIGA)
                    }
                    (LIGATURE_TYPE, COMMON_LIGATURES_OFF) => {
                        !feature_mask.contains(FeatureMask::LIGA)
                    }
                    (LIGATURE_TYPE, HISTORICAL_LIGATURES_ON) => {
                        feature_mask.contains(FeatureMask::HLIG)
                    }
                    (LIGATURE_TYPE, HISTORICAL_LIGATURES_OFF) => {
                        !feature_mask.contains(FeatureMask::HLIG)
                    }
                    (LIGATURE_TYPE, CONTEXTUAL_LIGATURES_ON) => {
                        feature_mask.contains(FeatureMask::CLIG)
                    }
                    (LIGATURE_TYPE, CONTEXTUAL_LIGATURES_OFF) => {
                        !feature_mask.contains(FeatureMask::CLIG)
                    }
                    _ => false,
                };

                if apply {
                    subfeature_flags =
                        (subfeature_flags & entry.disable_flags) | entry.enable_flags;
                }
            }
        }
    }
    Ok(subfeature_flags)
}

//------------------------------------ Ligature Substitution Test ----------------------------------------------------------
pub fn morx_ligature_test<'a>(scope: ReadScope<'a>) -> Result<(), ParseError> {
    let morx_table = scope.read::<MorxTable<'a>>()?;

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

    //let mut liga_subtable_no: u16 = 0;
    for chain in morx_table.morx_chains.iter() {
        for subtable in chain.subtables.iter() {
            if subtable.subtable_header.coverage & 0xFF == 2 {
                //liga_subtable_no += 1;
                //println!("Ligature subtable No: {}", liga_subtable_no);

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

//---------------------------------  Morx Substitution Test --------------------------------------------
pub fn morx_substitution_test<'a>(scope: ReadScope<'a>) -> Result<(), ParseError> {
    let morx_table = scope.read::<MorxTable<'a>>()?;

    let glyph1: RawGlyph<()> = RawGlyph {
        unicodes: tiny_vec![[char; 1]],
        glyph_index: 3,
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
        glyph_index: 604,
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

    let glyph3: RawGlyph<()> = RawGlyph {
        unicodes: tiny_vec![[char; 1]],
        glyph_index: 547,
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

    let glyph4: RawGlyph<()> = RawGlyph {
        unicodes: tiny_vec![[char; 1]],
        glyph_index: 528,
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

    let glyph5: RawGlyph<()> = RawGlyph {
        unicodes: tiny_vec![[char; 1]],
        glyph_index: 3,
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

    let mut glyphs: Vec<RawGlyph<()>> = vec![glyph1, glyph2, glyph3, glyph4, glyph5];

    let features = Features::Custom(Vec::new());

    let _res = apply(&morx_table, &mut glyphs, &features);

    //println!("The glyphs array after morx substitutions: {:?}", glyphs);

    //print glyph array after applying substitutions.
    for glyph in glyphs.iter() {
        println!("  {:?}", glyph);
    }

    Ok(())
}

//----------------------------------------------------------------------------------------------
