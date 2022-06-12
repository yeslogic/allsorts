//! `GDEF` font table parsing and glyph lookup and layout properties.

use crate::context::{ContextLookupHelper, GlyphTable, LookupFlag, MatchContext};
use crate::error::ParseError;

use crate::binary::read::{
    CheckIndex, ReadArray, ReadBinary, ReadBinaryDep, ReadCache, ReadCtxt, ReadFixedSizeDep,
    ReadFrom, ReadScope, ReadScopeOwned,
};
use crate::binary::U16Be;
use crate::size;
use crate::tag;
use log::warn;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;
use std::u16;

pub enum GSUB {}
pub enum GPOS {}

pub struct GDEFTable {
    pub opt_glyph_classdef: Option<ClassDef>,
    // pub opt_attach_list: Option<ReadScope<'a>>,
    // pub opt_lig_caret_list: Option<ReadScope<'a>>,
    pub opt_mark_attach_classdef: Option<ClassDef>,
    // TODO read additional GDEF 1.2 and 1.3 fields
}

// GSUB and GPOS tables have the same top-level structure
pub struct LayoutTable<T> {
    pub opt_script_list: Option<ScriptList>,
    pub opt_feature_list: Option<FeatureList>,
    pub opt_lookup_list: Option<LookupList<T>>,
}

pub struct ScriptList {
    script_records: Vec<ScriptRecord>,
}

pub struct ScriptRecord {
    pub script_tag: u32,
    script_table: ScriptTable,
}

pub struct ScriptTable {
    opt_default_langsys: Option<LangSys>,
    langsys_records: Vec<LangSysRecord>,
}

pub struct LangSysRecord {
    pub langsys_tag: u32,
    langsys_table: LangSys,
}

pub struct LangSys {
    _lookup_order: usize,           // reserved field, should be zero
    _required_feature_index: usize, // ignored for now, 0xFFFF
    feature_indices: Vec<u16>,
}

pub struct FeatureList {
    feature_records: Vec<FeatureRecord>,
}

pub struct FeatureRecord {
    pub feature_tag: u32,
    feature_table: FeatureTable,
}

pub struct FeatureTable {
    _feature_params: usize, // reserved field, should be zero
    pub lookup_indices: Vec<u16>,
}

pub struct LookupList<T> {
    scope_owned: ReadScopeOwned,
    lookup_offsets: Vec<u16>,
    phantom: PhantomData<T>,
}

pub struct Lookup<'a, T: LayoutTableType> {
    scope: ReadScope<'a>,
    lookup_type: LookupType<T>,
    pub lookup_flag: u16,
    subtable_offsets: ReadArray<'a, U16Be>,
    phantom: PhantomData<T>,
}

pub struct ExtensionSubst<'a, T: LayoutTableType> {
    scope: ReadScope<'a>,
    extension_lookup_type: T::BaseLookupType,
    extension_offset: u32,
}

pub struct LookupSubtableIter<'a, 'b, T: LayoutTableType> {
    lookup: &'b Lookup<'a, T>,
    index: usize,
}

pub struct ExtensionLookupSubtableIter<'a, 'b, T: LayoutTableType> {
    lookup_type: T::BaseLookupType,
    iter: LookupSubtableIter<'a, 'b, T>,
}

pub enum SmartLookupSubtableIter<'a, 'b, T: LayoutTableType> {
    Normal(T::BaseLookupType, LookupSubtableIter<'a, 'b, T>),
    Extension(ExtensionLookupSubtableIter<'a, 'b, T>),
}

pub enum LookupType<T: LayoutTableType> {
    Normal(T::BaseLookupType),
    Extension,
}

#[derive(Copy, Clone, PartialEq)]
pub enum SubstLookupType {
    SingleSubst,
    MultipleSubst,
    AlternateSubst,
    LigatureSubst,
    ContextSubst,
    ChainContextSubst,
    ReverseChainSingleSubst,
}

#[derive(Copy, Clone, PartialEq)]
pub enum PosLookupType {
    SinglePos,
    PairPos,
    CursivePos,
    MarkBasePos,
    MarkLigPos,
    MarkMarkPos,
    ContextPos,
    ChainContextPos,
}

pub enum SubstLookup {
    SingleSubst(Vec<SingleSubst>),
    MultipleSubst(Vec<MultipleSubst>),
    AlternateSubst(Vec<AlternateSubst>),
    LigatureSubst(Vec<LigatureSubst>),
    ContextSubst(Vec<ContextLookup<GSUB>>),
    ChainContextSubst(Vec<ChainContextLookup<GSUB>>),
    ReverseChainSingleSubst(Vec<ReverseChainSingleSubst>),
}

pub enum PosLookup {
    SinglePos(Vec<SinglePos>),
    PairPos(Vec<PairPos>),
    CursivePos(Vec<CursivePos>),
    MarkBasePos(Vec<MarkBasePos>),
    MarkLigPos(Vec<MarkLigPos>),
    MarkMarkPos(Vec<MarkBasePos>),
    ContextPos(Vec<ContextLookup<GPOS>>),
    ChainContextPos(Vec<ChainContextLookup<GPOS>>),
}

pub trait LayoutTableType: Sized {
    type LookupType;
    type BaseLookupType: Copy + PartialEq;
    fn check_lookup_type(lookup_type: u16) -> Result<LookupType<Self>, ParseError>;
}

impl<'a> ReadBinary<'a> for GDEFTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let table = ctxt.scope();

        let major_version = ctxt.read_u16be()?;
        ctxt.check(major_version == 1)?;
        let _minor_version = ctxt.read_u16be()?;
        let glyph_classdef_offset = usize::from(ctxt.read_u16be()?);
        let _attach_list_offset = usize::from(ctxt.read_u16be()?);
        let _lig_caret_list_offset = usize::from(ctxt.read_u16be()?);
        // MarkAttachClassDef was added to GDEF in OpenType 1.2 but they did not change the GDEF
        // version. This means that it's not possible to know from the version alone whether the
        // field should be read. Some implementations use GSUB/GPOS to determine if it should be
        // read, others use heuristics based on the offsets in the prior fields.  Based on testing
        // of this code and the fact that HarfBuzz _appears_ to always attempt to read the field we
        // do the same.
        //
        // See: https://github.com/yeslogic/prince/issues/297 for more detail.
        let mark_attach_classdef_offset = usize::from(ctxt.read_u16be()?);

        let gdef_header_size = 6 * size::U16;

        let opt_glyph_classdef = if glyph_classdef_offset == 0 {
            None
        } else if glyph_classdef_offset < gdef_header_size {
            None
        } else {
            Some(table.offset(glyph_classdef_offset).read::<ClassDef>()?)
        };

        /*
                let opt_attach_list = if attach_list_offset >= table.data().len() {
                    return Err(ParseError::BadOffset);
                } else if attach_list_offset == 0 {
                    None
                } else if attach_list_offset < gdef_header_size {
                    return Err(ParseError::BadOffset);
                } else {
                    Some(table.offset(attach_list_offset))
                };

                let opt_lig_caret_list = if lig_caret_list_offset >= table.data().len() {
                    return Err(ParseError::BadOffset);
                } else if lig_caret_list_offset == 0 {
                    None
                } else if lig_caret_list_offset < gdef_header_size {
                    return Err(ParseError::BadOffset);
                } else {
                    Some(table.offset(lig_caret_list_offset))
                };
        */
        let opt_mark_attach_classdef = if mark_attach_classdef_offset == 0 {
            None
        } else if mark_attach_classdef_offset < gdef_header_size {
            None
        } else {
            Some(
                table
                    .offset(mark_attach_classdef_offset)
                    .read::<ClassDef>()?,
            )
        };

        Ok(GDEFTable {
            opt_glyph_classdef,
            // opt_attach_list,
            // opt_lig_caret_list,
            opt_mark_attach_classdef,
        })
    }
}

impl<'a, T> ReadBinary<'a> for LayoutTable<T> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let table = ctxt.scope();

        let major_version = ctxt.read_u16be()?;
        let _minor_version = ctxt.read_u16be()?;
        let script_list_offset = usize::from(ctxt.read_u16be()?);
        let feature_list_offset = usize::from(ctxt.read_u16be()?);
        let lookup_list_offset = usize::from(ctxt.read_u16be()?);

        // We handle versions 1.x
        if major_version != 1 {
            return Err(ParseError::BadVersion);
        }

        let opt_script_list = if script_list_offset >= table.data().len() {
            return Err(ParseError::BadOffset);
        } else if script_list_offset == 0 {
            None
        } else {
            Some(table.offset(script_list_offset).read::<ScriptList>()?)
        };

        let opt_feature_list = if feature_list_offset >= table.data().len() {
            return Err(ParseError::BadOffset);
        } else if feature_list_offset == 0 {
            None
        } else {
            Some(table.offset(feature_list_offset).read::<FeatureList>()?)
        };

        let opt_lookup_list = if lookup_list_offset >= table.data().len() {
            return Err(ParseError::BadOffset);
        } else if lookup_list_offset == 0 {
            None
        } else {
            Some(table.offset(lookup_list_offset).read::<LookupList<T>>()?)
        };

        // Version 1.1 also includes an offset to a FeatureVariations table but we don't bother
        // reading that yet, since we don't use it.

        Ok(LayoutTable {
            opt_script_list,
            opt_feature_list,
            opt_lookup_list,
        })
    }
}

impl<'a> ReadBinary<'a> for ScriptList {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let script_count = usize::from(ctxt.read_u16be()?);
        let script_records = ctxt
            .read_array_dep::<ScriptRecord>(script_count, scope)?
            .read_to_vec()?;
        Ok(ScriptList { script_records })
    }
}

impl<'a> ReadBinaryDep<'a> for ScriptRecord {
    type Args = ReadScope<'a>;
    type HostType = ScriptRecord;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, scope: Self::Args) -> Result<Self, ParseError> {
        let script_tag = ctxt.read_u32be()?;
        let script_offset = ctxt.read_u16be()?;
        let script_table = scope
            .offset(usize::from(script_offset))
            .read::<ScriptTable>()?;
        Ok(ScriptRecord {
            script_tag,
            script_table,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for ScriptRecord {
    fn size(_scope: Self::Args) -> usize {
        size::U32 + size::U16
    }
}

impl<'a> ReadBinary<'a> for ScriptTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let default_langsys_offset = usize::from(ctxt.read_u16be()?);
        let opt_default_langsys = if default_langsys_offset != 0 {
            Some(scope.offset(default_langsys_offset).read::<LangSys>()?)
        } else {
            None
        };
        let langsys_count = usize::from(ctxt.read_u16be()?);
        let langsys_records = ctxt
            .read_array_dep::<LangSysRecord>(langsys_count, scope)?
            .read_to_vec()?;
        Ok(ScriptTable {
            opt_default_langsys,
            langsys_records,
        })
    }
}

impl<'a> ReadBinary<'a> for FeatureList {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let feature_count = usize::from(ctxt.read_u16be()?);
        let feature_records = ctxt
            .read_array_dep::<FeatureRecord>(feature_count, scope)?
            .read_to_vec()?;
        Ok(FeatureList { feature_records })
    }
}

impl FeatureList {
    pub fn nth_feature_record(&self, index: usize) -> Result<&FeatureRecord, ParseError> {
        self.feature_records.check_index(index)?;
        Ok(&self.feature_records[index])
    }
}

impl<'a> ReadBinaryDep<'a> for FeatureRecord {
    type Args = ReadScope<'a>;
    type HostType = FeatureRecord;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, scope: Self::Args) -> Result<Self, ParseError> {
        let feature_tag = ctxt.read_u32be()?;
        let feature_offset = ctxt.read_u16be()?;
        let feature_table = scope
            .offset(usize::from(feature_offset))
            .read::<FeatureTable>()?;
        Ok(FeatureRecord {
            feature_tag,
            feature_table,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for FeatureRecord {
    fn size(_scope: Self::Args) -> usize {
        size::U32 + size::U16
    }
}

impl<'a> ReadBinary<'a> for FeatureTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let _feature_params = usize::from(ctxt.read_u16be()?);
        let lookup_index_count = usize::from(ctxt.read_u16be()?);
        let lookup_indices = ctxt.read_array::<U16Be>(lookup_index_count)?.to_vec();
        Ok(FeatureTable {
            _feature_params,
            lookup_indices,
        })
    }
}

impl<'a, T> ReadBinary<'a> for LookupList<T> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope_owned = ReadScopeOwned::new(ctxt.scope());
        let lookup_count = usize::from(ctxt.read_u16be()?);
        let lookup_offsets = ctxt.read_array::<U16Be>(lookup_count)?.to_vec();
        Ok(LookupList {
            scope_owned,
            lookup_offsets,
            phantom: PhantomData,
        })
    }
}

impl<'a> ReadBinary<'a> for LangSys {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let _lookup_order = usize::from(ctxt.read_u16be()?);
        let _required_feature_index = usize::from(ctxt.read_u16be()?);
        let feature_index_count = usize::from(ctxt.read_u16be()?);
        let feature_indices = ctxt.read_array::<U16Be>(feature_index_count)?.to_vec();
        Ok(LangSys {
            _lookup_order,
            _required_feature_index,
            feature_indices,
        })
    }
}

impl LangSys {
    pub fn feature_indices_iter<'b>(&self) -> impl Iterator<Item = &u16> {
        self.feature_indices.iter()
    }
}

impl<T> LayoutTable<T> {
    pub fn find_script(&self, script_tag: u32) -> Result<Option<&ScriptTable>, ParseError> {
        if let Some(ref script_list) = self.opt_script_list {
            if let Some(ref script_table) = script_list.find_script(script_tag)? {
                return Ok(Some(script_table));
            }
        }
        Ok(None)
    }

    pub fn find_script_or_default(
        &self,
        script_tag: u32,
    ) -> Result<Option<&ScriptTable>, ParseError> {
        if let Some(ref script_list) = self.opt_script_list {
            if let Some(ref script_table) = script_list.find_script(script_tag)? {
                return Ok(Some(&script_table));
            } else {
                return script_list.find_script(tag::DFLT);
            }
        }
        Ok(None)
    }

    pub fn find_langsys_feature(
        &self,
        langsys: &LangSys,
        feature_tag: u32,
    ) -> Result<Option<&FeatureTable>, ParseError> {
        if let Some(ref feature_list) = self.opt_feature_list {
            for feature_index in &langsys.feature_indices {
                let feature_record =
                    feature_list.nth_feature_record(usize::from(*feature_index))?;
                if feature_record.feature_tag == feature_tag {
                    return Ok(Some(&feature_record.feature_table));
                }
            }
        }
        Ok(None)
    }

    pub fn feature_by_index(&self, feature_index: u16) -> Result<&FeatureRecord, ParseError> {
        if let Some(ref feature_list) = self.opt_feature_list {
            let feature_record = feature_list.nth_feature_record(usize::from(feature_index))?;
            Ok(feature_record)
        } else {
            Err(ParseError::BadIndex)
        }
    }
}

impl ScriptList {
    pub fn find_script(&self, script_tag: u32) -> Result<Option<&ScriptTable>, ParseError> {
        for script_record in &self.script_records {
            if script_record.script_tag == script_tag {
                return Ok(Some(&script_record.script_table));
            }
        }
        Ok(None)
    }
}

impl ScriptTable {
    pub fn default_langsys_record(&self) -> Option<&LangSys> {
        self.opt_default_langsys.as_ref()
    }

    pub fn find_langsys(&self, langsys_tag: u32) -> Result<Option<&LangSys>, ParseError> {
        for langsys_record in &self.langsys_records {
            if langsys_record.langsys_tag == langsys_tag {
                return Ok(Some(&langsys_record.langsys_table));
            }
        }
        Ok(None)
    }

    pub fn find_langsys_or_default(
        &self,
        opt_lang_tag: Option<u32>,
    ) -> Result<Option<&LangSys>, ParseError> {
        match opt_lang_tag {
            Some(lang_tag) => match self.find_langsys(lang_tag)? {
                Some(langsys_record) => Ok(Some(langsys_record)),
                None => Ok(self.default_langsys_record()),
            },
            None => Ok(self.default_langsys_record()),
        }
    }
}

impl<'a> ReadBinaryDep<'a> for LangSysRecord {
    type Args = ReadScope<'a>;
    type HostType = LangSysRecord;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, scope: Self::Args) -> Result<Self, ParseError> {
        let langsys_tag = ctxt.read_u32be()?;
        let langsys_offset = ctxt.read_u16be()?;
        let langsys_table = scope
            .offset(usize::from(langsys_offset))
            .read::<LangSys>()?;
        Ok(LangSysRecord {
            langsys_tag,
            langsys_table,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for LangSysRecord {
    fn size(_scope: Self::Args) -> usize {
        size::U32 + size::U16
    }
}

impl<T: LayoutTableType> LookupList<T> {
    pub fn lookup(&self, lookup_index: usize) -> Result<Lookup<'_, T>, ParseError> {
        self.lookup_offsets.check_index(lookup_index)?;
        let lookup_table_offset = self.lookup_offsets[lookup_index];
        self.scope_owned
            .scope()
            .offset(usize::from(lookup_table_offset))
            .read::<Lookup<'_, T>>()
    }
}

impl LookupList<GSUB> {
    pub fn lookup_cache_gsub(
        &self,
        cache: &LayoutCache<GSUB>,
        lookup_index: usize,
    ) -> Result<Rc<LookupCacheItem<SubstLookup>>, ParseError> {
        let lookup_vec = &mut cache.lookup_cache.borrow_mut();
        if lookup_index >= lookup_vec.len() {
            lookup_vec.resize(lookup_index + 1, None);
        }
        if let Some(ref lookup_cache_item) = lookup_vec[lookup_index] {
            Ok(Rc::clone(lookup_cache_item))
        } else {
            let lookup_cache_item = Rc::new(self.read_lookup_gsub(cache, lookup_index)?);
            lookup_vec[lookup_index] = Some(Rc::clone(&lookup_cache_item));
            Ok(lookup_cache_item)
        }
    }

    fn read_lookup_gsub(
        &self,
        cache: &LayoutCache<GSUB>,
        lookup_index: usize,
    ) -> Result<LookupCacheItem<SubstLookup>, ParseError> {
        let lookup = self.lookup(lookup_index)?;
        let lookup_flag = LookupFlag(lookup.lookup_flag);
        let lookup_type = lookup.get_lookup_type()?;
        let lookup_subtables = match lookup_type {
            SubstLookupType::SingleSubst => {
                SubstLookup::SingleSubst(lookup.read_subtables::<SingleSubst>(cache)?)
            }
            SubstLookupType::MultipleSubst => {
                SubstLookup::MultipleSubst(lookup.read_subtables::<MultipleSubst>(cache)?)
            }
            SubstLookupType::AlternateSubst => {
                SubstLookup::AlternateSubst(lookup.read_subtables::<AlternateSubst>(cache)?)
            }
            SubstLookupType::LigatureSubst => {
                SubstLookup::LigatureSubst(lookup.read_subtables::<LigatureSubst>(cache)?)
            }
            SubstLookupType::ContextSubst => {
                SubstLookup::ContextSubst(lookup.read_subtables::<ContextLookup<GSUB>>(cache)?)
            }
            SubstLookupType::ChainContextSubst => SubstLookup::ChainContextSubst(
                lookup.read_subtables::<ChainContextLookup<GSUB>>(cache)?,
            ),
            SubstLookupType::ReverseChainSingleSubst => SubstLookup::ReverseChainSingleSubst(
                lookup.read_subtables::<ReverseChainSingleSubst>(cache)?,
            ),
        };
        Ok(LookupCacheItem {
            lookup_flag,
            lookup_subtables,
        })
    }
}

impl LookupList<GPOS> {
    pub fn lookup_cache_gpos(
        &self,
        cache: &LayoutCache<GPOS>,
        lookup_index: usize,
    ) -> Result<Rc<LookupCacheItem<PosLookup>>, ParseError> {
        let lookup_vec = &mut cache.lookup_cache.borrow_mut();
        if lookup_index >= lookup_vec.len() {
            lookup_vec.resize(lookup_index + 1, None);
        }
        if let Some(ref lookup_cache_item) = lookup_vec[lookup_index] {
            Ok(Rc::clone(&lookup_cache_item))
        } else {
            let lookup_cache_item = Rc::new(self.read_lookup_gpos(cache, lookup_index)?);
            lookup_vec[lookup_index] = Some(Rc::clone(&lookup_cache_item));
            Ok(lookup_cache_item)
        }
    }

    fn read_lookup_gpos(
        &self,
        cache: &LayoutCache<GPOS>,
        lookup_index: usize,
    ) -> Result<LookupCacheItem<PosLookup>, ParseError> {
        let lookup = self.lookup(lookup_index)?;
        let lookup_flag = LookupFlag(lookup.lookup_flag);
        let lookup_type = lookup.get_lookup_type()?;
        let lookup_subtables = match lookup_type {
            PosLookupType::SinglePos => {
                PosLookup::SinglePos(lookup.read_subtables::<SinglePos>(cache)?)
            }
            PosLookupType::PairPos => PosLookup::PairPos(lookup.read_subtables::<PairPos>(cache)?),
            PosLookupType::CursivePos => {
                PosLookup::CursivePos(lookup.read_subtables::<CursivePos>(cache)?)
            }
            PosLookupType::MarkBasePos => {
                PosLookup::MarkBasePos(lookup.read_subtables::<MarkBasePos>(cache)?)
            }
            PosLookupType::MarkLigPos => {
                PosLookup::MarkLigPos(lookup.read_subtables::<MarkLigPos>(cache)?)
            }
            PosLookupType::MarkMarkPos => {
                PosLookup::MarkMarkPos(lookup.read_subtables::<MarkBasePos>(cache)?)
            }
            PosLookupType::ContextPos => {
                PosLookup::ContextPos(lookup.read_subtables::<ContextLookup<GPOS>>(cache)?)
            }
            PosLookupType::ChainContextPos => PosLookup::ChainContextPos(
                lookup.read_subtables::<ChainContextLookup<GPOS>>(cache)?,
            ),
        };
        Ok(LookupCacheItem {
            lookup_flag,
            lookup_subtables,
        })
    }
}

impl<'a, T: LayoutTableType> Lookup<'a, T> {
    fn subtable_iter<'b>(&'b self) -> LookupSubtableIter<'a, 'b, T> {
        LookupSubtableIter {
            lookup: self,
            index: 0,
        }
    }

    pub fn smart_subtable_iter<'b>(
        &'b self,
    ) -> Result<SmartLookupSubtableIter<'a, 'b, T>, ParseError> {
        match self.lookup_type {
            LookupType::Normal(lookup_type) => {
                let iter = self.subtable_iter();
                Ok(SmartLookupSubtableIter::Normal(lookup_type, iter))
            }
            LookupType::Extension => {
                if let Some(subtable) = self.subtable_iter().next() {
                    let ext_subtable = subtable.read::<ExtensionSubst<'_, T>>()?;
                    let lookup_type = ext_subtable.extension_lookup_type;
                    let iter = ExtensionLookupSubtableIter {
                        lookup_type,
                        iter: self.subtable_iter(),
                    };
                    Ok(SmartLookupSubtableIter::Extension(iter))
                } else {
                    Err(ParseError::BadValue)
                }
            }
        }
    }

    pub fn get_lookup_type(&self) -> Result<T::BaseLookupType, ParseError> {
        let mut subtables = self.smart_subtable_iter()?;
        let lookup_type = subtables.get_lookup_type();
        Ok(lookup_type)
    }

    pub fn find_subtable<S>(
        &self,
        f: impl Fn(ReadScope<'a>) -> Result<Option<S>, ParseError>,
    ) -> Result<Option<S>, ParseError> {
        let subtable_iter = self.smart_subtable_iter()?;
        for subtable_result in subtable_iter {
            let subtable = subtable_result?;
            match f(subtable) {
                Ok(Some(t)) => return Ok(Some(t)),
                Ok(None) => {}
                Err(err) => warn!("skipping invalid subtable: {}", err),
            }
        }
        Ok(None)
    }

    pub fn read_subtables<S: ReadBinaryDep<'a, Args = LayoutCache<T>>>(
        &self,
        cache: &LayoutCache<T>,
    ) -> Result<Vec<S::HostType>, ParseError> {
        let mut subtables = Vec::new();
        let subtable_iter = self.smart_subtable_iter()?;
        for subtable_result in subtable_iter {
            match subtable_result?.read_dep::<S>(Rc::clone(cache)) {
                Ok(subtable) => subtables.push(subtable),
                Err(err) => warn!("skipping invalid subtable: {}", err),
            }
        }
        Ok(subtables)
    }
}

impl<'a, T: LayoutTableType> ReadBinary<'a> for Lookup<'a, T> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let lookup_type = ctxt.read_u16be()?;
        let lookup_type = T::check_lookup_type(lookup_type)?;
        let lookup_flag = ctxt.read_u16be()?;
        let subtable_count = usize::from(ctxt.read_u16be()?);
        let subtable_offsets = ctxt.read_array::<U16Be>(subtable_count)?;
        Ok(Lookup {
            scope,
            lookup_type,
            lookup_flag,
            subtable_offsets,
            phantom: PhantomData,
        })
    }
}

impl<'a, T: LayoutTableType> ReadBinary<'a> for ExtensionSubst<'a, T> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let subst_format = ctxt.read_u16be()?;
        match subst_format {
            1 => {
                let extension_lookup_type = ctxt.read_u16be()?;
                let extension_lookup_type = match T::check_lookup_type(extension_lookup_type)? {
                    LookupType::Normal(lookup_type) => lookup_type,
                    LookupType::Extension => return Err(ParseError::BadVersion),
                };
                let extension_offset = ctxt.read_u32be()?;
                Ok(ExtensionSubst {
                    scope,
                    extension_lookup_type,
                    extension_offset,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a, 'b, T: LayoutTableType> Iterator for LookupSubtableIter<'a, 'b, T> {
    type Item = ReadScope<'a>;
    fn next(&mut self) -> Option<ReadScope<'a>> {
        if self.index < self.lookup.subtable_offsets.len() {
            let subtable_offset = self.lookup.subtable_offsets.get_item(self.index);
            let subtable = self.lookup.scope.offset(usize::from(subtable_offset));
            self.index += 1;
            Some(subtable)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.lookup.subtable_offsets.len();
        if self.index < len {
            let upper = len - self.index;
            (upper, Some(upper))
        } else {
            (0, Some(0))
        }
    }
}

impl<'a, 'b, T: LayoutTableType> Iterator for ExtensionLookupSubtableIter<'a, 'b, T> {
    type Item = Result<ReadScope<'a>, ParseError>;
    fn next(&mut self) -> Option<Result<ReadScope<'a>, ParseError>> {
        if let Some(subtable) = self.iter.next() {
            match subtable.read::<ExtensionSubst<'_, T>>() {
                Ok(ext_subtable) => {
                    if ext_subtable.extension_lookup_type != self.lookup_type {
                        return Some(Err(ParseError::BadVersion));
                    }
                    let subtable = ext_subtable
                        .scope
                        .offset(ext_subtable.extension_offset as usize);
                    Some(Ok(subtable))
                }
                Err(err) => Some(Err(err)),
            }
        } else {
            None
        }
    }
}

impl<'a, 'b, T: LayoutTableType> Iterator for SmartLookupSubtableIter<'a, 'b, T> {
    type Item = Result<ReadScope<'a>, ParseError>;
    fn next(&mut self) -> Option<Result<ReadScope<'a>, ParseError>> {
        match *self {
            SmartLookupSubtableIter::Normal(_, ref mut iter) => iter.next().map(|scope| Ok(scope)),
            SmartLookupSubtableIter::Extension(ref mut iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            SmartLookupSubtableIter::Normal(_lookup_type, iter) => iter.size_hint(),
            SmartLookupSubtableIter::Extension(iter) => iter.size_hint(),
        }
    }
}

impl<'a, 'b, T: LayoutTableType> SmartLookupSubtableIter<'a, 'b, T> {
    pub fn get_lookup_type(&mut self) -> T::BaseLookupType {
        match *self {
            SmartLookupSubtableIter::Normal(lookup_type, _) => lookup_type,
            SmartLookupSubtableIter::Extension(ref mut iter) => iter.lookup_type,
        }
    }
}

impl LayoutTableType for GSUB {
    type LookupType = SubstLookup;
    type BaseLookupType = SubstLookupType;
    fn check_lookup_type(lookup_type: u16) -> Result<LookupType<GSUB>, ParseError> {
        match lookup_type {
            1 => Ok(LookupType::Normal(SubstLookupType::SingleSubst)),
            2 => Ok(LookupType::Normal(SubstLookupType::MultipleSubst)),
            3 => Ok(LookupType::Normal(SubstLookupType::AlternateSubst)),
            4 => Ok(LookupType::Normal(SubstLookupType::LigatureSubst)),
            5 => Ok(LookupType::Normal(SubstLookupType::ContextSubst)),
            6 => Ok(LookupType::Normal(SubstLookupType::ChainContextSubst)),
            7 => Ok(LookupType::Extension),
            8 => Ok(LookupType::Normal(SubstLookupType::ReverseChainSingleSubst)),
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl LayoutTableType for GPOS {
    type LookupType = PosLookup;
    type BaseLookupType = PosLookupType;
    fn check_lookup_type(lookup_type: u16) -> Result<LookupType<GPOS>, ParseError> {
        match lookup_type {
            1 => Ok(LookupType::Normal(PosLookupType::SinglePos)),
            2 => Ok(LookupType::Normal(PosLookupType::PairPos)),
            3 => Ok(LookupType::Normal(PosLookupType::CursivePos)),
            4 => Ok(LookupType::Normal(PosLookupType::MarkBasePos)),
            5 => Ok(LookupType::Normal(PosLookupType::MarkLigPos)),
            6 => Ok(LookupType::Normal(PosLookupType::MarkMarkPos)),
            7 => Ok(LookupType::Normal(PosLookupType::ContextPos)),
            8 => Ok(LookupType::Normal(PosLookupType::ChainContextPos)),
            9 => Ok(LookupType::Extension),
            _ => Err(ParseError::BadVersion),
        }
    }
}

pub enum SingleSubst {
    Format1 {
        coverage: Rc<Coverage>,
        delta_glyph_index: i16,
    },
    Format2 {
        coverage: Rc<Coverage>,
        substitute_glyph_array: Vec<u16>,
    },
}

impl<'a> ReadBinaryDep<'a> for SingleSubst {
    type HostType = Self;
    type Args = LayoutCache<GSUB>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let subtable = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = subtable
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let delta_glyph_index = ctxt.read_i16be()?;
                Ok(SingleSubst::Format1 {
                    coverage,
                    delta_glyph_index,
                })
            }
            2 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = subtable
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let glyph_count = ctxt.read_u16be()?;
                let substitute_glyph_array =
                    ctxt.read_array::<U16Be>(usize::from(glyph_count))?.to_vec();
                Ok(SingleSubst::Format2 {
                    coverage,
                    substitute_glyph_array,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl SingleSubst {
    pub fn apply_glyph(&self, glyph: u16) -> Result<Option<u16>, ParseError> {
        match *self {
            SingleSubst::Format1 {
                ref coverage,
                delta_glyph_index,
            } => {
                if coverage.glyph_coverage_value(glyph).is_some() {
                    let new_glyph_index = glyph as isize + delta_glyph_index as isize;
                    // Addition of deltaGlyphID is modulo 65536, which is why the mask is used.
                    Ok(Some((new_glyph_index & 0xffff) as u16)) // Cast safe due to mask
                } else {
                    Ok(None)
                }
            }
            SingleSubst::Format2 {
                ref coverage,
                ref substitute_glyph_array,
            } => match coverage.glyph_coverage_value(glyph) {
                Some(coverage_index) => {
                    let coverage_index = usize::from(coverage_index);
                    substitute_glyph_array.check_index(coverage_index)?;
                    let new_glyph_index = substitute_glyph_array[coverage_index];
                    Ok(Some(new_glyph_index))
                }
                None => Ok(None),
            },
        }
    }
}

pub struct MultipleSubst {
    coverage: Rc<Coverage>,
    sequences: Vec<SequenceTable>,
}

pub struct SequenceTable {
    pub substitute_glyphs: Vec<u16>,
}

impl<'a> ReadBinaryDep<'a> for MultipleSubst {
    type HostType = Self;
    type Args = LayoutCache<GSUB>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let sequence_count = usize::from(ctxt.read_u16be()?);
                let sequence_offsets = ctxt.read_array::<U16Be>(sequence_count)?;
                let sequences = read_objects::<SequenceTable>(&scope, sequence_offsets)?;
                Ok(MultipleSubst {
                    coverage,
                    sequences,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl MultipleSubst {
    pub fn apply_glyph(&self, glyph: u16) -> Result<Option<&SequenceTable>, ParseError> {
        match self.coverage.glyph_coverage_value(glyph) {
            Some(coverage_index) => {
                let coverage_index = usize::from(coverage_index);
                self.sequences.check_index(coverage_index)?;
                let sequence = &self.sequences[coverage_index];
                Ok(Some(sequence))
            }
            None => Ok(None),
        }
    }
}

impl<'a> ReadBinary<'a> for SequenceTable {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let glyph_count = usize::from(ctxt.read_u16be()?);
        // The spec requires this, but implementations do not follow it.
        // ctxt.check(glyph_count > 0)?;
        let substitute_glyphs = ctxt.read_array::<U16Be>(glyph_count)?.to_vec();
        Ok(SequenceTable { substitute_glyphs })
    }
}

pub struct AlternateSubst {
    coverage: Rc<Coverage>,
    alternatesets: Vec<AlternateSet>,
}

pub struct AlternateSet {
    pub alternate_glyphs: Vec<u16>,
}

impl<'a> ReadBinaryDep<'a> for AlternateSubst {
    type HostType = Self;
    type Args = LayoutCache<GSUB>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let alternateset_count = usize::from(ctxt.read_u16be()?);
                let alternateset_offsets = ctxt.read_array::<U16Be>(alternateset_count)?;
                let alternatesets = read_objects::<AlternateSet>(&scope, alternateset_offsets)?;
                Ok(AlternateSubst {
                    coverage,
                    alternatesets,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl AlternateSubst {
    pub fn apply_glyph(&self, glyph: u16) -> Result<Option<&AlternateSet>, ParseError> {
        match self.coverage.glyph_coverage_value(glyph) {
            Some(coverage_index) => {
                let coverage_index = usize::from(coverage_index);
                self.alternatesets.check_index(coverage_index)?;
                let alternateset = &self.alternatesets[coverage_index];
                Ok(Some(alternateset))
            }
            None => Ok(None),
        }
    }
}

impl<'a> ReadBinary<'a> for AlternateSet {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let glyph_count = usize::from(ctxt.read_u16be()?);
        ctxt.check(glyph_count > 0)?;
        let alternate_glyphs = ctxt.read_array::<U16Be>(glyph_count)?.to_vec();
        Ok(AlternateSet { alternate_glyphs })
    }
}

pub struct LigatureSubst {
    coverage: Rc<Coverage>,
    ligaturesets: Vec<LigatureSet>,
}

pub struct LigatureSet {
    pub ligatures: Vec<Ligature>,
}

pub struct Ligature {
    pub ligature_glyph: u16,
    pub component_glyphs: Vec<u16>,
}

impl<'a> ReadBinaryDep<'a> for LigatureSubst {
    type HostType = Self;
    type Args = LayoutCache<GSUB>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let ligatureset_count = usize::from(ctxt.read_u16be()?);
                let ligatureset_offsets = ctxt.read_array::<U16Be>(ligatureset_count)?;
                let ligaturesets = read_objects::<LigatureSet>(&scope, ligatureset_offsets)?;
                Ok(LigatureSubst {
                    coverage,
                    ligaturesets,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a> LigatureSubst {
    pub fn apply_glyph(&self, glyph: u16) -> Result<Option<&LigatureSet>, ParseError> {
        match self.coverage.glyph_coverage_value(glyph) {
            Some(coverage_index) => {
                let coverage_index = usize::from(coverage_index);
                self.ligaturesets.check_index(coverage_index)?;
                let ligatureset = &self.ligaturesets[coverage_index];
                Ok(Some(ligatureset))
            }
            None => Ok(None),
        }
    }
}

impl<'a> ReadBinary<'a> for LigatureSet {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let ligature_count = usize::from(ctxt.read_u16be()?);
        let ligature_offsets = ctxt.read_array::<U16Be>(ligature_count)?;
        let ligatures = read_objects::<Ligature>(&scope, ligature_offsets)?;
        Ok(LigatureSet { ligatures })
    }
}

impl<'a> ReadBinary<'a> for Ligature {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let ligature_glyph = ctxt.read_u16be()?;
        let component_count = usize::from(ctxt.read_u16be()?);
        ctxt.check(component_count > 0)?;
        let component_glyphs = ctxt.read_array::<U16Be>(component_count - 1)?.to_vec();
        Ok(Ligature {
            ligature_glyph,
            component_glyphs,
        })
    }
}

#[derive(Clone, Copy)]
pub struct ValueFormat(u16);

impl<'a> ReadBinary<'a> for ValueFormat {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let value_format = ctxt.read_u16be()?;
        if value_format <= 0xFF {
            Ok(ValueFormat(value_format))
        } else {
            Err(ParseError::BadValue)
        }
    }
}

impl ValueFormat {
    pub fn size(self) -> usize {
        let mut num_fields = 0;
        for i in 0..8 {
            if ith_bit_set(self.0, i) {
                num_fields += 1;
            }
        }
        num_fields * size::U16
    }

    fn is_zero(self) -> bool {
        self.0 == 0
    }

    fn has_x_placement(self) -> bool {
        ith_bit_set(self.0, 0)
    }
    fn has_y_placement(self) -> bool {
        ith_bit_set(self.0, 1)
    }
    fn has_x_advance(self) -> bool {
        ith_bit_set(self.0, 2)
    }
    fn has_y_advance(self) -> bool {
        ith_bit_set(self.0, 3)
    }
    fn has_x_placement_device(self) -> bool {
        ith_bit_set(self.0, 4)
    }
    fn has_y_placement_device(self) -> bool {
        ith_bit_set(self.0, 5)
    }
    fn has_x_advance_device(self) -> bool {
        ith_bit_set(self.0, 6)
    }
    fn has_y_advance_device(self) -> bool {
        ith_bit_set(self.0, 7)
    }
}

fn ith_bit_set(flags: u16, i: u16) -> bool {
    (flags & (1 << i)) != 0
}

pub type ValueRecord = Option<Adjust>;

#[derive(Debug, Copy, Clone)]
pub struct Adjust {
    pub x_placement: i16,
    pub y_placement: i16,
    pub x_advance: i16,
    pub y_advance: i16,
}

impl<'a> ReadBinaryDep<'a> for ValueRecord {
    type Args = ValueFormat;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: ValueFormat) -> Result<Self, ParseError> {
        let value_format = args;
        if value_format.is_zero() {
            return Ok(None);
        }
        let x_pla = if value_format.has_x_placement() {
            ctxt.read_i16be()?
        } else {
            0
        };
        let y_pla = if value_format.has_y_placement() {
            ctxt.read_i16be()?
        } else {
            0
        };
        let x_adv = if value_format.has_x_advance() {
            ctxt.read_i16be()?
        } else {
            0
        };
        let y_adv = if value_format.has_y_advance() {
            ctxt.read_i16be()?
        } else {
            0
        };
        if value_format.has_x_placement_device() {
            let _ = ctxt.read_i16be()?;
        }
        if value_format.has_y_placement_device() {
            let _ = ctxt.read_i16be()?;
        }
        if value_format.has_x_advance_device() {
            let _ = ctxt.read_i16be()?;
        }
        if value_format.has_y_advance_device() {
            let _ = ctxt.read_i16be()?;
        }
        Ok(Some(Adjust {
            x_placement: x_pla,
            y_placement: y_pla,
            x_advance: x_adv,
            y_advance: y_adv,
        }))
    }
}

impl<'a> ReadFixedSizeDep<'a> for ValueRecord {
    fn size(value_format: ValueFormat) -> usize {
        value_format.size()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Anchor {
    pub x: i16,
    pub y: i16,
}

impl<'a> ReadBinary<'a> for Anchor {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        match ctxt.read_u16be()? {
            1 | 2 | 3 => {
                let x = ctxt.read_i16be()?;
                let y = ctxt.read_i16be()?;
                // Doesn't read other fields because we don't use them
                Ok(Anchor { x, y })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

pub enum SinglePos {
    Format1 {
        coverage: Rc<Coverage>,
        value_record: ValueRecord,
    },
    Format2 {
        coverage: Rc<Coverage>,
        value_records: Vec<ValueRecord>,
    },
}

impl<'a> ReadBinaryDep<'a> for SinglePos {
    type HostType = Self;
    type Args = LayoutCache<GPOS>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();

        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let value_format = ctxt.read::<ValueFormat>()?;
                let value_record = ctxt.read_dep::<ValueRecord>(value_format)?;
                Ok(SinglePos::Format1 {
                    coverage,
                    value_record,
                })
            }
            2 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let value_format = ctxt.read::<ValueFormat>()?;
                let value_count = usize::from(ctxt.read_u16be()?);
                let value_records = ctxt
                    .read_array_dep::<ValueRecord>(value_count, value_format)?
                    .read_to_vec()?;
                Ok(SinglePos::Format2 {
                    coverage,
                    value_records,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl SinglePos {
    pub fn apply(&self, glyph: u16) -> Result<ValueRecord, ParseError> {
        match *self {
            SinglePos::Format1 {
                ref coverage,
                value_record,
            } => {
                if coverage.glyph_coverage_value(glyph).is_some() {
                    Ok(value_record)
                } else {
                    Ok(None)
                }
            }
            SinglePos::Format2 {
                ref coverage,
                ref value_records,
            } => {
                if let Some(coverage_index) = coverage.glyph_coverage_value(glyph) {
                    let coverage_index = usize::from(coverage_index);
                    value_records.check_index(coverage_index)?;
                    Ok(value_records[coverage_index])
                } else {
                    Ok(None)
                }
            }
        }
    }
}

pub enum PairPos {
    Format1 {
        coverage: Rc<Coverage>,
        pairsets: Vec<PairSet>,
    },
    Format2 {
        coverage: Rc<Coverage>,
        classdef1: Rc<ClassDef>,
        classdef2: Rc<ClassDef>,
        class2_count: usize,
        class1_records: Vec<Class1Record>,
    },
}

impl<'a> ReadBinaryDep<'a> for PairPos {
    type HostType = Self;
    type Args = LayoutCache<GPOS>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();

        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let value_format1 = ctxt.read::<ValueFormat>()?;
                let value_format2 = ctxt.read::<ValueFormat>()?;
                let pairset_count = usize::from(ctxt.read_u16be()?);
                let pairset_offsets = ctxt.read_array::<U16Be>(pairset_count)?;
                let pairsets = read_objects_dep::<PairSet>(
                    &scope,
                    pairset_offsets,
                    (value_format1, value_format2),
                )?;
                Ok(PairPos::Format1 { coverage, pairsets })
            }
            2 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let value_format1 = ctxt.read::<ValueFormat>()?;
                let value_format2 = ctxt.read::<ValueFormat>()?;
                let classdef1_offset = usize::from(ctxt.read_u16be()?);
                let classdef2_offset = usize::from(ctxt.read_u16be()?);
                let classdef1 = scope
                    .offset(classdef1_offset)
                    .read_cache::<ClassDef>(&mut cache.classdefs.borrow_mut())?;
                let classdef2 = scope
                    .offset(classdef2_offset)
                    .read_cache::<ClassDef>(&mut cache.classdefs.borrow_mut())?;
                let class1_count = usize::from(ctxt.read_u16be()?);
                let class2_count = usize::from(ctxt.read_u16be()?);
                let class1_records = ctxt
                    .read_array_dep::<Class1Record>(
                        class1_count,
                        (class2_count, value_format1, value_format2),
                    )?
                    .read_to_vec()?;
                Ok(PairPos::Format2 {
                    coverage,
                    classdef1,
                    classdef2,
                    class2_count,
                    class1_records,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

pub struct PairSet {
    pair_value_records: Vec<PairValueRecord>,
}

impl<'a> ReadBinaryDep<'a> for PairSet {
    type Args = (ValueFormat, ValueFormat);
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let pair_value_count = usize::from(ctxt.read_u16be()?);
        let pair_value_records = ctxt
            .read_array_dep::<PairValueRecord>(pair_value_count, args)?
            .read_to_vec()?;
        Ok(PairSet { pair_value_records })
    }
}

pub struct PairValueRecord {
    second_glyph: u16,
    value_record1: ValueRecord,
    value_record2: ValueRecord,
}

impl<'a> ReadBinaryDep<'a> for PairValueRecord {
    type Args = (ValueFormat, ValueFormat);
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let (value_format1, value_format2) = args;
        let second_glyph = ctxt.read_u16be()?;
        let value_record1 = ctxt.read_dep::<ValueRecord>(value_format1)?;
        let value_record2 = ctxt.read_dep::<ValueRecord>(value_format2)?;
        Ok(PairValueRecord {
            second_glyph,
            value_record1,
            value_record2,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for PairValueRecord {
    fn size((value_format1, value_format2): Self::Args) -> usize {
        size::U16 + value_format1.size() + value_format2.size()
    }
}

pub struct Class1Record {
    class2_records: Vec<Class2Record>,
}

impl<'a> ReadBinaryDep<'a> for Class1Record {
    type Args = (usize, ValueFormat, ValueFormat);
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let (class2_count, value_format1, value_format2) = args;
        let class2_records = ctxt
            .read_array_dep::<Class2Record>(class2_count, (value_format1, value_format2))?
            .read_to_vec()?;
        Ok(Class1Record { class2_records })
    }
}

impl<'a> ReadFixedSizeDep<'a> for Class1Record {
    fn size((class2_count, value_format1, value_format2): Self::Args) -> usize {
        class2_count * Class2Record::size((value_format1, value_format2))
    }
}

pub struct Class2Record {
    value_record1: ValueRecord,
    value_record2: ValueRecord,
}

impl<'a> ReadBinaryDep<'a> for Class2Record {
    type Args = (ValueFormat, ValueFormat);
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let (value_format1, value_format2) = args;
        let value_record1 = ctxt.read_dep::<ValueRecord>(value_format1)?;
        let value_record2 = ctxt.read_dep::<ValueRecord>(value_format2)?;
        Ok(Class2Record {
            value_record1,
            value_record2,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for Class2Record {
    fn size((value_format1, value_format2): Self::Args) -> usize {
        value_format1.size() + value_format2.size()
    }
}

impl PairPos {
    pub fn apply(
        &self,
        glyph1: u16,
        glyph2: u16,
    ) -> Result<Option<(ValueRecord, ValueRecord)>, ParseError> {
        match *self {
            PairPos::Format1 {
                ref coverage,
                ref pairsets,
            } => {
                if let Some(coverage_index) = coverage.glyph_coverage_value(glyph1) {
                    let coverage_index = usize::from(coverage_index);
                    pairsets.check_index(coverage_index)?;
                    let pairset = &pairsets[coverage_index];
                    for pair_value_record in &pairset.pair_value_records {
                        if pair_value_record.second_glyph == glyph2 {
                            return Ok(Some((
                                pair_value_record.value_record1,
                                pair_value_record.value_record2,
                            )));
                        }
                    }
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
            PairPos::Format2 {
                ref coverage,
                ref classdef1,
                ref classdef2,
                class2_count,
                ref class1_records,
            } => {
                if coverage.glyph_coverage_value(glyph1).is_some() {
                    let class1_value = usize::from(classdef1.glyph_class_value(glyph1));
                    let class2_value = usize::from(classdef2.glyph_class_value(glyph2));
                    if class1_value < class1_records.len() && class2_value < class2_count {
                        let class1_record = &class1_records[class1_value];
                        let class2_record = &class1_record.class2_records[class2_value];
                        let adj1 = class2_record.value_record1;
                        let adj2 = class2_record.value_record2;
                        Ok(Some((adj1, adj2)))
                    } else {
                        Err(ParseError::BadIndex)
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }
}

pub struct CursivePos {
    coverage: Rc<Coverage>,
    entry_exit_records: Vec<EntryExitRecord>,
}

impl<'a> ReadBinaryDep<'a> for CursivePos {
    type HostType = Self;
    type Args = LayoutCache<GPOS>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();

        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let entry_exit_count = usize::from(ctxt.read_u16be()?);
                let entry_exit_records = ctxt
                    .read_array_dep::<EntryExitRecord>(entry_exit_count, scope.clone())?
                    .read_to_vec()?;
                Ok(CursivePos {
                    coverage,
                    entry_exit_records,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

struct EntryExitRecord {
    entry_anchor: Option<Anchor>,
    exit_anchor: Option<Anchor>,
}

impl<'a> ReadBinaryDep<'a> for EntryExitRecord {
    type Args = ReadScope<'a>;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, scope: Self::Args) -> Result<Self, ParseError> {
        let entry_anchor_offset = ctxt.read_u16be()?;
        let exit_anchor_offset = ctxt.read_u16be()?;
        let entry_anchor = if entry_anchor_offset != 0 {
            Some(
                scope
                    .offset(usize::from(entry_anchor_offset))
                    .read::<Anchor>()?,
            )
        } else {
            None
        };
        let exit_anchor = if exit_anchor_offset != 0 {
            Some(
                scope
                    .offset(usize::from(exit_anchor_offset))
                    .read::<Anchor>()?,
            )
        } else {
            None
        };
        Ok(EntryExitRecord {
            entry_anchor,
            exit_anchor,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for EntryExitRecord {
    fn size(_scope: Self::Args) -> usize {
        2 * size::U16
    }
}

impl CursivePos {
    pub fn apply(&self, glyph1: u16, glyph2: u16) -> Result<Option<(Anchor, Anchor)>, ParseError> {
        let coverage_value1 = self.coverage.glyph_coverage_value(glyph1);
        let coverage_value2 = self.coverage.glyph_coverage_value(glyph2);
        match (coverage_value1, coverage_value2) {
            (Some(coverage_index1), Some(coverage_index2)) => {
                let coverage_index1 = usize::from(coverage_index1);
                let coverage_index2 = usize::from(coverage_index2);
                self.entry_exit_records.check_index(coverage_index1)?;
                self.entry_exit_records.check_index(coverage_index2)?;
                let entry_exit1 = &self.entry_exit_records[coverage_index1];
                let entry_exit2 = &self.entry_exit_records[coverage_index2];
                match (entry_exit1.exit_anchor, entry_exit2.entry_anchor) {
                    (Some(glyph1_exit), Some(glyph2_entry)) => {
                        Ok(Some((glyph1_exit, glyph2_entry)))
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }
}

// also used for MarkToMark tables
pub struct MarkBasePos {
    mark_coverage: Rc<Coverage>,
    base_coverage: Rc<Coverage>,
    mark_class_count: usize,
    mark_array: MarkArray,
    base_array: BaseArray,
}

impl<'a> ReadBinaryDep<'a> for MarkBasePos {
    type HostType = Self;
    type Args = LayoutCache<GPOS>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();

        match ctxt.read_u16be()? {
            1 => {
                let mark_coverage_offset = usize::from(ctxt.read_u16be()?);
                let base_coverage_offset = usize::from(ctxt.read_u16be()?);
                let mark_class_count = usize::from(ctxt.read_u16be()?);
                let mark_array_offset = usize::from(ctxt.read_u16be()?);
                let base_array_offset = usize::from(ctxt.read_u16be()?);
                let mark_coverage = scope
                    .offset(mark_coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let base_coverage = scope
                    .offset(base_coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let mark_array = scope.offset(mark_array_offset).read::<MarkArray>()?;
                let base_array = scope
                    .offset(base_array_offset)
                    .read_dep::<BaseArray>(mark_class_count)?;
                Ok(MarkBasePos {
                    mark_coverage,
                    base_coverage,
                    mark_class_count,
                    mark_array,
                    base_array,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

struct BaseArray {
    base_records: Vec<BaseRecord>,
}

impl<'a> ReadBinaryDep<'a> for BaseArray {
    type Args = usize;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let mark_class_count = args;
        let scope = ctxt.scope();
        let base_count = usize::from(ctxt.read_u16be()?);
        let base_records = ctxt
            .read_array_dep::<BaseRecord>(base_count, (scope.clone(), mark_class_count))?
            .read_to_vec()?;
        Ok(BaseArray { base_records })
    }
}

struct BaseRecord {
    base_anchors: Vec<Option<Anchor>>,
}

impl<'a> ReadBinaryDep<'a> for BaseRecord {
    type Args = (ReadScope<'a>, usize);
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let (scope, mark_class_count) = args;
        let base_anchor_offsets = ctxt.read_array::<U16Be>(mark_class_count)?;
        let base_anchors = read_objects_nullable::<Anchor>(&scope, base_anchor_offsets)?;
        Ok(BaseRecord { base_anchors })
    }
}

impl<'a> ReadFixedSizeDep<'a> for BaseRecord {
    fn size((_scope, mark_class_count): Self::Args) -> usize {
        mark_class_count * size::U16
    }
}

struct MarkArray {
    mark_records: Vec<MarkRecord>,
}

impl<'a> ReadBinary<'a> for MarkArray {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let mark_count = usize::from(ctxt.read_u16be()?);
        let mark_records = ctxt
            .read_array_dep::<MarkRecord>(mark_count, scope.clone())?
            .read_to_vec()?;
        Ok(MarkArray { mark_records })
    }
}

struct MarkRecord {
    mark_class: u16,
    mark_anchor: Anchor,
}

impl<'a> ReadBinaryDep<'a> for MarkRecord {
    type Args = ReadScope<'a>;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, scope: Self::Args) -> Result<Self, ParseError> {
        let mark_class = ctxt.read_u16be()?;
        let mark_anchor_offset = ctxt.read_u16be()?;
        let mark_anchor = scope
            .offset(usize::from(mark_anchor_offset))
            .read::<Anchor>()?;
        Ok(MarkRecord {
            mark_class,
            mark_anchor,
        })
    }
}

impl<'a> ReadFixedSizeDep<'a> for MarkRecord {
    fn size(_scope: Self::Args) -> usize {
        2 * size::U16
    }
}

impl MarkBasePos {
    pub fn apply(&self, glyph1: u16, glyph2: u16) -> Result<Option<(Anchor, Anchor)>, ParseError> {
        let base_coverage_value = self.base_coverage.glyph_coverage_value(glyph1);
        let mark_coverage_value = self.mark_coverage.glyph_coverage_value(glyph2);
        match (base_coverage_value, mark_coverage_value) {
            (Some(base_coverage_index), Some(mark_coverage_index)) => {
                let base_coverage_index = usize::from(base_coverage_index);
                let mark_coverage_index = usize::from(mark_coverage_index);
                self.base_array
                    .base_records
                    .check_index(base_coverage_index)?;
                self.mark_array
                    .mark_records
                    .check_index(mark_coverage_index)?;
                let mark_record = &self.mark_array.mark_records[mark_coverage_index];
                let mark_class = usize::from(mark_record.mark_class);
                if mark_class < self.mark_class_count {
                    let mark_anchor = mark_record.mark_anchor;
                    let base_record = &self.base_array.base_records[base_coverage_index];
                    if let Some(base_anchor) = base_record.base_anchors[mark_class] {
                        Ok(Some((base_anchor, mark_anchor)))
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(ParseError::BadIndex)
                }
            }
            _ => Ok(None),
        }
    }
}

pub struct MarkLigPos {
    mark_coverage: Rc<Coverage>,
    liga_coverage: Rc<Coverage>,
    mark_class_count: usize,
    mark_array: MarkArray,
    ligature_array: LigatureArray,
}

impl<'a> ReadBinaryDep<'a> for MarkLigPos {
    type HostType = Self;
    type Args = LayoutCache<GPOS>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let mark_coverage_offset = usize::from(ctxt.read_u16be()?);
                let liga_coverage_offset = usize::from(ctxt.read_u16be()?);
                let mark_class_count = usize::from(ctxt.read_u16be()?);
                let mark_array_offset = usize::from(ctxt.read_u16be()?);
                let liga_array_offset = usize::from(ctxt.read_u16be()?);
                let mark_coverage = scope
                    .offset(mark_coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let liga_coverage = scope
                    .offset(liga_coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let mark_array = scope.offset(mark_array_offset).read::<MarkArray>()?;
                let ligature_array = scope
                    .offset(liga_array_offset)
                    .read_dep::<LigatureArray>(mark_class_count)?;
                Ok(MarkLigPos {
                    mark_coverage,
                    liga_coverage,
                    mark_class_count,
                    mark_array,
                    ligature_array,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

struct LigatureArray {
    ligature_attaches: Vec<LigatureAttach>,
}

impl<'a> ReadBinaryDep<'a> for LigatureArray {
    type Args = usize;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, mark_class_count: usize) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let ligature_count = usize::from(ctxt.read_u16be()?);
        let ligature_attach_offsets = ctxt.read_array::<U16Be>(ligature_count)?;
        let ligature_attaches =
            read_objects_dep::<LigatureAttach>(&scope, ligature_attach_offsets, mark_class_count)?;
        Ok(LigatureArray { ligature_attaches })
    }
}

struct LigatureAttach {
    component_records: Vec<ComponentRecord>,
}

impl<'a> ReadBinaryDep<'a> for LigatureAttach {
    type Args = usize;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let mark_class_count = args;
        let scope = ctxt.scope();
        let component_count = usize::from(ctxt.read_u16be()?);
        let component_records = ctxt
            .read_array_dep::<ComponentRecord>(component_count, (scope.clone(), mark_class_count))?
            .read_to_vec()?;
        Ok(LigatureAttach { component_records })
    }
}

struct ComponentRecord {
    ligature_anchors: Vec<Option<Anchor>>,
}

impl<'a> ReadBinaryDep<'a> for ComponentRecord {
    type Args = (ReadScope<'a>, usize);
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, args: Self::Args) -> Result<Self, ParseError> {
        let (scope, mark_class_count) = args;
        let ligature_anchor_offsets = ctxt.read_array::<U16Be>(mark_class_count)?;
        let ligature_anchors = read_objects_nullable::<Anchor>(&scope, ligature_anchor_offsets)?;
        Ok(ComponentRecord { ligature_anchors })
    }
}

impl<'a> ReadFixedSizeDep<'a> for ComponentRecord {
    fn size((_scope, mark_class_count): Self::Args) -> usize {
        mark_class_count * size::U16
    }
}

impl MarkLigPos {
    pub fn apply(
        &self,
        glyph1: u16,
        glyph2: u16,
        liga_component_index: usize,
    ) -> Result<Option<(Anchor, Anchor)>, ParseError> {
        let liga_coverage_value = self.liga_coverage.glyph_coverage_value(glyph1);
        let mark_coverage_value = self.mark_coverage.glyph_coverage_value(glyph2);
        match (liga_coverage_value, mark_coverage_value) {
            (Some(liga_coverage_index), Some(mark_coverage_index)) => {
                let liga_coverage_index = usize::from(liga_coverage_index);
                let mark_coverage_index = usize::from(mark_coverage_index);
                self.mark_array
                    .mark_records
                    .check_index(mark_coverage_index)?;
                let mark_record = &self.mark_array.mark_records[mark_coverage_index];
                let mark_class = usize::from(mark_record.mark_class);
                if mark_class < self.mark_class_count {
                    self.ligature_array
                        .ligature_attaches
                        .check_index(liga_coverage_index)?;
                    let liga_attach = &self.ligature_array.ligature_attaches[liga_coverage_index];
                    if liga_component_index < liga_attach.component_records.len() {
                        let component_record = &liga_attach.component_records[liga_component_index];
                        if let Some(liga_anchor) = component_record.ligature_anchors[mark_class] {
                            Ok(Some((liga_anchor, mark_record.mark_anchor)))
                        } else {
                            Ok(None)
                        }
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(ParseError::BadIndex)
                }
            }
            _ => Ok(None),
        }
    }
}

pub enum ContextLookup<T: LayoutTableType> {
    Format1 {
        coverage: Rc<Coverage>,
        subrulesets: Vec<Option<SubRuleSet>>,
        phantom: PhantomData<T>,
    },
    Format2 {
        coverage: Rc<Coverage>,
        classdef: Rc<ClassDef>,
        subclasssets: Vec<Option<SubClassSet>>,
        phantom: PhantomData<T>,
    },
    Format3 {
        coverages: Vec<Rc<Coverage>>,
        lookup_records: Vec<(u16, u16)>,
        phantom: PhantomData<T>,
    },
}

pub struct SubRuleSet {
    subrules: Vec<SubRule>,
}

pub struct SubRule {
    input_sequence: Vec<u16>,
    lookup_records: Vec<(u16, u16)>,
}

pub struct SubClassSet {
    subclassrules: Vec<SubClassRule>,
}

pub struct SubClassRule {
    input_sequence: Vec<u16>,
    lookup_records: Vec<(u16, u16)>,
}

pub enum ChainContextLookup<T: LayoutTableType> {
    Format1 {
        coverage: Rc<Coverage>,
        chainsubrulesets: Vec<Option<ChainSubRuleSet>>,
        phantom: PhantomData<T>,
    },
    Format2 {
        coverage: Rc<Coverage>,
        backtrack_classdef: Rc<ClassDef>,
        input_classdef: Rc<ClassDef>,
        lookahead_classdef: Rc<ClassDef>,
        chainsubclasssets: Vec<Option<ChainSubClassSet>>,
        phantom: PhantomData<T>,
    },
    Format3 {
        backtrack_coverages: Vec<Rc<Coverage>>,
        input_coverages: Vec<Rc<Coverage>>,
        lookahead_coverages: Vec<Rc<Coverage>>,
        lookup_records: Vec<(u16, u16)>,
        phantom: PhantomData<T>,
    },
}

pub struct ChainSubRuleSet {
    chainsubrules: Vec<ChainSubRule>,
}

pub struct ChainSubRule {
    backtrack_sequence: Vec<u16>,
    input_sequence: Vec<u16>,
    lookahead_sequence: Vec<u16>,
    lookup_records: Vec<(u16, u16)>,
}

pub struct ChainSubClassSet {
    chainsubclassrules: Vec<ChainSubClassRule>,
}

pub struct ChainSubClassRule {
    backtrack_sequence: Vec<u16>,
    input_sequence: Vec<u16>,
    lookahead_sequence: Vec<u16>,
    lookup_records: Vec<(u16, u16)>,
}

impl<'a, T: LayoutTableType> ReadBinaryDep<'a> for ContextLookup<T> {
    type HostType = Self;
    type Args = LayoutCache<T>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let subruleset_count = ctxt.read_u16be()?;
                let subruleset_offsets = ctxt.read_array::<U16Be>(usize::from(subruleset_count))?;
                let subrulesets = read_objects_nullable::<SubRuleSet>(&scope, subruleset_offsets)?;
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                Ok(ContextLookup::Format1 {
                    coverage,
                    subrulesets,
                    phantom: PhantomData,
                })
            }
            2 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let classdef_offset = usize::from(ctxt.read_u16be()?);
                let subclassset_count = ctxt.read_u16be()?;
                let subclassset_offsets =
                    ctxt.read_array::<U16Be>(usize::from(subclassset_count))?;
                let subclasssets =
                    read_objects_nullable::<SubClassSet>(&scope, subclassset_offsets)?;
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let classdef = scope
                    .offset(classdef_offset)
                    .read_cache::<ClassDef>(&mut cache.classdefs.borrow_mut())?;
                Ok(ContextLookup::Format2 {
                    coverage,
                    classdef,
                    subclasssets,
                    phantom: PhantomData,
                })
            }
            3 => {
                let glyph_count = usize::from(ctxt.read_u16be()?);
                ctxt.check(glyph_count > 0)?;
                let lookup_count = usize::from(ctxt.read_u16be()?);
                let coverage_offsets = ctxt.read_array::<U16Be>(glyph_count)?;
                let coverages = read_coverages(&scope, cache, coverage_offsets)?;
                let lookup_records = ctxt.read_array::<(U16Be, U16Be)>(lookup_count)?.to_vec();
                Ok(ContextLookup::Format3 {
                    coverages,
                    lookup_records,
                    phantom: PhantomData,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

/// GSUB Lookup Type 8 Subtable Formats
pub enum ReverseChainSingleSubst {
    /// Format 1
    Format1 {
        /// Coverage table for the single input glyph
        coverage: Rc<Coverage>,
        /// Array of backtrack sequence coverages, ordered by glyph sequence
        backtrack_coverages: Vec<Rc<Coverage>>,
        /// Array of lookahead sequence coverages, ordered by glyph sequence
        lookahead_coverages: Vec<Rc<Coverage>>,
        /// Array of substitute glyphs, ordered by coverage index
        substitute_glyphs: Vec<u16>,
    },
}

impl<'a> ReadBinaryDep<'a> for ReverseChainSingleSubst {
    type HostType = Self;
    type Args = LayoutCache<GSUB>;

    /// Parse a GSUB Lookup Type 8 Subtable from the given read context
    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let backtrack_count = usize::from(ctxt.read_u16be()?);
                let backtrack_coverage_offsets = ctxt.read_array::<U16Be>(backtrack_count)?;
                let lookahead_count = usize::from(ctxt.read_u16be()?);
                let lookahead_coverage_offsets = ctxt.read_array::<U16Be>(lookahead_count)?;
                let glyph_count = usize::from(ctxt.read_u16be()?);
                let substitute_glyphs = ctxt.read_array::<U16Be>(glyph_count)?.to_vec();
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let backtrack_coverages =
                    read_coverages(&scope, Rc::clone(&cache), backtrack_coverage_offsets)?;
                let lookahead_coverages =
                    read_coverages(&scope, Rc::clone(&cache), lookahead_coverage_offsets)?;

                ctxt.check(coverage.glyph_count() == glyph_count)?;
                Ok(ReverseChainSingleSubst::Format1 {
                    coverage,
                    backtrack_coverages,
                    lookahead_coverages,
                    substitute_glyphs,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

fn read_objects<'a, T: ReadBinary<'a, HostType = T>>(
    scope: &ReadScope<'a>,
    offsets: ReadArray<'a, U16Be>,
) -> Result<Vec<T::HostType>, ParseError> {
    let mut objects = Vec::with_capacity(offsets.len());
    for offset in &offsets {
        let object = scope.offset(usize::from(offset)).read::<T>()?;
        objects.push(object);
    }
    Ok(objects)
}

fn read_objects_dep<'a, T: ReadBinaryDep<'a, HostType = T>>(
    scope: &ReadScope<'a>,
    offsets: ReadArray<'a, U16Be>,
    args: T::Args,
) -> Result<Vec<T::HostType>, ParseError> {
    let mut objects = Vec::with_capacity(offsets.len());
    for offset in &offsets {
        let object = scope
            .offset(usize::from(offset))
            .read_dep::<T>(args.clone())?;
        objects.push(object);
    }
    Ok(objects)
}

fn read_objects_nullable<'a, T: ReadBinary<'a, HostType = T>>(
    scope: &ReadScope<'a>,
    offsets: ReadArray<'a, U16Be>,
) -> Result<Vec<Option<T::HostType>>, ParseError> {
    let mut objects = Vec::with_capacity(offsets.len());
    for offset in &offsets {
        if offset != 0 {
            let object = scope.offset(usize::from(offset)).read::<T>()?;
            objects.push(Some(object));
        } else {
            objects.push(None);
        }
    }
    Ok(objects)
}

fn read_coverages<'a, T: LayoutTableType>(
    scope: &ReadScope<'a>,
    cache: LayoutCache<T>,
    offsets: ReadArray<'a, U16Be>,
) -> Result<Vec<Rc<Coverage>>, ParseError> {
    let mut coverages = Vec::with_capacity(offsets.len());
    for coverage_offset in &offsets {
        let coverage = scope
            .offset(usize::from(coverage_offset))
            .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
        coverages.push(coverage);
    }
    Ok(coverages)
}

impl<'a> ReadBinary<'a> for SubRuleSet {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let subrule_count = usize::from(ctxt.read_u16be()?);
        let subrule_offsets = ctxt.read_array::<U16Be>(subrule_count)?;
        let subrules = read_objects::<SubRule>(&scope, subrule_offsets)?;
        Ok(SubRuleSet { subrules })
    }
}

impl<'a> ReadBinary<'a> for SubRule {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let glyph_count = usize::from(ctxt.read_u16be()?);
        ctxt.check(glyph_count > 0)?;
        let lookup_count = usize::from(ctxt.read_u16be()?);
        let input_sequence = ctxt.read_array::<U16Be>(glyph_count - 1)?.to_vec();
        let lookup_records = ctxt.read_array::<(U16Be, U16Be)>(lookup_count)?.to_vec();
        Ok(SubRule {
            input_sequence,
            lookup_records,
        })
    }
}

impl<'a> ReadBinary<'a> for SubClassSet {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let subclassrule_count = usize::from(ctxt.read_u16be()?);
        let subclassrule_offsets = ctxt.read_array::<U16Be>(subclassrule_count)?;
        let subclassrules = read_objects::<SubClassRule>(&scope, subclassrule_offsets)?;
        Ok(SubClassSet { subclassrules })
    }
}

impl<'a> ReadBinary<'a> for SubClassRule {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let glyph_count = usize::from(ctxt.read_u16be()?);
        ctxt.check(glyph_count > 0)?;
        let lookup_count = usize::from(ctxt.read_u16be()?);
        let input_sequence = ctxt.read_array::<U16Be>(glyph_count - 1)?.to_vec();
        let lookup_records = ctxt.read_array::<(U16Be, U16Be)>(lookup_count)?.to_vec();
        Ok(SubClassRule {
            input_sequence,
            lookup_records,
        })
    }
}

impl<'a, T: LayoutTableType> ReadBinaryDep<'a> for ChainContextLookup<T> {
    type HostType = Self;
    type Args = LayoutCache<T>;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, cache: Self::Args) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        match ctxt.read_u16be()? {
            1 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let chainsubruleset_count = ctxt.read_u16be()?;
                let chainsubruleset_offsets =
                    ctxt.read_array::<U16Be>(usize::from(chainsubruleset_count))?;
                let chainsubrulesets =
                    read_objects_nullable::<ChainSubRuleSet>(&scope, chainsubruleset_offsets)?;
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                Ok(ChainContextLookup::Format1 {
                    coverage,
                    chainsubrulesets,
                    phantom: PhantomData,
                })
            }
            2 => {
                let coverage_offset = usize::from(ctxt.read_u16be()?);
                let backtrack_classdef_offset = usize::from(ctxt.read_u16be()?);
                let input_classdef_offset = usize::from(ctxt.read_u16be()?);
                let lookahead_classdef_offset = usize::from(ctxt.read_u16be()?);
                let chainsubclassset_count = ctxt.read_u16be()?;
                let chainsubclassset_offsets =
                    ctxt.read_array::<U16Be>(usize::from(chainsubclassset_count))?;
                let chainsubclasssets =
                    read_objects_nullable::<ChainSubClassSet>(&scope, chainsubclassset_offsets)?;
                let coverage = scope
                    .offset(coverage_offset)
                    .read_cache::<Coverage>(&mut cache.coverages.borrow_mut())?;
                let backtrack_classdef = scope
                    .offset(backtrack_classdef_offset)
                    .read_cache::<ClassDef>(&mut cache.classdefs.borrow_mut())?;
                let input_classdef = scope
                    .offset(input_classdef_offset)
                    .read_cache::<ClassDef>(&mut cache.classdefs.borrow_mut())?;
                let lookahead_classdef = scope
                    .offset(lookahead_classdef_offset)
                    .read_cache::<ClassDef>(&mut cache.classdefs.borrow_mut())?;
                Ok(ChainContextLookup::Format2 {
                    coverage,
                    backtrack_classdef,
                    input_classdef,
                    lookahead_classdef,
                    chainsubclasssets,
                    phantom: PhantomData,
                })
            }
            3 => {
                let backtrack_count = usize::from(ctxt.read_u16be()?);
                let backtrack_coverage_offsets = ctxt.read_array::<U16Be>(backtrack_count)?;
                let input_count = usize::from(ctxt.read_u16be()?);
                ctxt.check(input_count > 0)?;
                let input_coverage_offsets = ctxt.read_array::<U16Be>(input_count)?;
                let lookahead_count = usize::from(ctxt.read_u16be()?);
                let lookahead_coverage_offsets = ctxt.read_array::<U16Be>(lookahead_count)?;
                let lookup_count = usize::from(ctxt.read_u16be()?);
                let lookup_records = ctxt.read_array::<(U16Be, U16Be)>(lookup_count)?.to_vec();
                let backtrack_coverages =
                    read_coverages(&scope, Rc::clone(&cache), backtrack_coverage_offsets)?;
                let input_coverages =
                    read_coverages(&scope, Rc::clone(&cache), input_coverage_offsets)?;
                let lookahead_coverages =
                    read_coverages(&scope, Rc::clone(&cache), lookahead_coverage_offsets)?;
                Ok(ChainContextLookup::Format3 {
                    backtrack_coverages,
                    input_coverages,
                    lookahead_coverages,
                    lookup_records,
                    phantom: PhantomData,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a> ReadBinary<'a> for ChainSubRuleSet {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let chainsubrule_count = usize::from(ctxt.read_u16be()?);
        let chainsubrule_offsets = ctxt.read_array::<U16Be>(chainsubrule_count)?;
        let chainsubrules = read_objects::<ChainSubRule>(&scope, chainsubrule_offsets)?;
        Ok(ChainSubRuleSet { chainsubrules })
    }
}

impl<'a> ReadBinary<'a> for ChainSubRule {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let backtrack_count = usize::from(ctxt.read_u16be()?);
        let backtrack_sequence = ctxt.read_array::<U16Be>(backtrack_count)?.to_vec();
        let input_count = usize::from(ctxt.read_u16be()?);
        ctxt.check(input_count > 0)?;
        let input_sequence = ctxt.read_array::<U16Be>(input_count - 1)?.to_vec();
        let lookahead_count = usize::from(ctxt.read_u16be()?);
        let lookahead_sequence = ctxt.read_array::<U16Be>(lookahead_count)?.to_vec();
        let lookup_count = usize::from(ctxt.read_u16be()?);
        let lookup_records = ctxt.read_array::<(U16Be, U16Be)>(lookup_count)?.to_vec();
        Ok(ChainSubRule {
            backtrack_sequence,
            input_sequence,
            lookahead_sequence,
            lookup_records,
        })
    }
}

impl<'a> ReadBinary<'a> for ChainSubClassSet {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let scope = ctxt.scope();
        let chainsubclassrule_count = usize::from(ctxt.read_u16be()?);
        let chainsubclassrule_offsets = ctxt.read_array::<U16Be>(chainsubclassrule_count)?;
        let chainsubclassrules =
            read_objects::<ChainSubClassRule>(&scope, chainsubclassrule_offsets)?;
        Ok(ChainSubClassSet { chainsubclassrules })
    }
}

impl<'a> ReadBinary<'a> for ChainSubClassRule {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let backtrack_count = usize::from(ctxt.read_u16be()?);
        let backtrack_sequence = ctxt.read_array::<U16Be>(backtrack_count)?.to_vec();
        let input_count = usize::from(ctxt.read_u16be()?);
        ctxt.check(input_count > 0)?;
        let input_sequence = ctxt.read_array::<U16Be>(input_count - 1)?.to_vec();
        let lookahead_count = usize::from(ctxt.read_u16be()?);
        let lookahead_sequence = ctxt.read_array::<U16Be>(lookahead_count)?.to_vec();
        let lookup_count = usize::from(ctxt.read_u16be()?);
        let lookup_records = ctxt.read_array::<(U16Be, U16Be)>(lookup_count)?.to_vec();
        Ok(ChainSubClassRule {
            backtrack_sequence,
            input_sequence,
            lookahead_sequence,
            lookup_records,
        })
    }
}

pub fn context_lookup_info<'a, T, Table: LayoutTableType>(
    context_lookup: &'a ContextLookup<Table>,
    glyph: u16,
    f: impl Fn(&MatchContext<'a>) -> bool,
) -> Result<Option<Box<ContextLookupHelper<'a, T>>>, ParseError> {
    match context_lookup {
        ContextLookup::Format1 {
            coverage,
            subrulesets,
            phantom: _,
        } => match coverage.glyph_coverage_value(glyph) {
            Some(coverage_index) => {
                let coverage_index = usize::from(coverage_index);
                subrulesets.check_index(coverage_index)?;
                if let Some(ref subruleset) = subrulesets[coverage_index] {
                    for subrule in &subruleset.subrules {
                        let match_context = MatchContext {
                            backtrack_table: GlyphTable::Empty,
                            input_table: GlyphTable::ById(&subrule.input_sequence),
                            lookahead_table: GlyphTable::Empty,
                        };
                        if f(&match_context) {
                            let lookup =
                                ContextLookupHelper::new(match_context, &subrule.lookup_records);
                            return Ok(Some(Box::new(lookup)));
                        }
                    }
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        },
        ContextLookup::Format2 {
            coverage,
            classdef,
            subclasssets,
            phantom: _,
        } => match coverage.glyph_coverage_value(glyph) {
            Some(_coverage_index) => {
                let class_value = usize::from(classdef.glyph_class_value(glyph));
                subclasssets.check_index(class_value)?;
                if let Some(ref subclassset) = subclasssets[class_value] {
                    for subclassrule in &subclassset.subclassrules {
                        let match_context = MatchContext {
                            backtrack_table: GlyphTable::Empty,
                            input_table: GlyphTable::ByClassDef(
                                Rc::clone(classdef),
                                &subclassrule.input_sequence,
                            ),
                            lookahead_table: GlyphTable::Empty,
                        };
                        if f(&match_context) {
                            let lookup = ContextLookupHelper::new(
                                match_context,
                                &subclassrule.lookup_records,
                            );
                            return Ok(Some(Box::new(lookup)));
                        }
                    }
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        },
        ContextLookup::Format3 {
            coverages,
            lookup_records,
            phantom: _,
        } => {
            if coverages.len() > 0 {
                match coverages[0].glyph_coverage_value(glyph) {
                    Some(_coverage_index) => {
                        let match_context = MatchContext {
                            backtrack_table: GlyphTable::Empty,
                            input_table: GlyphTable::ByCoverage(&coverages[1..]),
                            lookahead_table: GlyphTable::Empty,
                        };
                        if f(&match_context) {
                            let lookup = ContextLookupHelper::new(match_context, lookup_records);
                            Ok(Some(Box::new(lookup)))
                        } else {
                            Ok(None)
                        }
                    }
                    None => Ok(None),
                }
            } else {
                Ok(None)
            }
        }
    }
}

pub fn chain_context_lookup_info<'a, T, Table: LayoutTableType>(
    chain_context_lookup: &'a ChainContextLookup<Table>,
    glyph: u16,
    f: impl Fn(&MatchContext<'a>) -> bool,
) -> Result<Option<Box<ContextLookupHelper<'a, T>>>, ParseError> {
    match chain_context_lookup {
        ChainContextLookup::Format1 {
            coverage,
            ref chainsubrulesets,
            phantom: _,
        } => match coverage.glyph_coverage_value(glyph) {
            Some(coverage_index) => {
                let coverage_index = usize::from(coverage_index);
                chainsubrulesets.check_index(coverage_index)?;
                if let Some(ref chainsubruleset) = chainsubrulesets[coverage_index] {
                    for chainsubrule in &chainsubruleset.chainsubrules {
                        let match_context = MatchContext {
                            backtrack_table: GlyphTable::ById(&chainsubrule.backtrack_sequence),
                            input_table: GlyphTable::ById(&chainsubrule.input_sequence),
                            lookahead_table: GlyphTable::ById(&chainsubrule.lookahead_sequence),
                        };
                        if f(&match_context) {
                            let lookup = ContextLookupHelper::new(
                                match_context,
                                &chainsubrule.lookup_records,
                            );
                            return Ok(Some(Box::new(lookup)));
                        }
                    }
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        },
        ChainContextLookup::Format2 {
            coverage,
            backtrack_classdef,
            input_classdef,
            lookahead_classdef,
            chainsubclasssets,
            phantom: _,
        } => match coverage.glyph_coverage_value(glyph) {
            Some(_coverage_index) => {
                let class_value = usize::from(input_classdef.glyph_class_value(glyph));
                chainsubclasssets.check_index(class_value)?;
                if let Some(ref chainsubclassset) = chainsubclasssets[class_value] {
                    for chainsubclassrule in &chainsubclassset.chainsubclassrules {
                        let match_context = MatchContext {
                            backtrack_table: GlyphTable::ByClassDef(
                                Rc::clone(backtrack_classdef),
                                &chainsubclassrule.backtrack_sequence,
                            ),
                            input_table: GlyphTable::ByClassDef(
                                Rc::clone(input_classdef),
                                &chainsubclassrule.input_sequence,
                            ),
                            lookahead_table: GlyphTable::ByClassDef(
                                Rc::clone(lookahead_classdef),
                                &chainsubclassrule.lookahead_sequence,
                            ),
                        };
                        if f(&match_context) {
                            let lookup = ContextLookupHelper::new(
                                match_context,
                                &chainsubclassrule.lookup_records,
                            );
                            return Ok(Some(Box::new(lookup)));
                        }
                    }
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        },
        ChainContextLookup::Format3 {
            backtrack_coverages,
            input_coverages,
            lookahead_coverages,
            lookup_records,
            phantom: _,
        } => match input_coverages[0].glyph_coverage_value(glyph) {
            Some(_coverage_index) => {
                let match_context = MatchContext {
                    backtrack_table: GlyphTable::ByCoverage(&backtrack_coverages),
                    input_table: GlyphTable::ByCoverage(&input_coverages[1..]),
                    lookahead_table: GlyphTable::ByCoverage(&lookahead_coverages),
                };
                if f(&match_context) {
                    let lookup = ContextLookupHelper::new(match_context, lookup_records);
                    Ok(Some(Box::new(lookup)))
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        },
    }
}

impl ReverseChainSingleSubst {
    /// Apply the substitution to the supplied glyph
    pub fn apply_glyph(
        &self,
        glyph: u16,
        f: impl Fn(&MatchContext<'_>) -> bool,
    ) -> Result<Option<u16>, ParseError> {
        match self {
            ReverseChainSingleSubst::Format1 {
                coverage,
                backtrack_coverages,
                lookahead_coverages,
                substitute_glyphs,
            } => match coverage.glyph_coverage_value(glyph) {
                Some(coverage_index) => {
                    let match_context = MatchContext {
                        backtrack_table: GlyphTable::ByCoverage(backtrack_coverages),
                        input_table: GlyphTable::Empty,
                        lookahead_table: GlyphTable::ByCoverage(lookahead_coverages),
                    };
                    if f(&match_context) {
                        let coverage_index = usize::from(coverage_index);
                        substitute_glyphs.check_index(coverage_index)?;
                        Ok(Some(substitute_glyphs[coverage_index]))
                    } else {
                        Ok(None)
                    }
                }
                None => Ok(None),
            },
        }
    }
}

pub enum Coverage {
    Format1 {
        glyph_array: Vec<u16>,
    },
    Format2 {
        coverage_range_array: Vec<CoverageRangeRecord>,
    },
}

pub struct CoverageRangeRecord {
    start_glyph: u16,
    end_glyph: u16,
    start_coverage_index: u16,
}

impl<'a> ReadFrom<'a> for CoverageRangeRecord {
    type ReadType = (U16Be, U16Be, U16Be);
    fn from((start_glyph, end_glyph, start_coverage_index): (u16, u16, u16)) -> Self {
        CoverageRangeRecord {
            start_glyph,
            end_glyph,
            start_coverage_index,
        }
    }
}

impl<'a> ReadBinary<'a> for Coverage {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        match ctxt.read_u16be()? {
            1 => {
                let glyph_count = ctxt.read_u16be()?;
                let glyph_array = ctxt.read_array::<U16Be>(usize::from(glyph_count))?;
                let glyph_vec = glyph_array.to_vec();
                // TODO consider verifying glyph_vec is sorted when is_sorted is stabilised
                // The glyph indices must be in numerical order for binary searching of the list.
                // https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-format-1
                // https://doc.rust-lang.org/std/primitive.slice.html#method.is_sorted
                Ok(Coverage::Format1 {
                    glyph_array: glyph_vec,
                })
            }
            2 => {
                let coverage_range_count = ctxt.read_u16be()?;
                let coverage_range_array =
                    ctxt.read_array::<CoverageRangeRecord>(usize::from(coverage_range_count))?;
                let coverage_range_vec = coverage_range_array.to_vec();
                for coverage_range_record in &coverage_range_vec {
                    ctxt.check(
                        coverage_range_record.start_glyph <= coverage_range_record.end_glyph,
                    )?
                }
                Ok(Coverage::Format2 {
                    coverage_range_array: coverage_range_vec,
                })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl Coverage {
    pub fn glyph_coverage_value(&self, glyph: u16) -> Option<u16> {
        match *self {
            Coverage::Format1 { ref glyph_array } => {
                // The glyph indices must be in numerical order for binary searching of the list.
                // https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-format-1
                if let Ok(index) = glyph_array.binary_search(&glyph) {
                    Some(index as u16)
                } else {
                    None
                }
            }
            Coverage::Format2 {
                ref coverage_range_array,
            } => {
                for coverage_range in coverage_range_array {
                    if (glyph >= coverage_range.start_glyph) && (glyph <= coverage_range.end_glyph)
                    {
                        return Some(
                            coverage_range.start_coverage_index
                                + (glyph - coverage_range.start_glyph),
                        );
                    }
                }
                None
            }
        }
    }

    /// Convenience method to count the total number of glyphs covered
    pub fn glyph_count(&self) -> usize {
        match self {
            Coverage::Format1 { glyph_array } => glyph_array.len(),
            Coverage::Format2 {
                coverage_range_array,
            } => coverage_range_array
                .iter()
                .fold(0, |acc, coverage_range_record| {
                    acc + (usize::from(coverage_range_record.end_glyph))
                        - (usize::from(coverage_range_record.start_glyph))
                        + 1
                }),
        }
    }
}

pub enum ClassDef {
    Format1 {
        start_glyph: u16,
        class_value_array: Vec<u16>,
    },
    Format2 {
        class_range_array: Vec<ClassRangeRecord>,
    },
}

pub struct ClassRangeRecord {
    start_glyph: u16,
    end_glyph: u16,
    class_value: u16,
}

impl<'a> ReadFrom<'a> for ClassRangeRecord {
    type ReadType = (U16Be, U16Be, U16Be);
    fn from((start_glyph, end_glyph, class_value): (u16, u16, u16)) -> Self {
        ClassRangeRecord {
            start_glyph,
            end_glyph,
            class_value,
        }
    }
}

impl<'a> ReadBinary<'a> for ClassDef {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        match ctxt.read_u16be()? {
            1 => {
                let start_glyph = ctxt.read_u16be()?;
                let glyph_count = ctxt.read_u16be()?;
                let class_value_array =
                    ctxt.read_array::<U16Be>(usize::from(glyph_count))?.to_vec();
                Ok(ClassDef::Format1 {
                    start_glyph,
                    class_value_array,
                })
            }
            2 => {
                let class_range_count = usize::from(ctxt.read_u16be()?);
                let class_range_array = ctxt
                    .read_array::<ClassRangeRecord>(class_range_count)
                    // In the lookahead classdef table for a chaining context substitution
                    // (Format 2; PSTS feature), the Mangal font specifies a class_range_count that
                    // exceeds the number of elements that can be contained in the class_range_array.
                    // We use this hack as a fallback to cap the length based on available bytes
                    .or_else(|_| ctxt.read_array_upto_hack::<ClassRangeRecord>(class_range_count))?
                    .to_vec();
                Ok(ClassDef::Format2 { class_range_array })
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl ClassDef {
    pub fn glyph_class_value(&self, glyph: u16) -> u16 {
        match *self {
            ClassDef::Format1 {
                start_glyph,
                ref class_value_array,
            } => {
                if (glyph >= start_glyph)
                    && (usize::from(glyph - start_glyph) < class_value_array.len())
                {
                    let class_index = glyph - start_glyph;
                    let class_value = class_value_array[usize::from(class_index)];
                    class_value
                } else {
                    0
                }
            }
            ClassDef::Format2 {
                ref class_range_array,
            } => {
                for class_range in class_range_array {
                    if (glyph >= class_range.start_glyph) && (glyph <= class_range.end_glyph) {
                        return class_range.class_value;
                    }
                }
                0
            }
        }
    }
}

pub type LayoutCache<T> = Rc<LayoutCacheData<T>>;

pub type LookupCache<T> = Vec<Option<Rc<LookupCacheItem<T>>>>;

pub struct LookupCacheItem<T> {
    pub lookup_flag: LookupFlag,
    pub lookup_subtables: T,
}

pub struct LayoutCacheData<T: LayoutTableType> {
    pub layout_table: LayoutTable<T>,
    coverages: RefCell<ReadCache<Coverage>>,
    classdefs: RefCell<ReadCache<ClassDef>>,
    lookup_cache: RefCell<LookupCache<T::LookupType>>,

    /// maps (script_tag, opt_lang_tag) to FeatureMask
    /// opt_lang_tag = None is represented as `DFLT`
    pub supported_features: RefCell<HashMap<(u32, u32), u64>>,

    /// maps (script_tag, lang_tag, FeatureMask) to cached_lookups index
    pub lookups_index: RefCell<HashMap<(u32, u32, u64), usize>>,

    pub cached_lookups: RefCell<Vec<Vec<(usize, u32)>>>,
}

pub fn new_layout_cache<T: LayoutTableType>(layout_table: LayoutTable<T>) -> LayoutCache<T> {
    let coverages = RefCell::new(ReadCache::new());
    let classdefs = RefCell::new(ReadCache::new());
    let lookup_cache = RefCell::new(Vec::new());
    let supported_features = RefCell::new(HashMap::new());
    let lookups_index = RefCell::new(HashMap::new());
    let cached_lookups = RefCell::new(vec![Vec::new()]);
    Rc::new(LayoutCacheData {
        layout_table,
        coverages,
        classdefs,
        lookup_cache,
        supported_features,
        lookups_index,
        cached_lookups,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::write::{WriteBinary, WriteBuffer};

    fn make_gdef_header(glyph_classdef_offset: u16) -> Vec<u8> {
        let mut data = vec![
            0x00, 0x01, // major version
            0x00, 0x00, // minor version
        ];
        data.extend_from_slice(&glyph_classdef_offset.to_be_bytes());
        data.extend_from_slice(&[
            0x00, 0x00, // glyph classdef offset
            0x00, 0x00, // attach list offset
            0x00, 0x00, // lig caret list offset
            0x00, 0x00, // mark attach classdef offset
        ]);

        data
    }

    #[test]
    fn test_read_gdef_zero_classdef_offset() {
        let data = make_gdef_header(0);
        let gdef = ReadScope::new(&data).read::<GDEFTable>().unwrap();
        assert!(gdef.opt_glyph_classdef.is_none());
    }

    #[test]
    fn test_read_gdef_too_small_classdef_offset() {
        // Offset is not past the end of the header
        let data = make_gdef_header(1);
        let gdef = ReadScope::new(&data).read::<GDEFTable>().unwrap();
        assert!(gdef.opt_glyph_classdef.is_none());
    }

    #[test]
    fn test_read_gdef_too_big_classdef_offset() {
        // Offset past the end of the table
        let data = make_gdef_header(1000);
        match ReadScope::new(&data).read::<GDEFTable>() {
            Ok(_) => panic!("expected error got success"),
            Err(ParseError::BadEof) => {}
            Err(err) => panic!("expeceted ParseError::BadEof got {:?}", err),
        }
    }

    #[test]
    fn read_gpos_v1_x() {
        let mut w = WriteBuffer::new();
        U16Be::write(&mut w, 1u16).unwrap(); // major version
        U16Be::write(&mut w, 2u16).unwrap(); // minor version
        U16Be::write(&mut w, 0u16).unwrap(); // script_list_offset
        U16Be::write(&mut w, 0u16).unwrap(); // feature_list_offset
        U16Be::write(&mut w, 0u16).unwrap(); // lookup_list_offset
        let data = w.into_inner();
        assert!(ReadScope::new(&data).read::<LayoutTable<GPOS>>().is_ok())
    }
}
