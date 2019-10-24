//! Glyph substitution (`gsub`) implementation.
//!
//! > The Glyph Substitution (GSUB) table provides data for substition of glyphs for appropriate
//! > rendering of scripts, such as cursively-connecting forms in Arabic script, or for advanced
//! > typographic effects, such as ligatures.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/gsub>

use std::u16;

use crate::context::{ContextLookupHelper, Glyph, GlyphTable, MatchType};
use crate::error::{ParseError, ShapingError};
use crate::indic;
use crate::layout::{
    chain_context_lookup_info, context_lookup_info, AlternateSet, AlternateSubst,
    ChainContextLookup, ContextLookup, GDEFTable, LangSys, LayoutCache, LayoutTable, Ligature,
    LigatureSubst, LookupCacheItem, LookupList, MultipleSubst, SequenceTable, SingleSubst,
    SubstLookup, GSUB,
};
use crate::opentype;
use crate::tag;
use std::collections::hash_map::Entry;
use std::collections::BTreeMap;

const SUBST_RECURSION_LIMIT: usize = 2;

pub struct FeatureInfo {
    pub feature_tag: u32,
    pub alternate: Option<usize>,
}

type SubstContext<'a> = ContextLookupHelper<'a, GSUB>;

impl Ligature {
    pub fn matches<T>(
        &self,
        match_type: MatchType,
        opt_gdef_table: Option<&GDEFTable>,
        i: usize,
        glyphs: &[RawGlyph<T>],
    ) -> bool {
        let mut last_index = 0;
        match_type.match_front(
            opt_gdef_table,
            &GlyphTable::ById(&self.component_glyphs),
            glyphs,
            i,
            &mut last_index,
        )
    }

    pub fn apply<T: GlyphData>(
        &self,
        match_type: MatchType,
        opt_gdef_table: Option<&GDEFTable>,
        i: usize,
        glyphs: &mut Vec<RawGlyph<T>>,
    ) -> usize {
        let mut index = i + 1;
        let mut matched = 0;
        let mut skip = 0;
        while matched < self.component_glyphs.len() {
            if index < glyphs.len() {
                if match_type.match_glyph(opt_gdef_table, &glyphs[index]) {
                    matched += 1;
                    let mut unicodes = glyphs[index].unicodes.clone();
                    let extra_data = glyphs[index].extra_data.clone();
                    glyphs[i].unicodes.append(&mut unicodes);
                    glyphs[i].extra_data =
                        GlyphData::merge(glyphs[i].extra_data.clone(), extra_data);
                    glyphs.remove(index);
                } else {
                    glyphs[index].liga_component_pos = matched as u16;
                    skip += 1;
                    index += 1;
                }
            } else {
                panic!("ran out of glyphs");
            }
        }
        while index < glyphs.len()
            && MatchType::marks_only().match_glyph(opt_gdef_table, &glyphs[index])
        {
            glyphs[index].liga_component_pos = matched as u16;
            index += 1;
        }
        glyphs[i].glyph_index = Some(self.ligature_glyph);
        glyphs[i].glyph_origin = GlyphOrigin::Direct;
        skip
    }
}

#[derive(Clone, Debug)]
pub struct RawGlyph<T> {
    // TODO: Consider if there is a better representation for unicodes
    pub unicodes: Vec<char>,
    pub glyph_index: Option<u16>,
    pub liga_component_pos: u16,
    pub glyph_origin: GlyphOrigin,
    pub small_caps: bool,
    pub multi_subst_dup: bool,
    pub is_vert_alt: bool,
    pub fake_bold: bool,
    pub fake_italic: bool,
    pub extra_data: T,
}

/// `merge` is called during ligature substitution (i.e. merging of glyphs),
/// and determines how the `RawGlyph.extra_data` field should be merged
pub trait GlyphData: Clone {
    fn merge(data1: Self, data2: Self) -> Self;
}

impl GlyphData for () {
    fn merge(_data1: (), _data2: ()) {}
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum GlyphOrigin {
    Char(char),
    Direct,
}

impl<T> Glyph for RawGlyph<T> {
    fn get_glyph_index(&self) -> Option<u16> {
        self.glyph_index
    }
}

pub fn gsub_feature_would_apply<T: GlyphData>(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    langsys: &LangSys,
    feature_tag: u32,
    glyphs: &[RawGlyph<T>],
    i: usize,
) -> Result<bool, ParseError> {
    if let Some(feature_table) = gsub_table.find_langsys_feature(langsys, feature_tag)? {
        if let Some(ref lookup_list) = gsub_table.opt_lookup_list {
            for lookup_index in &feature_table.lookup_indices {
                let lookup_index = usize::from(*lookup_index);
                let lookup_cache_item = lookup_list.lookup_cache_gsub(gsub_cache, lookup_index)?;
                if gsub_lookup_would_apply(opt_gdef_table, &lookup_cache_item, glyphs, i)? {
                    return Ok(true);
                }
            }
        }
    }
    Ok(false)
}

pub fn gsub_lookup_would_apply<T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    lookup: &LookupCacheItem<SubstLookup>,
    glyphs: &[RawGlyph<T>],
    i: usize,
) -> Result<bool, ParseError> {
    let mut changes = false;
    let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
    if i < glyphs.len() && match_type.match_glyph(opt_gdef_table, &glyphs[i]) {
        match lookup.lookup_subtables {
            SubstLookup::SingleSubst(ref subtables) => {
                match singlesubst_would_apply(&subtables, i, glyphs)? {
                    Some(_output_glyph) => changes = true,
                    None => {}
                }
            }
            SubstLookup::MultipleSubst(ref subtables) => {
                match multiplesubst_would_apply(&subtables, i, glyphs)? {
                    Some(_sequence_table) => changes = true,
                    None => {}
                }
            }
            SubstLookup::AlternateSubst(ref subtables) => {
                match alternatesubst_would_apply(&subtables, i, glyphs)? {
                    Some(_alternate_set) => changes = true,
                    None => {}
                }
            }
            SubstLookup::LigatureSubst(ref subtables) => {
                match ligaturesubst_would_apply(opt_gdef_table, &subtables, match_type, i, glyphs)?
                {
                    Some(_ligature) => changes = true,
                    None => {}
                }
            }
            SubstLookup::ContextSubst(ref subtables) => {
                match contextsubst_would_apply(opt_gdef_table, &subtables, match_type, i, glyphs)? {
                    Some(_subst) => changes = true,
                    None => {}
                }
            }
            SubstLookup::ChainContextSubst(ref subtables) => {
                match chaincontextsubst_would_apply(
                    opt_gdef_table,
                    &subtables,
                    match_type,
                    i,
                    glyphs,
                )? {
                    Some(_subst) => changes = true,
                    None => {}
                }
            }
            _ => {}
        }
    }
    Ok(changes)
}

pub fn gsub_apply_lookup<T: GlyphData>(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    lookup_index: usize,
    feature_tag: u32,
    opt_alternate: Option<usize>,
    glyphs: &mut Vec<RawGlyph<T>>,
    pred: impl Fn(&RawGlyph<T>) -> bool,
) -> Result<(), ParseError> {
    if let Some(ref lookup_list) = gsub_table.opt_lookup_list {
        let lookup = lookup_list.lookup_cache_gsub(gsub_cache, lookup_index)?;
        let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
        let start = if feature_tag == tag::FINA {
            glyphs.len() - 1
        } else {
            0
        };
        match lookup.lookup_subtables {
            SubstLookup::SingleSubst(ref subtables) => {
                for i in start..glyphs.len() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        singlesubst(&subtables, feature_tag, i, glyphs)?;
                    }
                }
            }
            SubstLookup::MultipleSubst(ref subtables) => {
                let mut i = start;
                while i < glyphs.len() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        match multiplesubst(&subtables, i, glyphs)? {
                            Some(replace_count) => {
                                i += replace_count;
                            }
                            None => i += 1,
                        }
                    } else {
                        i += 1;
                    }
                }
            }
            SubstLookup::AlternateSubst(ref subtables) => {
                for i in start..glyphs.len() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        let alternate = opt_alternate.unwrap_or(0);
                        alternatesubst(&subtables, alternate, i, glyphs)?;
                    }
                }
            }
            SubstLookup::LigatureSubst(ref subtables) => {
                let mut i = start;
                while i < glyphs.len() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        match ligaturesubst(opt_gdef_table, &subtables, match_type, i, glyphs)? {
                            Some((_removed_count, skip_count)) => {
                                i += skip_count + 1;
                            }
                            None => i += 1,
                        }
                    } else {
                        i += 1;
                    }
                }
            }
            SubstLookup::ContextSubst(ref subtables) => {
                let mut i = start;
                while i < glyphs.len() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        match contextsubst(
                            SUBST_RECURSION_LIMIT,
                            gsub_cache,
                            lookup_list,
                            opt_gdef_table,
                            &subtables,
                            feature_tag,
                            match_type,
                            i,
                            glyphs,
                        )? {
                            Some((length, _changes)) => {
                                i += length;
                            }
                            None => i += 1,
                        }
                    } else {
                        i += 1;
                    }
                }
            }
            SubstLookup::ChainContextSubst(ref subtables) => {
                let mut i = start;
                while i < glyphs.len() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        match chaincontextsubst(
                            SUBST_RECURSION_LIMIT,
                            gsub_cache,
                            lookup_list,
                            opt_gdef_table,
                            &subtables,
                            feature_tag,
                            match_type,
                            i,
                            glyphs,
                        )? {
                            Some((length, _changes)) => {
                                i += length;
                            }
                            None => i += 1,
                        }
                    } else {
                        i += 1;
                    }
                }
            }
            // TODO implement support for reverse chain single subst gsub
            SubstLookup::ReverseChainSingleSubst => {}
        }
    }
    Ok(())
}

fn singlesubst_would_apply<T: GlyphData>(
    subtables: &[SingleSubst],
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<u16>, ParseError> {
    if let Some(glyph_index) = glyphs[i].glyph_index {
        for single_subst in subtables {
            if let Some(glyph_index) = single_subst.apply_glyph(glyph_index)? {
                return Ok(Some(glyph_index));
            }
        }
    }
    Ok(None)
}

fn singlesubst<T: GlyphData>(
    subtables: &[SingleSubst],
    subst_tag: u32,
    i: usize,
    glyphs: &mut [RawGlyph<T>],
) -> Result<(), ParseError> {
    if let Some(output_glyph) = singlesubst_would_apply(subtables, i, glyphs)? {
        glyphs[i].glyph_index = Some(output_glyph);
        glyphs[i].glyph_origin = GlyphOrigin::Direct;
        if subst_tag == tag::VERT || subst_tag == tag::VRT2 {
            glyphs[i].is_vert_alt = true;
        }
    }
    Ok(())
}

fn multiplesubst_would_apply<'a, T: GlyphData>(
    subtables: &'a [MultipleSubst],
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<&'a SequenceTable>, ParseError> {
    if let Some(glyph_index) = glyphs[i].glyph_index {
        for multiple_subst in subtables {
            if let Some(sequence_table) = multiple_subst.apply_glyph(glyph_index)? {
                return Ok(Some(sequence_table));
            }
        }
    }
    Ok(None)
}

fn multiplesubst<T: GlyphData>(
    subtables: &[MultipleSubst],
    i: usize,
    glyphs: &mut Vec<RawGlyph<T>>,
) -> Result<Option<usize>, ParseError> {
    match multiplesubst_would_apply(subtables, i, glyphs)? {
        Some(sequence_table) => {
            if sequence_table.substitute_glyphs.len() > 0 {
                let first_glyph_index = sequence_table.substitute_glyphs[0];
                glyphs[i].glyph_index = Some(first_glyph_index);
                glyphs[i].glyph_origin = GlyphOrigin::Direct;
                for j in 1..sequence_table.substitute_glyphs.len() {
                    let output_glyph_index = sequence_table.substitute_glyphs[j];
                    let glyph = RawGlyph {
                        unicodes: glyphs[i].unicodes.clone(),
                        glyph_index: Some(output_glyph_index),
                        liga_component_pos: 0, //glyphs[i].liga_component_pos,
                        glyph_origin: GlyphOrigin::Direct,
                        small_caps: glyphs[i].small_caps,
                        multi_subst_dup: true,
                        is_vert_alt: glyphs[i].is_vert_alt,
                        fake_bold: glyphs[i].fake_bold,
                        fake_italic: glyphs[i].fake_italic,
                        extra_data: glyphs[i].extra_data.clone(),
                    };
                    glyphs.insert(i + j, glyph);
                }
                Ok(Some(sequence_table.substitute_glyphs.len()))
            } else {
                // the spec forbids this, but implementations all allow it
                glyphs.remove(i);
                Ok(Some(0))
            }
        }
        None => Ok(None),
    }
}

fn alternatesubst_would_apply<'a, T: GlyphData>(
    subtables: &'a [AlternateSubst],
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<&'a AlternateSet>, ParseError> {
    if let Some(glyph_index) = glyphs[i].glyph_index {
        for alternate_subst in subtables {
            if let Some(alternate_set) = alternate_subst.apply_glyph(glyph_index)? {
                return Ok(Some(alternate_set));
            }
        }
    }
    Ok(None)
}

fn alternatesubst<T: GlyphData>(
    subtables: &[AlternateSubst],
    alternate: usize,
    i: usize,
    glyphs: &mut [RawGlyph<T>],
) -> Result<(), ParseError> {
    if let Some(alternateset) = alternatesubst_would_apply(subtables, i, glyphs)? {
        // TODO allow users to specify which alternate glyph they want
        if alternate < alternateset.alternate_glyphs.len() {
            glyphs[i].glyph_index = Some(alternateset.alternate_glyphs[alternate]);
            glyphs[i].glyph_origin = GlyphOrigin::Direct;
        }
    }
    Ok(())
}

fn ligaturesubst_would_apply<'a, T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &'a [LigatureSubst],
    match_type: MatchType,
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<&'a Ligature>, ParseError> {
    if let Some(glyph_index) = glyphs[i].glyph_index {
        for ligature_subst in subtables {
            if let Some(ligatureset) = ligature_subst.apply_glyph(glyph_index)? {
                for ligature in &ligatureset.ligatures {
                    if ligature.matches(match_type, opt_gdef_table, i, glyphs) {
                        return Ok(Some(ligature));
                    }
                }
            }
        }
    }
    Ok(None)
}

fn ligaturesubst<T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &[LigatureSubst],
    match_type: MatchType,
    i: usize,
    glyphs: &mut Vec<RawGlyph<T>>,
) -> Result<Option<(usize, usize)>, ParseError> {
    match ligaturesubst_would_apply(opt_gdef_table, subtables, match_type, i, glyphs)? {
        Some(ligature) => Ok(Some((
            ligature.component_glyphs.len(),
            ligature.apply(match_type, opt_gdef_table, i, glyphs),
        ))),
        None => Ok(None),
    }
}

fn contextsubst_would_apply<'a, T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &'a [ContextLookup<GSUB>],
    match_type: MatchType,
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<Box<SubstContext<'a>>>, ParseError> {
    if let Some(glyph_index) = glyphs[i].get_glyph_index() {
        for context_lookup in subtables {
            if let Some(context) = context_lookup_info(&context_lookup, glyph_index, |context| {
                context.matches(opt_gdef_table, match_type, glyphs, i)
            })? {
                return Ok(Some(context));
            }
        }
    }
    Ok(None)
}

fn contextsubst<'a, T: GlyphData>(
    recursion_limit: usize,
    gsub_cache: &LayoutCache<GSUB>,
    lookup_list: &LookupList<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &[ContextLookup<GSUB>],
    feature_tag: u32,
    match_type: MatchType,
    i: usize,
    glyphs: &mut Vec<RawGlyph<T>>,
) -> Result<Option<(usize, isize)>, ParseError> {
    match contextsubst_would_apply(opt_gdef_table, subtables, match_type, i, glyphs)? {
        Some(subst) => apply_subst_context(
            recursion_limit,
            gsub_cache,
            lookup_list,
            opt_gdef_table,
            feature_tag,
            match_type,
            &subst,
            i,
            glyphs,
        ),
        None => Ok(None),
    }
}

fn chaincontextsubst_would_apply<'a, T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &'a [ChainContextLookup<GSUB>],
    match_type: MatchType,
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<Box<SubstContext<'a>>>, ParseError> {
    if let Some(glyph_index) = glyphs[i].get_glyph_index() {
        for chain_context_lookup in subtables {
            if let Some(context) =
                chain_context_lookup_info(&chain_context_lookup, glyph_index, |context| {
                    context.matches(opt_gdef_table, match_type, glyphs, i)
                })?
            {
                return Ok(Some(context));
            }
        }
    }
    Ok(None)
}

fn chaincontextsubst<'a, T: GlyphData>(
    recursion_limit: usize,
    gsub_cache: &LayoutCache<GSUB>,
    lookup_list: &LookupList<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &[ChainContextLookup<GSUB>],
    feature_tag: u32,
    match_type: MatchType,
    i: usize,
    glyphs: &mut Vec<RawGlyph<T>>,
) -> Result<Option<(usize, isize)>, ParseError> {
    match chaincontextsubst_would_apply(opt_gdef_table, subtables, match_type, i, glyphs)? {
        Some(subst) => apply_subst_context(
            recursion_limit,
            gsub_cache,
            lookup_list,
            opt_gdef_table,
            feature_tag,
            match_type,
            &subst,
            i,
            glyphs,
        ),
        None => Ok(None),
    }
}

fn apply_subst_context<'a, T: GlyphData>(
    recursion_limit: usize,
    gsub_cache: &LayoutCache<GSUB>,
    lookup_list: &LookupList<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    feature_tag: u32,
    match_type: MatchType,
    subst: &SubstContext<'_>,
    i: usize,
    glyphs: &mut Vec<RawGlyph<T>>,
) -> Result<Option<(usize, isize)>, ParseError> {
    let mut changes = 0;
    let len = match match_type.find_nth(
        opt_gdef_table,
        glyphs,
        i,
        subst.match_context.input_table.len(),
    ) {
        Some(last) => last - i + 1,
        None => return Ok(None), // FIXME actually an error/impossible?
    };
    for (subst_index, subst_lookup_index) in subst.lookup_array {
        match apply_subst(
            recursion_limit,
            gsub_cache,
            lookup_list,
            opt_gdef_table,
            match_type,
            usize::from(*subst_index),
            usize::from(*subst_lookup_index),
            feature_tag,
            glyphs,
            i,
        )? {
            Some(changes0) => changes += changes0,
            None => {}
        }
    }
    let new_len = (len as isize) + changes;
    assert!(new_len >= 0, "apply_subst_list: len < 0");
    Ok(Some((new_len as usize, changes)))
}

fn apply_subst<'a, T: GlyphData>(
    recursion_limit: usize,
    gsub_cache: &LayoutCache<GSUB>,
    lookup_list: &LookupList<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    parent_match_type: MatchType,
    subst_index: usize,
    lookup_index: usize,
    feature_tag: u32,
    glyphs: &mut Vec<RawGlyph<T>>,
    index: usize,
) -> Result<Option<isize>, ParseError> {
    let lookup = lookup_list.lookup_cache_gsub(gsub_cache, lookup_index)?;
    let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
    let i = match parent_match_type.find_nth(opt_gdef_table, glyphs, index, subst_index) {
        Some(index1) => index1,
        None => return Ok(None), // FIXME error?
    };
    match lookup.lookup_subtables {
        SubstLookup::SingleSubst(ref subtables) => {
            singlesubst(&subtables, feature_tag, i, glyphs)?;
            Ok(Some(0))
        }
        SubstLookup::MultipleSubst(ref subtables) => match multiplesubst(&subtables, i, glyphs)? {
            Some(replace_count) => Ok(Some((replace_count as isize) - 1)),
            None => Ok(None),
        },
        SubstLookup::AlternateSubst(ref subtables) => {
            alternatesubst(&subtables, 0, i, glyphs)?;
            Ok(Some(0))
        }
        SubstLookup::LigatureSubst(ref subtables) => {
            match ligaturesubst(opt_gdef_table, &subtables, match_type, i, glyphs)? {
                Some((removed_count, _skip_count)) => Ok(Some(-(removed_count as isize))),
                None => Ok(None), // FIXME error?
            }
        }
        SubstLookup::ContextSubst(ref subtables) => {
            if recursion_limit > 0 {
                match contextsubst(
                    recursion_limit - 1,
                    gsub_cache,
                    lookup_list,
                    opt_gdef_table,
                    &subtables,
                    feature_tag,
                    match_type,
                    i,
                    glyphs,
                )? {
                    Some((_length, change)) => Ok(Some(change)),
                    None => Ok(None),
                }
            } else {
                Err(ParseError::LimitExceeded)
            }
        }
        SubstLookup::ChainContextSubst(ref subtables) => {
            if recursion_limit > 0 {
                match chaincontextsubst(
                    recursion_limit - 1,
                    gsub_cache,
                    lookup_list,
                    opt_gdef_table,
                    &subtables,
                    feature_tag,
                    match_type,
                    i,
                    glyphs,
                )? {
                    Some((_length, change)) => Ok(Some(change)),
                    None => Ok(None),
                }
            } else {
                Err(ParseError::LimitExceeded)
            }
        }
        // TODO implement support for reverse chain single subst gsub
        SubstLookup::ReverseChainSingleSubst => Ok(None),
    }
}

fn build_lookups_custom(
    gsub_table: &LayoutTable<GSUB>,
    langsys: &LangSys,
    feature_tags: &[FeatureInfo],
) -> Result<BTreeMap<usize, u32>, ParseError> {
    let mut lookups = BTreeMap::new();
    for feature_info in feature_tags {
        if let Some(feature_table) =
            gsub_table.find_langsys_feature(langsys, feature_info.feature_tag)?
        {
            for lookup_index in &feature_table.lookup_indices {
                lookups.insert(usize::from(*lookup_index), feature_info.feature_tag);
            }
        }
    }
    Ok(lookups)
}

pub fn build_lookups_default(
    gsub_table: &LayoutTable<GSUB>,
    langsys: &LangSys,
    feature_tags: &[u32],
) -> Result<Vec<(usize, u32)>, ParseError> {
    let mut lookups = BTreeMap::new();
    for feature_tag in feature_tags {
        if let Some(feature_table) = gsub_table.find_langsys_feature(langsys, *feature_tag)? {
            for lookup_index in &feature_table.lookup_indices {
                lookups.insert(usize::from(*lookup_index), *feature_tag);
            }
        } else if *feature_tag == tag::VRT2 {
            let vert_tag = tag::VERT;
            if let Some(feature_table) = gsub_table.find_langsys_feature(langsys, vert_tag)? {
                for lookup_index in &feature_table.lookup_indices {
                    lookups.insert(usize::from(*lookup_index), vert_tag);
                }
            }
        }
    }

    // note: iter() returns sorted by key
    //Ok(lookups.iter().map(|(k, v)| (*k, *v)).collect())
    Ok(lookups.into_iter().collect())
}

fn find_alternate(features_list: &[FeatureInfo], feature_tag: u32) -> Option<usize> {
    for feature_info in features_list {
        if feature_info.feature_tag == feature_tag {
            return feature_info.alternate;
        }
    }
    None
}

pub fn gsub_apply_custom<T: GlyphData>(
    gsub_cache: &LayoutCache<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    lang_tag: u32,
    features_list: &[FeatureInfo],
    num_glyphs: u16,
    glyphs: &mut Vec<RawGlyph<T>>,
) -> Result<(), ShapingError> {
    let gsub_table = &gsub_cache.layout_table;
    if let Some(script) = gsub_table.find_script_or_default(script_tag)? {
        if let Some(langsys) = script.find_langsys_or_default(lang_tag)? {
            let lookups = build_lookups_custom(&gsub_table, &langsys, &features_list)?;

            // note: iter() returns sorted by key
            for (lookup_index, feature_tag) in lookups {
                let alternate = find_alternate(&features_list, feature_tag);
                gsub_apply_lookup(
                    gsub_cache,
                    &gsub_table,
                    opt_gdef_table,
                    lookup_index,
                    feature_tag,
                    alternate,
                    glyphs,
                    |_| true,
                )?;
            }
        }
    }
    replace_missing_glyphs(glyphs, num_glyphs);
    Ok(())
}

pub fn replace_missing_glyphs<T: GlyphData>(glyphs: &mut Vec<RawGlyph<T>>, num_glyphs: u16) {
    for glyph in glyphs.iter_mut() {
        if let Some(glyph_index) = glyph.glyph_index {
            if glyph_index >= num_glyphs {
                glyph.unicodes = vec![];
                glyph.glyph_index = Some(0);
                glyph.liga_component_pos = 0;
                glyph.glyph_origin = GlyphOrigin::Direct;
                glyph.small_caps = false;
                glyph.multi_subst_dup = false;
                glyph.is_vert_alt = false;
                glyph.fake_bold = false;
                glyph.fake_italic = false;
            }
        }
    }
}

fn strip_joiners<T: GlyphData>(glyphs: &mut Vec<RawGlyph<T>>) {
    glyphs.retain(|g| match g.glyph_origin {
        GlyphOrigin::Char('\u{200C}') => false,
        GlyphOrigin::Char('\u{200D}') => false,
        _ => true,
    })
}

// Specialised to allow conversion between RawGlyphIndic and RawGlyph<()> types.
// Is there a better way to do this?
pub fn gsub_apply_default<'data>(
    make_dotted_circle: &impl Fn() -> Vec<RawGlyph<()>>,
    gsub_cache: &LayoutCache<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    lang_tag: u32,
    vertical: bool,
    num_glyphs: u16,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    let gsub_table = &gsub_cache.layout_table;
    if opentype::is_indic_script_tag(script_tag) {
        indic::gsub_apply_indic(
            make_dotted_circle,
            gsub_cache,
            &gsub_table,
            opt_gdef_table,
            script_tag,
            lang_tag,
            glyphs,
        )?;
    } else {
        if script_tag == tag::ARAB || script_tag == tag::SYRC {
            // TODO implement arabic shaping
            // Currently still calls our Mercury shaping code.
            // See fonts/fonts.m -> map_glyphs_shaping
        } else {
            if vertical {
                if let Some(script) = gsub_table.find_script_or_default(script_tag)? {
                    if let Some(langsys) = script.find_langsys_or_default(lang_tag)? {
                        // will try vert if vrt2 is not found
                        let feature_tags = &[
                            tag::VRT2,
                            tag::CCMP,
                            tag::LIGA,
                            tag::CLIG,
                            tag::RLIG,
                            tag::CALT,
                        ];
                        let lookups = build_lookups_default(&gsub_table, &langsys, feature_tags)?;
                        gsub_apply_lookups(
                            gsub_cache,
                            gsub_table,
                            opt_gdef_table,
                            &lookups,
                            glyphs,
                        )?;
                    }
                }
            } else {
                match gsub_cache
                    .default_lookups
                    .borrow_mut()
                    .entry((script_tag, lang_tag))
                {
                    Entry::Occupied(entry) => {
                        let lookups = entry.get();
                        gsub_apply_lookups(
                            gsub_cache,
                            gsub_table,
                            opt_gdef_table,
                            lookups,
                            glyphs,
                        )?;
                    }
                    Entry::Vacant(entry) => {
                        if let Some(script) = gsub_table.find_script_or_default(script_tag)? {
                            if let Some(langsys) = script.find_langsys_or_default(lang_tag)? {
                                let feature_tags =
                                    &[tag::CCMP, tag::LIGA, tag::CLIG, tag::RLIG, tag::CALT];
                                let lookups =
                                    build_lookups_default(&gsub_table, &langsys, feature_tags)?;
                                let lookups = entry.insert(lookups);
                                gsub_apply_lookups(
                                    gsub_cache,
                                    gsub_table,
                                    opt_gdef_table,
                                    &lookups,
                                    glyphs,
                                )?;
                            }
                        }
                    }
                }
            }
        }
    };

    strip_joiners(glyphs);
    replace_missing_glyphs(glyphs, num_glyphs);
    Ok(())
}

fn gsub_apply_lookups(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    lookups: &[(usize, u32)],
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    for (lookup_index, feature_tag) in lookups {
        gsub_apply_lookup(
            &gsub_cache,
            &gsub_table,
            opt_gdef_table,
            *lookup_index,
            *feature_tag,
            None,
            glyphs,
            |_| true,
        )?;
    }
    Ok(())
}
