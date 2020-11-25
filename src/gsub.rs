//! Glyph substitution (`gsub`) implementation.
//!
//! > The Glyph Substitution (GSUB) table provides data for substition of glyphs for appropriate
//! > rendering of scripts, such as cursively-connecting forms in Arabic script, or for advanced
//! > typographic effects, such as ligatures.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/gsub>

use std::collections::hash_map::Entry;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::u16;

use bitflags::bitflags;
use tinyvec::{tiny_vec, TinyVec};

use crate::context::{ContextLookupHelper, Glyph, GlyphTable, MatchType};
use crate::error::{ParseError, ShapingError};
use crate::layout::{
    chain_context_lookup_info, context_lookup_info, AlternateSet, AlternateSubst,
    ChainContextLookup, ContextLookup, GDEFTable, LangSys, LayoutCache, LayoutTable, Ligature,
    LigatureSubst, LookupCacheItem, LookupList, MultipleSubst, ReverseChainSingleSubst,
    SequenceTable, SingleSubst, SubstLookup, GSUB,
};
use crate::scripts;
use crate::scripts::ScriptType;
use crate::tag;
use crate::unicode::VariationSelector;

const SUBST_RECURSION_LIMIT: usize = 2;

pub struct FeatureInfo {
    pub feature_tag: u32,
    pub alternate: Option<usize>,
}

/// Type indicating the features to use when shaping text.
pub enum Features {
    /// A custom feature list.
    ///
    /// Only the supplied features will be applied when applying glyph substitutions (gsub).
    Custom(Vec<FeatureInfo>),
    /// A mask of features to enable.
    ///
    /// Unless you have a specific need for low-level control of the OpenType features to enable
    /// this variant should be preferred.
    ///
    /// Enabled bits will be used to enable OpenType features when applying glyph substitution
    /// (gsub). When this variant of the `Features` enum is used some common features are enabled
    /// by default based on the script and language.
    Mask(GsubFeatureMask),
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
                    let mut matched_glyph = glyphs.remove(index);
                    glyphs[i].unicodes.append(&mut matched_glyph.unicodes);
                    glyphs[i].extra_data =
                        GlyphData::merge(glyphs[i].extra_data.clone(), matched_glyph.extra_data);
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
        glyphs[i].glyph_index = self.ligature_glyph;
        glyphs[i].glyph_origin = GlyphOrigin::Direct;
        skip
    }
}

#[derive(Clone, Debug)]
pub struct RawGlyph<T> {
    pub unicodes: TinyVec<[char; 1]>,
    pub glyph_index: u16,
    pub liga_component_pos: u16,
    pub glyph_origin: GlyphOrigin,
    pub small_caps: bool,
    pub multi_subst_dup: bool,
    pub is_vert_alt: bool,
    pub fake_bold: bool,
    pub fake_italic: bool,
    pub variation: Option<VariationSelector>,
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
    fn get_glyph_index(&self) -> u16 {
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
    let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
    if i < glyphs.len() && match_type.match_glyph(opt_gdef_table, &glyphs[i]) {
        return match lookup.lookup_subtables {
            SubstLookup::SingleSubst(ref subtables) => {
                match singlesubst_would_apply(&subtables, &glyphs[i])? {
                    Some(_output_glyph) => Ok(true),
                    None => Ok(false),
                }
            }
            SubstLookup::MultipleSubst(ref subtables) => {
                match multiplesubst_would_apply(&subtables, i, glyphs)? {
                    Some(_sequence_table) => Ok(true),
                    None => Ok(false),
                }
            }
            SubstLookup::AlternateSubst(ref subtables) => {
                match alternatesubst_would_apply(&subtables, &glyphs[i])? {
                    Some(_alternate_set) => Ok(true),
                    None => Ok(false),
                }
            }
            SubstLookup::LigatureSubst(ref subtables) => {
                match ligaturesubst_would_apply(opt_gdef_table, &subtables, match_type, i, glyphs)?
                {
                    Some(_ligature) => Ok(true),
                    None => Ok(false),
                }
            }
            SubstLookup::ContextSubst(ref subtables) => {
                match contextsubst_would_apply(opt_gdef_table, &subtables, match_type, i, glyphs)? {
                    Some(_subst) => Ok(true),
                    None => Ok(false),
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
                    Some(_subst) => Ok(true),
                    None => Ok(false),
                }
            }
            SubstLookup::ReverseChainSingleSubst(ref subtables) => {
                match reversechainsinglesubst_would_apply(
                    opt_gdef_table,
                    &subtables,
                    match_type,
                    i,
                    glyphs,
                )? {
                    Some(_subst) => Ok(true),
                    None => Ok(false),
                }
            }
        };
    }
    Ok(false)
}

pub fn gsub_apply_lookup<T: GlyphData>(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    lookup_index: usize,
    feature_tag: u32,
    opt_alternate: Option<usize>,
    glyphs: &mut Vec<RawGlyph<T>>,
    start: usize,
    mut length: usize,
    pred: impl Fn(&RawGlyph<T>) -> bool,
) -> Result<usize, ParseError> {
    if let Some(ref lookup_list) = gsub_table.opt_lookup_list {
        let lookup = lookup_list.lookup_cache_gsub(gsub_cache, lookup_index)?;
        let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
        match lookup.lookup_subtables {
            SubstLookup::SingleSubst(ref subtables) => {
                for glyph in glyphs[start..(start + length)].iter_mut() {
                    if match_type.match_glyph(opt_gdef_table, glyph) && pred(glyph) {
                        singlesubst(&subtables, feature_tag, glyph)?;
                    }
                }
            }
            SubstLookup::MultipleSubst(ref subtables) => {
                let mut i = start;
                while i < start + length {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        match multiplesubst(&subtables, i, glyphs)? {
                            Some(replace_count) => {
                                i += replace_count;
                                length += replace_count;
                                length -= 1;
                            }
                            None => i += 1,
                        }
                    } else {
                        i += 1;
                    }
                }
            }
            SubstLookup::AlternateSubst(ref subtables) => {
                for glyph in glyphs[start..(start + length)].iter_mut() {
                    if match_type.match_glyph(opt_gdef_table, glyph) && pred(glyph) {
                        let alternate = opt_alternate.unwrap_or(0);
                        alternatesubst(&subtables, alternate, glyph)?;
                    }
                }
            }
            SubstLookup::LigatureSubst(ref subtables) => {
                let mut i = start;
                while i < start + length {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        match ligaturesubst(opt_gdef_table, &subtables, match_type, i, glyphs)? {
                            Some((removed_count, skip_count)) => {
                                i += skip_count + 1;
                                length -= removed_count;
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
                while i < start + length {
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
                            Some((input_length, changes)) => {
                                i += input_length;
                                length = checked_add(length, changes).unwrap();
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
                while i < start + length {
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
                            Some((input_length, changes)) => {
                                i += input_length;
                                length = checked_add(length, changes).unwrap();
                            }
                            None => i += 1,
                        }
                    } else {
                        i += 1;
                    }
                }
            }
            SubstLookup::ReverseChainSingleSubst(ref subtables) => {
                for i in (start..start + length).rev() {
                    if match_type.match_glyph(opt_gdef_table, &glyphs[i]) && pred(&glyphs[i]) {
                        reversechainsinglesubst(opt_gdef_table, subtables, match_type, i, glyphs)?;
                    }
                }
            }
        }
    }
    Ok(length)
}

fn singlesubst_would_apply<T: GlyphData>(
    subtables: &[SingleSubst],
    glyph: &RawGlyph<T>,
) -> Result<Option<u16>, ParseError> {
    let glyph_index = glyph.glyph_index;
    for single_subst in subtables {
        if let Some(glyph_index) = single_subst.apply_glyph(glyph_index)? {
            return Ok(Some(glyph_index));
        }
    }
    Ok(None)
}

fn singlesubst<T: GlyphData>(
    subtables: &[SingleSubst],
    subst_tag: u32,
    glyph: &mut RawGlyph<T>,
) -> Result<(), ParseError> {
    if let Some(output_glyph) = singlesubst_would_apply(subtables, glyph)? {
        glyph.glyph_index = output_glyph;
        glyph.glyph_origin = GlyphOrigin::Direct;
        if subst_tag == tag::VERT || subst_tag == tag::VRT2 {
            glyph.is_vert_alt = true;
        }
    }
    Ok(())
}

fn multiplesubst_would_apply<'a, T: GlyphData>(
    subtables: &'a [MultipleSubst],
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<&'a SequenceTable>, ParseError> {
    let glyph_index = glyphs[i].glyph_index;
    for multiple_subst in subtables {
        if let Some(sequence_table) = multiple_subst.apply_glyph(glyph_index)? {
            return Ok(Some(sequence_table));
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
                glyphs[i].glyph_index = first_glyph_index;
                glyphs[i].glyph_origin = GlyphOrigin::Direct;
                for j in 1..sequence_table.substitute_glyphs.len() {
                    let output_glyph_index = sequence_table.substitute_glyphs[j];
                    let glyph = RawGlyph {
                        unicodes: glyphs[i].unicodes.clone(),
                        glyph_index: output_glyph_index,
                        liga_component_pos: 0, //glyphs[i].liga_component_pos,
                        glyph_origin: GlyphOrigin::Direct,
                        small_caps: glyphs[i].small_caps,
                        multi_subst_dup: true,
                        is_vert_alt: glyphs[i].is_vert_alt,
                        fake_bold: glyphs[i].fake_bold,
                        fake_italic: glyphs[i].fake_italic,
                        extra_data: glyphs[i].extra_data.clone(),
                        variation: glyphs[i].variation,
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
    glyph: &RawGlyph<T>,
) -> Result<Option<&'a AlternateSet>, ParseError> {
    let glyph_index = glyph.glyph_index;
    for alternate_subst in subtables {
        if let Some(alternate_set) = alternate_subst.apply_glyph(glyph_index)? {
            return Ok(Some(alternate_set));
        }
    }
    Ok(None)
}

fn alternatesubst<T: GlyphData>(
    subtables: &[AlternateSubst],
    alternate: usize,
    glyph: &mut RawGlyph<T>,
) -> Result<(), ParseError> {
    if let Some(alternateset) = alternatesubst_would_apply(subtables, glyph)? {
        // TODO allow users to specify which alternate glyph they want
        if alternate < alternateset.alternate_glyphs.len() {
            glyph.glyph_index = alternateset.alternate_glyphs[alternate];
            glyph.glyph_origin = GlyphOrigin::Direct;
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
    let glyph_index = glyphs[i].glyph_index;
    for ligature_subst in subtables {
        if let Some(ligatureset) = ligature_subst.apply_glyph(glyph_index)? {
            for ligature in &ligatureset.ligatures {
                if ligature.matches(match_type, opt_gdef_table, i, glyphs) {
                    return Ok(Some(ligature));
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
    let glyph_index = glyphs[i].glyph_index;
    for context_lookup in subtables {
        if let Some(context) = context_lookup_info(&context_lookup, glyph_index, |context| {
            context.matches(opt_gdef_table, match_type, glyphs, i)
        })? {
            return Ok(Some(context));
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
    let glyph_index = glyphs[i].glyph_index;
    for chain_context_lookup in subtables {
        if let Some(context) =
            chain_context_lookup_info(&chain_context_lookup, glyph_index, |context| {
                context.matches(opt_gdef_table, match_type, glyphs, i)
            })?
        {
            return Ok(Some(context));
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

fn reversechainsinglesubst_would_apply<T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &[ReverseChainSingleSubst],
    match_type: MatchType,
    i: usize,
    glyphs: &[RawGlyph<T>],
) -> Result<Option<u16>, ParseError> {
    let glyph_index = glyphs[i].glyph_index;
    for reversechainsinglesubst in subtables {
        if let new_glyph_index @ Some(_) = reversechainsinglesubst
            .apply_glyph(glyph_index, |context| {
                context.matches(opt_gdef_table, match_type, glyphs, i)
            })?
        {
            return Ok(new_glyph_index);
        }
    }
    Ok(None)
}

fn reversechainsinglesubst<T: GlyphData>(
    opt_gdef_table: Option<&GDEFTable>,
    subtables: &[ReverseChainSingleSubst],
    match_type: MatchType,
    i: usize,
    glyphs: &mut [RawGlyph<T>],
) -> Result<(), ParseError> {
    if let Some(output_glyph_index) =
        reversechainsinglesubst_would_apply(opt_gdef_table, subtables, match_type, i, glyphs)?
    {
        glyphs[i].glyph_index = output_glyph_index;
        glyphs[i].glyph_origin = GlyphOrigin::Direct;
    }
    Ok(())
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
    match checked_add(len, changes) {
        Some(new_len) => Ok(Some((new_len as usize, changes))),
        None => panic!("apply_subst_context: len < 0"),
    }
}

fn checked_add(base: usize, changes: isize) -> Option<usize> {
    if changes < 0 {
        base.checked_sub(changes.wrapping_abs() as usize)
    } else {
        base.checked_add(changes as usize)
    }
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
            singlesubst(subtables, feature_tag, &mut glyphs[i])?;
            Ok(Some(0))
        }
        SubstLookup::MultipleSubst(ref subtables) => match multiplesubst(subtables, i, glyphs)? {
            Some(replace_count) => Ok(Some((replace_count as isize) - 1)),
            None => Ok(None),
        },
        SubstLookup::AlternateSubst(ref subtables) => {
            alternatesubst(subtables, 0, &mut glyphs[i])?;
            Ok(Some(0))
        }
        SubstLookup::LigatureSubst(ref subtables) => {
            match ligaturesubst(opt_gdef_table, subtables, match_type, i, glyphs)? {
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
                    subtables,
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
                    subtables,
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
        SubstLookup::ReverseChainSingleSubst(ref subtables) => {
            reversechainsinglesubst(opt_gdef_table, subtables, match_type, i, glyphs)?;
            Ok(Some(0))
        }
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

fn build_lookups_default(
    gsub_table: &LayoutTable<GSUB>,
    langsys: &LangSys,
    feature_masks: GsubFeatureMask,
) -> Result<Vec<(usize, u32)>, ParseError> {
    let mut lookups = BTreeMap::new();
    for (feature_mask, feature_tag) in FEATURE_MASKS {
        if feature_masks.contains(*feature_mask) {
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
    }

    // note: iter() returns sorted by key
    Ok(lookups.into_iter().collect())
}

fn make_supported_features_mask(
    gsub_table: &LayoutTable<GSUB>,
    langsys: &LangSys,
) -> Result<GsubFeatureMask, ParseError> {
    let mut feature_mask = GsubFeatureMask::empty();
    for feature_index in langsys.feature_indices_iter() {
        let feature_record = gsub_table.feature_by_index(*feature_index)?;
        feature_mask |= GsubFeatureMask::from_tag(feature_record.feature_tag);
    }
    Ok(feature_mask)
}

fn lang_tag_key(opt_lang_tag: Option<u32>) -> u32 {
    // `DFLT` is not a valid lang tag so we use it to indicate the default
    opt_lang_tag.unwrap_or(tag::DFLT)
}

fn get_supported_features(
    gsub_cache: &LayoutCache<GSUB>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
) -> Result<GsubFeatureMask, ParseError> {
    let feature_mask = match gsub_cache
        .supported_features
        .borrow_mut()
        .entry((script_tag, lang_tag_key(opt_lang_tag)))
    {
        Entry::Occupied(entry) => GsubFeatureMask::from_bits_truncate(*entry.get()),
        Entry::Vacant(entry) => {
            let gsub_table = &gsub_cache.layout_table;
            let feature_mask =
                if let Some(script) = gsub_table.find_script_or_default(script_tag)? {
                    if let Some(langsys) = script.find_langsys_or_default(opt_lang_tag)? {
                        make_supported_features_mask(gsub_table, langsys)?
                    } else {
                        GsubFeatureMask::empty()
                    }
                } else {
                    GsubFeatureMask::empty()
                };
            entry.insert(feature_mask.bits());
            feature_mask
        }
    };
    Ok(feature_mask)
}

fn find_alternate(features_list: &[FeatureInfo], feature_tag: u32) -> Option<usize> {
    for feature_info in features_list {
        if feature_info.feature_tag == feature_tag {
            return feature_info.alternate;
        }
    }
    None
}

/// Perform glyph substitution according to the supplied features, script and language.
///
/// `dotted_circle_index` is the glyph index of U+25CC DOTTED CIRCLE: ◌. This is inserted
/// when shaping some complex scripts where the input text contains incomplete syllables.
/// If you have an instance of `FontDataImpl` the glyph index can be retrieved via the
/// `lookup_glyph_index` method.
///
/// ## Example
///
/// The following shows a complete example of loading a font, mapping text to glyphs, and
/// applying glyph substitution.
///
/// ```
/// use std::convert::TryFrom;
/// use std::error::Error;
/// use std::rc::Rc;
///
/// use allsorts::binary::read::ReadScope;
/// use allsorts::error::ParseError;
/// use allsorts::font_data_impl::{FontDataImpl, MatchingPresentation};
/// use allsorts::fontfile::FontFile;
/// use allsorts::gsub::{Features, GlyphOrigin, GsubFeatureMask, RawGlyph};
/// use allsorts::tinyvec::tiny_vec;
/// use allsorts::unicode::VariationSelector;
/// use allsorts::DOTTED_CIRCLE;
/// use allsorts::{gsub, tag};
///
/// fn shape(text: &str) -> Result<Vec<RawGlyph<()>>, Box<dyn Error>> {
///     let script = tag::from_string("LATN")?;
///     let lang = tag::from_string("DFLT")?;
///     let buffer = std::fs::read("../tests/fonts/opentype/Klei.otf")?;
///     let scope = ReadScope::new(&buffer);
///     let font_file = scope.read::<FontFile<'_>>()?;
///     // Use a different index to access other fonts in a font collection (E.g. TTC)
///     let provider = font_file.table_provider(0)?;
///     let mut font_data_impl = match FontDataImpl::new(provider)? {
///         Some(font_data_impl) => font_data_impl,
///         None => {
///             return Err(Box::from(ParseError::MissingValue));
///         }
///     };
///
///     let opt_gsub_cache = font_data_impl.gsub_cache()?;
///     let opt_gpos_cache = font_data_impl.gpos_cache()?;
///     let opt_gdef_table = font_data_impl.gdef_table()?;
///     let opt_gdef_table = opt_gdef_table.as_ref().map(Rc::as_ref);
///
///     // Map glyphs
///     //
///     // We look ahead in the char stream for variation selectors. If one is found it is used for
///     // mapping the current glyph. When a variation selector is reached in the stream it is
///     // skipped as it was handled as part of the preceding character.
///     let mut chars_iter = text.chars().peekable();
///     let mut glyphs = Vec::new();
///     while let Some(ch) = chars_iter.next() {
///         match VariationSelector::try_from(ch) {
///             Ok(_) => {} // filter out variation selectors
///             Err(()) => {
///                 let vs = chars_iter
///                     .peek()
///                     .and_then(|&next| VariationSelector::try_from(next).ok());
///                 let (glyph_index, used_variation) = font_data_impl.lookup_glyph_index(
///                     ch,
///                     MatchingPresentation::NotRequired,
///                     vs,
///                 );
///                 let glyph = RawGlyph {
///                     unicodes: tiny_vec![[char; 1] => ch],
///                     glyph_index: glyph_index,
///                     liga_component_pos: 0,
///                     glyph_origin: GlyphOrigin::Char(ch),
///                     small_caps: false,
///                     multi_subst_dup: false,
///                     is_vert_alt: false,
///                     fake_bold: false,
///                     fake_italic: false,
///                     extra_data: (),
///                     variation: Some(used_variation),
///                 };
///                 glyphs.push(glyph);
///             }
///         }
///     }
///
///     let (dotted_circle_index, _) = font_data_impl.lookup_glyph_index(
///         DOTTED_CIRCLE,
///         MatchingPresentation::NotRequired,
///         None,
///     );
///
///     // Apply gsub if table is present
///     let num_glyphs = font_data_impl.num_glyphs();
///     if let Some(gsub_cache) = opt_gsub_cache {
///         gsub::apply(
///             dotted_circle_index,
///             &gsub_cache,
///             opt_gdef_table,
///             script,
///             Some(lang),
///             &Features::Mask(GsubFeatureMask::default()),
///             num_glyphs,
///             &mut glyphs,
///         )?;
///     }
///
///     // This is where you would apply `gpos` if the table is present.
///
///     Ok(glyphs)
/// }
///
/// match shape("This is the first example.") {
///     Ok(glyphs) => {
///         assert!(!glyphs.is_empty());
///     }
///     Err(err) => eprintln!("Unable to shape text: {}", err),
/// }
/// ```
pub fn apply(
    dotted_circle_index: u16,
    gsub_cache: &LayoutCache<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    features: &Features,
    num_glyphs: u16,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    match features {
        Features::Custom(features_list) => gsub_apply_custom(
            gsub_cache,
            opt_gdef_table,
            script_tag,
            opt_lang_tag,
            features_list,
            num_glyphs,
            glyphs,
        ),
        Features::Mask(feature_mask) => gsub_apply_default(
            dotted_circle_index,
            gsub_cache,
            opt_gdef_table,
            script_tag,
            opt_lang_tag,
            *feature_mask,
            num_glyphs,
            glyphs,
        ),
    }
}

fn gsub_apply_custom(
    gsub_cache: &LayoutCache<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    features_list: &[FeatureInfo],
    num_glyphs: u16,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    let gsub_table = &gsub_cache.layout_table;
    if let Some(script) = gsub_table.find_script_or_default(script_tag)? {
        if let Some(langsys) = script.find_langsys_or_default(opt_lang_tag)? {
            let lookups = build_lookups_custom(gsub_table, langsys, features_list)?;

            // note: iter() returns sorted by key
            for (lookup_index, feature_tag) in lookups {
                let alternate = find_alternate(features_list, feature_tag);
                if feature_tag == tag::FINA && glyphs.len() > 0 {
                    gsub_apply_lookup(
                        gsub_cache,
                        gsub_table,
                        opt_gdef_table,
                        lookup_index,
                        feature_tag,
                        alternate,
                        glyphs,
                        glyphs.len() - 1,
                        1,
                        |_| true,
                    )?;
                } else {
                    gsub_apply_lookup(
                        gsub_cache,
                        gsub_table,
                        opt_gdef_table,
                        lookup_index,
                        feature_tag,
                        alternate,
                        glyphs,
                        0,
                        glyphs.len(),
                        |_| true,
                    )?;
                }
            }
        }
    }
    replace_missing_glyphs(glyphs, num_glyphs);
    Ok(())
}

pub fn replace_missing_glyphs<T: GlyphData>(glyphs: &mut Vec<RawGlyph<T>>, num_glyphs: u16) {
    for glyph in glyphs.iter_mut() {
        if glyph.glyph_index >= num_glyphs {
            glyph.unicodes = tiny_vec![];
            glyph.glyph_index = 0;
            glyph.liga_component_pos = 0;
            glyph.glyph_origin = GlyphOrigin::Direct;
            glyph.small_caps = false;
            glyph.multi_subst_dup = false;
            glyph.is_vert_alt = false;
            glyph.fake_bold = false;
            glyph.fake_italic = false;
            glyph.fake_italic = false;
            glyph.variation = None;
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

bitflags! {
    // It is possible to squeeze these flags into a `u32` if we represent features
    // that are never applied together as numbers instead of separate bits.
    pub struct GsubFeatureMask: u64 {
        const ABVF = 1 << 0;
        const ABVS = 1 << 1;
        const AFRC = 1 << 2;
        const AKHN = 1 << 3;
        const BLWF = 1 << 4;
        const BLWS = 1 << 5;
        const C2SC = 1 << 6;
        const CALT = 1 << 7;
        const CCMP = 1 << 8;
        const CFAR = 1 << 9;
        const CJCT = 1 << 10;
        const CLIG = 1 << 11;
        const DLIG = 1 << 12;
        const FINA = 1 << 13;
        const FIN2 = 1 << 14;
        const FIN3 = 1 << 15;
        const FRAC = 1 << 16;
        const HALF = 1 << 17;
        const HALN = 1 << 18;
        const HLIG = 1 << 19;
        const INIT = 1 << 20;
        const ISOL = 1 << 21;
        const LIGA = 1 << 22;
        const LNUM = 1 << 23;
        const LOCL = 1 << 24;
        const MEDI = 1 << 25;
        const MED2 = 1 << 26;
        const MSET = 1 << 27;
        const NUKT = 1 << 28;
        const ONUM = 1 << 29;
        const ORDN = 1 << 30;
        const PNUM = 1 << 31;
        const PREF = 1 << 32;
        const PRES = 1 << 33;
        const PSTF = 1 << 34;
        const PSTS = 1 << 35;
        const RCLT = 1 << 36;
        const RKRF = 1 << 37;
        const RLIG = 1 << 38;
        const RPHF = 1 << 39;
        const SMCP = 1 << 40;
        const TNUM = 1 << 41;
        const VATU = 1 << 42;
        const VRT2_OR_VERT = 1 << 43;
        const ZERO = 1 << 44;
    }
}

const FEATURE_MASKS: &[(GsubFeatureMask, u32)] = &[
    (GsubFeatureMask::ABVF, tag::ABVF),
    (GsubFeatureMask::ABVS, tag::ABVS),
    (GsubFeatureMask::AFRC, tag::AFRC),
    (GsubFeatureMask::AKHN, tag::AKHN),
    (GsubFeatureMask::BLWF, tag::BLWF),
    (GsubFeatureMask::BLWS, tag::BLWS),
    (GsubFeatureMask::C2SC, tag::C2SC),
    (GsubFeatureMask::CALT, tag::CALT),
    (GsubFeatureMask::CCMP, tag::CCMP),
    (GsubFeatureMask::CFAR, tag::CFAR),
    (GsubFeatureMask::CJCT, tag::CJCT),
    (GsubFeatureMask::CLIG, tag::CLIG),
    (GsubFeatureMask::DLIG, tag::DLIG),
    (GsubFeatureMask::FINA, tag::FINA),
    (GsubFeatureMask::FIN2, tag::FIN2),
    (GsubFeatureMask::FIN3, tag::FIN3),
    (GsubFeatureMask::FRAC, tag::FRAC),
    (GsubFeatureMask::HALF, tag::HALF),
    (GsubFeatureMask::HALN, tag::HALN),
    (GsubFeatureMask::HLIG, tag::HLIG),
    (GsubFeatureMask::INIT, tag::INIT),
    (GsubFeatureMask::ISOL, tag::ISOL),
    (GsubFeatureMask::LIGA, tag::LIGA),
    (GsubFeatureMask::LNUM, tag::LNUM),
    (GsubFeatureMask::LOCL, tag::LOCL),
    (GsubFeatureMask::MEDI, tag::MEDI),
    (GsubFeatureMask::MED2, tag::MED2),
    (GsubFeatureMask::MSET, tag::MSET),
    (GsubFeatureMask::NUKT, tag::NUKT),
    (GsubFeatureMask::ONUM, tag::ONUM),
    (GsubFeatureMask::ORDN, tag::ORDN),
    (GsubFeatureMask::PNUM, tag::PNUM),
    (GsubFeatureMask::PREF, tag::PREF),
    (GsubFeatureMask::PRES, tag::PRES),
    (GsubFeatureMask::PSTF, tag::PSTF),
    (GsubFeatureMask::PSTS, tag::PSTS),
    (GsubFeatureMask::RCLT, tag::RCLT),
    (GsubFeatureMask::RKRF, tag::RKRF),
    (GsubFeatureMask::RLIG, tag::RLIG),
    (GsubFeatureMask::RPHF, tag::RPHF),
    (GsubFeatureMask::SMCP, tag::SMCP),
    (GsubFeatureMask::TNUM, tag::TNUM),
    (GsubFeatureMask::VATU, tag::VATU),
    (GsubFeatureMask::VRT2_OR_VERT, tag::VRT2),
    (GsubFeatureMask::ZERO, tag::ZERO),
];

impl GsubFeatureMask {
    pub fn from_tag(tag: u32) -> GsubFeatureMask {
        match tag {
            tag::ABVF => GsubFeatureMask::ABVF,
            tag::ABVS => GsubFeatureMask::ABVS,
            tag::AFRC => GsubFeatureMask::AFRC,
            tag::AKHN => GsubFeatureMask::AKHN,
            tag::BLWF => GsubFeatureMask::BLWF,
            tag::BLWS => GsubFeatureMask::BLWS,
            tag::C2SC => GsubFeatureMask::C2SC,
            tag::CALT => GsubFeatureMask::CALT,
            tag::CCMP => GsubFeatureMask::CCMP,
            tag::CFAR => GsubFeatureMask::CFAR,
            tag::CJCT => GsubFeatureMask::CJCT,
            tag::CLIG => GsubFeatureMask::CLIG,
            tag::DLIG => GsubFeatureMask::DLIG,
            tag::FINA => GsubFeatureMask::FINA,
            tag::FIN2 => GsubFeatureMask::FIN2,
            tag::FIN3 => GsubFeatureMask::FIN3,
            tag::FRAC => GsubFeatureMask::FRAC,
            tag::HALF => GsubFeatureMask::HALF,
            tag::HALN => GsubFeatureMask::HALN,
            tag::HLIG => GsubFeatureMask::HLIG,
            tag::INIT => GsubFeatureMask::INIT,
            tag::ISOL => GsubFeatureMask::ISOL,
            tag::LIGA => GsubFeatureMask::LIGA,
            tag::LNUM => GsubFeatureMask::LNUM,
            tag::LOCL => GsubFeatureMask::LOCL,
            tag::MEDI => GsubFeatureMask::MEDI,
            tag::MED2 => GsubFeatureMask::MED2,
            tag::MSET => GsubFeatureMask::MSET,
            tag::NUKT => GsubFeatureMask::NUKT,
            tag::ONUM => GsubFeatureMask::ONUM,
            tag::ORDN => GsubFeatureMask::ORDN,
            tag::PNUM => GsubFeatureMask::PNUM,
            tag::PREF => GsubFeatureMask::PREF,
            tag::PRES => GsubFeatureMask::PRES,
            tag::PSTF => GsubFeatureMask::PSTF,
            tag::PSTS => GsubFeatureMask::PSTS,
            tag::RCLT => GsubFeatureMask::RCLT,
            tag::RKRF => GsubFeatureMask::RKRF,
            tag::RLIG => GsubFeatureMask::RLIG,
            tag::RPHF => GsubFeatureMask::RPHF,
            tag::SMCP => GsubFeatureMask::SMCP,
            tag::TNUM => GsubFeatureMask::TNUM,
            tag::VATU => GsubFeatureMask::VATU,
            tag::VERT => GsubFeatureMask::VRT2_OR_VERT,
            tag::VRT2 => GsubFeatureMask::VRT2_OR_VERT,
            tag::ZERO => GsubFeatureMask::ZERO,
            _ => GsubFeatureMask::empty(),
        }
    }
}

impl Default for GsubFeatureMask {
    fn default() -> Self {
        GsubFeatureMask::CCMP
            | GsubFeatureMask::RLIG
            | GsubFeatureMask::CLIG
            | GsubFeatureMask::LIGA
            | GsubFeatureMask::LOCL
            | GsubFeatureMask::CALT
    }
}

pub fn features_supported(
    gsub_cache: &LayoutCache<GSUB>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    feature_mask: GsubFeatureMask,
) -> Result<bool, ShapingError> {
    let supported_features = get_supported_features(gsub_cache, script_tag, opt_lang_tag)?;
    Ok(supported_features.contains(feature_mask))
}

pub fn get_lookups_cache_index(
    gsub_cache: &LayoutCache<GSUB>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    feature_mask: GsubFeatureMask,
) -> Result<usize, ParseError> {
    let index = match gsub_cache.lookups_index.borrow_mut().entry((
        script_tag,
        lang_tag_key(opt_lang_tag),
        feature_mask.bits(),
    )) {
        Entry::Occupied(entry) => *entry.get(),
        Entry::Vacant(entry) => {
            let gsub_table = &gsub_cache.layout_table;
            if let Some(script) = gsub_table.find_script_or_default(script_tag)? {
                if let Some(langsys) = script.find_langsys_or_default(opt_lang_tag)? {
                    let lookups = build_lookups_default(gsub_table, langsys, feature_mask)?;
                    let index = gsub_cache.cached_lookups.borrow().len();
                    gsub_cache.cached_lookups.borrow_mut().push(lookups);
                    *entry.insert(index)
                } else {
                    *entry.insert(0)
                }
            } else {
                *entry.insert(0)
            }
        }
    };
    Ok(index)
}

fn gsub_apply_default(
    dotted_circle_index: u16,
    gsub_cache: &LayoutCache<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    mut feature_mask: GsubFeatureMask,
    num_glyphs: u16,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    let gsub_table = &gsub_cache.layout_table;
    match ScriptType::from(script_tag) {
        ScriptType::Arabic => scripts::arabic::gsub_apply_arabic(
            gsub_cache,
            gsub_table,
            opt_gdef_table,
            script_tag,
            opt_lang_tag,
            glyphs,
        )?,
        ScriptType::Indic => scripts::indic::gsub_apply_indic(
            dotted_circle_index,
            gsub_cache,
            gsub_table,
            opt_gdef_table,
            script_tag,
            opt_lang_tag,
            glyphs,
        )?,
        ScriptType::Syriac => scripts::syriac::gsub_apply_syriac(
            gsub_cache,
            gsub_table,
            opt_gdef_table,
            script_tag,
            opt_lang_tag,
            glyphs,
        )?,
        ScriptType::Default => {
            feature_mask &= get_supported_features(gsub_cache, script_tag, opt_lang_tag)?;
            if feature_mask.contains(GsubFeatureMask::FRAC) {
                let index_frac =
                    get_lookups_cache_index(gsub_cache, script_tag, opt_lang_tag, feature_mask)?;
                feature_mask.remove(GsubFeatureMask::FRAC);
                let index =
                    get_lookups_cache_index(gsub_cache, script_tag, opt_lang_tag, feature_mask)?;
                let lookups = &gsub_cache.cached_lookups.borrow()[index];
                let lookups_frac = &gsub_cache.cached_lookups.borrow()[index_frac];
                gsub_apply_lookups_frac(
                    gsub_cache,
                    gsub_table,
                    opt_gdef_table,
                    lookups,
                    lookups_frac,
                    glyphs,
                )?;
            } else {
                let index =
                    get_lookups_cache_index(gsub_cache, script_tag, opt_lang_tag, feature_mask)?;
                let lookups = &gsub_cache.cached_lookups.borrow()[index];
                gsub_apply_lookups(gsub_cache, gsub_table, opt_gdef_table, lookups, glyphs)?;
            }
        }
    }

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
    gsub_apply_lookups_impl(
        gsub_cache,
        gsub_table,
        opt_gdef_table,
        lookups,
        glyphs,
        0,
        glyphs.len(),
    )?;
    Ok(())
}

fn gsub_apply_lookups_impl(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    lookups: &[(usize, u32)],
    glyphs: &mut Vec<RawGlyph<()>>,
    start: usize,
    mut length: usize,
) -> Result<usize, ShapingError> {
    for (lookup_index, feature_tag) in lookups {
        length = gsub_apply_lookup(
            gsub_cache,
            gsub_table,
            opt_gdef_table,
            *lookup_index,
            *feature_tag,
            None,
            glyphs,
            start,
            length,
            |_| true,
        )?;
    }
    Ok(length)
}

fn gsub_apply_lookups_frac(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    opt_gdef_table: Option<&GDEFTable>,
    lookups: &[(usize, u32)],
    lookups_frac: &[(usize, u32)],
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    let mut i = 0;
    while i < glyphs.len() {
        if let Some((start_pos, _slash_pos, end_pos)) = find_fraction(&glyphs[i..]) {
            if start_pos > 0 {
                i += gsub_apply_lookups_impl(
                    gsub_cache,
                    gsub_table,
                    opt_gdef_table,
                    lookups,
                    glyphs,
                    i,
                    start_pos,
                )?;
            }
            i += gsub_apply_lookups_impl(
                gsub_cache,
                gsub_table,
                opt_gdef_table,
                lookups_frac,
                glyphs,
                i,
                end_pos - start_pos + 1,
            )?;
        } else {
            gsub_apply_lookups_impl(
                gsub_cache,
                gsub_table,
                opt_gdef_table,
                lookups,
                glyphs,
                i,
                glyphs.len() - i,
            )?;
            break;
        }
    }
    Ok(())
}

fn find_fraction(glyphs: &[RawGlyph<()>]) -> Option<(usize, usize, usize)> {
    let slash_pos = glyphs
        .iter()
        .position(|g| g.glyph_origin == GlyphOrigin::Char('/'))?;
    let mut start_pos = slash_pos;
    while start_pos > 0 {
        match glyphs[start_pos - 1].glyph_origin {
            GlyphOrigin::Char(c) if c.is_digit(10) => {
                start_pos -= 1;
            }
            _ => break,
        }
    }
    let mut end_pos = slash_pos;
    while end_pos + 1 < glyphs.len() {
        match glyphs[end_pos + 1].glyph_origin {
            GlyphOrigin::Char(c) if c.is_digit(10) => {
                end_pos += 1;
            }
            _ => break,
        }
    }
    if start_pos < slash_pos && slash_pos < end_pos {
        Some((start_pos, slash_pos, end_pos))
    } else {
        None
    }
}
