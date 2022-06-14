//! Glyph positioning (`gpos`) implementation.
//!
//! > The Glyph Positioning table (GPOS) provides precise control over glyph placement for
//! > sophisticated text layout and rendering in each script and language system that a font
//! > supports.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/gpos>

use crate::context::{ContextLookupHelper, Glyph, LookupFlag, MatchType};
use crate::error::ParseError;
use crate::gdef::gdef_is_mark;
use crate::gsub::{FeatureInfo, Features, RawGlyph};
use crate::layout::{
    chain_context_lookup_info, context_lookup_info, Adjust, Anchor, ChainContextLookup,
    ContextLookup, CursivePos, GDEFTable, LangSys, LayoutCache, LayoutTable, LookupList,
    MarkBasePos, MarkLigPos, PairPos, PosLookup, SinglePos, ValueRecord, GPOS,
};
use crate::scripts;
use crate::scripts::ScriptType;
use crate::tag;

use itertools::Itertools;
use tinyvec::tiny_vec;
use unicode_general_category::GeneralCategory;

type PosContext<'a> = ContextLookupHelper<'a, GPOS>;

/// Apply glyph positioning rules to glyph `Info`.
pub fn apply(
    gpos_cache: &LayoutCache<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    kerning: bool,
    features: &Features,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    let gpos_table = &gpos_cache.layout_table;
    let script_type = ScriptType::from(script_tag);

    let script = match script_type {
        ScriptType::Indic => {
            let indic2_tag = scripts::indic::indic2_tag(script_tag);
            match gpos_table.find_script(indic2_tag)? {
                Some(script) => script,
                None => match gpos_table.find_script_or_default(script_tag)? {
                    Some(script) => script,
                    None => return Ok(()),
                },
            }
        }
        _ => match gpos_table.find_script_or_default(script_tag)? {
            Some(script) => script,
            None => return Ok(()),
        },
    };

    let langsys = match script.find_langsys_or_default(opt_lang_tag)? {
        Some(langsys) => langsys,
        None => return Ok(()),
    };

    let base_features: &[u32] = match script_type {
        ScriptType::Arabic => &[tag::CURS, tag::KERN, tag::MARK, tag::MKMK],
        ScriptType::Indic => &[
            tag::ABVM,
            tag::BLWM,
            tag::DIST,
            tag::KERN,
            tag::MARK,
            tag::MKMK,
        ],
        ScriptType::Khmer => &[tag::ABVM, tag::BLWM, tag::DIST, tag::MARK, tag::MKMK],
        ScriptType::Syriac => &[tag::CURS, tag::KERN, tag::MARK, tag::MKMK],
        ScriptType::ThaiLao => &[tag::KERN, tag::MARK, tag::MKMK],
        ScriptType::Default if kerning => &[tag::DIST, tag::KERN, tag::MARK, tag::MKMK],
        ScriptType::Default => &[tag::DIST, tag::MARK, tag::MKMK],
    };

    apply_features(
        &gpos_cache,
        &gpos_table,
        opt_gdef_table,
        &langsys,
        base_features.iter().map(|&feature_tag| FeatureInfo {
            feature_tag,
            alternate: None,
        }),
        infos,
    )?;
    match features {
        Features::Custom(custom) => apply_features(
            &gpos_cache,
            gpos_table,
            opt_gdef_table,
            &langsys,
            custom.iter().copied(),
            infos,
        ),
        Features::Mask(mask) => apply_features(
            &gpos_cache,
            gpos_table,
            opt_gdef_table,
            &langsys,
            mask.iter(),
            infos,
        ),
    }
}

/// Apply glyph positioning using specified OpenType features.
///
/// Generally use `gpos::apply`, which will enable features based on script and language. Use
/// this method if you need more low-level control over the enabled features.
pub fn apply_features(
    gpos_cache: &LayoutCache<GPOS>,
    gpos_table: &LayoutTable<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    langsys: &LangSys,
    features: impl Iterator<Item = FeatureInfo>,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    let mut lookup_indices = tiny_vec!([u16; 128]);
    for feature in features {
        if let Some(feature_table) =
            gpos_table.find_langsys_feature(&langsys, feature.feature_tag)?
        {
            // Sort and remove duplicates
            lookup_indices.clear();
            lookup_indices.extend_from_slice(&feature_table.lookup_indices);
            lookup_indices.sort_unstable();
            for lookup_index in lookup_indices.iter().copied().dedup().map(usize::from) {
                gpos_apply_lookup(gpos_cache, gpos_table, opt_gdef_table, lookup_index, infos)?;
            }
        }
    }
    Ok(())
}

/// Apply basic mark processing when there is no `gpos` table available.
///
/// Call this method when there is no `LayoutCache<GPOS>` available for this font.
pub fn apply_fallback(infos: &mut [Info]) {
    for info in infos.iter_mut() {
        if !info.is_mark && unicodes_are_marks(&info.glyph.unicodes) {
            info.is_mark = true;
        }
    }
    let mut base_index = 0;
    for (i, info) in infos.iter_mut().enumerate().skip(1) {
        if info.is_mark {
            info.placement = Placement::MarkOverprint(base_index);
        } else {
            base_index = i;
        }
    }
}

fn unicodes_are_marks(unicodes: &[char]) -> bool {
    unicodes
        .iter()
        .copied()
        .map(unicode_general_category::get_general_category)
        .all(|cat| cat == GeneralCategory::NonspacingMark)
}

fn gpos_apply_lookup(
    gpos_cache: &LayoutCache<GPOS>,
    gpos_table: &LayoutTable<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    lookup_index: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    if let Some(ref lookup_list) = gpos_table.opt_lookup_list {
        let lookup = lookup_list.lookup_cache_gpos(gpos_cache, lookup_index)?;
        let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
        match lookup.lookup_subtables {
            PosLookup::SinglePos(ref subtables) => {
                forall_glyphs_match(match_type, opt_gdef_table, infos, |i, infos| {
                    singlepos(&subtables, &mut infos[i])
                })
            }
            PosLookup::PairPos(ref subtables) => {
                // Spec suggests that the lookup will only be applied to the second glyph if it was
                // not repositioned, ie. if the value_format is zero, but applying the lookup
                // regardless does not break any test cases.
                forall_glyph_pairs_match(match_type, opt_gdef_table, infos, |i1, i2, infos| {
                    pairpos(&subtables, i1, i2, infos)
                })
            }
            PosLookup::CursivePos(ref subtables) => forall_glyph_pairs_match(
                MatchType::ignore_marks(),
                opt_gdef_table,
                infos,
                |i1, i2, infos| cursivepos(&subtables, i1, i2, lookup.lookup_flag, infos),
            ),
            PosLookup::MarkBasePos(ref subtables) => {
                forall_base_mark_glyph_pairs(infos, |i1, i2, infos| {
                    markbasepos(&subtables, i1, i2, infos)
                })
            }
            PosLookup::MarkLigPos(ref subtables) => {
                forall_base_mark_glyph_pairs(infos, |i1, i2, infos| {
                    markligpos(&subtables, i1, i2, infos)
                })
            }
            PosLookup::MarkMarkPos(ref subtables) => {
                forall_mark_mark_glyph_pairs(infos, |i1, i2, infos| {
                    markmarkpos(&subtables, i1, i2, infos)
                })
            }
            PosLookup::ContextPos(ref subtables) => {
                forall_glyphs_match(match_type, opt_gdef_table, infos, |i, infos| {
                    contextpos(
                        gpos_cache,
                        &lookup_list,
                        opt_gdef_table,
                        match_type,
                        &subtables,
                        i,
                        infos,
                    )
                })
            }
            PosLookup::ChainContextPos(ref subtables) => {
                forall_glyphs_match(match_type, opt_gdef_table, infos, |i, infos| {
                    chaincontextpos(
                        gpos_cache,
                        &lookup_list,
                        opt_gdef_table,
                        match_type,
                        &subtables,
                        i,
                        infos,
                    )
                })
            }
        }
    } else {
        Ok(())
    }
}

fn gpos_lookup_singlepos(
    subtables: &[SinglePos],
    glyph_index: u16,
) -> Result<ValueRecord, ParseError> {
    for singlepos in subtables {
        if let Some(val) = singlepos.apply(glyph_index)? {
            return Ok(Some(val));
        }
    }
    Ok(None)
}

fn gpos_lookup_pairpos(
    subtables: &[PairPos],
    glyph_index1: u16,
    glyph_index2: u16,
) -> Result<Option<(ValueRecord, ValueRecord)>, ParseError> {
    for pairpos in subtables {
        if let Some((val1, val2)) = pairpos.apply(glyph_index1, glyph_index2)? {
            return Ok(Some((val1, val2)));
        }
    }
    Ok(None)
}

fn gpos_lookup_cursivepos(
    subtables: &[CursivePos],
    glyph_index1: u16,
    glyph_index2: u16,
) -> Result<Option<(Anchor, Anchor)>, ParseError> {
    for cursivepos in subtables {
        if let Some((an1, an2)) = cursivepos.apply(glyph_index1, glyph_index2)? {
            return Ok(Some((an1, an2)));
        }
    }
    Ok(None)
}

fn gpos_lookup_markbasepos(
    subtables: &[MarkBasePos],
    glyph_index1: u16,
    glyph_index2: u16,
) -> Result<Option<(Anchor, Anchor)>, ParseError> {
    for markbasepos in subtables {
        if let Some((an1, an2)) = markbasepos.apply(glyph_index1, glyph_index2)? {
            return Ok(Some((an1, an2)));
        }
    }
    Ok(None)
}

fn gpos_lookup_markligpos(
    subtables: &[MarkLigPos],
    glyph_index1: u16,
    glyph_index2: u16,
    liga_component_index: u16,
) -> Result<Option<(Anchor, Anchor)>, ParseError> {
    for markligpos in subtables {
        if let Some((an1, an2)) = markligpos.apply(
            glyph_index1,
            glyph_index2,
            usize::from(liga_component_index),
        )? {
            return Ok(Some((an1, an2)));
        }
    }
    Ok(None)
}

fn gpos_lookup_markmarkpos(
    subtables: &[MarkBasePos],
    glyph_index1: u16,
    glyph_index2: u16,
) -> Result<Option<(Anchor, Anchor)>, ParseError> {
    for markmarkpos in subtables {
        if let Some((an1, an2)) = markmarkpos.apply(glyph_index1, glyph_index2)? {
            return Ok(Some((an1, an2)));
        }
    }
    Ok(None)
}

fn gpos_lookup_contextpos<'a>(
    opt_gdef_table: Option<&GDEFTable>,
    match_type: MatchType,
    subtables: &'a [ContextLookup<GPOS>],
    glyph_index: u16,
    i: usize,
    infos: &mut [Info],
) -> Result<Option<Box<PosContext<'a>>>, ParseError> {
    for context_lookup in subtables {
        if let Some(context) = context_lookup_info(&context_lookup, glyph_index, |context| {
            context.matches(opt_gdef_table, match_type, infos, i)
        })? {
            return Ok(Some(context));
        }
    }
    Ok(None)
}

fn gpos_lookup_chaincontextpos<'a>(
    opt_gdef_table: Option<&GDEFTable>,
    match_type: MatchType,
    subtables: &'a [ChainContextLookup<GPOS>],
    glyph_index: u16,
    i: usize,
    infos: &mut [Info],
) -> Result<Option<Box<PosContext<'a>>>, ParseError> {
    for chain_context_lookup in subtables {
        if let Some(context) =
            chain_context_lookup_info(&chain_context_lookup, glyph_index, |context| {
                context.matches(opt_gdef_table, match_type, infos, i)
            })?
        {
            return Ok(Some(context));
        }
    }
    Ok(None)
}

/// Adjustment to the placement of a glyph as a result of kerning and
/// placement of an attachment relative to a base glyph.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Placement {
    None,
    /// Placement offset by distance delta.
    ///
    /// Fields
    /// (delta x, delta y)
    Distance(i32, i32),
    /// An anchored mark.
    ///
    /// This is a mark where its anchor is aligned with the base glyph anchor.
    ///
    /// Fields:
    /// (base glyph index in `Vec<Info>`, base glyph anchor, mark anchor)
    MarkAnchor(usize, Anchor, Anchor),
    /// An overprint mark.
    ///
    /// This mark is shown at the same position as the base glyph.
    ///
    /// Fields:
    /// (base glyph index in `Vec<Info>`)
    MarkOverprint(usize),
    /// Cursive anchored placement.
    ///
    /// Fields:
    /// * exit glyph index in `Vec<Info>`,
    /// * [RIGHT_TO_LEFT flag from lookup table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookupFlags_1),
    /// * exit glyph anchor,
    /// * entry glyph anchor
    ///
    /// <https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-3-cursive-attachment-positioning-subtable>
    CursiveAnchor(usize, bool, Anchor, Anchor),
}

impl Placement {
    fn combine_distance(&mut self, x2: i32, y2: i32) {
        use Placement::*;

        *self = match *self {
            None | MarkOverprint(_) => Distance(x2, y2),
            // FIXME HarfBuzz also updates cursive anchors
            // but we haven't found any fonts that test this codepath yet.
            CursiveAnchor(..) => Distance(x2, y2),
            Distance(x1, y1) => Distance(x1 + x2, y1 + y2),
            MarkAnchor(i, an1, an2) => {
                let x = an1.x + (x2 as i16);
                let y = an1.y + (y2 as i16);
                MarkAnchor(i, Anchor { x, y }, an2)
            }
        }
    }
}

/// A positioned glyph.
///
/// This struct is the output of applying glyph positioning (`gpos`). It contains the glyph
/// and information about how it should be positioned.
///
/// For more information about glyph placement refer to the OpenType documentation:
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#positioning-glyphs-with-opentype>
#[derive(Clone, Debug)]
pub struct Info {
    /// The glyph.
    pub glyph: RawGlyph<()>,
    /// An offset from the horizontal glyph advance position for this glyph.
    pub kerning: i16,
    /// When not `Placement::None` indicates that this glyph should be placed according to
    /// the variant.
    pub placement: Placement,
    is_mark: bool,
}

impl Glyph for Info {
    fn get_glyph_index(&self) -> u16 {
        self.glyph.glyph_index
    }
}

impl Info {
    pub fn init_from_glyphs(
        opt_gdef_table: Option<&GDEFTable>,
        glyphs: Vec<RawGlyph<()>>,
    ) -> Vec<Info> {
        let mut infos = Vec::with_capacity(glyphs.len());
        for glyph in glyphs {
            let is_mark = gdef_is_mark(opt_gdef_table, glyph.glyph_index);
            let info = Info {
                glyph,
                kerning: 0,
                placement: Placement::None,
                is_mark,
            };
            infos.push(info);
        }
        infos
    }
}

impl Adjust {
    fn apply(&self, info: &mut Info) {
        if self.x_placement == 0 && self.y_placement == 0 {
            if self.x_advance != 0 && self.y_advance == 0 {
                info.kerning += self.x_advance;
            } else if self.y_advance != 0 {
                // error: y_advance non-zero
            } else {
                // both zero, do nothing
            }
        } else {
            if self.y_advance == 0 {
                info.placement
                    .combine_distance(i32::from(self.x_placement), i32::from(self.y_placement));
                if self.x_advance != 0 {
                    info.kerning += self.x_advance;
                }
            } else {
                // error: y_advance non-zero
            }
        }
    }
}

fn forall_glyphs_match(
    match_type: MatchType,
    opt_gdef_table: Option<&GDEFTable>,
    infos: &mut [Info],
    f: impl Fn(usize, &mut [Info]) -> Result<(), ParseError>,
) -> Result<(), ParseError> {
    for i in 0..infos.len() {
        if match_type.match_glyph(opt_gdef_table, &infos[i]) {
            f(i, infos)?;
        }
    }
    Ok(())
}

fn forall_glyph_pairs_match(
    match_type: MatchType,
    opt_gdef_table: Option<&GDEFTable>,
    infos: &mut [Info],
    f: impl Fn(usize, usize, &mut [Info]) -> Result<(), ParseError>,
) -> Result<(), ParseError> {
    if let Some(mut i1) = match_type.find_first(opt_gdef_table, infos) {
        while let Some(i2) = match_type.find_next(opt_gdef_table, infos, i1) {
            f(i1, i2, infos)?;
            i1 = i2;
        }
    }
    Ok(())
}

fn forall_base_mark_glyph_pairs(
    infos: &mut [Info],
    f: impl Fn(usize, usize, &mut [Info]) -> Result<(), ParseError>,
) -> Result<(), ParseError> {
    let mut i = 0;
    'outer: while i + 1 < infos.len() {
        if !infos[i].is_mark {
            for j in i + 1..infos.len() {
                f(i, j, infos)?;
                if !infos[j].is_mark {
                    i = j;
                    continue 'outer;
                }
            }
        }
        i += 1;
    }
    Ok(())
}

fn forall_mark_mark_glyph_pairs(
    infos: &mut [Info],
    f: impl Fn(usize, usize, &mut [Info]) -> Result<(), ParseError>,
) -> Result<(), ParseError> {
    let mut start = 0;
    'outer: loop {
        let mut i = start;
        while i + 1 < infos.len() {
            if infos[i].is_mark {
                for j in i + 1..infos.len() {
                    f(i, j, infos)?;
                    if !infos[j].is_mark {
                        start = i + 1;
                        continue 'outer;
                    }
                }
            }
            i += 1;
        }
        break;
    }
    Ok(())
}

fn singlepos(subtables: &[SinglePos], i: &mut Info) -> Result<(), ParseError> {
    let glyph_index = i.glyph.glyph_index;
    if let Some(adj) = gpos_lookup_singlepos(subtables, glyph_index)? {
        adj.apply(i);
    }
    Ok(())
}

fn pairpos(
    subtables: &[PairPos],
    i1: usize,
    i2: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    match gpos_lookup_pairpos(
        subtables,
        infos[i1].glyph.glyph_index,
        infos[i2].glyph.glyph_index,
    )? {
        Some((opt_adj1, opt_adj2)) => {
            if let Some(adj1) = opt_adj1 {
                adj1.apply(&mut infos[i1]);
            }
            if let Some(adj2) = opt_adj2 {
                adj2.apply(&mut infos[i2]);
            }
            Ok(())
        }
        None => Ok(()),
    }
}

fn cursivepos(
    subtables: &[CursivePos],
    i1: usize,
    i2: usize,
    lookup_flag: LookupFlag,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    match gpos_lookup_cursivepos(
        subtables,
        infos[i1].glyph.glyph_index,
        infos[i2].glyph.glyph_index,
    )? {
        Some((anchor1, anchor2)) => {
            infos[i1].placement =
                Placement::CursiveAnchor(i2, lookup_flag.get_rtl(), anchor2, anchor1);
            Ok(())
        }
        None => Ok(()),
    }
}

fn markbasepos(
    subtables: &[MarkBasePos],
    i1: usize,
    i2: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    match gpos_lookup_markbasepos(
        subtables,
        infos[i1].glyph.glyph_index,
        infos[i2].glyph.glyph_index,
    )? {
        Some((anchor1, anchor2)) => {
            infos[i2].placement = Placement::MarkAnchor(i1, anchor1, anchor2);
            infos[i2].is_mark = true;
            Ok(())
        }
        None => Ok(()),
    }
}

fn markligpos(
    subtables: &[MarkLigPos],
    i1: usize,
    i2: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    match gpos_lookup_markligpos(
        subtables,
        infos[i1].glyph.glyph_index,
        infos[i2].glyph.glyph_index,
        infos[i2].glyph.liga_component_pos,
    )? {
        Some((anchor1, anchor2)) => {
            infos[i2].placement = Placement::MarkAnchor(i1, anchor1, anchor2);
            infos[i2].is_mark = true;
            Ok(())
        }
        None => Ok(()),
    }
}

fn markmarkpos(
    subtables: &[MarkBasePos],
    i1: usize,
    i2: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    match gpos_lookup_markmarkpos(
        subtables,
        infos[i1].glyph.glyph_index,
        infos[i2].glyph.glyph_index,
    )? {
        Some((anchor1, anchor2)) => {
            infos[i2].placement = Placement::MarkAnchor(i1, anchor1, anchor2);
            infos[i2].is_mark = true;
            Ok(())
        }
        None => Ok(()),
    }
}

fn contextpos<'a>(
    gpos_cache: &LayoutCache<GPOS>,
    lookup_list: &LookupList<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    match_type: MatchType,
    subtables: &[ContextLookup<GPOS>],
    i: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    let glyph_index = infos[i].glyph.glyph_index;
    match gpos_lookup_contextpos(opt_gdef_table, match_type, subtables, glyph_index, i, infos)? {
        Some(pos) => apply_pos_context(
            gpos_cache,
            lookup_list,
            opt_gdef_table,
            match_type,
            &pos,
            i,
            infos,
        ),
        None => Ok(()),
    }
}

fn chaincontextpos<'a>(
    gpos_cache: &LayoutCache<GPOS>,
    lookup_list: &LookupList<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    match_type: MatchType,
    subtables: &[ChainContextLookup<GPOS>],
    i: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    let glyph_index = infos[i].glyph.glyph_index;
    match gpos_lookup_chaincontextpos(opt_gdef_table, match_type, subtables, glyph_index, i, infos)?
    {
        Some(pos) => apply_pos_context(
            gpos_cache,
            lookup_list,
            opt_gdef_table,
            match_type,
            &pos,
            i,
            infos,
        ),
        None => Ok(()),
    }
}

fn apply_pos_context<'a>(
    gpos_cache: &LayoutCache<GPOS>,
    lookup_list: &LookupList<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    _match_type: MatchType,
    pos: &PosContext<'_>,
    i: usize,
    infos: &mut [Info],
) -> Result<(), ParseError> {
    for (pos_index, pos_lookup_index) in pos.lookup_array {
        apply_pos(
            gpos_cache,
            lookup_list,
            opt_gdef_table,
            usize::from(*pos_index),
            usize::from(*pos_lookup_index),
            infos,
            i,
        )?;
    }
    Ok(())
}

fn apply_pos<'a>(
    gpos_cache: &LayoutCache<GPOS>,
    lookup_list: &LookupList<GPOS>,
    opt_gdef_table: Option<&GDEFTable>,
    pos_index: usize,
    lookup_index: usize,
    infos: &mut [Info],
    index: usize,
) -> Result<(), ParseError> {
    let lookup = lookup_list.lookup_cache_gpos(gpos_cache, lookup_index)?;
    let match_type = MatchType::from_lookup_flag(lookup.lookup_flag);
    let i1;
    match match_type.find_nth(opt_gdef_table, infos, index, pos_index) {
        Some(index1) => i1 = index1,
        None => return Ok(()),
    }
    match lookup.lookup_subtables {
        PosLookup::SinglePos(ref subtables) => singlepos(&subtables, &mut infos[i1]),
        PosLookup::PairPos(ref subtables) => {
            if let Some(i2) = match_type.find_next(opt_gdef_table, infos, i1) {
                pairpos(&subtables, i1, i2, infos)
            } else {
                Ok(())
            }
        }
        PosLookup::CursivePos(ref subtables) => {
            if let Some(i2) = match_type.find_next(opt_gdef_table, infos, i1) {
                cursivepos(&subtables, i1, i2, lookup.lookup_flag, infos)
            } else {
                Ok(())
            }
        }
        PosLookup::MarkBasePos(ref subtables) => {
            // FIXME is this correct?
            if let Some(base_index) = MatchType::ignore_marks().find_prev(opt_gdef_table, infos, i1)
            {
                markbasepos(&subtables, base_index, i1, infos)
            } else {
                Ok(())
            }
        }
        PosLookup::MarkLigPos(ref subtables) => {
            // FIXME is this correct?
            if let Some(base_index) = MatchType::ignore_marks().find_prev(opt_gdef_table, infos, i1)
            {
                markligpos(&subtables, base_index, i1, infos)
            } else {
                Ok(())
            }
        }
        PosLookup::MarkMarkPos(ref subtables) => {
            // FIXME is this correct?
            if let Some(base_index) = match_type.find_prev(opt_gdef_table, infos, i1) {
                markmarkpos(&subtables, base_index, i1, infos)
            } else {
                Ok(())
            }
        }
        PosLookup::ContextPos(ref _subtables) => Ok(()),
        PosLookup::ChainContextPos(ref _subtables) => Ok(()),
    }
}
