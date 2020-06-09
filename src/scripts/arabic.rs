use crate::error::{ParseError, ShapingError};
use crate::gsub::{self, build_lookups, GlyphData, GlyphOrigin, RawGlyph};
use crate::layout::{GDEFTable, LangSys, LayoutCache, LayoutTable, GSUB};
use crate::tag;

use std::convert::From;
use unicode_joining_type::{get_joining_type, JoiningType};

#[derive(Clone)]
struct ArabicData {
    joining_type: JoiningType,
    feature_tag: u32,
}

type ArabicGlyph = RawGlyph<ArabicData>;

impl GlyphData for ArabicData {
    fn merge(data1: ArabicData, _data2: ArabicData) -> ArabicData {
        // TODO use the canonical combining class
        data1
    }
}

impl ArabicGlyph {
    fn joining_type(&self) -> JoiningType {
        self.extra_data.joining_type
    }

    fn feature_tag(&self) -> u32 {
        self.extra_data.feature_tag
    }

    fn set_feature_tag(&mut self, feature_tag: u32) {
        self.extra_data.feature_tag = feature_tag
    }
}

impl From<&RawGlyph<()>> for ArabicGlyph {
    fn from(raw_glyph: &RawGlyph<()>) -> ArabicGlyph {
        let joining_type = match raw_glyph.glyph_origin {
            GlyphOrigin::Char(c) => get_joining_type(c),
            GlyphOrigin::Direct => JoiningType::NonJoining,
        };

        ArabicGlyph {
            unicodes: raw_glyph.unicodes.clone(),
            glyph_index: raw_glyph.glyph_index,
            liga_component_pos: raw_glyph.liga_component_pos,
            glyph_origin: raw_glyph.glyph_origin,
            small_caps: raw_glyph.small_caps,
            multi_subst_dup: raw_glyph.multi_subst_dup,
            is_vert_alt: raw_glyph.is_vert_alt,
            fake_bold: raw_glyph.fake_bold,
            fake_italic: raw_glyph.fake_italic,
            extra_data: ArabicData {
                joining_type,
                feature_tag: tag::ISOL,
            },
        }
    }
}

impl From<&ArabicGlyph> for RawGlyph<()> {
    fn from(arabic_glyph: &ArabicGlyph) -> RawGlyph<()> {
        RawGlyph {
            unicodes: arabic_glyph.unicodes.clone(),
            glyph_index: arabic_glyph.glyph_index,
            liga_component_pos: arabic_glyph.liga_component_pos,
            glyph_origin: arabic_glyph.glyph_origin,
            small_caps: arabic_glyph.small_caps,
            multi_subst_dup: arabic_glyph.multi_subst_dup,
            is_vert_alt: arabic_glyph.is_vert_alt,
            fake_bold: arabic_glyph.fake_bold,
            fake_italic: arabic_glyph.fake_italic,
            extra_data: (),
        }
    }
}

pub fn gsub_apply_arabic(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    lang_tag: u32,
    raw_glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    let langsys = match gsub_table.find_script(script_tag)? {
        Some(s) => match s.find_langsys_or_default(lang_tag)? {
            Some(v) => v,
            None => return Ok(()),
        },
        None => return Ok(()),
    };

    let arabic_glyphs = &mut raw_glyphs.iter().map(ArabicGlyph::from).collect();

    // apply CCMP
    apply_lookup(
        &[tag::CCMP],
        gsub_cache,
        gsub_table,
        gdef_table,
        langsys,
        arabic_glyphs,
        |_, _| true,
    )?;

    // apply joining state

    {
        let mut previous_i = arabic_glyphs
            .iter()
            .position(|g| !should_skip(g))
            .unwrap_or(0);

        for i in (previous_i + 1)..arabic_glyphs.len() {
            if should_skip(&arabic_glyphs[i]) {
                continue;
            }

            match arabic_glyphs[previous_i].joining_type() {
                JoiningType::LeftJoining | JoiningType::DualJoining | JoiningType::JoinCausing => {
                    match arabic_glyphs[i].joining_type() {
                        JoiningType::RightJoining
                        | JoiningType::DualJoining
                        | JoiningType::JoinCausing => {
                            arabic_glyphs[i].set_feature_tag(tag::FINA);

                            match arabic_glyphs[previous_i].feature_tag() {
                                tag::ISOL => arabic_glyphs[previous_i].set_feature_tag(tag::INIT),
                                tag::FINA => arabic_glyphs[previous_i].set_feature_tag(tag::MEDI),
                                _ => {}
                            }
                        }
                        JoiningType::LeftJoining | JoiningType::NonJoining => {
                            if arabic_glyphs[previous_i].feature_tag() == tag::INIT {
                                arabic_glyphs[previous_i].set_feature_tag(tag::ISOL)
                            }
                        }
                        JoiningType::Transparent => {}
                    }
                }
                JoiningType::RightJoining | JoiningType::NonJoining => {
                    if arabic_glyphs[previous_i].feature_tag() == tag::INIT {
                        arabic_glyphs[previous_i].set_feature_tag(tag::ISOL)
                    }
                }
                JoiningType::Transparent => {}
            }

            previous_i = i;
        }
    }

    // apply language-form and typographic-form GSUB substitutions
    apply_lookup(
        &[tag::ISOL, tag::FINA, tag::MEDI, tag::INIT],
        gsub_cache,
        gsub_table,
        gdef_table,
        langsys,
        arabic_glyphs,
        |g, feature_tag| g.feature_tag() == feature_tag,
    )?;

    apply_lookup(
        &[tag::RLIG, tag::CALT, tag::LIGA, tag::MSET],
        gsub_cache,
        gsub_table,
        gdef_table,
        langsys,
        arabic_glyphs,
        |_, _| true,
    )?;

    *raw_glyphs = arabic_glyphs.iter().map(RawGlyph::from).collect();

    Ok(())
}

fn apply_lookup(
    feature_tags: &[u32],
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    gdef_table: Option<&GDEFTable>,
    langsys: &LangSys,
    arabic_glyphs: &mut Vec<RawGlyph<ArabicData>>,
    pred: impl Fn(&RawGlyph<ArabicData>, u32) -> bool + Copy,
) -> Result<(), ParseError> {
    for (lookup_index, feature_tag) in build_lookups(gsub_table, langsys, feature_tags)? {
        gsub::gsub_apply_lookup(
            gsub_cache,
            gsub_table,
            gdef_table,
            lookup_index,
            feature_tag,
            None,
            arabic_glyphs,
            0,
            arabic_glyphs.len(),
            |g| pred(g, feature_tag),
        )?;
    }

    Ok(())
}

fn should_skip(arabic_glyph: &ArabicGlyph) -> bool {
    arabic_glyph.joining_type() == JoiningType::Transparent || arabic_glyph.multi_subst_dup
}
