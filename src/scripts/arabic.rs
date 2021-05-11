//! Implementation of font shaping for Arabic scripts
//!
//! Code herein follows the specification at:
//! <https://github.com/n8willis/opentype-shaping-documents/blob/master/opentype-shaping-arabic-general.md>

use crate::error::{ParseError, ShapingError};
use crate::gsub::{self, FeatureMask, GlyphData, GlyphOrigin, RawGlyph};
use crate::layout::{GDEFTable, LayoutCache, LayoutTable, GSUB};
use crate::tag;

use std::convert::From;
use unicode_ccc::{get_canonical_combining_class, CanonicalCombiningClass};
use unicode_joining_type::{get_joining_type, JoiningType};

#[derive(Clone)]
struct ArabicData {
    joining_type: JoiningType,
    canonical_combining_class: CanonicalCombiningClass,
    feature_tag: u32,
}

impl GlyphData for ArabicData {
    fn merge(data1: ArabicData, _data2: ArabicData) -> ArabicData {
        // TODO hold off for future Unicode normalisation changes
        data1
    }
}

// Arabic glyphs are represented as `RawGlyph` structs with `ArabicData` for its `extra_data`.
type ArabicGlyph = RawGlyph<ArabicData>;

impl ArabicGlyph {
    fn is_transparent(&self) -> bool {
        self.extra_data.joining_type == JoiningType::Transparent || self.multi_subst_dup
    }

    fn is_left_joining(&self) -> bool {
        self.extra_data.joining_type == JoiningType::LeftJoining
            || self.extra_data.joining_type == JoiningType::DualJoining
            || self.extra_data.joining_type == JoiningType::JoinCausing
    }

    fn is_right_joining(&self) -> bool {
        self.extra_data.joining_type == JoiningType::RightJoining
            || self.extra_data.joining_type == JoiningType::DualJoining
            || self.extra_data.joining_type == JoiningType::JoinCausing
    }

    fn canonical_combining_class(&self) -> CanonicalCombiningClass {
        self.extra_data.canonical_combining_class
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
        // Since there's no `Char` to work out the `ArabicGlyph`s joining type when the glyph's
        // `glyph_origin` is `GlyphOrigin::Direct`, we fallback to `JoiningType::NonJoining` as
        // the safest approach
        let joining_type = match raw_glyph.glyph_origin {
            GlyphOrigin::Char(c) => get_joining_type(c),
            GlyphOrigin::Direct => JoiningType::NonJoining,
        };

        let canonical_combining_class = match raw_glyph.glyph_origin {
            GlyphOrigin::Char(c) => get_canonical_combining_class(c),
            GlyphOrigin::Direct => CanonicalCombiningClass::NotReordered,
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
            variation: raw_glyph.variation,
            extra_data: ArabicData {
                joining_type,
                canonical_combining_class,
                // For convenience, we loosely follow the spec (`2. Computing letter joining
                // states`) here by initialising all `ArabicGlyph`s to `tag::ISOL`
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
            variation: arabic_glyph.variation,
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
    lang_tag: Option<u32>,
    raw_glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    match gsub_table.find_script(script_tag)? {
        Some(s) => {
            if s.find_langsys_or_default(lang_tag)?.is_none() {
                return Ok(());
            }
        }
        None => return Ok(()),
    }

    let arabic_glyphs = &mut raw_glyphs.iter().map(ArabicGlyph::from).collect();

    reorder_marks(arabic_glyphs);

    // 1. Compound character composition and decomposition

    apply_lookups(
        FeatureMask::CCMP,
        gsub_cache,
        gsub_table,
        gdef_table,
        script_tag,
        lang_tag,
        arabic_glyphs,
        |_, _| true,
    )?;

    // 2. Computing letter joining states

    {
        let mut previous_i = arabic_glyphs
            .iter()
            .position(|g| !g.is_transparent())
            .unwrap_or(0);

        for i in (previous_i + 1)..arabic_glyphs.len() {
            if arabic_glyphs[i].is_transparent() {
                continue;
            }

            if arabic_glyphs[previous_i].is_left_joining() && arabic_glyphs[i].is_right_joining() {
                arabic_glyphs[i].set_feature_tag(tag::FINA);

                match arabic_glyphs[previous_i].feature_tag() {
                    tag::ISOL => arabic_glyphs[previous_i].set_feature_tag(tag::INIT),
                    tag::FINA => arabic_glyphs[previous_i].set_feature_tag(tag::MEDI),
                    _ => {}
                }
            }

            previous_i = i;
        }
    }

    // 3. Applying the stch feature
    //
    // TODO hold off for future generalised solution (including the Syriac Abbreviation Mark)

    // 4. Applying the language-form substitution features from GSUB

    const LANGUAGE_FEATURES: &'static [(FeatureMask, bool)] = &[
        (FeatureMask::LOCL, true),
        (FeatureMask::ISOL, false),
        (FeatureMask::FINA, false),
        (FeatureMask::MEDI, false),
        (FeatureMask::INIT, false),
        (FeatureMask::RLIG, true),
        (FeatureMask::RCLT, true),
        (FeatureMask::CALT, true),
    ];

    for &(feature_mask, is_global) in LANGUAGE_FEATURES {
        apply_lookups(
            feature_mask,
            gsub_cache,
            gsub_table,
            gdef_table,
            script_tag,
            lang_tag,
            arabic_glyphs,
            |g, feature_tag| is_global || g.feature_tag() == feature_tag,
        )?;
    }

    // 5. Applying the typographic-form substitution features from GSUB
    //
    // Note that we skip `GSUB`'s `DLIG` and `CSWH` features as results would differ from other
    // Arabic shapers

    const TYPOGRAPHIC_FEATURES: &'static [FeatureMask] = &[FeatureMask::LIGA, FeatureMask::MSET];

    for &feature_mask in TYPOGRAPHIC_FEATURES {
        apply_lookups(
            feature_mask,
            gsub_cache,
            gsub_table,
            gdef_table,
            script_tag,
            lang_tag,
            arabic_glyphs,
            |_, _| true,
        )?;
    }

    // 6. Mark reordering
    //
    // This is done prior to stage 1.

    *raw_glyphs = arabic_glyphs.iter().map(RawGlyph::from).collect();

    Ok(())
}

fn apply_lookups(
    feature_mask: FeatureMask,
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    lang_tag: Option<u32>,
    arabic_glyphs: &mut Vec<ArabicGlyph>,
    pred: impl Fn(&ArabicGlyph, u32) -> bool + Copy,
) -> Result<(), ParseError> {
    let index = gsub::get_lookups_cache_index(gsub_cache, script_tag, lang_tag, feature_mask)?;
    let lookups = &gsub_cache.cached_lookups.borrow()[index];

    for &(lookup_index, feature_tag) in lookups {
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

/// Reorder Arabic marks per AMTRA. See: https://www.unicode.org/reports/tr53/.
fn reorder_marks(glyphs: &mut Vec<ArabicGlyph>) {
    for gs in
        glyphs.split_mut(|g| g.canonical_combining_class() == CanonicalCombiningClass::NotReordered)
    {
        reorder_marks_nfd(gs);
        reorder_marks_shadda(gs);
        reorder_marks_other_combining(gs, CanonicalCombiningClass::Above);
        reorder_marks_other_combining(gs, CanonicalCombiningClass::Below);
    }
}

fn reorder_marks_nfd(glyphs: &mut [ArabicGlyph]) {
    // 1. Normalise the input to NFD.
    fn comparator(g1: &ArabicGlyph, g2: &ArabicGlyph) -> std::cmp::Ordering {
        (g1.canonical_combining_class() as u8).cmp(&(g2.canonical_combining_class() as u8))
    }
    glyphs.sort_by(comparator)
}

fn reorder_marks_shadda(glyphs: &mut [ArabicGlyph]) {
    use std::cmp::Ordering;

    // 2a. Move any Shadda characters to the beginning of S, where S is a max
    // length substring of non-starter characters.
    fn comparator(g1: &ArabicGlyph, _g2: &ArabicGlyph) -> Ordering {
        if g1.canonical_combining_class() == CanonicalCombiningClass::CCC33 {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    }
    glyphs.sort_by(comparator)
}

fn reorder_marks_other_combining(glyphs: &mut [ArabicGlyph], ccc: CanonicalCombiningClass) {
    assert!(ccc == CanonicalCombiningClass::Below || ccc == CanonicalCombiningClass::Above);

    // Get the start index of a possible sequence of characters with canonical
    // combining class equal to `ccc`. (Assumes that `glyphs` is normalised to
    // NFD.)
    let first = glyphs
        .iter()
        .position(|g| g.canonical_combining_class() == ccc);

    if let Some(first) = first {
        // 2b/2c. If the sequence of characters _begins_ with any MCM characters,
        // move the sequence of such characters to the beginning of S.
        let count = glyphs[first..]
            .iter()
            .take_while(|g| is_modifier_combining_glyph(g))
            .count();
        glyphs[..(first + count)].rotate_right(count);
    }
}

fn is_modifier_combining_glyph(glyph: &ArabicGlyph) -> bool {
    match glyph.glyph_origin {
        GlyphOrigin::Char(ch) => is_modifier_combining_mark(ch),
        GlyphOrigin::Direct => false,
    }
}

fn is_modifier_combining_mark(ch: char) -> bool {
    match ch {
        '\u{0654}' => true, // Hamza Above
        '\u{0655}' => true, // Hamza Below
        '\u{0658}' => true, // Mark Noon Ghunna
        '\u{06DC}' => true, // Small High Seen
        '\u{06E3}' => true, // Small Low Seen
        '\u{06E7}' => true, // Small High Yeh
        '\u{06E8}' => true, // Small High Noon
        '\u{08D3}' => true, // Small Low Waw
        '\u{08F3}' => true, // Small High Waw
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // https://www.unicode.org/reports/tr53/#Demonstrating_AMTRA.
    mod reorder_marks {
        use super::*;
        use crate::tinyvec::tiny_vec;

        #[test]
        fn test_artificial() {
            let cs = vec![
                '\u{0618}', '\u{0619}', '\u{064E}', '\u{064F}', '\u{0654}', '\u{0658}', '\u{0653}',
                '\u{0654}', '\u{0651}', '\u{0656}', '\u{0651}', '\u{065C}', '\u{0655}', '\u{0650}',
            ];
            let cs_exp = vec![
                '\u{0654}', '\u{0658}', '\u{0651}', '\u{0651}', '\u{0618}', '\u{064E}', '\u{0619}',
                '\u{064F}', '\u{0650}', '\u{0656}', '\u{065C}', '\u{0655}', '\u{0653}', '\u{0654}',
            ];
            test_reorder_marks(&cs, &cs_exp);
        }

        // Variant of `test_artificial` where U+0656 is replaced with U+0655
        // to test the reordering of MCM characters for the ccc = 220 group.
        #[test]
        fn test_artificial_custom() {
            let cs = vec![
                '\u{0618}', '\u{0619}', '\u{064E}', '\u{064F}', '\u{0654}', '\u{0658}', '\u{0653}',
                '\u{0654}', '\u{0651}', '\u{0655}', '\u{0651}', '\u{065C}', '\u{0655}', '\u{0650}',
            ];
            let cs_exp = vec![
                '\u{0655}', '\u{0654}', '\u{0658}', '\u{0651}', '\u{0651}', '\u{0618}', '\u{064E}',
                '\u{0619}', '\u{064F}', '\u{0650}', '\u{065C}', '\u{0655}', '\u{0653}', '\u{0654}',
            ];
            test_reorder_marks(&cs, &cs_exp);
        }

        #[test]
        fn test_example1() {
            let cs1 = vec!['\u{0627}', '\u{064F}', '\u{0654}'];
            let cs1_exp = vec!['\u{0627}', '\u{0654}', '\u{064F}'];
            test_reorder_marks(&cs1, &cs1_exp);

            let cs2 = vec!['\u{0627}', '\u{064F}', '\u{034F}', '\u{0654}'];
            test_reorder_marks(&cs2, &cs2);

            let cs3 = vec!['\u{0649}', '\u{0650}', '\u{0655}'];
            let cs3_exp = vec!['\u{0649}', '\u{0655}', '\u{0650}'];
            test_reorder_marks(&cs3, &cs3_exp);

            let cs4 = vec!['\u{0649}', '\u{0650}', '\u{034F}', '\u{0655}'];
            test_reorder_marks(&cs4, &cs4);
        }

        #[test]
        fn test_example2a() {
            let cs = vec!['\u{0635}', '\u{06DC}', '\u{0652}'];
            test_reorder_marks(&cs, &cs);
        }

        #[test]
        fn test_example2b() {
            let cs1 = vec!['\u{0647}', '\u{0652}', '\u{06DC}'];
            let cs1_exp = vec!['\u{0647}', '\u{06DC}', '\u{0652}'];
            test_reorder_marks(&cs1, &cs1_exp);

            let cs2 = vec!['\u{0647}', '\u{0652}', '\u{034F}', '\u{06DC}'];
            test_reorder_marks(&cs2, &cs2);
        }

        #[test]
        fn test_example3() {
            let cs1 = vec!['\u{0640}', '\u{0650}', '\u{0651}', '\u{06E7}'];
            // The expected output in https://www.unicode.org/reports/tr53/#Example3
            //
            // [U+0640, U+0650, U+06E7, U+0651]
            //
            // is incorrect, in that it fails to account for U+0651 Shadda moving to
            // the front of U+0650 Kasra, per step 2a of AMTRA.
            //
            // U+06E7 Small High Yeh should then move to the front of Shadda per step
            // 2b, resulting in:
            let cs1_exp = vec!['\u{0640}', '\u{06E7}', '\u{0651}', '\u{0650}'];
            test_reorder_marks(&cs1, &cs1_exp);

            let cs2 = vec!['\u{0640}', '\u{0650}', '\u{0651}', '\u{034F}', '\u{06E7}'];
            // As above, Shadda should move to the front of Kasra, so the expected
            // output in https://www.unicode.org/reports/tr53/#Example3
            //
            // [U+0640, U+0650, U+0651, U+034F, U+06E7]
            //
            // (i.e. no changes) is also incorrect.
            let cs2_exp = vec!['\u{0640}', '\u{0651}', '\u{0650}', '\u{034F}', '\u{06E7}'];
            test_reorder_marks(&cs2, &cs2_exp);
        }

        #[test]
        fn test_example4a() {
            let cs = vec!['\u{0640}', '\u{0652}', '\u{034F}', '\u{06E8}'];
            test_reorder_marks(&cs, &cs);
        }

        #[test]
        fn test_example4b() {
            let cs1 = vec!['\u{06C6}', '\u{064F}', '\u{06E8}'];
            let cs1_exp = vec!['\u{06C6}', '\u{06E8}', '\u{064F}'];
            test_reorder_marks(&cs1, &cs1_exp);

            let cs2 = vec!['\u{06C6}', '\u{064F}', '\u{034F}', '\u{06E8}'];
            test_reorder_marks(&cs2, &cs2);
        }

        fn test_reorder_marks(cs: &Vec<char>, cs_exp: &Vec<char>) {
            let mut gs = cs.iter().map(to_mock_glyph).collect();
            reorder_marks(&mut gs);

            let cs_act = gs.iter().map(from_mock_glyph).collect::<Vec<_>>();
            assert_eq!(cs_exp, &cs_act);
        }

        fn to_mock_glyph(ch: &char) -> ArabicGlyph {
            ArabicGlyph {
                unicodes: tiny_vec![[char; 1] => *ch],
                glyph_index: 0,
                liga_component_pos: 0,
                glyph_origin: GlyphOrigin::Char(*ch),
                small_caps: false,
                multi_subst_dup: false,
                is_vert_alt: false,
                fake_bold: false,
                fake_italic: false,
                variation: None,
                extra_data: ArabicData {
                    joining_type: JoiningType::NonJoining,
                    canonical_combining_class: get_canonical_combining_class(*ch),
                    feature_tag: tag::ISOL,
                },
            }
        }

        fn from_mock_glyph(g: &ArabicGlyph) -> char {
            match g.glyph_origin {
                GlyphOrigin::Char(ch) => ch,
                GlyphOrigin::Direct => unreachable!(),
            }
        }
    }
}
