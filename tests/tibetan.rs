mod common;
mod shape;

use std::path::Path;
use std::sync::Arc;

use allsorts::binary::read::ReadScope;
use allsorts::gsub::{self, Features};
use allsorts::scripts::preprocess_text;
use allsorts::tables::cmap::CmapSubtable;
use allsorts::tables::OpenTypeFont;
use allsorts::{tag, Font, DOTTED_CIRCLE};

fn test(text: &str, expected: Vec<u16>) {
    let font_path = "tests/fonts/noto/NotoSerifTibetan-Regular.ttf";
    let font_buffer = common::read_fixture(Path::new(font_path));
    let opentype_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFont<'_>>()
        .unwrap();
    let font_table_provider = opentype_file
        .table_provider(0)
        .expect("error reading font file");
    let mut font = Font::new(Box::new(font_table_provider)).expect("error reading font data");
    let gsub_cache = font
        .gsub_cache()
        .expect("unable to get gsub cache")
        .expect("missing gsub table");
    let gdef_table = font.gdef_table().expect("unable to get gdef table");

    let script_tag = tag::TIBT;
    let lang_tag = None;

    let cmap_subtable_data = font.cmap_subtable_data().to_vec();
    let cmap_subtable = ReadScope::new(&cmap_subtable_data)
        .read::<CmapSubtable<'_>>()
        .expect("no suitable cmap subtable");

    let mut chars = text.chars().collect();
    preprocess_text(&mut chars, script_tag);
    let mut raw_glyphs = chars
        .into_iter()
        .flat_map(|ch| shape::map_glyph(&cmap_subtable, ch))
        .flatten()
        .collect();
    let dotted_circle_index = cmap_subtable
        .map_glyph(DOTTED_CIRCLE as u32)
        .unwrap()
        .unwrap_or(0);

    gsub::apply(
        dotted_circle_index,
        &gsub_cache,
        gdef_table.as_ref().map(Arc::as_ref),
        script_tag,
        lang_tag,
        &Features::default(),
        &[],
        None,
        font.num_glyphs(),
        &mut raw_glyphs,
    )
    .unwrap();

    let actual: Vec<u16> = raw_glyphs.into_iter().map(|g| g.glyph_index).collect();
    assert!(
        !actual.contains(&dotted_circle_index),
        "output contains dotted circle (glyph {dotted_circle_index}): {actual:?}",
    );
    assert_eq!(expected, actual);
}

#[cfg(test)]
mod tests {
    use super::*;

    // Base consonant KA
    #[test]
    fn test_ka() {
        test("\u{0F40}", vec![6]);
    }

    // KA + vowel sign I
    #[test]
    fn test_ka_i() {
        test("\u{0F40}\u{0F72}", vec![6, 1328]);
    }

    // KA + subjoined KA (stacked consonant)
    #[test]
    fn test_ka_subjoined_ka() {
        test("\u{0F40}\u{0F90}", vec![158]);
    }

    // Deprecated compound vowel U+0F73 (decomposes to U+0F71 + U+0F72)
    #[test]
    fn test_compound_vowel_0f73() {
        test("\u{0F40}\u{0F73}", vec![155, 1328]);
    }

    // Deprecated compound vowel U+0F75 (decomposes to U+0F71 + U+0F74)
    #[test]
    fn test_compound_vowel_0f75() {
        test("\u{0F40}\u{0F75}", vec![156]);
    }

    // Deprecated compound vowel U+0F77 (decomposes to U+0FB2 + U+0F71 + U+0F80)
    #[test]
    fn test_compound_vowel_0f77() {
        test("\u{0F40}\u{0F77}", vec![181, 1347]);
    }

    // Deprecated compound vowel U+0F79 (decomposes to U+0FB3 + U+0F71 + U+0F80)
    #[test]
    fn test_compound_vowel_0f79() {
        test("\u{0F40}\u{0F79}", vec![185, 1347]);
    }

    // Deprecated compound vowel U+0F81 (decomposes to U+0F71 + U+0F80)
    #[test]
    fn test_compound_vowel_0f81() {
        test("\u{0F40}\u{0F81}", vec![155, 1347]);
    }

    // "bod yig" (བོད་ཡིག) - the Tibetan name for the Tibetan script
    #[test]
    fn test_bod_yig() {
        test(
            "\u{0F56}\u{0F7C}\u{0F51}\u{0F0B}\u{0F61}\u{0F72}\u{0F42}",
            vec![27, 1341, 22, 1261, 38, 1328, 8],
        );
    }

    // "bkra shis bde legs" (བཀྲ་ཤིས་བདེ་ལེགས) - auspicious greeting
    #[test]
    fn test_bkra_shis_bde_legs() {
        test(
            "\u{0F56}\u{0F40}\u{0FB2}\u{0F0B}\u{0F64}\u{0F72}\u{0F66}\u{0F0B}\u{0F56}\u{0F51}\u{0F7A}\u{0F0B}\u{0F63}\u{0F7A}\u{0F42}\u{0F66}",
            vec![27, 180, 1261, 41, 1328, 43, 1261, 27, 22, 1337, 1261, 40, 1337, 8, 43],
        );
    }

    // Stacked consonant: SA + subjoined TA
    #[test]
    fn test_sa_ta() {
        test("\u{0F66}\u{0FA4}", vec![1135]);
    }

    // OM syllable
    #[test]
    fn test_om() {
        test("\u{0F00}", vec![135, 1342]);
    }

    // Pre-decomposed compound vowels (as they appear in normalized text)

    // KA + AA + I (pre-decomposed U+0F73)
    #[test]
    fn test_ka_aa_i() {
        test("\u{0F40}\u{0F71}\u{0F72}", vec![155, 1328]);
    }

    // KA + AA + U (pre-decomposed U+0F75)
    #[test]
    fn test_ka_aa_u() {
        test("\u{0F40}\u{0F71}\u{0F74}", vec![156]);
    }

    // KA + subjoined RA + AA + reversed I (pre-decomposed U+0F77)
    #[test]
    fn test_ka_ra_aa_reversed_i() {
        test("\u{0F40}\u{0FB2}\u{0F71}\u{0F80}", vec![181, 1347]);
    }

    // KA + subjoined LA + AA + reversed I (pre-decomposed U+0F79)
    #[test]
    fn test_ka_la_aa_reversed_i() {
        test("\u{0F40}\u{0FB3}\u{0F71}\u{0F80}", vec![185, 1347]);
    }

    // KA + AA + reversed I (pre-decomposed U+0F81)
    #[test]
    fn test_ka_aa_reversed_i() {
        test("\u{0F40}\u{0F71}\u{0F80}", vec![155, 1347]);
    }

    // "Om mani padme hum" (ༀ་མ་ཎི་པདྨེ་ཧཱུྃ) - the famous mantra

    // NNA + I
    #[test]
    fn test_nna_i() {
        test("\u{0F4E}\u{0F72}", vec![19, 1328]);
    }

    // PA + DA + subjoined MA + E
    #[test]
    fn test_padme() {
        test("\u{0F54}\u{0F51}\u{0FA8}\u{0F7A}", vec![25, 524, 1337]);
    }

    // HA + AA + U + candrabindu (hum with anusvara)
    #[test]
    fn test_hum() {
        test("\u{0F67}\u{0F71}\u{0F74}\u{0F83}", vec![1180, 1351]);
    }

    // Full mantra: Om mani padme hum
    #[test]
    fn test_om_mani_padme_hum() {
        test(
            "\u{0F00}\u{0F0B}\u{0F58}\u{0F0B}\u{0F4E}\u{0F72}\u{0F0B}\u{0F54}\u{0F51}\u{0FA8}\u{0F7A}\u{0F0B}\u{0F67}\u{0F71}\u{0F74}\u{0F83}",
            vec![135, 1342, 1261, 29, 1261, 19, 1328, 1261, 25, 524, 1337, 1261, 1180, 1351],
        );
    }
}
