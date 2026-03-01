mod common;
mod shape;

#[cfg(test)]
mod mongolian_tests {
    use crate::common;
    use crate::shape;

    use allsorts::binary::read::ReadScope;
    use allsorts::gsub::{FeatureMask, RawGlyph};
    use allsorts::scripts::mongolian::gsub_apply_mongolian;
    use allsorts::scripts::preprocess_text;
    use allsorts::tables::cmap::CmapSubtable;
    use allsorts::tables::OpenTypeFont;
    use allsorts::tag;
    use allsorts::Font;
    use std::sync::Arc;

    // Test isolated, initial, medial, and final forms.
    //
    // All Mongolian letters are dual-joining, so:
    //   1 letter  = ISOL
    //   2 letters = INIT FINA
    //   3 letters = INIT MEDI FINA
    //   4 letters = INIT MEDI MEDI FINA
    #[test]
    fn isol_init_medi_fina() {
        test(
            None,
            vec![
                // Single letter -> isolated form
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1820}",
                    vec![5],
                ),
                // Two letters -> initial + final forms
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1820}\u{1821}",
                    vec![8, 19],
                ),
                // Three letters -> initial + medial + final forms
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1820}\u{1821}\u{1822}",
                    vec![8, 18, 29],
                ),
                // Four letters -> initial + medial + medial + final forms
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1820}\u{1821}\u{1822}\u{1823}",
                    vec![8, 18, 28, 34],
                ),
            ],
        )
    }

    // Test Mongolian words from the forum thread:
    // https://www.princexml.com/forum/topic/5196/mongolian-baiti-font-composition
    //
    // All words match HarfBuzz output exactly.
    #[test]
    fn forum_words() {
        test(
            None,
            vec![
                // "teden" (ᠲᠡᠳᠡᠨ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1832}\u{1821}\u{1833}\u{1821}\u{1828}",
                    vec![144, 18, 153, 18, 76],
                ),
                // "ende" (ᠡᠨᠳᠡ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1821}\u{1828}\u{1833}\u{1821}",
                    vec![16, 74, 153, 19],
                ),
                // "ulan" (ᠤᠯᠠᠨ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1824}\u{182F}\u{1820}\u{1828}",
                    vec![39, 128, 10, 76],
                ),
                // "dorji" (ᠳᠣᠷᠵᠢ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1833}\u{1823}\u{1837}\u{1835}\u{1822}",
                    vec![150, 32, 176, 163, 29],
                ),
                // "sahal" (ᠰᠠᠬᠠᠯ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1830}\u{1820}\u{182C}\u{1820}\u{182F}",
                    vec![131, 10, 99, 10, 129],
                ),
                // "ni" (ᠨᠢ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1828}\u{1822}",
                    vec![72, 29],
                ),
            ],
        )
    }

    // Words with contextual alternates and ligatures.
    //
    // The Noto Sans Mongolian font uses control glyphs (inserted/removed by
    // calt/rclt lookups) to implement contextual alternates. These words
    // exercise the contextual substitution path.
    #[test]
    fn forum_words_contextual() {
        test(
            None,
            vec![
                // "itgegchid" (ᠢᠲᠡᠭᠡᠭᠴᠢᠳ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{1822}\u{1832}\u{1821}\u{182D}\u{1821}\u{182D}\u{1834}\u{1822}\u{1833}",
                    vec![24, 145, 18, 948, 115, 158, 26, 154],
                ),
                // "bolon" (ᠪᠣᠯᠤᠨ)
                (
                    "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                    "\u{182A}\u{1823}\u{182F}\u{1824}\u{1828}",
                    vec![831, 128, 41, 76],
                ),
            ],
        )
    }

    // Test a full sentence from the forum test case (sentence 2):
    // ᠡᠨᠳᠡ ᠤᠯᠠᠨ ᠦᠢᠯᠡᠳᠪᠦᠷᠢ ᠪᠠᠢᠭᠤᠯᠤᠭᠳᠠᠵᠤ ᠪᠠᠢᠨ᠎ᠠ᠃
    #[test]
    fn forum_sentence() {
        test(
            None,
            vec![(
                "tests/fonts/noto/NotoSansMongolian-Regular.ttf",
                concat!(
                    "\u{1821}\u{1828}\u{1833}\u{1821}", // ende
                    "\u{0020}",
                    "\u{1824}\u{182F}\u{1820}\u{1828}", // ulan
                    "\u{0020}",
                    "\u{1826}\u{1822}\u{182F}\u{1821}\u{1833}\u{182A}\u{1826}\u{1837}\u{1822}", // uildbueri
                    "\u{0020}",
                    "\u{182A}\u{1820}\u{1822}\u{182D}\u{1824}\u{182F}\u{1824}\u{182D}\u{1833}\u{1820}\u{1835}\u{1824}", // baigulugdaju
                    "\u{0020}",
                    "\u{182A}\u{1820}\u{1822}\u{1828}", // bain
                    "\u{180E}",                          // Mongolian Vowel Separator
                    "\u{1820}",                          // a
                    "\u{1803}",                          // Full Stop
                ),
                vec![
                    16, 74, 153, 19,   // ende
                    1597, // space
                    39, 128, 10, 76,   // ulan
                    1597, // space
                    58, 26, 128, 18, 152, 832, 176, 29,   // uildbueri
                    1597, // space
                    811, 28, 114, 41, 128, 41, 113, 153, 10, 163, 43,   // baigulugdaju
                    1597, // space
                    811, 28, // bain
                    1463, 1479, // MVS + a
                    7, 1539, // full stop
                ],
            )],
        )
    }

    fn test(lang_tag: Option<u32>, test_cases: Vec<(&str, &str, Vec<u16>)>) {
        let script_tag = tag::MONG;

        for (font_path, text, expected) in test_cases {
            let font_contents = common::read_fixture(font_path);
            let opentype_file = ReadScope::new(&font_contents)
                .read::<OpenTypeFont>()
                .unwrap();

            let font_table_provider = opentype_file
                .table_provider(0)
                .expect("Error getting font file");

            let mut font =
                Font::new(Box::new(font_table_provider)).expect("Error getting font data");

            let gsub_cache = font
                .gsub_cache()
                .expect("Error getting GSUB cache")
                .expect("Missing GSUB table");

            let cmap_subtable_data = font.cmap_subtable_data().to_vec();

            let cmap_subtable = ReadScope::new(&cmap_subtable_data)
                .read::<CmapSubtable<'_>>()
                .expect("Error getting CMAP subtable");

            let mut chars: Vec<char> = text.chars().collect();
            preprocess_text(&mut chars, script_tag);

            let mut raw_glyphs: Vec<RawGlyph<()>> = chars
                .into_iter()
                .flat_map(|ch| shape::map_glyph(&cmap_subtable, ch))
                .flatten()
                .collect();
            let max_glyphs = raw_glyphs.len().saturating_mul(10);

            gsub_apply_mongolian(
                &gsub_cache,
                &gsub_cache.layout_table,
                font.gdef_table()
                    .expect("Error getting GDEF table")
                    .as_ref()
                    .map(Arc::as_ref),
                script_tag,
                lang_tag,
                None,
                FeatureMask::empty(),
                &mut raw_glyphs,
                max_glyphs,
            )
            .unwrap();

            let actual: Vec<u16> = raw_glyphs.iter().map(|g| g.glyph_index).collect();
            assert_eq!(
                actual, expected,
                "Mismatch for text {:?}\n  actual:   {:?}\n  expected: {:?}",
                text, actual, expected,
            );
        }
    }
}
