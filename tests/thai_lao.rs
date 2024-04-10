mod common;
mod shape;

use std::path::Path;
use std::rc::Rc;

use allsorts::binary::read::ReadScope;
use allsorts::gsub::{self, FeatureMask, Features};
use allsorts::scripts::preprocess_text;
use allsorts::tables::cmap::CmapSubtable;
use allsorts::tables::OpenTypeFont;
use allsorts::{tag, Font, DOTTED_CIRCLE};

fn test(script_tag: &str, lang_tag: &str, font_path: &str, text: &str, expected: Vec<u16>) {
    let font_buffer = common::read_fixture(Path::new("tests/fonts").join(font_path));
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
    let gdef_table = font.gdef_table().expect("unable toflattenget gdef table");

    let script_tag = tag::from_string(script_tag).expect("invalid script tag");
    let lang_tag = Some(tag::from_string(lang_tag).expect("invalid language tag"));

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
        gdef_table.as_ref().map(Rc::as_ref),
        script_tag,
        lang_tag,
        &Features::Mask(FeatureMask::default()),
        None,
        font.num_glyphs(),
        &mut raw_glyphs,
    )
    .unwrap();

    let actual: Vec<u16> = raw_glyphs.into_iter().map(|g| g.glyph_index).collect();
    assert_eq!(expected, actual);
}

#[cfg(test)]
mod tests {
    use super::*;

    mod thai {
        use super::*;

        #[test]
        fn test_am1() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E33}",
                vec![78, 55],
            );
        }

        #[test]
        fn test_am2() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E49}\u{0E33}",
                vec![78, 94, 55],
            );
        }

        #[test]
        fn test_am3() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E49}\u{0E4D}\u{0E32}",
                vec![74, 78, 55],
            );
        }

        #[test]
        fn test_am4() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E19}\u{0E49}\u{0E19}\u{0E49}\u{0E33}",
                vec![30, 74, 30, 78, 94, 55],
            );
        }

        #[test]
        fn test_am5() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E19}\u{0E49}\u{0E19}\u{0E49}\u{0E4D}\u{0E32}",
                vec![30, 74, 30, 74, 78, 55],
            );
        }

        #[test]
        fn test_phinthu1() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E19}\u{0E38}\u{0E3A}",
                vec![30, 61, 63],
            );
        }

        #[test]
        fn test_phinthu2() {
            test(
                "thai",
                "THA",
                "noto/NotoSansThai-Regular.ttf",
                "\u{0E19}\u{0E3A}\u{0E38}",
                vec![30, 61, 63],
            );
        }
    }

    mod lao {
        use super::*;

        #[test]
        fn test_am1() {
            test(
                "lao",
                "LAO",
                "noto/NotoSansLao-Regular.ttf",
                "\u{0EB3}",
                vec![35],
            );
        }

        #[test]
        fn test_am2() {
            test(
                "lao",
                "LAO",
                "noto/NotoSansLao-Regular.ttf",
                "\u{0EC9}\u{0EB3}",
                vec![74, 34],
            );
        }

        #[test]
        fn test_am3() {
            test(
                "lao",
                "LAO",
                "noto/NotoSansLao-Regular.ttf",
                "\u{0EC9}\u{0ECD}\u{0EB2}",
                vec![68, 35],
            );
        }

        #[test]
        fn test_am4() {
            test(
                "lao",
                "LAO",
                "noto/NotoSansLao-Regular.ttf",
                "\u{0E99}\u{0EC9}\u{0E99}\u{0EC9}\u{0EB3}",
                vec![15, 68, 15, 99, 34],
            );
        }

        #[test]
        fn test_am5() {
            test(
                "lao",
                "LAO",
                "noto/NotoSansLao-Regular.ttf",
                "\u{0E99}\u{0EC9}\u{0E99}\u{0EC9}\u{0ECD}\u{0EB2}",
                vec![15, 68, 15, 68, 35],
            );
        }
    }
}
