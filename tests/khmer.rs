mod common;
mod shape;

use std::path::Path;
use std::rc::Rc;

use allsorts::binary::read::ReadScope;
use allsorts::error::ShapingError;
use allsorts::gsub::{self, FeatureMask, Features};
use allsorts::scripts::preprocess_text;
use allsorts::tables::cmap::CmapSubtable;
use allsorts::tables::{FontTableProvider, OpenTypeFont};
use allsorts::{tag, Font, DOTTED_CIRCLE};

// Variant of `bin/shape::shape_ttf`
fn shape_ttf_khmer<'a, T: FontTableProvider>(
    font: &mut Font<T>,
    script_tag: u32,
    lang_tag: Option<u32>,
    text: &str,
) -> Result<Vec<u16>, ShapingError> {
    let cmap_subtable_data = font.cmap_subtable_data().to_vec();
    let cmap_subtable = ReadScope::new(&cmap_subtable_data)
        .read::<CmapSubtable<'_>>()
        .expect("no suitable cmap subtable");

    let mut chars = text.chars().collect();
    preprocess_text(&mut chars, script_tag);

    let opt_glyphs = chars
        .iter()
        .map(|ch| shape::map_glyph(&cmap_subtable, *ch))
        .collect::<Result<Vec<_>, _>>()?;

    // Mimic the existing behaviour of Prince, which is to split a sequence if
    // a font is missing a character glyph. We previously copied the behaviour
    // in `shape.rs`, where the missing glyphs are merely omitted. This can be
    // misleading, especially when comparing the glyph indices as generated by
    // the corpus test against the PDF as generated by Prince.
    //
    // Example:
    //   Assumptions:
    //     1. A and B can form a ligature A+B iff D is base.
    //     2. A+B and C can form a ligature A+B+C iff D is base.
    //
    //   Test sequence: [A, B, Missing, C, D]
    //          Prince: [A, B] [Missing] [C, D] - No ligation. The sequence is split, and
    //                                            D is no longer the base of A and B
    //        shape.rs: [A+B+C, D] - Unexpected ligature; doesn't match Prince's PDF output
    let mut glyphs = Vec::new();
    for gs in opt_glyphs.split_inclusive(Option::is_none) {
        glyphs.push(gs.iter().flatten().cloned().collect());
    }

    let gsub_cache = font
        .gsub_cache()
        .expect("unable to get gsub cache")
        .expect("missing gsub table");
    let gdef_table = font.gdef_table().expect("unable to get gdef table");

    let dotted_circle_index = cmap_subtable.map_glyph(DOTTED_CIRCLE as u32)?.unwrap_or(0);
    for gs in glyphs.iter_mut() {
        gsub::apply(
            dotted_circle_index,
            &gsub_cache,
            gdef_table.as_ref().map(Rc::as_ref),
            script_tag,
            lang_tag,
            &Features::Mask(FeatureMask::default()),
            None,
            font.num_glyphs(),
            gs,
        )?;
    }

    let glyph_indices = glyphs
        .into_iter()
        .flatten()
        .map(|g| g.glyph_index)
        .collect();

    Ok(glyph_indices)
}

#[cfg(not(feature = "prince"))]
fn read_fixture_font<P: AsRef<Path>>(path: P) -> Vec<u8> {
    common::read_fixture(Path::new("tests/fonts").join(path))
}

#[cfg(feature = "prince")]
fn read_fixture_font<P: AsRef<Path>>(path: P) -> Vec<u8> {
    [
        Path::new("tests/fonts").join(path.as_ref()),
        Path::new("../../../tests/data/fonts").join(path.as_ref()),
    ]
    .iter()
    .find(|path| path.is_file())
    .map(common::read_fixture)
    .unwrap_or_else(|| {
        panic!(
            "unable to find fixture font {}",
            path.as_ref().to_string_lossy()
        )
    })
}

fn run_test<P: AsRef<Path>>(
    test_data: &TestData,
    expected_outputs_path: P,
    font_path: P,
    ignore: &[u16],
    expected_num_fail: usize,
) {
    let inputs = common::read_inputs("tests/khmer", test_data.inputs_path);
    let expected_outputs =
        common::parse_expected_outputs("tests/khmer", expected_outputs_path, ignore);
    assert_eq!(expected_outputs.len(), inputs.len());

    let font_buffer = read_fixture_font(font_path);
    let opentype_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFont<'_>>()
        .unwrap();
    let font_table_provider = opentype_file
        .table_provider(0)
        .expect("error reading font file");
    let mut font = Font::new(Box::new(font_table_provider)).expect("error reading font data");
    let script_tag = tag::from_string(test_data.script_tag).expect("invalid script tag");
    let lang_tag = tag::from_string(test_data.lang_tag).ok();

    let mut num_pass = 0;
    let mut num_fail = 0;
    for (i, input) in inputs.iter().enumerate() {
        let (expected_output, reason) = &expected_outputs[i];
        let line_str = format!("line {:0>5}: {}", i + 1, input);

        match shape_ttf_khmer(&mut font, script_tag, lang_tag, input) {
            Ok(actual_output) if &actual_output == expected_output => {
                if let Some(reason) = reason {
                    println!("[SUCCESS]");
                    println!("{}", line_str);
                    println!("    reason: {}", reason);
                    println!();
                }
                num_pass += 1;
            }
            Ok(actual_output) => {
                println!("{}", line_str);
                println!("  expected: {:?}", expected_output);
                println!("    actual: {:?}", actual_output);
                if let Some(reason) = reason {
                    println!("    reason: {}", reason);
                }
                println!();
                num_fail += 1;
            }
            Err(error) => {
                println!("{}", line_str);
                println!("  expected: {:?}", expected_output);
                println!("    actual: {:?}", error);
                if let Some(reason) = reason {
                    println!("    reason: {}", reason);
                }
                println!();
                num_fail += 1;
            }
        }
    }

    println!("total: {:?}", inputs.len());
    println!(" pass: {:?}", num_pass);
    println!(" fail: {:?}", num_fail);

    assert_eq!(num_pass + num_fail, inputs.len());
    assert_eq!(num_fail, expected_num_fail);
}

fn run_test_bad<P: AsRef<Path>>(test_data: &TestData, font_path: P) {
    let inputs = common::read_inputs("tests/khmer", test_data.inputs_path);

    let font_buffer = read_fixture_font(font_path);
    let opentype_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFont<'_>>()
        .unwrap();
    let font_table_provider = opentype_file
        .table_provider(0)
        .expect("error reading font file");
    let mut font = Font::new(Box::new(font_table_provider)).expect("error reading font data");
    let script_tag = tag::from_string(test_data.script_tag).expect("invalid script tag");
    let lang_tag = tag::from_string(test_data.lang_tag).ok();

    for input in inputs {
        shape_ttf_khmer(&mut font, script_tag, lang_tag, &input).unwrap();
    }
}

struct TestData<'a> {
    inputs_path: &'a str,
    script_tag: &'a str,
    lang_tag: &'a str,
}

#[cfg(test)]
mod khmer {
    use super::*;

    mod good {
        use super::*;

        const TEST_DATA: TestData = TestData {
            inputs_path: "good",
            script_tag: "khmr",
            lang_tag: "KHM",
        };

        #[test]
        fn test_battambang() {
            run_test(
                &TEST_DATA,
                "harfbuzz/good-battambang",
                "khmer/Battambang-Regular.ttf",
                &[3],
                165,
            );
        }

        #[test]
        #[cfg(feature = "prince")]
        fn test_daunpenh() {
            run_test(
                &TEST_DATA,
                "harfbuzz/good-daunpenh",
                "khmer/daunpenh.ttf",
                &[3],
                422,
            );
        }

        #[test]
        #[cfg(feature = "prince")]
        fn test_khmer_ui() {
            run_test(
                &TEST_DATA,
                "harfbuzz/good-khmer-ui",
                "khmer/KhmerUI.ttf",
                &[3],
                423,
            );
        }

        #[test]
        fn test_noto_sans() {
            run_test(
                &TEST_DATA,
                "harfbuzz/good-noto-sans",
                "noto/NotoSansKhmer-Regular.ttf",
                &[3],
                419,
            );
        }

        #[test]
        fn test_noto_serif() {
            run_test(
                &TEST_DATA,
                "harfbuzz/good-noto-serif",
                "noto/NotoSerifKhmer-Regular.ttf",
                &[3],
                419,
            );
        }
    }

    mod bad {
        use super::*;

        const TEST_DATA: TestData = TestData {
            inputs_path: "bad",
            script_tag: "khmr",
            lang_tag: "KHM",
        };

        #[test]
        fn test_battambang() {
            run_test_bad(&TEST_DATA, "khmer/Battambang-Regular.ttf");
        }

        #[test]
        #[cfg(feature = "prince")]
        fn test_daunpenh() {
            run_test_bad(&TEST_DATA, "khmer/daunpenh.ttf");
        }

        #[test]
        #[cfg(feature = "prince")]
        fn test_khmer_ui() {
            run_test_bad(&TEST_DATA, "khmer/KhmerUI.ttf");
        }

        #[test]
        fn test_noto_sans() {
            run_test_bad(&TEST_DATA, "noto/NotoSansKhmer-Regular.ttf");
        }

        #[test]
        fn test_noto_serif() {
            run_test_bad(&TEST_DATA, "noto/NotoSerifKhmer-Regular.ttf");
        }
    }
}
