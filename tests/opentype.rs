// Workaround rustfmt bug:
// https://github.com/rust-lang/rustfmt/issues/3794
#[path = "common.rs"]
mod common;
#[path = "shape.rs"]
mod shape;

use std::convert::TryFrom;
use std::path::Path;
use std::rc::Rc;

use allsorts::binary::read::ReadScope;
use allsorts::error::ShapingError;
use allsorts::font_data_impl::FontDataImpl;
use allsorts::gsub::gsub_apply_default;
use allsorts::tables::cmap::{Cmap, CmapSubtable, EncodingId, PlatformId};
use allsorts::tables::glyf::{
    BoundingBox, GlyfRecord, GlyfTable, Glyph, GlyphData, Point, SimpleGlyph, SimpleGlyphFlag,
};
use allsorts::tables::loca::LocaTable;
use allsorts::tables::{
    FontTableProvider, HeadTable, IndexToLocFormat, MaxpTable, OpenTypeFile, OpenTypeFont,
};
use allsorts::tag;

use crate::common::read_fixture;

#[test]
fn test_decode_head() {
    let buffer = read_fixture("tests/fonts/opentype/test-font.ttf");
    let file = ReadScope::new(&buffer).read::<OpenTypeFile>().unwrap();
    let expected = HeadTable {
        major_version: 1,
        minor_version: 0,
        font_revision: 65536,
        check_sum_adjustment: 3079630960,
        magic_number: 0x5F0F3CF5,
        flags: 9,
        units_per_em: 2048,
        created: 3371744314,
        modified: 3635473311,
        x_min: 1761,
        y_min: 565,
        x_max: 2007,
        y_max: 1032,
        mac_style: 0,
        lowest_rec_ppem: 9,
        font_direction_hint: 2,
        index_to_loc_format: IndexToLocFormat::Short,
        glyph_data_format: 0,
    };

    match file.font {
        OpenTypeFont::Single(ttf) => {
            let head = ttf
                .read_table(&file.scope, tag::HEAD)
                .expect("unable to read head table")
                .expect("head table not found")
                .read::<HeadTable>()
                .expect("error parsing head table");

            assert_eq!(head, expected);
        }
        OpenTypeFont::Collection(_) => unreachable!(),
    }
}

#[test]
fn test_decode_loca() {
    let buffer = read_fixture("tests/fonts/opentype/test-font.ttf");
    let file = ReadScope::new(&buffer).read::<OpenTypeFile>().unwrap();

    match file.font {
        OpenTypeFont::Single(ttf) => {
            let head = ttf
                .read_table(&file.scope, tag::HEAD)
                .expect("unable to read head table")
                .expect("head table not found")
                .read::<HeadTable>()
                .expect("error parsing head table");
            let maxp = ttf
                .read_table(&file.scope, tag::MAXP)
                .expect("unable to read maxp table")
                .expect("maxp table not found")
                .read::<MaxpTable>()
                .expect("error parsing maxp table");
            let loca = ttf
                .read_table(&file.scope, tag::LOCA)
                .expect("unable to read loca table")
                .expect("loca table not found")
                .read_dep::<LocaTable>((usize::from(maxp.num_glyphs), head.index_to_loc_format))
                .expect("error parsing loca table");

            assert_eq!(loca.offsets.len(), usize::from(maxp.num_glyphs + 1))
        }
        OpenTypeFont::Collection(_) => unreachable!(),
    }
}

#[test]
fn test_decode_glyf() {
    let buffer = read_fixture("tests/fonts/opentype/test-font.ttf");
    let file = ReadScope::new(&buffer).read::<OpenTypeFile>().unwrap();
    let glyph = Glyph {
        number_of_contours: 1,
        bounding_box: BoundingBox {
            x_min: 1761,
            y_min: 565,
            x_max: 2007,
            y_max: 1032,
        },
        data: GlyphData::Simple(SimpleGlyph {
            end_pts_of_contours: vec![2],
            instructions: vec![],
            flags: vec![
                SimpleGlyphFlag::from_bits_truncate(1),
                SimpleGlyphFlag::from_bits_truncate(51),
                SimpleGlyphFlag::from_bits_truncate(3),
            ],
            coordinates: vec![Point(1761, 565), Point(2007, 565), Point(1884, 1032)],
        }),
    };
    let expected = GlyfTable {
        records: vec![
            GlyfRecord::Empty,
            GlyfRecord::Empty,
            GlyfRecord::Parsed(glyph),
        ],
    };

    match file.font {
        OpenTypeFont::Single(ttf) => {
            let head = ttf
                .read_table(&file.scope, tag::HEAD)
                .expect("unable to read head table")
                .expect("head table not found")
                .read::<HeadTable>()
                .expect("error parsing head table");
            let maxp = ttf
                .read_table(&file.scope, tag::MAXP)
                .expect("unable to read maxp table")
                .expect("maxp table not found")
                .read::<MaxpTable>()
                .expect("error parsing maxp table");
            let loca = ttf
                .read_table(&file.scope, tag::LOCA)
                .expect("unable to read loca table")
                .expect("loca table not found")
                .read_dep::<LocaTable>((usize::from(maxp.num_glyphs), head.index_to_loc_format))
                .expect("error parsing loca table");
            let mut glyf = ttf
                .read_table(&file.scope, tag::GLYF)
                .expect("unable to read glyf table")
                .expect("glyf table not found")
                .read_dep::<GlyfTable>(&loca)
                .expect("error parsing glyf table");
            glyf.records.iter_mut().for_each(|rec| rec.parse().unwrap());

            assert_eq!(glyf, expected);
        }
        OpenTypeFont::Collection(_) => unreachable!(),
    }
}

#[test]
#[cfg(feature = "prince")]
fn test_decode_cmap_format_2() {
    let font_buffer = read_fixture("../../../data/fonts/HardGothicNormal.ttf");
    let scope = ReadScope::new(&font_buffer);
    let font_file = scope
        .read::<OpenTypeFile>()
        .expect("error reading font file");
    let ttf = match font_file.font {
        OpenTypeFont::Single(ttf) => ttf,
        OpenTypeFont::Collection(_ttc) => panic!("expected TTF"),
    };
    let cmap = ttf
        .read_table(&font_file.scope, tag::CMAP)
        .unwrap()
        .unwrap()
        .read::<Cmap>()
        .unwrap();

    // Encoding 2 (Script code) is Chinese (Traditional)
    // http://mirror.informatimago.com/next/developer.apple.com/documentation/mac/Text/Text-534.html#MARKER-2-93
    let encoding_record = cmap
        .find_subtable(PlatformId::MACINTOSH, EncodingId(2))
        .unwrap();
    let cmap_subtable = cmap
        .scope
        .offset(usize::try_from(encoding_record.offset).unwrap())
        .read::<CmapSubtable<'_>>()
        .unwrap();

    match cmap_subtable {
        CmapSubtable::Format2 { .. } => {}
        _ => panic!("expected cmap sub-table format 2 got something else"),
    }

    // Classic Mac OS Traditional Chinese encoding maps 0xA5CF to Unicode '\u0x7529' 甩
    // ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/APPLE/CHINTRAD.TXT describes the encoding.
    // The expected value was determined by examining the glyph table generated by the tx tool
    // that is part of https://github.com/adobe-type-tools/afdko/
    // tx -dump data/fonts/HardGothicNormal.ttf  | grep U+7529
    assert_eq!(cmap_subtable.map_glyph(0xA5CF).unwrap(), Some(629));

    // NOTE: Further test coverage of this format is done as part of
    // aots::cmap2_test1
}

fn shape<'a, T: FontTableProvider>(
    font: &mut FontDataImpl<T>,
    script_tag: u32,
    lang_tag: u32,
    text: &str,
) -> Result<Vec<u16>, ShapingError> {
    let cmap_subtable_data = font.cmap_subtable_data().to_vec();
    let cmap_subtable = ReadScope::new(&cmap_subtable_data)
        .read::<CmapSubtable<'_>>()
        .expect("no suitable cmap subtable");

    let opt_glyphs_res: Result<Vec<_>, _> = text
        .chars()
        .map(|ch| shape::map_glyph(&cmap_subtable, ch))
        .collect();
    let opt_glyphs = opt_glyphs_res?;
    let mut glyphs = opt_glyphs.into_iter().flatten().collect();

    let gsub_cache = font
        .gsub_cache()
        .expect("unable to get gsub cache")
        .expect("missing gsub table");
    let gdef_table = font.gdef_table().expect("unable to get gdef table");
    let vertical = false;

    gsub_apply_default(
        &|| shape::make_dotted_circle(&cmap_subtable),
        &gsub_cache,
        gdef_table.as_ref().map(Rc::as_ref),
        script_tag,
        lang_tag,
        vertical,
        font.num_glyphs(),
        &mut glyphs,
    )?;

    let glyph_indices = glyphs
        .into_iter()
        .map(|g| g.glyph_index.unwrap_or(0)) // Set to 0 if `None`, but shouldn't happen
        .collect();

    Ok(glyph_indices)
}

fn test_shape_emoji(text: &str, expected: &[u16]) {
    let font_buffer = common::read_fixture(Path::new(
        "tests/fonts/opentype/TwitterColorEmoji-SVGinOT.ttf",
    ));
    let opentype_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFile<'_>>()
        .unwrap();
    let font_table_provider = opentype_file
        .font_provider(0)
        .expect("error reading font file");
    let mut font_data_impl = FontDataImpl::new(Box::new(font_table_provider))
        .expect("error reading font data")
        .expect("missing required font tables");

    let glyph_ids = shape(&mut font_data_impl, tag::LATN, tag::DFLT, text).unwrap();
    assert_eq!(glyph_ids, expected);
}

#[test]
fn test_shape_emoji_sequence() {
    test_shape_emoji("👩🏿", &[1653]);
}

#[test]
fn test_shape_emoji_hair_component() {
    test_shape_emoji("👨🏻‍🦳", &[2790]);
}

#[test]
fn test_shape_emoji_flag() {
    test_shape_emoji("🇦🇺", &[1382]);
}

#[test]
fn test_shape_emoji_zwj_sequence() {
    test_shape_emoji("👨‍👨‍👧‍👦", &[1759]);
}
