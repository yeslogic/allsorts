// Workaround rustfmt bug:
// https://github.com/rust-lang/rustfmt/issues/3794
#[path = "shape.rs"]
mod shape;

use core::convert::TryFrom;
use alloc::rc::Rc;

use allsorts::binary::read::ReadScope;
use allsorts::error::ShapingError;
use allsorts::gsub::{self, Features, GsubFeatureMask};
use allsorts::tables::cmap::{Cmap, CmapSubtable, EncodingId, PlatformId};
use allsorts::tables::glyf::{
    BoundingBox, GlyfRecord, GlyfTable, Glyph, GlyphData, Point, SimpleGlyph, SimpleGlyphFlag,
};
use allsorts::tables::loca::LocaTable;
use allsorts::tables::{
    FontTableProvider, HeadTable, IndexToLocFormat, MaxpTable, OpenTypeData, OpenTypeFont,
};
use allsorts::{tag, Font, DOTTED_CIRCLE};

#[test]
fn test_decode_head() {
    let buffer = include_bytes!("tests/fonts/opentype/test-font.ttf");
    let file = ReadScope::new(&buffer).read::<OpenTypeFont>().unwrap();
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

    match file.data {
        OpenTypeData::Single(ttf) => {
            let head = ttf
                .read_table(&file.scope, tag::HEAD)
                .expect("unable to read head table")
                .expect("head table not found")
                .read::<HeadTable>()
                .expect("error parsing head table");

            assert_eq!(head, expected);
        }
        OpenTypeData::Collection(_) => unreachable!(),
    }
}

#[test]
fn test_decode_loca() {
    let buffer = include_bytes!("tests/fonts/opentype/test-font.ttf");
    let file = ReadScope::new(&buffer).read::<OpenTypeFont>().unwrap();

    match file.data {
        OpenTypeData::Single(ttf) => {
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
        OpenTypeData::Collection(_) => unreachable!(),
    }
}

#[test]
fn test_decode_glyf() {
    let buffer = include_bytes!("tests/fonts/opentype/test-font.ttf");
    let file = ReadScope::new(&buffer).read::<OpenTypeFont>().unwrap();
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

    match file.data {
        OpenTypeData::Single(ttf) => {
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
        OpenTypeData::Collection(_) => unreachable!(),
    }
}

#[test]
#[cfg(feature = "prince")]
fn test_decode_cmap_format_2() {
    let font_buffer = include_bytes!("../../../tests/data/fonts/HardGothicNormal.ttf");
    let scope = ReadScope::new(&font_buffer);
    let font_file = scope
        .read::<OpenTypeFont>()
        .expect("error reading font file");
    let ttf = match font_file.data {
        OpenTypeData::Single(ttf) => ttf,
        OpenTypeData::Collection(_ttc) => panic!("expected TTF"),
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
    font: &mut Font<T>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
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

    let dotted_circle_index = cmap_subtable.map_glyph(DOTTED_CIRCLE as u32)?.unwrap_or(0);
    gsub::apply(
        dotted_circle_index,
        &gsub_cache,
        gdef_table.as_ref().map(Rc::as_ref),
        script_tag,
        opt_lang_tag,
        &Features::Mask(GsubFeatureMask::default()),
        font.num_glyphs(),
        &mut glyphs,
    )?;

    let glyph_indices = glyphs.into_iter().map(|g| g.glyph_index).collect();

    Ok(glyph_indices)
}

fn test_shape_emoji(text: &str, expected: &[u16]) {
    let font_buffer = common::include_bytes!(Path::new(
        "tests/fonts/opentype/TwitterColorEmoji-SVGinOT.ttf",
    ));
    let opentype_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFont<'_>>()
        .unwrap();
    let font_table_provider = opentype_file
        .table_provider(0)
        .expect("error reading font file");
    let mut font = Font::new(Box::new(font_table_provider))
        .expect("error reading font data")
        .expect("missing required font tables");

    let glyph_ids = shape(&mut font, tag::LATN, None, text).unwrap();
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

#[test]
fn test_reverse_chaining_contextual_single_substitution() {
    let font_buffer = include_bytes!("tests/fonts/opentype/Ubuntu Mono with Numderline.ttf");
    let opentype_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFont<'_>>()
        .unwrap();
    let font_table_provider = opentype_file
        .table_provider(0)
        .expect("error reading font file");
    let mut font = Font::new(Box::new(font_table_provider))
        .expect("error reading font data")
        .expect("missing required font tables");

    let script_tag = tag::DFLT;
    let opt_lang_tag = None;

    // The output glyphs are copies of the numerals. Digits in a number at indexes 345 have
    // underlines.
    // https://blog.janestreet.com/commas-in-big-numbers-everywhere/
    // The modified glyphs in the font are:

    // GID  | Glyph | Name  | GID  | Glyph | Name  | GID  | Glpyh | Name  | GID  | Glpyh | Name  |
    // -----+-------+-------+------+-------+-------+------+-------+-------+------+-------+-------|
    // 1183 |   0   | nd0,0 | 1203 |   0   | nd2,0 | 1223 |  _0_  | nd4,0 | 1243 |   0   | nd6,0 |
    // 1184 |   1   | nd0,1 | 1204 |   1   | nd2,1 | 1224 |  _1_  | nd4,1 | 1244 |   1   | nd6,1 |
    // 1185 |   2   | nd0,2 | 1205 |   2   | nd2,2 | 1225 |  _2_  | nd4,2 | 1245 |   2   | nd6,2 |
    // 1186 |   3   | nd0,3 | 1206 |   3   | nd2,3 | 1226 |  _3_  | nd4,3 | 1246 |   3   | nd6,3 |
    // 1187 |   4   | nd0,4 | 1207 |   4   | nd2,4 | 1227 |  _4_  | nd4,4 | 1247 |   4   | nd6,4 |
    // 1188 |   5   | nd0,5 | 1208 |   5   | nd2,5 | 1228 |  _5_  | nd4,5 | 1248 |   5   | nd6,5 |
    // 1189 |   6   | nd0,6 | 1209 |   6   | nd2,6 | 1229 |  _6_  | nd4,6 | 1249 |   6   | nd6,6 |
    // 1190 |   7   | nd0,7 | 1210 |   7   | nd2,7 | 1230 |  _7_  | nd4,7 | 1250 |   7   | nd6,7 |
    // 1191 |   8   | nd0,8 | 1211 |   8   | nd2,8 | 1231 |  _8_  | nd4,8 | 1251 |   8   | nd6,8 |
    // 1192 |   9   | nd0,9 | 1212 |   9   | nd2,9 | 1232 |  _9_  | nd4,9 | 1252 |   9   | nd6,9 |
    //
    // 1193 |   0   | nd1,0 | 1213 |  _0_  | nd3,0 | 1233 |  _0_  | nd5,0 |
    // 1194 |   1   | nd1,1 | 1214 |  _1_  | nd3,1 | 1234 |  _1_  | nd5,1 |
    // 1195 |   2   | nd1,2 | 1215 |  _2_  | nd3,2 | 1235 |  _2_  | nd5,2 |
    // 1196 |   3   | nd1,3 | 1216 |  _3_  | nd3,3 | 1236 |  _3_  | nd5,3 |
    // 1197 |   4   | nd1,4 | 1217 |  _4_  | nd3,4 | 1237 |  _4_  | nd5,4 |
    // 1198 |   5   | nd1,5 | 1218 |  _5_  | nd3,5 | 1238 |  _5_  | nd5,5 |
    // 1199 |   6   | nd1,6 | 1219 |  _6_  | nd3,6 | 1239 |  _6_  | nd5,6 |
    // 1200 |   7   | nd1,7 | 1220 |  _7_  | nd3,7 | 1240 |  _7_  | nd5,7 |
    // 1201 |   8   | nd1,8 | 1221 |  _8_  | nd3,8 | 1241 |  _8_  | nd5,8 |
    // 1202 |   9   | nd1,9 | 1222 |  _9_  | nd3,9 | 1242 |  _9_  | nd5,9 |

    let test_cases = vec![
        ("1", vec![20]),
        ("2", vec![21]),
        ("4", vec![23]),
        ("8", vec![27]),
        ("16", vec![20, 25]),
        ("32", vec![22, 21]),
        ("64", vec![25, 23]),
        ("128", vec![20, 21, 27]),
        ("256", vec![21, 24, 25]),
        ("512", vec![24, 20, 21]),
        ("1024", vec![1214, 1203, 1195, 1187]),
        ("2048", vec![1215, 1203, 1197, 1191]),
        ("4096", vec![1217, 1203, 1202, 1189]),
        ("8192", vec![1221, 1204, 1202, 1185]),
        ("16384", vec![1224, 1219, 1206, 1201, 1187]),
        ("32768", vec![1226, 1215, 1210, 1199, 1191]),
        ("65536", vec![1229, 1218, 1208, 1196, 1189]),
        ("131072", vec![1234, 1226, 1214, 1203, 1200, 1185]),
        ("262144", vec![1235, 1229, 1215, 1204, 1197, 1187]),
        ("524288", vec![1238, 1225, 1217, 1205, 1201, 1191]),
        ("1048576", vec![1244, 1233, 1227, 1221, 1208, 1200, 1189]),
        ("2097152", vec![1245, 1233, 1232, 1220, 1204, 1198, 1185]),
        ("4194304", vec![1247, 1234, 1232, 1217, 1206, 1193, 1187]),
        ("8388608", vec![1251, 1236, 1231, 1221, 1209, 1193, 1191]),
        (
            "16777216",
            vec![1194, 1249, 1240, 1230, 1220, 1205, 1194, 1189],
        ),
        (
            "33554432",
            vec![1196, 1246, 1238, 1228, 1217, 1207, 1196, 1185],
        ),
        (
            "67108864",
            vec![1199, 1250, 1234, 1223, 1221, 1211, 1199, 1187],
        ),
        (
            "134217728",
            vec![1204, 1196, 1247, 1235, 1224, 1220, 1210, 1195, 1191],
        ),
        (
            "268435456",
            vec![1205, 1199, 1251, 1237, 1226, 1218, 1207, 1198, 1189],
        ),
        (
            "536870912",
            vec![1208, 1196, 1249, 1241, 1230, 1213, 1212, 1194, 1185],
        ),
        (
            "1073741824",
            vec![1214, 1203, 1200, 1246, 1240, 1227, 1214, 1211, 1195, 1187],
        ),
        (
            "2147483648",
            vec![1215, 1204, 1197, 1250, 1237, 1231, 1216, 1209, 1197, 1191],
        ),
        (
            "4294967296",
            vec![1217, 1205, 1202, 1247, 1242, 1229, 1220, 1205, 1202, 1189],
        ),
        (
            "8589934592",
            vec![1221, 1208, 1201, 1252, 1242, 1226, 1217, 1208, 1202, 1185],
        ),
        (
            "17179869184",
            vec![
                1224, 1220, 1204, 1200, 1252, 1241, 1229, 1222, 1204, 1201, 1187,
            ],
        ),
        (
            "34359738368",
            vec![
                1226, 1217, 1206, 1198, 1252, 1240, 1226, 1221, 1206, 1199, 1191,
            ],
        ),
        (
            "68719476736",
            vec![
                1229, 1221, 1210, 1194, 1252, 1237, 1230, 1219, 1210, 1196, 1189,
            ],
        ),
        (
            "137438953472",
            vec![
                1234, 1226, 1220, 1207, 1196, 1251, 1242, 1228, 1216, 1207, 1200, 1185,
            ],
        ),
        (
            "274877906944",
            vec![
                1235, 1230, 1217, 1211, 1200, 1250, 1242, 1223, 1219, 1212, 1197, 1187,
            ],
        ),
        (
            "549755813888",
            vec![
                1238, 1227, 1222, 1210, 1198, 1248, 1241, 1224, 1216, 1211, 1201, 1191,
            ],
        ),
    ];

    for (input, output) in test_cases {
        assert_eq!(
            shape(&mut font, script_tag, opt_lang_tag, input).unwrap(),
            output,
        );
    }
}
