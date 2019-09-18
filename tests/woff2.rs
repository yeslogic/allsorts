mod common;

use std::path::Path;

use allsorts::binary::read::ReadScope;
use allsorts::tables::glyf::{
    BoundingBox, CompositeGlyph, CompositeGlyphArgument, CompositeGlyphFlag, GlyfRecord, GlyfTable,
    Glyph, GlyphData, Point, SimpleGlyph, SimpleGlyphFlag,
};
use allsorts::tables::{HeadTable, HheaTable, HmtxTable, LongHorMetric, MaxpTable};
use allsorts::tag;
use allsorts::woff2::{Woff2File, Woff2GlyfTable, Woff2HmtxTable, Woff2LocaTable};

use crate::common::read_fixture;

macro_rules! read_table {
    ($file:ident, $tag:path, $t:ty) => {
        $file
            .read_table($tag, 0)
            .expect("error reading table")
            .expect("no table found")
            .scope()
            .read::<$t>()
            .expect("unable to parse")
    };
}

fn with_woff2_glyf_table<'a, F, P>(path: P, f: F)
where
    F: FnOnce(GlyfTable) -> (),
    P: AsRef<Path>,
{
    let buffer = read_fixture(path);
    let woff = ReadScope::new(&buffer)
        .read::<Woff2File>()
        .expect("error reading Woff2File");
    let entry = woff
        .table_directory
        .iter()
        .find(|entry| entry.tag == tag::GLYF)
        .expect("unable to find glyf table entry");
    let table = entry
        .read_table(&woff.table_data_block_scope())
        .expect("unable to read table");
    let head = read_table!(woff, tag::HEAD, HeadTable);
    let maxp = read_table!(woff, tag::MAXP, MaxpTable);
    let loca_entry = woff
        .find_table_entry(tag::LOCA, 0)
        .expect("no loca table found");
    let loca = loca_entry
        .read_table(&woff.table_data_block_scope())
        .expect("error reading loca table");
    let loca = loca
        .scope()
        .read_dep::<Woff2LocaTable>((
            &loca_entry,
            usize::from(maxp.num_glyphs),
            head.index_to_loc_format,
        ))
        .expect("error parsing loca table");
    let glyf = table
        .scope()
        .read_dep::<Woff2GlyfTable>((&entry, &loca))
        .expect("unable to read Woff2GlyfTable");

    f(glyf)
}

fn with_woff2_hmtx_table<'a, F, P>(path: P, f: F)
where
    F: FnOnce(HmtxTable) -> (),
    P: AsRef<Path>,
{
    let buffer = read_fixture(path);
    let woff = ReadScope::new(&buffer)
        .read::<Woff2File>()
        .expect("error reading Woff2File");
    let glyf_entry = woff
        .find_table_entry(tag::GLYF, 0)
        .expect("unable to find glyf entry");
    let glyf_table = glyf_entry
        .read_table(&woff.table_data_block_scope())
        .expect("unable to read table");
    let head = read_table!(woff, tag::HEAD, HeadTable);
    let maxp = read_table!(woff, tag::MAXP, MaxpTable);
    let hhea = read_table!(woff, tag::HHEA, HheaTable);
    let loca_entry = woff
        .find_table_entry(tag::LOCA, 0)
        .expect("no loca table found");
    let loca = loca_entry
        .read_table(&woff.table_data_block_scope())
        .expect("error reading loca table");
    let loca = loca
        .scope()
        .read_dep::<Woff2LocaTable>((
            &loca_entry,
            usize::from(maxp.num_glyphs),
            head.index_to_loc_format,
        ))
        .expect("error parsing loca table");
    let glyf = glyf_table
        .scope()
        .read_dep::<Woff2GlyfTable>((&glyf_entry, &loca))
        .expect("unable to read Woff2GlyfTable");
    let hmtx_entry = woff
        .find_table_entry(tag::HMTX, 0)
        .expect("unable to find hmtx entry");
    let hmtx_table = hmtx_entry
        .read_table(&woff.table_data_block_scope())
        .expect("unable to read table");
    let hmtx = hmtx_table
        .scope()
        .read_dep::<Woff2HmtxTable>((
            &hmtx_entry,
            &glyf,
            usize::from(maxp.num_glyphs),
            usize::from(hhea.num_h_metrics),
        ))
        .expect("unable to read Woff2GlyfTable");

    f(hmtx)
}

#[test]
fn test_woff2_transformed_glyf_table() {
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
                SimpleGlyphFlag::from_bits_truncate(1),
                SimpleGlyphFlag::from_bits_truncate(1),
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

    with_woff2_glyf_table("tests/woff2/test-font.woff2", |glyf| {
        assert_eq!(glyf, expected)
    });
}

#[test]
fn test_woff2_transformed_glyf_table_composite_glyph_counts() {
    with_woff2_glyf_table("tests/woff2/SFNT-TTF-Composite.woff2", |glyf| {
        let glyph_counts = glyf.records.iter().fold(
            (0, 0, 0),
            |(empty, simple, composite), record| match record {
                GlyfRecord::Empty => (empty + 1, simple, composite),
                GlyfRecord::Parsed(Glyph {
                    data: GlyphData::Simple(_),
                    ..
                }) => (empty, simple + 1, composite),
                GlyfRecord::Parsed(Glyph {
                    data: GlyphData::Composite { .. },
                    ..
                }) => (empty, simple, composite + 1),
                GlyfRecord::Present(_) => unreachable!(),
            },
        );

        assert_eq!(glyph_counts, (2, 8, 2))
    });
}

#[test]
fn test_woff2_transformed_glyf_table_composite_glyph() {
    // Examine the 'F' glyph, which is the first composite glyph in the font
    let expected = Glyph {
        number_of_contours: -1,
        bounding_box: BoundingBox {
            x_min: 205,
            y_min: 0,
            x_max: 4514,
            y_max: 1434,
        },
        data: GlyphData::Composite {
            glyphs: vec![
                CompositeGlyph {
                    flags: CompositeGlyphFlag::from_bits_truncate(0x1027),
                    glyph_index: 7,
                    argument1: CompositeGlyphArgument::I16(3453),
                    argument2: CompositeGlyphArgument::I16(0),
                    scale: None,
                },
                CompositeGlyph {
                    flags: CompositeGlyphFlag::from_bits_truncate(0x1027),
                    glyph_index: 6,
                    argument1: CompositeGlyphArgument::I16(2773),
                    argument2: CompositeGlyphArgument::I16(0),
                    scale: None,
                },
                CompositeGlyph {
                    flags: CompositeGlyphFlag::from_bits_truncate(0x1027),
                    glyph_index: 5,
                    argument1: CompositeGlyphArgument::I16(1182),
                    argument2: CompositeGlyphArgument::I16(0),
                    scale: None,
                },
                CompositeGlyph {
                    flags: CompositeGlyphFlag::from_bits_truncate(0x1007),
                    glyph_index: 4,
                    argument1: CompositeGlyphArgument::I16(205),
                    argument2: CompositeGlyphArgument::I16(0),
                    scale: None,
                },
            ],
            instructions: &[],
        },
    };

    with_woff2_glyf_table("tests/woff2/SFNT-TTF-Composite.woff2", |glyf| {
        let actual = glyf
            .records
            .iter()
            .map(|glyph| match glyph {
                GlyfRecord::Parsed(
                    found @ Glyph {
                        data: GlyphData::Composite { .. },
                        ..
                    },
                ) => Some(found),
                _ => None,
            })
            .find(|candidate| candidate.is_some())
            .unwrap()
            .unwrap();

        assert_eq!(*actual, expected)
    });
}

#[test]
fn test_woff2_regular_hmtx_table() {
    with_woff2_hmtx_table("tests/woff2/test-font.woff2", |hmtx| {
        let expected = vec![
            LongHorMetric {
                advance_width: 1536,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 1536,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 4719,
                lsb: 205,
            },
        ];
        let h_metrics: Vec<_> = hmtx.h_metrics.iter().collect();

        assert_eq!(h_metrics, expected);
        assert!(hmtx.left_side_bearings.is_empty());
    });
}

// Test that transformed hmtx table is reconstructed as expected
#[test]
fn test_woff2_transformed_hmtx_table() {
    with_woff2_hmtx_table("tests/woff2/roundtrip-hmtx-lsb-001.woff2", |hmtx| {
        // The expected values were determined as follows:
        // $ woff2_decompress tests/woff2/roundtrip-hmtx-lsb-001.woff2
        // $ ttx tests/woff2/roundtrip-hmtx-lsb-001.ttf
        // then examining the TTX file:
        // <hmtx>
        //     <mtx name=".notdef" width="4708" lsb="0"/>
        //     <mtx name="space" width="4719" lsb="0"/>
        //     <mtx name="F" width="1536" lsb="205"/>
        //     <mtx name="P" width="1536" lsb="205"/>
        // </hmtx>
        // woff2_decompress is part of https://github.com/google/woff2
        let expected = vec![
            LongHorMetric {
                advance_width: 4708,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 4719,
                lsb: 0,
            },
            LongHorMetric {
                advance_width: 1536,
                lsb: 205,
            },
            LongHorMetric {
                advance_width: 1536,
                lsb: 205,
            },
        ];
        let h_metrics: Vec<_> = hmtx.h_metrics.iter().collect();
        let left_side_bearings: Vec<_> = hmtx.left_side_bearings.iter().collect();

        assert_eq!(h_metrics, expected);
        assert_eq!(left_side_bearings, vec![0, 0, 205, 205]);
    });
}

// Test that WOFF2 file containing a font collection is traversable with the correct tables present
#[test]
fn test_woff2_ttc() {
    let buffer = read_fixture("tests/woff2/roundtrip-offset-tables-001.woff2");
    let woff = ReadScope::new(&buffer)
        .read::<Woff2File>()
        .expect("error reading Woff2File");

    // Expected values determined by running:
    // woff2_info tests/woff2/roundtrip-offset-tables-001.woff2
    // woff2_info is part of https://github.com/google/woff2
    let expected = vec![
        vec![
            tag::OS_2,
            tag::VDMX,
            tag::CMAP,
            tag::GLYF,
            tag::LOCA,
            tag::HEAD,
            tag::HHEA,
            tag::HMTX,
            tag::MAXP,
            tag::NAME,
            tag::POST
        ];
        3
    ];

    if let Some(ref collection_directory) = woff.collection_directory {
        let collection_tables: Vec<Vec<_>> = collection_directory
            .fonts()
            .map(|font| font.table_entries(&woff).map(|table| table.tag).collect())
            .collect();

        assert_eq!(collection_tables, expected);
    } else {
        panic!("expected font to contain a collection but it did not");
    }
}
