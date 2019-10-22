// Workaround rustfmt bug:
// https://github.com/rust-lang/rustfmt/issues/3794
#[path = "common.rs"]
mod common;

use std::convert::TryFrom;
use std::path::Path;

use itertools::Itertools;

use allsorts::binary::read::ReadScope;
use allsorts::error::ShapingError;
use allsorts::gpos::{self, Placement};
use allsorts::gsub::{self, FeatureInfo, GlyphOrigin, RawGlyph};
use allsorts::layout::{new_layout_cache, GDEFTable, LayoutTable, GPOS, GSUB};
use allsorts::tables::cmap::{Cmap, CmapSubtable, EncodingId, PlatformId};
use allsorts::tables::{HheaTable, HmtxTable, MaxpTable, OffsetTable, OpenTypeFile, OpenTypeFont};
use allsorts::tag;

use crate::common::read_fixture;

fn cmap_test(font: &str, platform: u16, encoding: u16, inputs: &[u32], expected: &[u16]) {
    let font_buffer = read_fixture(Path::new("tests/aots").join(font));
    let font_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFile>()
        .expect("error reading font file");
    let ttf = match font_file.font {
        OpenTypeFont::Single(offset_table) => offset_table,
        OpenTypeFont::Collection(_) => panic!("expected a TTF font"),
    };
    let cmap = ttf
        .read_table(&font_file.scope, tag::CMAP)
        .unwrap()
        .unwrap()
        .read::<Cmap>()
        .unwrap();
    let encoding_record = cmap
        .find_subtable(PlatformId(platform), EncodingId(encoding))
        .unwrap();
    let cmap_subtable = cmap
        .scope
        .offset(usize::try_from(encoding_record.offset).unwrap())
        .read::<CmapSubtable<'_>>()
        .unwrap();

    let actual = inputs
        .iter()
        .map(|char_code| cmap_subtable.map_glyph(*char_code).unwrap().unwrap_or(0))
        .collect_vec();
    assert_eq!(actual, expected);
}

// uvs stands for Unicode Variation Selector and relates to CMAP Format 14
fn cmap_uvs_test(font: &str, inputs: &[u32], expected: &[u32]) {
    let font_buffer = read_fixture(Path::new("tests/aots").join(font));
    let font_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFile>()
        .expect("error reading font file");
    let ttf = match font_file.font {
        OpenTypeFont::Single(offset_table) => offset_table,
        OpenTypeFont::Collection(_) => panic!("expected a TTF font"),
    };
    let cmap = ttf
        .read_table(&font_file.scope, tag::CMAP)
        .unwrap()
        .unwrap()
        .read::<Cmap>()
        .unwrap();
    let encoding_record = cmap
        .find_subtable(PlatformId::UNICODE, EncodingId::WINDOWS_UNICODE_UCS4)
        .unwrap();
    let _cmap_subtable = cmap
        .scope
        .offset(usize::try_from(encoding_record.offset).unwrap())
        .read::<CmapSubtable<'_>>()
        .unwrap();

    let actual: Vec<u32> = Vec::with_capacity(inputs.len() / 2);
    for chunk in inputs.chunks(2) {
        let _char_code = chunk[0];
        let _variation_selector = chunk[1];

        // TODO: Implement when we support CMAP Format 14
    }

    assert_eq!(actual, expected);
}

fn gsub_test(
    font: &str,
    script: &str,
    language: &str,
    features: &str,
    glyph_ids: &[u16],
    expected: &[u16],
) {
    let script = tag::from_string(script).unwrap();
    let language = tag::from_string(language).unwrap();
    let features = tag::from_string(features).unwrap();

    // Load font
    let font_buffer = read_fixture(Path::new("tests/aots").join(font));
    let font_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFile>()
        .expect("error reading font file");
    let ttf = match font_file.font {
        OpenTypeFont::Single(offset_table) => offset_table,
        OpenTypeFont::Collection(_) => panic!("expected a TTF font"),
    };
    let mut glyphs = glyph_ids
        .iter()
        .map(|glyph_id| make_direct_glyph(*glyph_id))
        .collect();

    // Do gsub
    shape_ttf(
        &font_file.scope,
        ttf,
        script,
        language,
        features,
        &mut glyphs,
    )
    .unwrap();
    let glyph_indices = glyphs.into_iter().flat_map(|g| g.glyph_index).collect_vec();

    assert_eq!(glyph_indices, expected);
}

fn gpos_test(
    font: &str,
    script: &str,
    language: &str,
    features: &str,
    glyph_ids: &[u16],
    xdeltas: &[i32],
    ydeltas: &[i32],
    _refpos: Option<&[i16]>,
    _components: Option<&[i8]>,
) {
    let script = tag::from_string(script).unwrap();
    let language = tag::from_string(language).unwrap();
    let features = tag::from_string(features).unwrap();

    // Load font
    let font_buffer = read_fixture(Path::new("tests/aots").join(font));
    let font_file = ReadScope::new(&font_buffer)
        .read::<OpenTypeFile>()
        .expect("error reading font file");
    let ttf = match font_file.font {
        OpenTypeFont::Single(offset_table) => offset_table,
        OpenTypeFont::Collection(_) => panic!("expected a TTF font"),
    };

    let maxp_data = ttf
        .read_table(&font_file.scope, tag::MAXP)
        .unwrap()
        .unwrap();
    let maxp = maxp_data.read::<MaxpTable>().unwrap();
    let hhea_data = ttf
        .read_table(&font_file.scope, tag::HHEA)
        .unwrap()
        .unwrap();
    let hhea = hhea_data.read::<HheaTable>().unwrap();
    let hmtx_data = ttf
        .read_table(&font_file.scope, tag::HMTX)
        .unwrap()
        .unwrap();
    let hmtx = hmtx_data
        .read_dep::<HmtxTable>((
            usize::from(maxp.num_glyphs),
            usize::from(hhea.num_h_metrics),
        ))
        .unwrap();
    let gpos_record = ttf
        .read_table(&font_file.scope, tag::GPOS)
        .unwrap()
        .unwrap();
    let gpos_table = gpos_record.read::<LayoutTable<GPOS>>().unwrap();
    let opt_gdef_table = match ttf.find_table_record(tag::GDEF) {
        Some(gdef_record) => Some(
            gdef_record
                .read_table(&font_file.scope)
                .unwrap()
                .read::<GDEFTable>()
                .unwrap(),
        ),
        None => None,
    };
    let mut glyphs = glyph_ids
        .iter()
        .map(|glyph_id| make_direct_glyph(*glyph_id))
        .collect();

    // Apply GSUB if table is present
    if ttf.find_table_record(tag::GSUB).is_some() {
        shape_ttf(
            &font_file.scope,
            ttf,
            script,
            language,
            features,
            &mut glyphs,
        )
        .unwrap();
    }

    // Apply GPOS
    // Rc<RefCell<LayoutCacheData<T>>> LayoutCache<GSUB>;
    let cache = new_layout_cache(gpos_table);
    let script = cache
        .layout_table
        .find_script_or_default(script)
        .unwrap()
        .unwrap();
    let langsys = script.find_langsys_or_default(language).unwrap().unwrap();
    let mut infos = gpos::Info::init_from_glyphs(opt_gdef_table.as_ref(), glyphs).unwrap();
    gpos::gpos_apply0(
        &cache,
        &cache.layout_table,
        opt_gdef_table.as_ref(),
        &langsys,
        &[features],
        &mut infos,
    )
    .unwrap();

    let pos = glyph_positions(&infos, &hmtx, hhea.num_h_metrics);
    let actual_x_deltas: Vec<i32> = pos
        .iter()
        .enumerate()
        .map(|(i, (x, _))| *x - 1500 * i as i32)
        .collect();
    assert_eq!(actual_x_deltas, xdeltas);

    let actual_y_deltas: Vec<i32> = pos.iter().map(|(_, y)| *y).collect();
    assert_eq!(actual_y_deltas, ydeltas);
}

fn glyph_positions(infos: &[gpos::Info], hmtx: &HmtxTable, num_h_metrics: u16) -> Vec<(i32, i32)> {
    let mut pos = Vec::new();

    let mut x = 0;
    let y = 0;
    for i in 0..infos.len() {
        let glyph_info = &infos[i];
        let horizontal_advance = if i == 0 {
            0
        } else {
            let info = &infos[i - 1];
            i32::from(
                hmtx.horizontal_advance(info.glyph.glyph_index.unwrap(), num_h_metrics)
                    .unwrap(),
            )
        };

        let width = if glyph_info.kerning != 0 {
            horizontal_advance + i32::from(glyph_info.kerning)
        } else {
            horizontal_advance
        };

        // Adjust for distance placement
        match glyph_info.placement {
            Placement::Distance(dx, dy) => {
                pos.push((x + horizontal_advance + dx, y + dy));
            }
            Placement::Anchor(_, _) | Placement::None => {
                pos.push((x + horizontal_advance, y));
            }
        }

        x += width;
    }

    pos
}

// Variant of `bin/shape::shape_ttf`
fn shape_ttf<'a>(
    scope: &ReadScope<'a>,
    ttf: OffsetTable<'a>,
    script: u32,
    lang: u32,
    features: u32,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    let gsub_record = ttf.find_table_record(tag::GSUB).unwrap();
    let gsub_table = gsub_record
        .read_table(&scope)?
        .read::<LayoutTable<GSUB>>()?;
    let num_glyphs = ttf.read_table(&scope, tag::MAXP).unwrap().unwrap()
        .read::<MaxpTable>()?
        .num_glyphs;
    let opt_gdef_table = match ttf.find_table_record(tag::GDEF) {
        Some(gdef_record) => Some(gdef_record.read_table(&scope)?.read::<GDEFTable>()?),
        None => None,
    };

    let cache = new_layout_cache(gsub_table);
    gsub::gsub_apply_custom(
        &cache,
        opt_gdef_table.as_ref(),
        script,
        lang,
        &[FeatureInfo {
            feature_tag: features,
            alternate: None,
        }],
        num_glyphs,
        glyphs,
    )?;

    Ok(())
}

fn make_direct_glyph(glyph_index: u16) -> RawGlyph<()> {
    RawGlyph {
        unicodes: vec![],
        glyph_index: Some(glyph_index),
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Direct,
        small_caps: false,
        multi_subst_dup: false,
        is_vert_alt: false,
        fake_bold: false,
        fake_italic: false,
        extra_data: (),
    }
}

mod aots {
    use super::*;

    include!("aots/testcases.rs");
}
