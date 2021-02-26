use allsorts::binary::read::ReadScope;
use allsorts::error::{ParseError, ShapingError};
use allsorts::font::read_cmap_subtable;
use allsorts::gpos::{self, Info};
use allsorts::gsub::{self, Features, GlyphOrigin, FeatureMask, RawGlyph};
use allsorts::layout::{new_layout_cache, GDEFTable, LayoutTable, GPOS, GSUB};
use allsorts::tables::cmap::{Cmap, CmapSubtable};
use allsorts::tables::{MaxpTable, OffsetTable, OpenTypeData, OpenTypeFont, TTCHeader};
use allsorts::{tag, DOTTED_CIRCLE};

use std::convert::TryFrom;
use std::path::Path;

use criterion::{criterion_group, criterion_main, Criterion};
use tinyvec::tiny_vec;

fn shape<P: AsRef<Path>>(filename: P, script_tag: u32, opt_lang_tag: Option<u32>, text: &str) {
    let buffer = std::fs::read(filename).unwrap();
    let fontfile = ReadScope::new(&buffer).read::<OpenTypeFont>().unwrap();

    match fontfile.data {
        OpenTypeData::Single(ttf) => {
            shape_ttf(&fontfile.scope, ttf, script_tag, opt_lang_tag, text).unwrap()
        }
        OpenTypeData::Collection(ttc) => {
            shape_ttc(fontfile.scope, ttc, script_tag, opt_lang_tag, text).unwrap()
        }
    }
}

fn shape_ttc<'a>(
    scope: ReadScope<'a>,
    ttc: TTCHeader<'a>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    text: &str,
) -> Result<(), ShapingError> {
    for offset_table_offset in &ttc.offset_tables {
        let offset_table_offset = usize::try_from(offset_table_offset)?;
        let offset_table = scope.offset(offset_table_offset).read::<OffsetTable>()?;
        shape_ttf(&scope, offset_table, script_tag, opt_lang_tag, text)?;
    }
    Ok(())
}

fn shape_ttf<'a>(
    scope: &ReadScope<'a>,
    ttf: OffsetTable<'a>,
    script_tag: u32,
    opt_lang_tag: Option<u32>,
    text: &str,
) -> Result<(), ShapingError> {
    let cmap = if let Some(cmap_scope) = ttf.read_table(&scope, tag::CMAP)? {
        cmap_scope.read::<Cmap>()?
    } else {
        println!("no cmap table");
        return Ok(());
    };
    let (_, cmap_subtable) = if let Some(cmap_subtable) = read_cmap_subtable(&cmap)? {
        cmap_subtable
    } else {
        println!("no suitable cmap subtable");
        return Ok(());
    };
    let num_glyphs = match ttf.read_table(&scope, tag::MAXP)? {
        Some(maxp_scope) => {
            let maxp = maxp_scope.read::<MaxpTable>()?;
            maxp.num_glyphs
        }
        None => {
            println!("no maxp table");
            return Ok(());
        }
    };
    let opt_glyphs_res: Result<Vec<_>, _> = text
        .chars()
        .map(|ch| map_glyph(&cmap_subtable, ch))
        .collect();
    let opt_glyphs = opt_glyphs_res?;
    let mut glyphs = opt_glyphs.into_iter().flatten().collect();
    if let Some(gsub_record) = ttf.find_table_record(tag::GSUB) {
        let gsub_table = gsub_record
            .read_table(&scope)?
            .read::<LayoutTable<GSUB>>()?;
        let opt_gdef_table = match ttf.find_table_record(tag::GDEF) {
            Some(gdef_record) => Some(gdef_record.read_table(&scope)?.read::<GDEFTable>()?),
            None => None,
        };
        let opt_gpos_table = match ttf.find_table_record(tag::GPOS) {
            Some(gpos_record) => Some(
                gpos_record
                    .read_table(&scope)?
                    .read::<LayoutTable<GPOS>>()?,
            ),
            None => None,
        };
        let gsub_cache = new_layout_cache(gsub_table);
        let dotted_circle_index = cmap_subtable.map_glyph(DOTTED_CIRCLE as u32)?.unwrap_or(0);
        let _res = gsub::apply(
            dotted_circle_index,
            &gsub_cache,
            opt_gdef_table.as_ref(),
            script_tag,
            opt_lang_tag,
            &Features::Mask(FeatureMask::default()),
            num_glyphs,
            &mut glyphs,
        )?;

        match opt_gpos_table {
            Some(gpos_table) => {
                let kerning = true;
                let mut infos = Info::init_from_glyphs(opt_gdef_table.as_ref(), glyphs);
                let gpos_cache = new_layout_cache(gpos_table);
                gpos::apply(
                    &gpos_cache,
                    opt_gdef_table.as_ref(),
                    kerning,
                    &[],
                    script_tag,
                    opt_lang_tag,
                    &mut infos,
                )?;
            }
            None => {}
        }
    } else {
        println!("no GSUB table");
    }
    Ok(())
}

fn map_glyph(cmap_subtable: &CmapSubtable, ch: char) -> Result<Option<RawGlyph<()>>, ParseError> {
    if let Some(glyph_index) = cmap_subtable.map_glyph(ch as u32)? {
        let glyph = make_glyph(ch, glyph_index);
        Ok(Some(glyph))
    } else {
        Ok(None)
    }
}

fn make_glyph(ch: char, glyph_index: u16) -> RawGlyph<()> {
    RawGlyph {
        unicodes: tiny_vec![[char; 1] => ch],
        glyph_index: glyph_index,
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Char(ch),
        small_caps: false,
        multi_subst_dup: false,
        is_vert_alt: false,
        fake_bold: false,
        fake_italic: false,
        extra_data: (),
        variation: None,
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("shape Hello World Noto Serif Regular", |b| {
        b.iter(|| {
            shape(
                Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("../../../tests/data/fonts/noto/NotoSerif-Regular.ttf"),
                tag::DFLT,
                None,
                "Hello World",
            )
        })
    });

    c.bench_function("shape FTL.txt Noto Serif Regular", |b| {
        b.iter(|| {
            shape(
                Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("../../../tests/data/fonts/noto/NotoSerif-Regular.ttf"),
                tag::DFLT,
                None,
                include_str!("../../../../data/doc/contrib/freetype/FTL.TXT"),
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
