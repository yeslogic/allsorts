use allsorts::error::ParseError;
use allsorts::gsub::{GlyphOrigin, RawGlyph};
use allsorts::tables::cmap::CmapSubtable;

use tinyvec::tiny_vec;

// Copy of `bin/shape::make_dotted_circle`
pub fn make_dotted_circle(cmap_subtable: &CmapSubtable) -> Vec<RawGlyph<()>> {
    match map_glyph(cmap_subtable, '\u{25CC}') {
        Ok(Some(raw_glyph)) => vec![raw_glyph],
        _ => Vec::new(),
    }
}

// Variant of `bin/shape::map_glyph`
pub fn map_glyph(
    cmap_subtable: &CmapSubtable,
    ch: char,
) -> Result<Option<RawGlyph<()>>, ParseError> {
    // Specially handle ZWNJ character, so as to mimic existing Prince behaviour
    if ch == '\u{200C}' {
        Ok(Some(make_zwnj()))
    } else {
        cmap_subtable
            .map_glyph(ch as u32)
            .map(|opt_index| opt_index.map(|index| make_glyph(ch, index)))
    }
}

// Copy of `bin/shape::make_glyph`
pub fn make_glyph(ch: char, glyph_index: u16) -> RawGlyph<()> {
    RawGlyph {
        unicodes: tiny_vec![[char; 1], ch],
        glyph_index: Some(glyph_index),
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Char(ch),
        small_caps: false,
        multi_subst_dup: false,
        is_vert_alt: false,
        fake_bold: false,
        fake_italic: false,
        extra_data: (),
    }
}

pub fn make_zwnj() -> RawGlyph<()> {
    RawGlyph {
        unicodes: tiny_vec![],
        glyph_index: None,
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Char('\u{200C}'),
        small_caps: false,
        multi_subst_dup: false,
        is_vert_alt: false,
        fake_bold: false,
        fake_italic: false,
        extra_data: (),
    }
}
