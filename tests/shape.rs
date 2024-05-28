use allsorts::error::ParseError;
use allsorts::gsub::{GlyphOrigin, RawGlyph, RawGlyphFlags};
use allsorts::tables::cmap::CmapSubtable;

use tinyvec::tiny_vec;

// Variant of `bin/shape::map_glyph`
pub fn map_glyph(
    cmap_subtable: &CmapSubtable,
    ch: char,
) -> Result<Option<RawGlyph<()>>, ParseError> {
    // If the ZWNJ glyph is missing, try to fake its existence using the SPACE
    // U+0020 glyph. Mimics Prince's behaviour.
    if ch == '\u{200C}' {
        match cmap_subtable.map_glyph(ch as u32)? {
            Some(index) => Ok(Some(make_glyph(ch, index))),
            None => match cmap_subtable.map_glyph(0x0020)? {
                Some(index) => Ok(Some(make_glyph(ch, index))),
                None => Ok(None),
            },
        }
    } else {
        cmap_subtable
            .map_glyph(ch as u32)
            .map(|opt_index| opt_index.map(|index| make_glyph(ch, index)))
    }
}

// Copy of `bin/shape::make_glyph`
pub fn make_glyph(ch: char, glyph_index: u16) -> RawGlyph<()> {
    RawGlyph {
        unicodes: tiny_vec![[char; 1] => ch],
        glyph_index,
        liga_component_pos: 0,
        glyph_origin: GlyphOrigin::Char(ch),
        flags: RawGlyphFlags::empty(),
        extra_data: (),
        variation: None,
    }
}
