//! Utilites for calculating glyph advance.

use crate::binary::read::ReadScope;
use crate::error::ParseError;
use crate::tables::{HheaTable, HmtxTable, MaxpTable};

pub fn fontcode_advance(
    maxp: &MaxpTable,
    hhea: &HheaTable,
    hmtx_data: &[u8],
    glyph: u16,
) -> Result<u16, ParseError> {
    let glyph = usize::from(glyph);
    let num_glyphs = usize::from(maxp.num_glyphs);
    let num_metrics = usize::from(hhea.num_h_metrics);
    let hmtx = ReadScope::new(hmtx_data).read_dep::<HmtxTable<'_>>((num_glyphs, num_metrics))?;

    if glyph > num_glyphs - 1 {
        Ok(0)
    } else if glyph < num_metrics {
        Ok(hmtx.h_metrics.get_item(glyph).advance_width)
    } else if num_metrics > 0 {
        Ok(hmtx.h_metrics.get_item(num_metrics - 1).advance_width)
    } else {
        Err(ParseError::BadIndex)
    }
}
