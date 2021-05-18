//! Implementation of font shaping for Thai and Lao scripts, following the specification at:
//! https://github.com/n8willis/opentype-shaping-documents/blob/master/opentype-shaping-thai-lao.md.

use crate::error::ShapingError;
use crate::gsub::{self, FeatureMask, RawGlyph};
use crate::layout::{GDEFTable, LayoutCache, LayoutTable, GSUB};

pub fn gsub_apply_thai_lao(
    gsub_cache: &LayoutCache<GSUB>,
    gsub_table: &LayoutTable<GSUB>,
    gdef_table: Option<&GDEFTable>,
    script_tag: u32,
    lang_tag: Option<u32>,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ShapingError> {
    const FEATURE_MASKS: &'static [FeatureMask] = &[FeatureMask::LOCL, FeatureMask::CCMP];

    for &feature_mask in FEATURE_MASKS {
        let index = gsub::get_lookups_cache_index(gsub_cache, script_tag, lang_tag, feature_mask)?;
        let lookups = &gsub_cache.cached_lookups.borrow()[index];

        for &(lookup_index, feature_tag) in lookups {
            gsub::gsub_apply_lookup(
                gsub_cache,
                gsub_table,
                gdef_table,
                lookup_index,
                feature_tag,
                None,
                glyphs,
                0,
                glyphs.len(),
                |_| true,
            )?;
        }
    }

    Ok(())
}
