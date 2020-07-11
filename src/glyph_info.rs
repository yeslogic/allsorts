#![deny(missing_docs)]

//! Utilities for accessing glyph information such as advance.

use std::borrow::Cow;
use std::collections::HashMap;

use crate::binary::read::ReadScope;
use crate::error::ParseError;
use crate::font_data_impl::Encoding;
use crate::macroman::macroman_to_char;
use crate::post::PostTable;
use crate::tables::cmap::CmapSubtable;
use crate::tables::{HheaTable, HmtxTable, MaxpTable};

/// Retrieve glyph advance.
///
/// Since the `hhea` and `vhea` tables share the same format this function will return horizontal
/// or vertical advance depending on whether `hhea` or `vhea` is supplied to the `hhea` argument.
pub fn advance(
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

rental! {
    mod rentable {
        use super::*;

        #[rental]
        pub struct Post {
            data: Box<[u8]>,
            table: PostTable<'data>,
        }
    }
}

/// Structure for looking up glyph names.
pub struct GlyphNames {
    post: Option<rentable::Post>,
    cmap: Option<CmapMappings>,
}

struct CmapMappings {
    encoding: Encoding,
    mappings: HashMap<u16, u32>,
}

impl GlyphNames {
    /// Construct a new `GlyphNames` instance.
    pub fn new(
        cmap_subtable: &Option<(Encoding, CmapSubtable<'_>)>,
        post_data: Option<Box<[u8]>>,
    ) -> Self {
        let post = post_data.and_then(|data| {
            rentable::Post::try_new_or_drop(data, |data| {
                ReadScope::new(data).read::<PostTable<'_>>()
            })
            .ok()
        });
        let cmap = cmap_subtable
            .as_ref()
            .and_then(|(encoding, subtable)| CmapMappings::new(*encoding, subtable));
        GlyphNames { post, cmap }
    }

    /// Look up the name of `gid` in the `post` and `cmap` tables.
    pub fn glyph_name<'a>(&self, gid: u16) -> Cow<'a, str> {
        // Glyph 0 is always .notdef
        if gid == 0 {
            return Cow::from(".notdef");
        }

        self.glyph_name_from_post(gid)
            .or_else(|| self.glyph_name_from_cmap(gid))
            .unwrap_or_else(|| Cow::from(format!("g{}", gid)))
    }

    fn glyph_name_from_post<'a>(&self, gid: u16) -> Option<Cow<'a, str>> {
        let post = self.post.as_ref()?;
        post.glyph_name(gid)
    }

    fn glyph_name_from_cmap<'a>(&self, gid: u16) -> Option<Cow<'a, str>> {
        let cmap = self.cmap.as_ref()?;
        cmap.glyph_name(gid)
    }
}

impl rentable::Post {
    fn glyph_name<'a>(&self, gid: u16) -> Option<Cow<'a, str>> {
        self.rent(|post: &PostTable<'_>| {
            match post.glyph_name(gid) {
                Ok(Some(glyph_name)) if glyph_name != ".notdef" => {
                    // Doesn't seem possible to avoid this allocation
                    Some(Cow::from(glyph_name.to_owned()))
                }
                _ => None,
            }
        })
    }
}

impl CmapMappings {
    fn new(encoding: Encoding, subtable: &CmapSubtable<'_>) -> Option<CmapMappings> {
        let mappings = subtable.mappings().ok()?;

        Some(CmapMappings { encoding, mappings })
    }

    fn glyph_name<'a>(&self, gid: u16) -> Option<Cow<'a, str>> {
        let &ch = self.mappings.get(&gid)?;
        match self.encoding {
            Encoding::AppleRoman => glyph_names::glyph_name(macroman_to_unicode(ch)?),
            Encoding::Unicode => glyph_names::glyph_name(ch),
            Encoding::Symbol => None,
            Encoding::Big5 => None, // FIXME
        }
    }
}

fn macroman_to_unicode(ch: u32) -> Option<u32> {
    macroman_to_char(ch as u8).map(|ch| ch as u32)
}
