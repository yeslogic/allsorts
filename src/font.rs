use std::borrow::Cow;
use std::convert::{self, TryFrom};
use std::rc::Rc;

use bitflags::bitflags;
use rustc_hash::FxHashMap;
use tinyvec::tiny_vec;

use crate::big5::unicode_to_big5;
use crate::binary::read::ReadScope;
use crate::bitmap::cbdt::{self, CBDTTable, CBLCTable};
use crate::bitmap::sbix::Sbix as SbixTable;
use crate::bitmap::{BitDepth, BitmapGlyph};
use crate::error::{ParseError, ShapingError};
use crate::glyph_info::GlyphNames;
use crate::gpos::Info;
use crate::gsub::{Features, GlyphOrigin, RawGlyph};
use crate::layout::{new_layout_cache, GDEFTable, LayoutCache, LayoutTable, GPOS, GSUB};
use crate::macroman::char_to_macroman;
use crate::tables::cmap::{Cmap, CmapSubtable, EncodingId, EncodingRecord, PlatformId};
use crate::tables::os2::Os2;
use crate::tables::svg::SvgTable;
use crate::tables::{FontTableProvider, HeadTable, HheaTable, MaxpTable};
use crate::unicode::{self, VariationSelector};
use crate::{glyph_info, tag};
use crate::{gpos, gsub, DOTTED_CIRCLE};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Encoding {
    Unicode = 1,
    Symbol = 2,
    AppleRoman = 3,
    Big5 = 4,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum OutlineFormat {
    Glyf,
    Cff,
    Svg,
    None,
}

enum LazyLoad<T> {
    NotLoaded,
    Loaded(Option<T>),
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum MatchingPresentation {
    Required,
    NotRequired,
}

/// For now `GlyphCache` only stores the index of U+25CC DOTTED CIRCLE. The intention is for this
/// to become a more general cache in the future.
///
/// `None` indicates that dotted circle has never been looked up. A value otherwise is the index of
/// the glyph.
struct GlyphCache(Option<(u16, VariationSelector)>);

pub struct Font<T: FontTableProvider> {
    pub font_table_provider: T,
    cmap_table: Box<[u8]>,
    pub maxp_table: MaxpTable,
    hmtx_table: Box<[u8]>,
    pub hhea_table: HheaTable,
    vmtx_table: LazyLoad<Box<[u8]>>,
    vhea_table: LazyLoad<Rc<HheaTable>>,
    cmap_subtable_offset: usize,
    pub cmap_subtable_encoding: Encoding,
    gdef_cache: LazyLoad<Rc<GDEFTable>>,
    gsub_cache: LazyLoad<LayoutCache<GSUB>>,
    gpos_cache: LazyLoad<LayoutCache<GPOS>>,
    os2_us_first_char_index: LazyLoad<u16>,
    glyph_cache: GlyphCache,
    pub glyph_table_flags: GlyphTableFlags,
    embedded_images: LazyLoad<Rc<Images>>,
}

pub enum Images {
    Embedded {
        cblc: tables::CBLC,
        cbdt: tables::CBDT,
    },
    Sbix(tables::Sbix),
    Svg(tables::Svg),
}

rental! {
    mod tables {
        use super::*;

        #[rental]
        pub struct CBLC {
            data: Box<[u8]>,
            table: CBLCTable<'data>
        }

        #[rental(covariant)]
        pub struct CBDT {
            data: Box<[u8]>,
            table: CBDTTable<'data>
        }

        #[rental]
        pub struct Sbix {
            data: Box<[u8]>,
            table: SbixTable<'data>
        }

        #[rental]
        pub struct Svg {
            data: Box<[u8]>,
            table: SvgTable<'data>
        }
    }
}

bitflags! {
    pub struct GlyphTableFlags: u8 {
        const GLYF = 1 << 0;
        const CFF  = 1 << 1;
        const SVG  = 1 << 2;
        const SBIX = 1 << 3;
        const CBDT = 1 << 4;
    }
}

const TABLE_TAG_FLAGS: &[(u32, GlyphTableFlags)] = &[
    (tag::GLYF, GlyphTableFlags::GLYF),
    (tag::CFF, GlyphTableFlags::CFF),
    (tag::SVG, GlyphTableFlags::SVG),
    (tag::SBIX, GlyphTableFlags::SBIX),
    (tag::CBDT, GlyphTableFlags::CBDT),
];

impl<T: FontTableProvider> Font<T> {
    pub fn new(provider: T) -> Result<Option<Font<T>>, ParseError> {
        let cmap_table = read_and_box_table(&provider, tag::CMAP)?;

        match charmap_info(&cmap_table)? {
            Some((cmap_subtable_encoding, cmap_subtable_offset)) => {
                let maxp_table =
                    ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
                let hmtx_table = read_and_box_table(&provider, tag::HMTX)?;
                let hhea_table =
                    ReadScope::new(&provider.read_table_data(tag::HHEA)?).read::<HheaTable>()?;

                let mut glyph_table_flags = GlyphTableFlags::empty();
                for &(table, flag) in TABLE_TAG_FLAGS {
                    if provider.has_table(table) {
                        glyph_table_flags |= flag
                    }
                }

                Ok(Some(Font {
                    font_table_provider: provider,
                    cmap_table,
                    maxp_table,
                    hmtx_table,
                    hhea_table,
                    vmtx_table: LazyLoad::NotLoaded,
                    vhea_table: LazyLoad::NotLoaded,
                    cmap_subtable_offset: usize::try_from(cmap_subtable_offset)?,
                    cmap_subtable_encoding,
                    gdef_cache: LazyLoad::NotLoaded,
                    gsub_cache: LazyLoad::NotLoaded,
                    gpos_cache: LazyLoad::NotLoaded,
                    os2_us_first_char_index: LazyLoad::NotLoaded,
                    glyph_cache: GlyphCache::new(),
                    glyph_table_flags,
                    embedded_images: LazyLoad::NotLoaded,
                }))
            }
            None => Ok(None),
        }
    }

    pub fn num_glyphs(&self) -> u16 {
        self.maxp_table.num_glyphs
    }

    pub fn lookup_glyph_index(
        &mut self,
        ch: char,
        match_presentation: MatchingPresentation,
        variation_selector: Option<VariationSelector>,
    ) -> (u16, VariationSelector) {
        self.glyph_cache.get(ch).unwrap_or_else(|| {
            let (glyph_index, used_variation) =
                self.map_unicode_to_glyph(ch, match_presentation, variation_selector);
            self.glyph_cache.put(ch, glyph_index, used_variation);
            (glyph_index, used_variation)
        })
    }

    /// Convenience method to shape the supplied glyphs.
    ///
    /// The method maps applies glyph substitution (`gsub`) and glyph positioning (`gpos`). Use
    /// `map_glyphs` to turn text into glyphs that can be accepted by this method.
    ///
    /// Arguments:
    ///
    /// * `glyphs`: the glyphs to be shaped.
    /// * `script_tag`: the [OpenType script tag](https://docs.microsoft.com/en-us/typography/opentype/spec/scripttags) of the text.
    /// * `opt_lang_tag`: the [OpenType language tag](https://docs.microsoft.com/en-us/typography/opentype/spec/languagetags) of the text.
    /// * `features`: the [OpenType features](https://docs.microsoft.com/en-us/typography/opentype/spec/featuretags) to enable.
    /// * `kerning`: when applying `gpos` if this argument is `true` the `kern` OpenType feature
    ///   is enabled for non-complex scripts. If it is `false` then the `kern` feature is not
    ///   enabled for non-complex scripts.
    ///
    /// ## Example
    ///
    /// ```
    /// use allsorts::binary::read::ReadScope;
    /// use allsorts::font::MatchingPresentation;
    /// use allsorts::fontfile::FontFile;
    /// use allsorts::gsub::{self, Features, GsubFeatureMask};
    /// use allsorts::DOTTED_CIRCLE;
    /// use allsorts::{tag, Font};
    ///
    /// let script = tag::LATN;
    /// let lang = tag::DFLT;
    /// let buffer = std::fs::read("tests/fonts/opentype/Klei.otf")
    ///     .expect("unable to read Klei.otf");
    /// let scope = ReadScope::new(&buffer);
    /// let font_file = scope.read::<FontFile<'_>>().expect("unable to parse font");
    /// // Use a different index to access other fonts in a font collection (E.g. TTC)
    /// let provider = font_file
    ///     .table_provider(0)
    ///     .expect("unable to create table provider");
    /// let mut font = Font::new(provider)
    ///     .expect("unable to load font tables")
    ///     .expect("unable to find suitable cmap sub-table");
    ///
    /// let glyphs = font.map_glyphs("Shaping in a jiffy."); // Klei ligates ff
    /// let glyph_infos = font
    ///     .shape(
    ///         glyphs,
    ///         script,
    ///         Some(lang),
    ///         &Features::Mask(GsubFeatureMask::default()),
    ///         true,
    ///     )
    ///     .expect("error shaping text");
    /// // We expect ff to be ligated so the number of glyphs (18) should be one less than the
    /// // number of input characters (19).
    /// assert_eq!(glyph_infos.len(), 18);
    /// ```
    pub fn shape(
        &mut self,
        mut glyphs: Vec<RawGlyph<()>>,
        script_tag: u32,
        opt_lang_tag: Option<u32>,
        features: &Features,
        kerning: bool,
    ) -> Result<Vec<Info>, ShapingError> {
        let opt_gsub_cache = self.gsub_cache()?;
        let opt_gpos_cache = self.gpos_cache()?;
        let opt_gdef_table = self.gdef_table()?;
        let opt_gdef_table = opt_gdef_table.as_ref().map(Rc::as_ref);
        let (dotted_circle_index, _) =
            self.lookup_glyph_index(DOTTED_CIRCLE, MatchingPresentation::NotRequired, None);

        // Apply gsub if table is present
        let num_glyphs = self.num_glyphs();
        if let Some(gsub_cache) = opt_gsub_cache {
            gsub::apply(
                dotted_circle_index,
                &gsub_cache,
                opt_gdef_table,
                script_tag,
                opt_lang_tag,
                features,
                num_glyphs,
                &mut glyphs,
            )?;
        }

        // Apply gpos if table is present
        let mut infos = Info::init_from_glyphs(opt_gdef_table, glyphs)?;
        if let Some(gpos_cache) = opt_gpos_cache {
            gpos::apply(
                &gpos_cache,
                opt_gdef_table,
                kerning,
                script_tag,
                opt_lang_tag,
                &mut infos,
            )?;
        } else {
            gpos::apply_fallback(&mut infos);
        }

        Ok(infos)
    }

    /// Map text to glyphs.
    ///
    /// This method maps text into glyphs, which can then be passed to `shape`.
    ///
    /// The `match_presentation` argument controls glyph mapping in the presence of emoji/text
    /// variation selectors. If `MatchingPresentation::NotRequired` is passed then glyph mapping
    /// will succeed if the font contains a mapping for a given character, regardless of whether
    /// it has the tables necessary to support the requested presentation. If
    /// `MatchingPresentation::Required` is passed then a character with emoji presentation,
    /// either by default or requested via variation selector will only map to a glyph if the font
    /// has mapping for the character, and it has the necessary tables for color emoji.
    pub fn map_glyphs(
        &mut self,
        text: &str,
        match_presentation: MatchingPresentation,
    ) -> Vec<RawGlyph<()>> {
        // We look ahead in the char stream for variation selectors. If one is found it is used for
        // mapping the current glyph. When a variation selector is reached in the stream it is
        // skipped as it was handled as part of the preceding character.
        let mut chars_iter = text.chars().peekable();
        let mut glyphs = Vec::new();
        while let Some(ch) = chars_iter.next() {
            match VariationSelector::try_from(ch) {
                Ok(_) => {} // filter out variation selectors
                Err(()) => {
                    let vs = chars_iter
                        .peek()
                        .and_then(|&next| VariationSelector::try_from(next).ok());
                    let (glyph_index, used_variation) =
                        self.lookup_glyph_index(ch, match_presentation, vs);
                    let glyph = RawGlyph {
                        unicodes: tiny_vec![[char; 1] => ch],
                        glyph_index,
                        liga_component_pos: 0,
                        glyph_origin: GlyphOrigin::Char(ch),
                        small_caps: false,
                        multi_subst_dup: false,
                        is_vert_alt: false,
                        fake_bold: false,
                        fake_italic: false,
                        extra_data: (),
                        variation: Some(used_variation),
                    };
                    glyphs.push(glyph);
                }
            }
        }
        glyphs
    }

    fn map_glyph(&self, char_code: u32) -> u16 {
        match ReadScope::new(self.cmap_subtable_data()).read::<CmapSubtable<'_>>() {
            // TODO: Cache the parsed CmapSubtable
            Ok(cmap_subtable) => match cmap_subtable.map_glyph(char_code) {
                Ok(Some(glyph_index)) => glyph_index,
                _ => 0,
            },
            Err(_err) => 0,
        }
    }

    fn map_unicode_to_glyph(
        &mut self,
        ch: char,
        match_presentation: MatchingPresentation,
        variation_selector: Option<VariationSelector>,
    ) -> (u16, VariationSelector) {
        let used_selector = Self::resolve_default_presentation(ch, variation_selector);
        let glyph_index = match self.cmap_subtable_encoding {
            Encoding::Unicode => {
                self.lookup_glyph_index_with_variation(ch as u32, match_presentation, used_selector)
            }
            Encoding::Symbol => {
                let char_code = self.legacy_symbol_char_code(ch);
                self.lookup_glyph_index_with_variation(char_code, match_presentation, used_selector)
            }
            Encoding::AppleRoman => match char_to_macroman(ch) {
                Some(char_code) => self.lookup_glyph_index_with_variation(
                    u32::from(char_code) as u32,
                    match_presentation,
                    used_selector,
                ),
                None => {
                    let char_code = self.legacy_symbol_char_code(ch);
                    self.lookup_glyph_index_with_variation(
                        char_code,
                        match_presentation,
                        used_selector,
                    )
                }
            },
            Encoding::Big5 => match unicode_to_big5(ch) {
                Some(char_code) => self.lookup_glyph_index_with_variation(
                    u32::from(char_code),
                    match_presentation,
                    used_selector,
                ),
                None => 0,
            },
        };
        (glyph_index, used_selector)
    }

    // The symbol encoding was created to support fonts with arbitrary ornaments or symbols not
    // supported in Unicode or other standard encodings. A format 4 subtable would be used,
    // typically with up to 224 graphic characters assigned at code positions beginning with
    // 0xF020. This corresponds to a sub-range within the Unicode Private-Use Area (PUA), though
    // this is not a Unicode encoding. In legacy usage, some applications would represent the
    // symbol characters in text using a single-byte encoding, and then map 0x20 to the
    // OS/2.usFirstCharIndex value in the font.
    // — https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#encoding-records-and-encodings
    fn legacy_symbol_char_code(&mut self, ch: char) -> u32 {
        let char_code0 = if ch < '\u{F000}' || ch > '\u{F0FF}' {
            ch as u32
        } else {
            ch as u32 - 0xF000
        };
        let provider = &self.font_table_provider;
        let first_char = if let Ok(Some(us_first_char_index)) =
            self.os2_us_first_char_index.get_or_load(|| {
                load_os2_table(provider)?
                    .map(|os2| Ok(os2.us_first_char_index))
                    .transpose()
            }) {
            u32::from(us_first_char_index)
        } else {
            0x20
        };
        char_code0 - 0x20 + first_char
    }

    fn lookup_glyph_index_with_variation(
        &mut self,
        char_code: u32,
        match_presentation: MatchingPresentation,
        variation_selector: VariationSelector,
    ) -> u16 {
        if match_presentation == MatchingPresentation::Required {
            let glyf_or_cff = GlyphTableFlags::GLYF | GlyphTableFlags::CFF;

            // This match aims to only return a non-zero index if the font supports the requested
            // presentation. So, if you want the glyph index for a code point using emoji presentation,
            // the font must have suitable tables. On the flip side, if you want a glyph with text
            // presentation then the font must have glyf or CFF outlines.
            if (variation_selector == VariationSelector::VS16 && self.supports_emoji())
                || (variation_selector == VariationSelector::VS15
                    && self.glyph_table_flags.intersects(glyf_or_cff))
            {
                self.map_glyph(char_code)
            } else {
                0
            }
        } else {
            self.map_glyph(char_code)
        }
    }

    fn resolve_default_presentation(
        ch: char,
        variation_selector: Option<VariationSelector>,
    ) -> VariationSelector {
        variation_selector.unwrap_or_else(|| {
            // `None` indicates no selector present so for emoji determine the default presentation.
            if unicode::bool_prop_emoji_presentation(ch) {
                VariationSelector::VS16
            } else {
                VariationSelector::VS15
            }
        })
    }

    pub fn glyph_names<'a>(&self, ids: &[u16]) -> Vec<Cow<'a, str>> {
        let post = read_and_box_optional_table(&self.font_table_provider, tag::POST)
            .ok()
            .and_then(convert::identity);
        let cmap = ReadScope::new(self.cmap_subtable_data())
            .read::<CmapSubtable<'_>>()
            .ok()
            .map(|table| (self.cmap_subtable_encoding, table));
        let glyph_namer = GlyphNames::new(&cmap, post);
        let names = ids.iter().map(|&gid| glyph_namer.glyph_name(gid));
        unique_glyph_names(names, ids.len())
    }

    /// Find an image matching the supplied criteria.
    ///
    /// * `glyph_index` is the glyph to lookup.
    /// * `target_ppem` is the desired size. If an exact match can't be found the nearest one will
    ///    be returned, favouring being oversize vs. undersized.
    /// * `max_bit_depth` is the maximum accepted bit depth of the bitmap to return. If you accept
    ///   all bit depths then use `BitDepth::ThirtyTwo`.
    pub fn lookup_glyph_image(
        &mut self,
        glyph_index: u16,
        target_ppem: u16,
        max_bit_depth: BitDepth,
    ) -> Result<Option<BitmapGlyph>, ParseError> {
        let embedded_bitmaps = match self.embedded_images()? {
            Some(embedded_bitmaps) => embedded_bitmaps,
            None => return Ok(None),
        };
        match embedded_bitmaps.as_ref() {
            Images::Embedded { cblc, cbdt } => cblc.rent(|cblc: &CBLCTable<'_>| {
                let target_ppem = if target_ppem > u16::from(std::u8::MAX) {
                    std::u8::MAX
                } else {
                    target_ppem as u8
                };
                let bitmap = match cblc.find_strike(glyph_index, target_ppem, max_bit_depth) {
                    Some(matching_strike) => {
                        let cbdt = cbdt.suffix();
                        cbdt::lookup(glyph_index, &matching_strike, cbdt)?.map(|bitmap| {
                            BitmapGlyph::try_from((&matching_strike.bitmap_size.inner, bitmap))
                        })
                    }
                    None => None,
                };
                bitmap.transpose()
            }),
            Images::Sbix(sbix) => {
                self.lookup_sbix_glyph_bitmap(sbix, false, glyph_index, target_ppem, max_bit_depth)
            }
            Images::Svg(svg) => self.lookup_svg_glyph(svg, glyph_index),
        }
    }

    /// Perform sbix lookup with `dupe` handling.
    ///
    /// The `dupe` flag indicates if this this a dupe lookup or not. To avoid potential infinite
    /// recursion we only follow one level of `dupe` indirection.
    fn lookup_sbix_glyph_bitmap(
        &self,
        sbix: &tables::Sbix,
        dupe: bool,
        glyph_index: u16,
        target_ppem: u16,
        max_bit_depth: BitDepth,
    ) -> Result<Option<BitmapGlyph>, ParseError> {
        sbix.rent(|sbix_table: &SbixTable<'_>| {
            match sbix_table.find_strike(glyph_index, target_ppem, max_bit_depth) {
                Some(strike) => {
                    match strike.read_glyph(glyph_index)? {
                        Some(ref glyph) if glyph.graphic_type == tag::DUPE => {
                            // The special graphicType of 'dupe' indicates that the data field
                            // contains a uint16, big-endian glyph ID. The bitmap data for the
                            // indicated glyph should be used for the current glyph.
                            // — https://docs.microsoft.com/en-us/typography/opentype/spec/sbix#glyph-data
                            if dupe {
                                // We're already inside a `dupe` lookup and have encountered another
                                Ok(None)
                            } else {
                                // Try again with the glyph id stored in data
                                let dupe_glyph_index =
                                    ReadScope::new(glyph.data).ctxt().read_u16be()?;
                                self.lookup_sbix_glyph_bitmap(
                                    sbix,
                                    true,
                                    dupe_glyph_index,
                                    target_ppem,
                                    max_bit_depth,
                                )
                            }
                        }
                        Some(glyph) => Ok(Some(BitmapGlyph::from((strike, &glyph)))),
                        None => Ok(None),
                    }
                }
                None => Ok(None),
            }
        })
    }

    fn lookup_svg_glyph(
        &self,
        svg: &tables::Svg,
        glyph_index: u16,
    ) -> Result<Option<BitmapGlyph>, ParseError> {
        svg.rent(
            |svg_table: &SvgTable<'_>| match svg_table.lookup_glyph(glyph_index)? {
                Some(svg_record) => BitmapGlyph::try_from(&svg_record).map(Some),
                None => Ok(None),
            },
        )
    }

    fn embedded_images(&mut self) -> Result<Option<Rc<Images>>, ParseError> {
        let provider = &self.font_table_provider;
        let num_glyphs = usize::from(self.maxp_table.num_glyphs);
        let table_flags = self.glyph_table_flags;
        self.embedded_images.get_or_load(|| {
            if table_flags.contains(GlyphTableFlags::SVG) {
                let images = load_svg(provider).map(Images::Svg)?;
                Ok(Some(Rc::new(images)))
            } else if table_flags.contains(GlyphTableFlags::CBDT) {
                let images =
                    load_cblc_cbdt(provider).map(|(cblc, cbdt)| Images::Embedded { cblc, cbdt })?;
                Ok(Some(Rc::new(images)))
            } else if table_flags.contains(GlyphTableFlags::SBIX) {
                let images = load_sbix(provider, num_glyphs).map(Images::Sbix)?;
                Ok(Some(Rc::new(images)))
            } else {
                Ok(None)
            }
        })
    }

    pub fn supports_emoji(&mut self) -> bool {
        match self.embedded_images() {
            Ok(Some(_)) => true,
            _ => false,
        }
    }

    pub fn horizontal_advance(&mut self, glyph: u16) -> Option<u16> {
        glyph_info::advance(&self.maxp_table, &self.hhea_table, &self.hmtx_table, glyph).ok()
    }

    pub fn vertical_advance(&mut self, glyph: u16) -> Option<u16> {
        let provider = &self.font_table_provider;
        let vmtx = self
            .vmtx_table
            .get_or_load(|| read_and_box_optional_table(provider, tag::VMTX))
            .ok()?;
        let vhea = self.vhea_table().ok()?;

        if let (Some(vhea), Some(vmtx_table)) = (vhea, vmtx) {
            Some(glyph_info::advance(&self.maxp_table, &vhea, &vmtx_table, glyph).unwrap())
        } else {
            None
        }
    }

    pub fn head_table(&self) -> Result<Option<HeadTable>, ParseError> {
        self.font_table_provider
            .table_data(tag::HEAD)?
            .map(|data| ReadScope::new(&data).read::<HeadTable>())
            .transpose()
    }

    pub fn os2_table(&self) -> Result<Option<Os2>, ParseError> {
        load_os2_table(&self.font_table_provider)
    }

    pub fn gdef_table(&mut self) -> Result<Option<Rc<GDEFTable>>, ParseError> {
        let provider = &self.font_table_provider;
        self.gdef_cache.get_or_load(|| {
            if let Some(gdef_data) = provider.table_data(tag::GDEF)? {
                let gdef = ReadScope::new(&gdef_data).read::<GDEFTable>()?;
                Ok(Some(Rc::new(gdef)))
            } else {
                Ok(None)
            }
        })
    }

    pub fn gsub_cache(&mut self) -> Result<Option<LayoutCache<GSUB>>, ParseError> {
        let provider = &self.font_table_provider;
        self.gsub_cache.get_or_load(|| {
            if let Some(gsub_data) = provider.table_data(tag::GSUB)? {
                let gsub = ReadScope::new(&gsub_data).read::<LayoutTable<GSUB>>()?;
                let cache = new_layout_cache::<GSUB>(gsub);
                Ok(Some(cache))
            } else {
                Ok(None)
            }
        })
    }

    pub fn gpos_cache(&mut self) -> Result<Option<LayoutCache<GPOS>>, ParseError> {
        let provider = &self.font_table_provider;
        self.gpos_cache.get_or_load(|| {
            if let Some(gpos_data) = provider.table_data(tag::GPOS)? {
                let gpos = ReadScope::new(&gpos_data).read::<LayoutTable<GPOS>>()?;
                let cache = new_layout_cache::<GPOS>(gpos);
                Ok(Some(cache))
            } else {
                Ok(None)
            }
        })
    }

    pub fn vhea_table(&mut self) -> Result<Option<Rc<HheaTable>>, ParseError> {
        let provider = &self.font_table_provider;
        self.vhea_table.get_or_load(|| {
            if let Some(vhea_data) = provider.table_data(tag::VHEA)? {
                let vhea = ReadScope::new(&vhea_data).read::<HheaTable>()?;
                Ok(Some(Rc::new(vhea)))
            } else {
                Ok(None)
            }
        })
    }

    pub fn cmap_subtable_data(&self) -> &[u8] {
        &self.cmap_table[self.cmap_subtable_offset..]
    }
}

impl<T> LazyLoad<T> {
    /// Return loaded value, calls the supplied closure if not already loaded.
    ///
    /// It's expected that `T` is cheap to clone, either because it's wrapped in an `Rc`
    /// or is `Copy`.
    fn get_or_load(
        &mut self,
        do_load: impl FnOnce() -> Result<Option<T>, ParseError>,
    ) -> Result<Option<T>, ParseError>
    where
        T: Clone,
    {
        match self {
            LazyLoad::Loaded(Some(ref data)) => Ok(Some(data.clone())),
            LazyLoad::Loaded(None) => Ok(None),
            LazyLoad::NotLoaded => {
                let data = do_load()?;
                *self = LazyLoad::Loaded(data.clone());
                Ok(data)
            }
        }
    }
}

impl GlyphCache {
    fn new() -> Self {
        GlyphCache(None)
    }

    fn get(&self, ch: char) -> Option<(u16, VariationSelector)> {
        if ch == DOTTED_CIRCLE {
            self.0
        } else {
            None
        }
    }

    fn put(&mut self, ch: char, glyph_index: u16, variation_selector: VariationSelector) {
        if ch == DOTTED_CIRCLE {
            match self.0 {
                Some(_) => panic!("duplicate entry"),
                None => self.0 = Some((glyph_index, variation_selector)),
            }
        }
    }
}

fn read_and_box_table(
    provider: &impl FontTableProvider,
    tag: u32,
) -> Result<Box<[u8]>, ParseError> {
    provider
        .read_table_data(tag)
        .map(|table| Box::from(table.into_owned()))
}

fn read_and_box_optional_table(
    provider: &impl FontTableProvider,
    tag: u32,
) -> Result<Option<Box<[u8]>>, ParseError> {
    Ok(provider
        .table_data(tag)?
        .map(|table| Box::from(table.into_owned())))
}

fn load_os2_table(provider: &impl FontTableProvider) -> Result<Option<Os2>, ParseError> {
    provider
        .table_data(tag::OS_2)?
        .map(|data| ReadScope::new(&data).read_dep::<Os2>(data.len()))
        .transpose()
}

fn load_cblc_cbdt(
    provider: &impl FontTableProvider,
) -> Result<(tables::CBLC, tables::CBDT), ParseError> {
    let cblc_data = read_and_box_table(provider, tag::CBLC)?;
    let cbdt_data = read_and_box_table(provider, tag::CBDT)?;

    let cblc = tables::CBLC::try_new_or_drop(cblc_data, |data| {
        ReadScope::new(data).read::<CBLCTable<'_>>()
    })?;
    let cbdt = tables::CBDT::try_new_or_drop(cbdt_data, |data| {
        ReadScope::new(data).read::<CBDTTable<'_>>()
    })?;

    Ok((cblc, cbdt))
}

fn load_sbix(
    provider: &impl FontTableProvider,
    num_glyphs: usize,
) -> Result<tables::Sbix, ParseError> {
    let sbix_data = read_and_box_table(provider, tag::SBIX)?;
    tables::Sbix::try_new_or_drop(sbix_data, |data| {
        ReadScope::new(data).read_dep::<SbixTable<'_>>(num_glyphs)
    })
}

fn load_svg(provider: &impl FontTableProvider) -> Result<tables::Svg, ParseError> {
    let svg_data = read_and_box_table(provider, tag::SVG)?;
    tables::Svg::try_new_or_drop(svg_data, |data| ReadScope::new(data).read::<SvgTable<'_>>())
}

fn charmap_info(cmap_buf: &[u8]) -> Result<Option<(Encoding, u32)>, ParseError> {
    let cmap = ReadScope::new(cmap_buf).read::<Cmap<'_>>()?;
    Ok(find_good_cmap_subtable(&cmap)
        .map(|(encoding, encoding_record)| (encoding, encoding_record.offset)))
}

pub fn read_cmap_subtable<'a>(
    cmap: &Cmap<'a>,
) -> Result<Option<(Encoding, CmapSubtable<'a>)>, ParseError> {
    if let Some((encoding, encoding_record)) = find_good_cmap_subtable(&cmap) {
        let subtable = cmap
            .scope
            .offset(usize::try_from(encoding_record.offset)?)
            .read::<CmapSubtable<'_>>()?;
        Ok(Some((encoding, subtable)))
    } else {
        Ok(None)
    }
}

pub fn find_good_cmap_subtable(cmap: &Cmap<'_>) -> Option<(Encoding, EncodingRecord)> {
    // MS UNICODE, UCS-4 (32 bit)
    if let Some(encoding_record) =
        cmap.find_subtable(PlatformId::WINDOWS, EncodingId::WINDOWS_UNICODE_UCS4)
    {
        return Some((Encoding::Unicode, encoding_record));
    }

    // MS UNICODE, UCS-2 (16 bit)
    if let Some(encoding_record) =
        cmap.find_subtable(PlatformId::WINDOWS, EncodingId::WINDOWS_UNICODE_BMP_UCS2)
    {
        return Some((Encoding::Unicode, encoding_record));
    }

    // Apple UNICODE, UCS-4 (32 bit)
    if let Some(encoding_record) =
        cmap.find_subtable(PlatformId::UNICODE, EncodingId::MACINTOSH_UNICODE_UCS4)
    {
        return Some((Encoding::Unicode, encoding_record));
    }

    // Any UNICODE table
    if let Some(encoding_record) = cmap.find_subtable_for_platform(PlatformId::UNICODE) {
        return Some((Encoding::Unicode, encoding_record));
    }

    // MS Symbol
    if let Some(encoding_record) =
        cmap.find_subtable(PlatformId::WINDOWS, EncodingId::WINDOWS_SYMBOL)
    {
        return Some((Encoding::Symbol, encoding_record));
    }

    // Apple Roman
    if let Some(encoding_record) =
        cmap.find_subtable(PlatformId::MACINTOSH, EncodingId::MACINTOSH_APPLE_ROMAN)
    {
        return Some((Encoding::AppleRoman, encoding_record));
    }

    // Big5
    if let Some(encoding_record) = cmap.find_subtable(PlatformId::WINDOWS, EncodingId::WINDOWS_BIG5)
    {
        return Some((Encoding::Big5, encoding_record));
    }

    None
}

fn unique_glyph_names<'a>(
    names: impl Iterator<Item = Cow<'a, str>>,
    capacity: usize,
) -> Vec<Cow<'a, str>> {
    let mut seen = FxHashMap::with_capacity_and_hasher(capacity, Default::default());
    let mut unique_names = Vec::with_capacity(capacity);

    for name in names.map(Rc::new) {
        let alt = *seen
            .entry(Rc::clone(&name))
            .and_modify(|alt| *alt += 1)
            .or_insert(0);
        let unique_name = if alt == 0 {
            name
        } else {
            // name is not unique, generate a new name for it
            Rc::new(Cow::from(format!("{}.alt{:02}", name, alt)))
        };

        unique_names.push(unique_name)
    }
    drop(seen);

    // NOTE(unwrap): Safe as `seen` is the only other thing that holds a reference
    // to name and it's been dropped.
    unique_names
        .into_iter()
        .map(|name| Rc::try_unwrap(name).unwrap())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitmap::{Bitmap, EncapsulatedBitmap};
    use crate::tables::OpenTypeFile;
    use crate::tests::read_fixture;

    #[test]
    fn test_glyph_names() {
        let font_buffer = read_fixture("tests/fonts/opentype/TwitterColorEmoji-SVGinOT.ttf");
        let opentype_file = ReadScope::new(&font_buffer)
            .read::<OpenTypeFile<'_>>()
            .unwrap();
        let font_table_provider = opentype_file
            .font_provider(0)
            .expect("error reading font file");
        let font = Font::new(Box::new(font_table_provider))
            .expect("error reading font data")
            .expect("missing required font tables");

        let names = font.glyph_names(&[0, 5, 45, 71, 1311, 3086]);
        assert_eq!(
            names,
            &[
                Cow::from(".notdef"),
                Cow::from("copyright"),
                Cow::from("uni25B6"),
                Cow::from("smileface"),
                Cow::from("u1FA95"),
                Cow::from("1f468-200d-1f33e")
            ]
        );
    }

    #[test]
    fn test_glyph_names_post_v3() {
        // This font is a CFF font with a version 3 post table (no names in table).
        let font_buffer = read_fixture("tests/fonts/opentype/Klei.otf");
        let opentype_file = ReadScope::new(&font_buffer)
            .read::<OpenTypeFile<'_>>()
            .unwrap();
        let font_table_provider = opentype_file
            .font_provider(0)
            .expect("error reading font file");
        let font = Font::new(Box::new(font_table_provider))
            .expect("error reading font data")
            .expect("missing required font tables");

        let names = font.glyph_names(&[0, 5, 45, 100, 763, 1000 /* out of range */]);
        assert_eq!(
            names,
            &[
                Cow::from(".notdef"),
                Cow::from("dollar"),
                Cow::from("L"),
                Cow::from("yen"),
                Cow::from("uniFB00"),
                Cow::from("g1000") // out of range gid is assigned fallback name
            ]
        );
    }

    #[test]
    fn test_unique_glyph_names() {
        let names = vec!["A"; 3].into_iter().map(Cow::from);
        let unique_names = unique_glyph_names(names, 3);
        assert_eq!(
            unique_names,
            &[Cow::from("A"), Cow::from("A.alt01"), Cow::from("A.alt02")]
        );
    }

    #[test]
    fn test_lookup_sbix() {
        let font_buffer = read_fixture("tests/fonts/sbix/sbix-dupe.ttf");
        let opentype_file = ReadScope::new(&font_buffer)
            .read::<OpenTypeFile<'_>>()
            .unwrap();
        let font_table_provider = opentype_file
            .font_provider(0)
            .expect("error reading font file");
        let mut font = Font::new(Box::new(font_table_provider))
            .expect("error reading font data")
            .expect("missing required font tables");

        // Successfully read bitmap
        match font.lookup_glyph_image(1, 100, BitDepth::ThirtyTwo) {
            Ok(Some(BitmapGlyph {
                bitmap: Bitmap::Encapsulated(EncapsulatedBitmap { data, .. }),
                ..
            })) => {
                assert_eq!(data.len(), 224);
            }
            _ => panic!("Expected encapsulated bitmap, got something else."),
        }

        // Successfully read bitmap pointed at by `dupe` record. Should end up returning data for
        // glyph 1.
        match font.lookup_glyph_image(2, 100, BitDepth::ThirtyTwo) {
            Ok(Some(BitmapGlyph {
                bitmap: Bitmap::Encapsulated(EncapsulatedBitmap { data, .. }),
                ..
            })) => {
                assert_eq!(data.len(), 224);
            }
            _ => panic!("Expected encapsulated bitmap, got something else."),
        }

        // Handle recursive `dupe` record. Should return Ok(None) as recursion is stopped at one
        // level.
        match font.lookup_glyph_image(3, 100, BitDepth::ThirtyTwo) {
            Ok(None) => {}
            _ => panic!("Expected Ok(None) got something else"),
        }
    }
}
