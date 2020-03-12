use std::borrow::Cow;
use std::convert::{self, TryFrom};
use std::rc::Rc;

use crate::binary::read::ReadScope;
use crate::error::ParseError;
use crate::glyph_info::GlyphNames;
use crate::layout::{new_layout_cache, GDEFTable, LayoutCache, LayoutTable, GPOS, GSUB};
use crate::tables::cmap::{Cmap, CmapSubtable, EncodingId, EncodingRecord, PlatformId};
use crate::tables::os2::Os2;
use crate::tables::{FontTableProvider, HeadTable, HheaTable, MaxpTable};
use crate::{glyph_info, tag};

#[derive(Copy, Clone)]
pub enum Encoding {
    Unicode = 1,
    Symbol = 2,
    AppleRoman = 3,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum OutlineFormat {
    Glyf,
    Cff,
    None,
}

enum LazyLoad<T> {
    NotLoaded,
    Loaded(Option<T>),
}

pub struct FontDataImpl<T: FontTableProvider> {
    pub font_table_provider: Box<T>,
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
    pub outline_format: OutlineFormat,
}

impl<T: FontTableProvider> FontDataImpl<T> {
    pub fn new(provider: Box<T>) -> Result<Option<FontDataImpl<T>>, ParseError> {
        let cmap_table = read_and_box_table(provider.as_ref(), tag::CMAP)?;

        match charmap_info(&cmap_table)? {
            Some((cmap_subtable_encoding, cmap_subtable_offset)) => {
                let maxp_table =
                    ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
                let hmtx_table = read_and_box_table(provider.as_ref(), tag::HMTX)?;
                let hhea_table =
                    ReadScope::new(&provider.read_table_data(tag::HHEA)?).read::<HheaTable>()?;

                let outline_format = if provider.has_table(tag::GLYF) {
                    OutlineFormat::Glyf
                } else if provider.has_table(tag::CFF) {
                    OutlineFormat::Cff
                } else {
                    OutlineFormat::None
                };

                Ok(Some(FontDataImpl {
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
                    outline_format,
                }))
            }
            None => Ok(None),
        }
    }

    pub fn num_glyphs(&self) -> u16 {
        self.maxp_table.num_glyphs
    }

    pub fn lookup_glyph_index(&self, char_code: u32) -> u32 {
        match ReadScope::new(self.cmap_subtable_data()).read::<CmapSubtable<'_>>() {
            Ok(cmap_subtable) => match cmap_subtable.map_glyph(char_code) {
                Ok(Some(glyph_index)) => u32::from(glyph_index),
                _ => 0,
            },
            Err(_err) => 0,
        }
    }

    pub fn glyph_names<'a>(&self, ids: impl Iterator<Item = u16>) -> Vec<Cow<'a, str>> {
        let post = read_and_box_optional_table(self.font_table_provider.as_ref(), tag::POST)
            .ok()
            .and_then(convert::identity);
        let cmap = ReadScope::new(self.cmap_subtable_data())
            .read::<CmapSubtable<'_>>()
            .ok()
            .map(|table| (self.cmap_subtable_encoding, table));
        let glyph_namer = GlyphNames::new(&cmap, post);
        ids.map(|gid| glyph_namer.glyph_name(gid)).collect()
    }

    pub fn horizontal_advance(&mut self, glyph: u16) -> u16 {
        glyph_info::advance(&self.maxp_table, &self.hhea_table, &self.hmtx_table, glyph).unwrap()
    }

    pub fn vertical_advance(&mut self, glyph: u16) -> Option<u16> {
        let provider = &self.font_table_provider;
        let vmtx = self
            .vmtx_table
            .get_or_load(|| read_and_box_optional_table(provider.as_ref(), tag::VMTX))
            .ok()?;
        let vhea = self
            .vhea_table
            .get_or_load(|| {
                if let Some(vhea_data) = provider.table_data(tag::VHEA)? {
                    let vhea = ReadScope::new(&vhea_data).read::<HheaTable>()?;
                    Ok(Some(Rc::new(vhea)))
                } else {
                    Ok(None)
                }
            })
            .ok()?;

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
        self.font_table_provider
            .table_data(tag::OS_2)?
            .map(|data| ReadScope::new(&data).read_dep::<Os2>(data.len()))
            .transpose()
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

    None
}

#[cfg(test)]
mod tests {
    use super::*;
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
        let font_data_impl = FontDataImpl::new(Box::new(font_table_provider))
            .expect("error reading font data")
            .expect("missing required font tables");

        let names = font_data_impl.glyph_names([0, 5, 45, 71, 1311, 3086].iter().copied());
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
        let font_data_impl = FontDataImpl::new(Box::new(font_table_provider))
            .expect("error reading font data")
            .expect("missing required font tables");

        let names = font_data_impl.glyph_names(
            [0, 5, 45, 100, 763, 1000 /* out of range */]
                .iter()
                .copied(),
        );
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
}
