use crate::binary::write::{WriteBinaryDep, WriteBuffer};
use crate::error::{ParseError, ReadWriteError, WriteError};
use crate::fontfile::FontFile;
use crate::read::ReadScope;
use crate::tables::glyf::GlyfTable;
use crate::tables::loca::owned::LocaTable;
use crate::tables::{
    FontTableProvider, HeadTable, HheaTable, HmtxTable, IndexToLocFormat, MaxpTable, OffsetTable,
    OpenTypeFont, TTCHeader,
};
use crate::woff::WoffFile;
use crate::woff2::{
    TableDirectoryEntry, Woff2File, Woff2GlyfTable, Woff2HmtxTable, Woff2LocaTable,
};
use crate::{subset, tag};
use itertools::Either;
use libc::c_int;
use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::ffi::c_void;
use std::ptr;

pub type LoadTableFunc = unsafe extern "C" fn(*mut c_void, u32, *mut u8, *mut usize) -> c_int;
pub type LoadFontFunc = unsafe extern "C" fn(*mut c_void, *mut *mut u8, *mut u32) -> c_int;
pub type FreeFontFunc = unsafe extern "C" fn(*mut c_void);

extern "C" {
    fn prince_custom_malloc(size: usize) -> *mut c_void;
}

pub enum FontTablesImpl {
    FontImpl(FontImpl),
    FuncImpl(FuncImpl),
}

pub struct FontImpl {
    entire_font: Option<Box<[u8]>>,
    // TODO don't duplicate data from entire_font
    tables: HashMap<u32, FontTable>,
}

type FontTable = Box<[u8]>;

pub struct FuncImpl {
    data: *mut c_void,
    load_table_func: LoadTableFunc,
    load_font_func: LoadFontFunc,
    free_font_func: FreeFontFunc,
}

impl FontTablesImpl {
    pub fn new_font_impl(bytes: &[u8], index: usize) -> Result<Self, ReadWriteError> {
        FontImpl::new(bytes, index).map(|font_impl| FontTablesImpl::FontImpl(font_impl))
    }

    pub fn new_func_impl(
        data: *mut c_void,
        load_table_func: LoadTableFunc,
        load_font_func: LoadFontFunc,
        free_font_func: FreeFontFunc,
    ) -> Self {
        FontTablesImpl::FuncImpl(FuncImpl {
            data,
            load_table_func,
            load_font_func,
            free_font_func,
        })
    }

    pub fn get_table_length(&self, tag: u32) -> Option<usize> {
        match self {
            FontTablesImpl::FontImpl(font_impl) => font_impl.get_table_length(tag),
            FontTablesImpl::FuncImpl(func_impl) => func_impl.get_table_length(tag),
        }
    }

    pub fn get_table<'a>(&'a self, tag: u32) -> Option<Cow<'a, [u8]>> {
        match self {
            FontTablesImpl::FontImpl(font_impl) => font_impl
                .get_table_data(tag)
                .map(|data| Cow::Borrowed(data)),
            FontTablesImpl::FuncImpl(func_impl) => {
                func_impl.get_table_data(tag).map(|data| Cow::Owned(data))
            }
        }
    }

    pub unsafe fn load_table(&self, tag: u32, buffer: *mut u8, length: *mut usize) -> c_int {
        match self {
            FontTablesImpl::FontImpl(font_impl) => {
                if buffer.is_null() {
                    match font_impl.get_table_length(tag) {
                        Some(table_length) => {
                            *length = table_length;
                            0
                        }
                        None => 1,
                    }
                } else {
                    match font_impl.get_table_data(tag) {
                        Some(table_data) => {
                            if *length >= table_data.len() {
                                *length = table_data.len();
                                ptr::copy(table_data.as_ptr(), buffer, *length);
                                0
                            } else {
                                1
                            }
                        }
                        None => 1,
                    }
                }
            }
            FontTablesImpl::FuncImpl(func_impl) => {
                (func_impl.load_table_func)(func_impl.data, tag, buffer, length)
            }
        }
    }

    pub fn load_font(&self, buffer: *mut *mut u8, length: *mut u32) -> c_int {
        match self {
            FontTablesImpl::FontImpl(font_impl) => match font_impl.entire_font {
                Some(ref font_bytes) => unsafe {
                    let ptr = prince_custom_malloc(font_bytes.len()) as *mut u8;
                    ptr::copy_nonoverlapping(font_bytes.as_ptr(), ptr, font_bytes.len());
                    *buffer = ptr;
                    *length = font_bytes.len() as u32;
                    0
                },
                None => {
                    let tags: Vec<_> = font_impl.tables.keys().cloned().collect();
                    match subset::whole_font(self, &tags) {
                        Ok(font_bytes) => unsafe {
                            let ptr = prince_custom_malloc(font_bytes.len()) as *mut u8;
                            ptr::copy_nonoverlapping(font_bytes.as_ptr(), ptr, font_bytes.len());
                            *buffer = ptr;
                            *length = font_bytes.len() as u32;

                            0
                        },
                        Err(_err) => 1,
                    }
                }
            },
            FontTablesImpl::FuncImpl(func_impl) => unsafe {
                (func_impl.load_font_func)(func_impl.data, buffer, length)
            },
        }
    }

    pub fn has_table(&self, tag: u32) -> bool {
        self.get_table_length(tag).is_some()
    }
}

impl Drop for FontTablesImpl {
    fn drop(&mut self) {
        match self {
            FontTablesImpl::FontImpl { .. } => {}
            FontTablesImpl::FuncImpl(func_impl) => unsafe {
                (func_impl.free_font_func)(func_impl.data)
            },
        }
    }
}

impl FontTableProvider for FontTablesImpl {
    fn table_data<'a>(&'a self, tag: u32) -> Result<Option<Cow<'a, [u8]>>, ParseError> {
        Ok(self.get_table(tag))
    }

    fn has_table<'a>(&'a self, tag: u32) -> bool {
        self.has_table(tag)
    }
}

impl FontImpl {
    pub fn new(bytes: &[u8], index: usize) -> Result<Self, ReadWriteError> {
        let mut entire_font = None;
        let fontfile = ReadScope::new(bytes).read::<FontFile<'_>>()?;
        let tables = match fontfile {
            FontFile::OpenType(opentype) => match opentype.font {
                OpenTypeFont::Single(ttf) => {
                    entire_font = Some(Box::from(bytes));
                    ttf_read_tables(opentype.scope, ttf)?
                }
                OpenTypeFont::Collection(ttc) => ttc_read_tables(opentype.scope, ttc, index)?,
            },
            FontFile::Woff(woff) => woff_read_tables(woff)?,
            FontFile::Woff2(woff2) => woff2_read_tables(woff2, index)?,
        };
        Ok(FontImpl {
            entire_font,
            tables,
        })
    }

    pub fn get_table_length(&self, tag: u32) -> Option<usize> {
        self.tables.get(&tag).map(|table| table.len())
    }

    pub fn get_table_data(&self, tag: u32) -> Option<&[u8]> {
        self.tables.get(&tag).map(|table| &**table)
    }
}

fn ttf_read_tables<'a>(
    scope: ReadScope<'a>,
    ttf: OffsetTable<'a>,
) -> Result<HashMap<u32, FontTable>, ParseError> {
    let mut tables = HashMap::new();
    for table_record in &ttf.table_records {
        let tag = table_record.table_tag;
        let data: Box<[u8]> = Box::from(table_record.read_table(&scope)?.data());
        tables.insert(tag, data);
    }
    Ok(tables)
}

fn ttc_read_tables<'a>(
    scope: ReadScope<'a>,
    ttc: TTCHeader<'a>,
    index: usize,
) -> Result<HashMap<u32, FontTable>, ParseError> {
    if index < ttc.offset_tables.len() {
        let ttf_offset = usize::try_from(ttc.offset_tables.get_item(index))?;
        let ttf = scope.offset(ttf_offset).read::<OffsetTable<'_>>()?;
        ttf_read_tables(scope, ttf)
    } else {
        Err(ParseError::BadIndex)
    }
}

fn woff_read_tables<'a>(woff: WoffFile<'a>) -> Result<HashMap<u32, FontTable>, ParseError> {
    let mut tables = HashMap::with_capacity(woff.table_directory.len());
    for table_entry in &woff.table_directory {
        let tag = table_entry.tag;
        let data: Box<[u8]> = Box::from(table_entry.read_table(&woff.scope)?.scope().data());
        tables.insert(tag, data);
    }
    Ok(tables)
}

macro_rules! read_table {
    ($file:ident, $tag:path, $t:ty, $index:expr) => {
        $file
            .read_table($tag, $index)?
            .ok_or(ParseError::MissingValue)?
            .scope()
            .read::<$t>()
    };
}

fn woff2_read_tables<'a>(
    woff: Woff2File<'a>,
    index: usize,
) -> Result<HashMap<u32, FontTable>, ReadWriteError> {
    let mut tables = HashMap::with_capacity(woff.table_directory.len());

    // if hmtx is transformed then that means we have to parse glyf
    // otherwise we only have to parse glyf if it's transformed
    let hmtx_entry = woff
        .find_table_entry(tag::HMTX, index)
        .ok_or(ParseError::MissingValue)?;
    let hmtx_table = hmtx_entry.read_table(&woff.table_data_block_scope())?;
    let glyf_entry = woff
        .find_table_entry(tag::GLYF, index)
        .ok_or(ParseError::MissingValue)?;
    let glyf_table = glyf_entry.read_table(&woff.table_data_block_scope())?;

    if hmtx_entry.transform_length.is_some() || glyf_entry.transform_length.is_some() {
        let mut head = read_table!(woff, tag::HEAD, HeadTable, index)?;
        let maxp = read_table!(woff, tag::MAXP, MaxpTable, index)?;
        let hhea = read_table!(woff, tag::HHEA, HheaTable, index)?;
        let loca_entry = woff
            .find_table_entry(tag::LOCA, index)
            .ok_or(ParseError::MissingValue)?;
        let loca = loca_entry.read_table(&woff.table_data_block_scope())?;
        let loca = loca.scope().read_dep::<Woff2LocaTable>((
            &loca_entry,
            usize::from(maxp.num_glyphs),
            head.index_to_loc_format,
        ))?;
        let glyf = glyf_table
            .scope()
            .read_dep::<Woff2GlyfTable>((&glyf_entry, &loca))?;

        if hmtx_entry.transform_length.is_some() {
            let hmtx = hmtx_table.scope().read_dep::<Woff2HmtxTable>((
                &hmtx_entry,
                &glyf,
                usize::from(maxp.num_glyphs),
                usize::from(hhea.num_h_metrics),
            ))?;
            let ((), data) = table_buffer::<_, HmtxTable<'_>>(&hmtx, ())?;
            tables.insert(tag::HMTX, Box::from(data));
        }

        // Add head, glyf and loca
        let (loca, data) = table_buffer::<_, GlyfTable<'_>>(glyf, head.index_to_loc_format)?;
        tables.insert(tag::GLYF, Box::from(data));
        match loca.offsets.last() {
            Some(&last) if (last / 2) > u32::from(std::u16::MAX) => {
                head.index_to_loc_format = IndexToLocFormat::Long
            }
            _ => {}
        }
        let (_placeholder, data) = table_buffer::<_, HeadTable>(&head, ())?;
        tables.insert(tag::HEAD, Box::from(data));
        let ((), data) = table_buffer::<_, LocaTable>(loca, head.index_to_loc_format)?;
        tables.insert(tag::LOCA, Box::from(data));
    }

    // Add remaining tables
    for table_entry in table_directory(&woff, index) {
        let tag = table_entry.tag;
        if tables.contains_key(&tag) {
            // Skip tables that were inserted above
            continue;
        }
        let data: Box<[u8]> = Box::from(
            table_entry
                .read_table(&woff.table_data_block_scope())?
                .scope()
                .data(),
        );
        tables.insert(tag, data);
    }
    Ok(tables)
}

fn table_directory<'a>(
    woff: &'a Woff2File<'a>,
    index: usize,
) -> impl Iterator<Item = &TableDirectoryEntry> {
    if let Some(collection_directory) = &woff.collection_directory {
        Either::Left(
            collection_directory
                .get(index)
                .map(|font| font.table_entries(&woff))
                .unwrap(), // NOTE(unwrap): It's assumed that index is determined valid in woff2_read_tables
        )
    } else {
        Either::Right(woff.table_directory.iter())
    }
}

fn table_buffer<HostType, T: WriteBinaryDep<HostType>>(
    table: HostType,
    args: T::Args,
) -> Result<(T::Output, Vec<u8>), WriteError> {
    let mut buffer = WriteBuffer::new();
    let output = T::write_dep(&mut buffer, table, args)?;
    Ok((output, buffer.into_inner()))
}

impl FuncImpl {
    pub fn get_table_length(&self, tag: u32) -> Option<usize> {
        unsafe {
            let mut length = 0;
            let res = (self.load_table_func)(self.data, tag, ptr::null_mut(), &mut length);
            if res == 0 && length != 0 {
                Some(length)
            } else {
                None
            }
        }
    }

    pub fn get_table_data(&self, tag: u32) -> Option<Vec<u8>> {
        unsafe {
            // Determine length of buffer required
            let mut length = 0;
            if (self.load_table_func)(self.data, tag, ptr::null_mut(), &mut length) != 0 {
                return None;
            }

            // Allocate buffer and fill it
            let mut buf = Vec::with_capacity(length);
            if (self.load_table_func)(self.data, tag, buf.as_mut_ptr(), &mut length) != 0 {
                return None;
            }
            buf.set_len(length);
            Some(buf)
        }
    }
}

pub struct SystemFontLoader<'a> {
    handle: &'a *mut c_void,
    load_table_func: LoadTableFunc,
    tags: &'a [u32],
}

impl<'a> SystemFontLoader<'a> {
    pub fn new(handle: &'a *mut c_void, load_table_func: LoadTableFunc, tags: &'a [u32]) -> Self {
        SystemFontLoader {
            handle,
            load_table_func,
            tags,
        }
    }
}

impl<'a> FontTableProvider for SystemFontLoader<'a> {
    fn table_data<'b>(&'b self, tag: u32) -> Result<Option<Cow<'b, [u8]>>, ParseError> {
        if self.tags.contains(&tag) {
            let mut length = 0;
            if unsafe { (self.load_table_func)(*self.handle, tag, ptr::null_mut(), &mut length) }
                != 0
            {
                return Err(ParseError::MissingValue);
            }

            let mut buffer = Vec::with_capacity(length);
            if unsafe {
                (self.load_table_func)(*self.handle, tag, buffer.as_mut_ptr(), &mut length)
            } != 0
            {
                return Err(ParseError::MissingValue);
            }
            unsafe { buffer.set_len(length) };

            Ok(Some(Cow::Owned(buffer)))
        } else {
            Ok(None)
        }
    }

    fn has_table<'b>(&'b self, tag: u32) -> bool {
        self.tags.contains(&tag)
    }
}

#[cfg(test)]
mod tests {
    use super::{FontImpl, FontTablesImpl};
    use crate::error::ReadWriteError;
    use crate::read::ReadScope;
    use crate::tables::glyf::GlyfTable;
    use crate::tables::loca::LocaTable;
    use crate::tables::{FontTableProvider, HeadTable, MaxpTable};
    use crate::tag;
    use crate::tests::read_fixture;

    // This is a test that ensure this font can be parsed.
    // Prior to a fix it caused out of bounds array access and failed to parse.
    #[test]
    fn test_woff2_cabin() -> Result<(), ReadWriteError> {
        let buffer = read_fixture("../../../data/fonts/woff2/Cabin.woff2");
        let woff2 = FontImpl::new(&buffer, 0).unwrap();
        let provider = FontTablesImpl::FontImpl(woff2);
        let head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
        let maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
        let loca_data = provider.read_table_data(tag::LOCA)?;
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((usize::from(maxp.num_glyphs), head.index_to_loc_format))?;
        let glyf_data = provider.read_table_data(tag::GLYF)?;
        assert!(ReadScope::new(&glyf_data)
            .read_dep::<GlyfTable<'_>>(&loca)
            .is_ok());

        Ok(())
    }
}
