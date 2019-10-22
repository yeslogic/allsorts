use std::borrow::Cow;

use crate::binary::read::{ReadBinary, ReadCtxt};
use crate::error::{ParseError, ReadWriteError};
use crate::tables::{FontTableProvider, OpenTypeFile, CFF_MAGIC, TTCF_MAGIC, TTF_MAGIC};
use crate::woff::{self, WoffFile};
use crate::woff2::{self, Woff2File};

pub enum FontFile<'a> {
    OpenType(OpenTypeFile<'a>),
    Woff(WoffFile<'a>),
    Woff2(Woff2File<'a>),
}

pub struct FileTableProvider<'a> {
    provider: Box<dyn FontTableProvider + 'a>,
}

impl<'a> ReadBinary<'a> for FontFile<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let mut peek = ctxt.clone();
        let magic = peek.read_u32be()?;
        match magic {
            TTF_MAGIC | CFF_MAGIC => Ok(FontFile::OpenType(OpenTypeFile::read(ctxt)?)),
            TTCF_MAGIC => Ok(FontFile::OpenType(OpenTypeFile::read(ctxt)?)),
            woff::MAGIC => Ok(FontFile::Woff(WoffFile::read(ctxt)?)),
            woff2::MAGIC => Ok(FontFile::Woff2(Woff2File::read(ctxt)?)),
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl<'a> FontTableProvider for FileTableProvider<'a> {
    fn table_data<'b>(&'b self, tag: u32) -> Result<Option<Cow<'b, [u8]>>, ParseError> {
        self.provider.table_data(tag)
    }

    fn has_table(&self, tag: u32) -> bool {
        self.provider.has_table(tag)
    }
}

impl<'a> FontFile<'a> {
    pub fn table_provider(&'a self, index: usize) -> Result<FileTableProvider<'a>, ReadWriteError> {
        match self {
            FontFile::OpenType(file) => {
                let provider = file.font_provider(index)?;
                Ok(FileTableProvider {
                    provider: Box::new(provider),
                })
            }
            FontFile::Woff(file) => {
                // This clone is relatively cheap as WoffFile is mostly holding borrowed data
                Ok(FileTableProvider {
                    provider: Box::new(file.clone()),
                })
            }
            FontFile::Woff2(file) => {
                let provider = file.table_provider(index)?;
                Ok(FileTableProvider {
                    provider: Box::new(provider),
                })
            }
        }
    }
}
