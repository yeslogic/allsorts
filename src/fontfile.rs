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

impl<'a> FontFile<'a> {
    pub fn table_provider(
        &'a self,
        index: usize,
    ) -> Result<Box<dyn FontTableProvider + 'a>, ReadWriteError> {
        match self {
            FontFile::OpenType(file) => {
                let provider = file.font_provider(index)?;
                Ok(Box::new(provider))
            }
            FontFile::Woff(file) => {
                // This clone is relatively cheap as WoffFile is mostly holding borrowed data
                Ok(Box::new(file.clone()))
            }
            FontFile::Woff2(file) => {
                let provider = file.table_provider(index)?;
                Ok(Box::new(provider))
            }
        }
    }
}
