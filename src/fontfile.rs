use crate::error::ParseError;
use crate::read::{ReadBinary, ReadCtxt};
use crate::tables::{OpenTypeFile, CFF_MAGIC, TTCF_MAGIC, TTF_MAGIC};
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
