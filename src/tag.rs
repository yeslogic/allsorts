use crate::error::ParseError;
use std::fmt;

/// Generate a 4-byte font table tag from byte string
///
/// Example:
///
/// ```
/// assert_eq!(tag!(b"glyf"), 0x676C7966);
/// ```
macro_rules! tag {
    ($w:expr) => {
        tag(*$w)
    };
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct DisplayTag(pub u32);

const fn tag(chars: [u8; 4]) -> u32 {
    ((chars[3] as u32) << 0)
        | ((chars[2] as u32) << 8)
        | ((chars[1] as u32) << 16)
        | ((chars[0] as u32) << 24)
}

pub fn from_string(s: &str) -> Result<u32, ParseError> {
    if s.len() > 4 {
        return Err(ParseError::BadValue);
    }

    let mut tag: u32 = 0;
    let mut count = 0;

    for c in s.chars() {
        if !c.is_ascii() || c.is_ascii_control() {
            return Err(ParseError::BadValue);
        }

        tag = (tag << 8) | (c as u32);
        count += 1;
    }

    while count < 4 {
        tag = (tag << 8) | (' ' as u32);
        count += 1;
    }

    Ok(tag)
}

impl fmt::Display for DisplayTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tag = self.0;
        let mut s = String::with_capacity(4);
        s.push(char::from((tag >> 24) as u8));
        s.push(char::from(((tag >> 16) & 255) as u8));
        s.push(char::from(((tag >> 8) & 255) as u8));
        s.push(char::from((tag & 255) as u8));
        if s.chars().any(|c| !c.is_ascii() || c.is_ascii_control()) {
            write!(f, "0x{:08x}", tag)
        } else {
            s.fmt(f)
        }
    }
}

impl fmt::Debug for DisplayTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_string().fmt(f)
    }
}

pub const ABVF: u32 = tag!(b"abvf");
pub const ABVM: u32 = tag!(b"abvm");
pub const ABVS: u32 = tag!(b"abvs");
pub const ACNT: u32 = tag!(b"acnt");
pub const AKHN: u32 = tag!(b"akhn");
pub const ARAB: u32 = tag!(b"arab");
pub const AVAR: u32 = tag!(b"avar");
pub const BASE: u32 = tag!(b"BASE");
pub const BDAT: u32 = tag!(b"bdat");
pub const BENG: u32 = tag!(b"beng");
pub const BLOC: u32 = tag!(b"bloc");
pub const BLWF: u32 = tag!(b"blwf");
pub const BLWM: u32 = tag!(b"blwm");
pub const BLWS: u32 = tag!(b"blws");
pub const BNG2: u32 = tag!(b"bng2");
pub const BSLN: u32 = tag!(b"bsln");
pub const CALT: u32 = tag!(b"calt");
pub const CBDT: u32 = tag!(b"CBDT");
pub const CBLC: u32 = tag!(b"CBLC");
pub const CCMP: u32 = tag!(b"ccmp");
pub const CFAR: u32 = tag!(b"cfar");
pub const CFF: u32 = tag!(b"CFF ");
pub const CJCT: u32 = tag!(b"cjct");
pub const CLIG: u32 = tag!(b"clig");
pub const CMAP: u32 = tag!(b"cmap");
pub const COLR: u32 = tag!(b"COLR");
pub const CPAL: u32 = tag!(b"CPAL");
pub const CURS: u32 = tag!(b"curs");
pub const CVAR: u32 = tag!(b"cvar");
pub const CVT: u32 = tag!(b"cvt ");
pub const CYRL: u32 = tag!(b"cyrl");
pub const DEV2: u32 = tag!(b"dev2");
pub const DEVA: u32 = tag!(b"deva");
pub const DFLT: u32 = tag!(b"DFLT");
pub const DIST: u32 = tag!(b"dist");
pub const EBDT: u32 = tag!(b"EBDT");
pub const EBLC: u32 = tag!(b"EBLC");
pub const EBSC: u32 = tag!(b"EBSC");
pub const FDSC: u32 = tag!(b"fdsc");
pub const FEAT2: u32 = tag!(b"Feat");
pub const FEAT: u32 = tag!(b"feat");
pub const FINA: u32 = tag!(b"fina");
pub const FMTX: u32 = tag!(b"fmtx");
pub const FPGM: u32 = tag!(b"fpgm");
pub const FVAR: u32 = tag!(b"fvar");
pub const GASP: u32 = tag!(b"gasp");
pub const GDEF: u32 = tag!(b"GDEF");
pub const GJR2: u32 = tag!(b"gjr2");
pub const GLAT: u32 = tag!(b"Glat");
pub const GLOC: u32 = tag!(b"Gloc");
pub const GLYF: u32 = tag!(b"glyf");
pub const GPOS: u32 = tag!(b"GPOS");
pub const GREK: u32 = tag!(b"grek");
pub const GSUB: u32 = tag!(b"GSUB");
pub const GUJR: u32 = tag!(b"gujr");
pub const GUR2: u32 = tag!(b"gur2");
pub const GURU: u32 = tag!(b"guru");
pub const GVAR: u32 = tag!(b"gvar");
pub const HALF: u32 = tag!(b"half");
pub const HALN: u32 = tag!(b"haln");
pub const HDMX: u32 = tag!(b"hdmx");
pub const HEAD: u32 = tag!(b"head");
pub const HHEA: u32 = tag!(b"hhea");
pub const HMTX: u32 = tag!(b"hmtx");
pub const HSTY: u32 = tag!(b"hsty");
pub const INIT: u32 = tag!(b"init");
pub const JSTF: u32 = tag!(b"JSTF");
pub const JUST: u32 = tag!(b"just");
pub const KERN: u32 = tag!(b"kern");
pub const KND2: u32 = tag!(b"knd2");
pub const KNDA: u32 = tag!(b"knda");
pub const LATN: u32 = tag!(b"latn");
pub const LCAR: u32 = tag!(b"lcar");
pub const LIGA: u32 = tag!(b"liga");
pub const LOCA: u32 = tag!(b"loca");
pub const LOCL: u32 = tag!(b"locl");
pub const LTSH: u32 = tag!(b"LTSH");
pub const MARK: u32 = tag!(b"mark");
pub const MATH: u32 = tag!(b"MATH");
pub const MAXP: u32 = tag!(b"maxp");
pub const MKMK: u32 = tag!(b"mkmk");
pub const MLM2: u32 = tag!(b"mlm2");
pub const MLYM: u32 = tag!(b"mlym");
pub const MORT: u32 = tag!(b"mort");
pub const MORX: u32 = tag!(b"morx");
pub const NAME: u32 = tag!(b"name");
pub const NUKT: u32 = tag!(b"nukt");
pub const OPBD: u32 = tag!(b"opbd");
pub const ORY2: u32 = tag!(b"ory2");
pub const ORYA: u32 = tag!(b"orya");
pub const OS_2: u32 = tag!(b"OS/2");
pub const OTTO: u32 = tag!(b"OTTO");
pub const PCLT: u32 = tag!(b"PCLT");
pub const POST: u32 = tag!(b"post");
pub const PREF: u32 = tag!(b"pref");
pub const PREP: u32 = tag!(b"prep");
pub const PRES: u32 = tag!(b"pres");
pub const PROP: u32 = tag!(b"prop");
pub const PSTF: u32 = tag!(b"pstf");
pub const PSTS: u32 = tag!(b"psts");
pub const RKRF: u32 = tag!(b"rkrf");
pub const RLIG: u32 = tag!(b"rlig");
pub const RPHF: u32 = tag!(b"rphf");
pub const SBIX: u32 = tag!(b"sbix");
pub const SILF: u32 = tag!(b"Silf");
pub const SILL: u32 = tag!(b"Sill");
pub const SINH: u32 = tag!(b"sinh");
pub const SVG: u32 = tag!(b"SVG ");
pub const SYRC: u32 = tag!(b"syrc");
pub const TAML: u32 = tag!(b"taml");
pub const TEL2: u32 = tag!(b"tel2");
pub const TELU: u32 = tag!(b"telu");
pub const TML2: u32 = tag!(b"tml2");
pub const TRAK: u32 = tag!(b"trak");
pub const TTCF: u32 = tag!(b"ttcf");
pub const VATU: u32 = tag!(b"vatu");
pub const VDMX: u32 = tag!(b"VDMX");
pub const VERT: u32 = tag!(b"vert");
pub const VHEA: u32 = tag!(b"vhea");
pub const VMTX: u32 = tag!(b"vmtx");
pub const VORG: u32 = tag!(b"VORG");
pub const VRT2: u32 = tag!(b"vrt2");
pub const ZAPF: u32 = tag!(b"Zapf");

#[cfg(test)]
mod tests {
    use super::*;

    mod from_string {
        use super::*;

        #[test]
        fn test_four_chars() {
            let tag = from_string("beng").expect("invalid tag");

            assert_eq!(tag, 1650814567);
        }

        #[test]
        fn test_three_chars() {
            let tag = from_string("BEN").expect("invalid tag");

            assert_eq!(tag, 1111838240);
        }
    }

    mod display_tag {
        use crate::tag::{DisplayTag, NAME};

        #[test]
        fn test_ascii() {
            assert_eq!(DisplayTag(NAME).to_string(), "name".to_string());
        }

        #[test]
        fn test_non_ascii() {
            assert_eq!(DisplayTag(0x12345678).to_string(), "0x12345678".to_string());
        }
    }
}
