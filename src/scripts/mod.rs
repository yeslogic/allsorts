pub mod arabic;
pub mod indic;
pub mod khmer;
pub mod myanmar;
mod syllable;
pub mod syriac;
pub mod thai_lao;

use crate::gsub::{GlyphOrigin, RawGlyph};
use crate::scripts::syllable::SyllableChar;
use crate::tag;
use crate::unicode::mcc::sort_by_modified_combining_class;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScriptType {
    Arabic,
    Default,
    Indic,
    Khmer,
    Myanmar,
    Syriac,
    ThaiLao,
}

impl From<u32> for ScriptType {
    fn from(script_tag: u32) -> Self {
        match script_tag {
            tag::ARAB => ScriptType::Arabic,
            tag::LATN => ScriptType::Default,
            tag::CYRL => ScriptType::Default,
            tag::GREK => ScriptType::Default,
            tag::DEVA => ScriptType::Indic,
            tag::BENG => ScriptType::Indic,
            tag::GURU => ScriptType::Indic,
            tag::GUJR => ScriptType::Indic,
            tag::ORYA => ScriptType::Indic,
            tag::TAML => ScriptType::Indic,
            tag::TELU => ScriptType::Indic,
            tag::KNDA => ScriptType::Indic,
            tag::MLYM => ScriptType::Indic,
            tag::SINH => ScriptType::Indic,
            tag::KHMR => ScriptType::Khmer,
            tag::MYMR => ScriptType::Myanmar,
            tag::MYM2 => ScriptType::Myanmar,
            tag::SYRC => ScriptType::Syriac,
            tag::THAI => ScriptType::ThaiLao,
            tag::LAO => ScriptType::ThaiLao,
            _ => ScriptType::Default,
        }
    }
}

impl<T> SyllableChar for RawGlyph<T> {
    fn char(&self) -> char {
        match self.glyph_origin {
            GlyphOrigin::Char(ch) => ch,
            GlyphOrigin::Direct => panic!("unexpected glyph origin"),
        }
    }
}

pub fn preprocess_text(cs: &mut Vec<char>, script_tag: u32) {
    match ScriptType::from(script_tag) {
        ScriptType::Arabic => arabic::reorder_marks(cs),
        ScriptType::Default => sort_by_modified_combining_class(cs),
        ScriptType::Indic => indic::preprocess_indic(cs, script_tag),
        ScriptType::Khmer => khmer::preprocess_khmer(cs),
        ScriptType::Myanmar => {}
        ScriptType::Syriac => sort_by_modified_combining_class(cs),
        ScriptType::ThaiLao => thai_lao::reorder_marks(cs),
    }
}
