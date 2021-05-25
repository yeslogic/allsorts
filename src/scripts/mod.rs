pub mod arabic;
pub mod indic;
mod syllable;
pub mod syriac;
pub mod thai_lao;

use crate::tag;
use crate::unicode::mcc::sort_by_modified_combining_class;

#[derive(std::cmp::PartialEq)]
pub enum ScriptType {
    Arabic,
    Default,
    Indic,
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
            tag::SYRC => ScriptType::Syriac,
            tag::THAI => ScriptType::ThaiLao,
            tag::LAO => ScriptType::ThaiLao,
            _ => ScriptType::Default,
        }
    }
}

pub fn preprocess_text(cs: &mut Vec<char>, script_tag: u32) {
    match ScriptType::from(script_tag) {
        ScriptType::Arabic => arabic::reorder_marks(cs),
        ScriptType::Default => sort_by_modified_combining_class(cs),
        ScriptType::Indic => indic::preprocess_indic(cs, script_tag),
        ScriptType::Syriac => sort_by_modified_combining_class(cs),
        ScriptType::ThaiLao => thai_lao::reorder_marks(cs),
    }
}
