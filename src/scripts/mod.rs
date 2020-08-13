pub mod arabic;
pub mod indic;

use crate::tag;

pub enum Scripts {
    Arabic,
    CyrillicGreekLatin,
    Indic,
    Syriac,
    Unknown,
}

pub fn get_script_type(script_tag: u32) -> Scripts {
    match script_tag {
        tag::ARAB => Scripts::Arabic,
        tag::LATN => Scripts::CyrillicGreekLatin,
        tag::CYRL => Scripts::CyrillicGreekLatin,
        tag::GREK => Scripts::CyrillicGreekLatin,
        tag::DEVA => Scripts::Indic,
        tag::BENG => Scripts::Indic,
        tag::GURU => Scripts::Indic,
        tag::GUJR => Scripts::Indic,
        tag::ORYA => Scripts::Indic,
        tag::TAML => Scripts::Indic,
        tag::TELU => Scripts::Indic,
        tag::KNDA => Scripts::Indic,
        tag::MLYM => Scripts::Indic,
        tag::SYRC => Scripts::Syriac,
        _ => Scripts::Unknown,
    }
}
