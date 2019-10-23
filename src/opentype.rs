//! Misc utilities.

use crate::tag;

pub fn is_indic_script_tag(t: u32) -> bool {
    t == tag::DEVA
        || t == tag::BENG
        || t == tag::GURU
        || t == tag::GUJR
        || t == tag::ORYA
        || t == tag::TAML
        || t == tag::TELU
        || t == tag::KNDA
        || t == tag::MLYM
}
