use unicode_ccc::{get_canonical_combining_class, CanonicalCombiningClass};

/// An enumeration of the Unicode
/// [Canonical_Combining_Class values](http://www.unicode.org/reports/tr44/#Canonical_Combining_Class_Values),
/// with the following modifications:
///
/// * Replace CCC84 with CCC4.
/// * Replace CCC91 with CCC5.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ModifiedCombiningClass {
    NotReordered = 0,
    Overlay = 1,
    CCC4 = 4,
    CCC5 = 5,
    HanReading = 6,
    Nukta = 7,
    KanaVoicing = 8,
    Virama = 9,
    CCC10 = 10,
    CCC11 = 11,
    CCC12 = 12,
    CCC13 = 13,
    CCC14 = 14,
    CCC15 = 15,
    CCC16 = 16,
    CCC17 = 17,
    CCC18 = 18,
    CCC19 = 19,
    CCC20 = 20,
    CCC21 = 21,
    CCC22 = 22,
    CCC23 = 23,
    CCC24 = 24,
    CCC25 = 25,
    CCC26 = 26,
    CCC27 = 27,
    CCC28 = 28,
    CCC29 = 29,
    CCC30 = 30,
    CCC31 = 31,
    CCC32 = 32,
    CCC33 = 33,
    CCC34 = 34,
    CCC35 = 35,
    CCC36 = 36,
    CCC103 = 103,
    CCC107 = 107,
    CCC118 = 118,
    CCC122 = 122,
    CCC129 = 129,
    CCC130 = 130,
    CCC132 = 132,
    AttachedBelowLeft = 200,
    AttachedBelow = 202,
    AttachedAbove = 214,
    AttachedAboveRight = 216,
    BelowLeft = 218,
    Below = 220,
    BelowRight = 222,
    Left = 224,
    Right = 226,
    AboveLeft = 228,
    Above = 230,
    AboveRight = 232,
    DoubleBelow = 233,
    DoubleAbove = 234,
    IotaSubscript = 240,
}

impl From<CanonicalCombiningClass> for ModifiedCombiningClass {
    fn from(ccc: CanonicalCombiningClass) -> Self {
        use CanonicalCombiningClass as C;
        use ModifiedCombiningClass as M;

        match ccc {
            C::NotReordered => M::NotReordered,
            C::Overlay => M::Overlay,
            C::HanReading => M::HanReading,
            C::Nukta => M::Nukta,
            C::KanaVoicing => M::KanaVoicing,
            C::Virama => M::Virama,
            // Hebrew
            // Reordered in accordance with the SBL Hebrew Font User Manual:
            // https://www.sbl-site.org/Fonts/SBLHebrewUserManual1.5x.pdf.
            C::CCC10 => M::CCC22,
            C::CCC11 => M::CCC15,
            C::CCC12 => M::CCC16,
            C::CCC13 => M::CCC17,
            C::CCC14 => M::CCC23,
            C::CCC15 => M::CCC18,
            C::CCC16 => M::CCC19,
            C::CCC17 => M::CCC20,
            C::CCC18 => M::CCC21,
            C::CCC19 => M::CCC14,
            C::CCC20 => M::CCC24,
            C::CCC21 => M::CCC12,
            C::CCC22 => M::CCC25,
            C::CCC23 => M::CCC13,
            C::CCC24 => M::CCC10,
            C::CCC25 => M::CCC11,
            C::CCC26 => M::CCC26,
            // Arabic
            C::CCC27 => M::CCC27,
            C::CCC28 => M::CCC28,
            C::CCC29 => M::CCC29,
            C::CCC30 => M::CCC30,
            C::CCC31 => M::CCC31,
            C::CCC32 => M::CCC32,
            C::CCC33 => M::CCC33,
            C::CCC34 => M::CCC34,
            C::CCC35 => M::CCC35,
            // Syriac
            C::CCC36 => M::CCC36,
            // Telugu
            // Map `CCC84` and `CCC91` to the otherwise unassigned `CCC4` and `CCC5` values. If
            // left as-is, the Telugu length marks U+0C55 and U+0C56 have the undesirable effect
            // of being reordered after a Halant.
            //
            // Test case: `"\u{0C15}\u{0C4D}\u{0C56}"` should not produce a dotted circle.
            C::CCC84 => M::CCC4,
            C::CCC91 => M::CCC5,
            // Thai
            C::CCC103 => M::CCC103,
            C::CCC107 => M::CCC107,
            // Lao
            C::CCC118 => M::CCC118,
            C::CCC122 => M::CCC122,
            // Tibetan
            C::CCC129 => M::CCC129,
            C::CCC130 => M::CCC130,
            C::CCC132 => M::CCC132,
            C::AttachedBelowLeft => M::AttachedBelowLeft,
            C::AttachedBelow => M::AttachedBelow,
            C::AttachedAbove => M::AttachedAbove,
            C::AttachedAboveRight => M::AttachedAboveRight,
            C::BelowLeft => M::BelowLeft,
            C::Below => M::Below,
            C::BelowRight => M::BelowRight,
            C::Left => M::Left,
            C::Right => M::Right,
            C::AboveLeft => M::AboveLeft,
            C::Above => M::Above,
            C::AboveRight => M::AboveRight,
            C::DoubleBelow => M::DoubleBelow,
            C::DoubleAbove => M::DoubleAbove,
            C::IotaSubscript => M::IotaSubscript,
        }
    }
}

/// Returns the modified combining class value of a `char`. Relies on the `unicode-ccc` crate to
/// retrieve the _canonical_ combining class value, then maps it to its corresponding _modified_
/// value.
pub fn modified_combining_class(c: char) -> ModifiedCombiningClass {
    get_canonical_combining_class(c).into()
}

/// Sorts sub-slices of non-starter `char`s (i.e. `char`s with non-zero combining class values) by
/// their modified combining class values. This sort is stable.
pub fn sort_by_modified_combining_class(cs: &mut [char]) {
    fn comparator(c1: &char, c2: &char) -> std::cmp::Ordering {
        modified_combining_class(*c1).cmp(&modified_combining_class(*c2))
    }

    for css in
        cs.split_mut(|&c| modified_combining_class(c) == ModifiedCombiningClass::NotReordered)
    {
        css.sort_by(comparator)
    }
}
