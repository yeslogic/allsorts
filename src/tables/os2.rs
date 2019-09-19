/// `OS/2` table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/os2>
pub struct Os2 {
    pub version: u16,
    pub x_avg_char_width: i16,
    pub us_weight_class: u16,
    pub us_width_class: u16,
    pub fs_type: u16,
    pub y_subscript_x_size: i16,
    pub y_subscript_y_size: i16,
    pub y_subscript_x_offset: i16,
    pub y_subscript_y_offset: i16,
    pub y_superscript_x_size: i16,
    pub y_superscript_y_size: i16,
    pub y_superscript_x_offset: i16,
    pub y_superscript_y_offset: i16,
    pub y_strikeout_size: i16,
    pub y_strikeout_position: i16,
    pub s_family_class: i16,
    pub panose: [u8; 10],
    pub ul_unicode_range1: u32,
    pub ul_unicode_range2: u32,
    pub ul_unicode_range3: u32,
    pub ul_unicode_range4: u32,
    pub ach_vend_id: u32, // tag
    pub fs_selection: u16,
    pub us_first_char_index: u16,
    pub us_last_char_index: u16,

    // Note: Documentation for OS/2 version 0 in Apple’s TrueType Reference Manual stops at the
    // usLastCharIndex field and does not include the last five fields of the table as it was
    // defined by Microsoft. Some legacy TrueType fonts may have been built with a shortened
    // version 0 OS/2 table. Applications should check the table length for a version 0 OS/2 table
    // before reading these fields.
    pub s_typo_ascender: Option<i16>,
    pub s_typo_descender: Option<i16>,
    pub s_typo_line_gap: Option<i16>,
    pub us_win_ascent: Option<u16>,
    pub us_win_descent: Option<u16>,
    pub extra: Version,
}

pub enum Version {
    Version0,
    Version1 {
        ul_code_page_range1: u32,
        ul_code_page_range2: u32,
    },
    Version2to4 {
        sx_height: i16,
        s_cap_height: i16,
        us_default_char: u16,
        us_break_char: u16,
        us_max_context: u16,
    },
    Version5 {
        us_lower_optical_point_size: u16,
        us_upper_optical_point_size: u16,
    },
}

// The format of this table has changed over time. The original TrueType specification had this
// table at 68 bytes long. The first OpenType version had it at 78 bytes long, and the current
// OpenType version is even larger. To determine which kind of table your software is dealing with,
// it's best both to consider the table's version and its size.
