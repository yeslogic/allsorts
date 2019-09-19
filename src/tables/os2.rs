use std::convert::TryInto;

use crate::binary::read::{ReadBinaryDep, ReadCtxt};
use crate::binary::{I16Be, U16Be, U32Be};
use crate::error::ParseError;

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
    pub extra: Version,
}

/// Version specific OS/2 table data
pub enum Version {
    // Note: Documentation for OS/2 version 0 in Apple’s TrueType Reference Manual stops at the
    // usLastCharIndex field and does not include the last five fields of the table as it was
    // defined by Microsoft. Some legacy TrueType fonts may have been built with a shortened
    // version 0 OS/2 table. Applications should check the table length for a version 0 OS/2 table
    // before reading these fields.
    Version0 {
        s_typo_ascender: Option<i16>,
        s_typo_descender: Option<i16>,
        s_typo_line_gap: Option<i16>,
        us_win_ascent: Option<u16>,
        us_win_descent: Option<u16>,
    },
    Version1 {
        s_typo_ascender: i16,
        s_typo_descender: i16,
        s_typo_line_gap: i16,
        us_win_ascent: u16,
        us_win_descent: u16,
        ul_code_page_range1: u32,
        ul_code_page_range2: u32,
    },
    Version2to4 {
        s_typo_ascender: i16,
        s_typo_descender: i16,
        s_typo_line_gap: i16,
        us_win_ascent: u16,
        us_win_descent: u16,
        ul_code_page_range1: u32,
        ul_code_page_range2: u32,
        sx_height: i16,
        s_cap_height: i16,
        us_default_char: u16,
        us_break_char: u16,
        us_max_context: u16,
    },
    Version5 {
        s_typo_ascender: i16,
        s_typo_descender: i16,
        s_typo_line_gap: i16,
        us_win_ascent: u16,
        us_win_descent: u16,
        ul_code_page_range1: u32,
        ul_code_page_range2: u32,
        sx_height: i16,
        s_cap_height: i16,
        us_default_char: u16,
        us_break_char: u16,
        us_max_context: u16,
        us_lower_optical_point_size: u16,
        us_upper_optical_point_size: u16,
    },
}

impl<'a> ReadBinaryDep<'a> for Os2 {
    type HostType = Self;
    type Args = usize;

    // The format of this table has changed over time. The original TrueType specification had this
    // table at 68 bytes long. The first OpenType version had it at 78 bytes long, and the current
    // OpenType version is even larger. To determine which kind of table your software is dealing
    // with, it's best both to consider the table's version and its size.
    fn read_dep(ctxt: &mut ReadCtxt<'a>, table_size: usize) -> Result<Self, ParseError> {
        let version = ctxt.read::<U16Be>()?;
        let x_avg_char_width = ctxt.read::<I16Be>()?;
        let us_weight_class = ctxt.read::<U16Be>()?;
        let us_width_class = ctxt.read::<U16Be>()?;
        let fs_type = ctxt.read::<U16Be>()?;
        let y_subscript_x_size = ctxt.read::<I16Be>()?;
        let y_subscript_y_size = ctxt.read::<I16Be>()?;
        let y_subscript_x_offset = ctxt.read::<I16Be>()?;
        let y_subscript_y_offset = ctxt.read::<I16Be>()?;
        let y_superscript_x_size = ctxt.read::<I16Be>()?;
        let y_superscript_y_size = ctxt.read::<I16Be>()?;
        let y_superscript_x_offset = ctxt.read::<I16Be>()?;
        let y_superscript_y_offset = ctxt.read::<I16Be>()?;
        let y_strikeout_size = ctxt.read::<I16Be>()?;
        let y_strikeout_position = ctxt.read::<I16Be>()?;
        let s_family_class = ctxt.read::<I16Be>()?;
        // NOTE(unwrap): Safe as slice is guaranteed to have 10 elements
        let panose: [u8; 10] = ctxt.read_slice(10)?.try_into().unwrap();
        let ul_unicode_range1 = ctxt.read::<U32Be>()?;
        let ul_unicode_range2 = ctxt.read::<U32Be>()?;
        let ul_unicode_range3 = ctxt.read::<U32Be>()?;
        let ul_unicode_range4 = ctxt.read::<U32Be>()?;
        let ach_vend_id = ctxt.read::<U32Be>()?;
        let fs_selection = ctxt.read::<U16Be>()?;
        let us_first_char_index = ctxt.read::<U16Be>()?;
        let us_last_char_index = ctxt.read::<U16Be>()?;

        // Read version specific fields
        let mut s_typo_ascender = None;
        let mut s_typo_descender = None;
        let mut s_typo_line_gap = None;
        let mut us_win_ascent = None;
        let mut us_win_descent = None;
        let mut ul_code_page_range1 = None;
        let mut ul_code_page_range2 = None;
        let mut sx_height = None;
        let mut s_cap_height = None;
        let mut us_default_char = None;
        let mut us_break_char = None;
        let mut us_max_context = None;
        let mut us_lower_optical_point_size = None;
        let mut us_upper_optical_point_size = None;

        if table_size >= 78 {
            s_typo_ascender = Some(ctxt.read::<I16Be>()?);
            s_typo_descender = Some(ctxt.read::<I16Be>()?);
            s_typo_line_gap = Some(ctxt.read::<I16Be>()?);
            us_win_ascent = Some(ctxt.read::<U16Be>()?);
            us_win_descent = Some(ctxt.read::<U16Be>()?);
        }

        if version >= 1 {
            ul_code_page_range1 = Some(ctxt.read::<U32Be>()?);
            ul_code_page_range2 = Some(ctxt.read::<U32Be>()?);
        }
        if version >= 2 {
            sx_height = Some(ctxt.read::<I16Be>()?);
            s_cap_height = Some(ctxt.read::<I16Be>()?);
            us_default_char = Some(ctxt.read::<U16Be>()?);
            us_break_char = Some(ctxt.read::<U16Be>()?);
            us_max_context = Some(ctxt.read::<U16Be>()?);
        }
        if version >= 5 {
            us_lower_optical_point_size = Some(ctxt.read::<U16Be>()?);
            us_upper_optical_point_size = Some(ctxt.read::<U16Be>()?);
        }

        let extra = match version {
            0 => Version::Version0 {
                s_typo_ascender,
                s_typo_descender,
                s_typo_line_gap,
                us_win_ascent,
                us_win_descent,
            },
            1 => Version::Version1 {
                s_typo_ascender: s_typo_ascender.unwrap(),
                s_typo_descender: s_typo_descender.unwrap(),
                s_typo_line_gap: s_typo_line_gap.unwrap(),
                us_win_ascent: us_win_ascent.unwrap(),
                us_win_descent: us_win_descent.unwrap(),
                ul_code_page_range1: ul_code_page_range1.unwrap(),
                ul_code_page_range2: ul_code_page_range2.unwrap(),
            },
            2..=4 => Version::Version2to4 {
                s_typo_ascender: s_typo_ascender.unwrap(),
                s_typo_descender: s_typo_descender.unwrap(),
                s_typo_line_gap: s_typo_line_gap.unwrap(),
                us_win_ascent: us_win_ascent.unwrap(),
                us_win_descent: us_win_descent.unwrap(),
                ul_code_page_range1: ul_code_page_range1.unwrap(),
                ul_code_page_range2: ul_code_page_range2.unwrap(),
                sx_height: sx_height.unwrap(),
                s_cap_height: s_cap_height.unwrap(),
                us_default_char: us_default_char.unwrap(),
                us_break_char: us_break_char.unwrap(),
                us_max_context: us_max_context.unwrap(),
            },
            _ => Version::Version5 {
                s_typo_ascender: s_typo_ascender.unwrap(),
                s_typo_descender: s_typo_descender.unwrap(),
                s_typo_line_gap: s_typo_line_gap.unwrap(),
                us_win_ascent: us_win_ascent.unwrap(),
                us_win_descent: us_win_descent.unwrap(),
                ul_code_page_range1: ul_code_page_range1.unwrap(),
                ul_code_page_range2: ul_code_page_range2.unwrap(),
                sx_height: sx_height.unwrap(),
                s_cap_height: s_cap_height.unwrap(),
                us_default_char: us_default_char.unwrap(),
                us_break_char: us_break_char.unwrap(),
                us_max_context: us_max_context.unwrap(),
                us_lower_optical_point_size: us_lower_optical_point_size.unwrap(),
                us_upper_optical_point_size: us_upper_optical_point_size.unwrap(),
            },
        };

        Ok(Os2 {
            version,
            x_avg_char_width,
            us_weight_class,
            us_width_class,
            fs_type,
            y_subscript_x_size,
            y_subscript_y_size,
            y_subscript_x_offset,
            y_subscript_y_offset,
            y_superscript_x_size,
            y_superscript_y_size,
            y_superscript_x_offset,
            y_superscript_y_offset,
            y_strikeout_size,
            y_strikeout_position,
            s_family_class,
            panose,
            ul_unicode_range1,
            ul_unicode_range2,
            ul_unicode_range3,
            ul_unicode_range4,
            ach_vend_id,
            fs_selection,
            us_first_char_index,
            us_last_char_index,
            extra,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::read::ReadScope;
    use crate::tables::{FontTableProvider, OpenTypeFile};
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    fn test_read() {
        let buffer = read_fixture("tests/opentype/HardGothicNormal.ttf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFile<'_>>().unwrap();
        let provider = opentype_file.font_provider(0).unwrap();
        let os_2_data = provider.read_table_data(tag::OS_2).unwrap();

        let os_2 = ReadScope::new(&os_2_data)
            .read_dep::<Os2>(os_2_data.len())
            .expect("unable to parse OS/2 table");
        assert_eq!(os_2.version, 1);
    }
}
