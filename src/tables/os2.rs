//! Parsing of the `OS/2` table.
//!
//! > The OS/2 table consists of a set of metrics and other data that are required in OpenType fonts.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/os2>

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
    // Note: Documentation for OS/2 version 0 in Apple’s TrueType Reference Manual stops at the
    // usLastCharIndex field and does not include the last five fields of the table as it was
    // defined by Microsoft. Some legacy TrueType fonts may have been built with a shortened
    // version 0 OS/2 table. Applications should check the table length for a version 0 OS/2 table
    // before reading these fields.
    pub version0: Option<Version0>,
    pub version1: Option<Version1>,
    pub version2to4: Option<Version2to4>,
    pub version5: Option<Version5>,
}

pub struct Version0 {
    pub s_typo_ascender: i16,
    pub s_typo_descender: i16,
    pub s_typo_line_gap: i16,
    pub us_win_ascent: u16,
    pub us_win_descent: u16,
}

pub struct Version1 {
    pub ul_code_page_range1: u32,
    pub ul_code_page_range2: u32,
}

pub struct Version2to4 {
    pub sx_height: i16,
    pub s_cap_height: i16,
    pub us_default_char: u16,
    pub us_break_char: u16,
    pub us_max_context: u16,
}

pub struct Version5 {
    pub us_lower_optical_point_size: u16,
    pub us_upper_optical_point_size: u16,
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
        let version0 = if table_size >= 78 {
            let s_typo_ascender = ctxt.read::<I16Be>()?;
            let s_typo_descender = ctxt.read::<I16Be>()?;
            let s_typo_line_gap = ctxt.read::<I16Be>()?;
            let us_win_ascent = ctxt.read::<U16Be>()?;
            let us_win_descent = ctxt.read::<U16Be>()?;
            Some(Version0 {
                s_typo_ascender,
                s_typo_descender,
                s_typo_line_gap,
                us_win_ascent,
                us_win_descent,
            })
        } else {
            None
        };

        let version1 = if version >= 1 {
            let ul_code_page_range1 = ctxt.read::<U32Be>()?;
            let ul_code_page_range2 = ctxt.read::<U32Be>()?;
            Some(Version1 {
                ul_code_page_range1,
                ul_code_page_range2,
            })
        } else {
            None
        };

        let version2to4 = if version >= 2 {
            let sx_height = ctxt.read::<I16Be>()?;
            let s_cap_height = ctxt.read::<I16Be>()?;
            let us_default_char = ctxt.read::<U16Be>()?;
            let us_break_char = ctxt.read::<U16Be>()?;
            let us_max_context = ctxt.read::<U16Be>()?;
            Some(Version2to4 {
                sx_height,
                s_cap_height,
                us_default_char,
                us_break_char,
                us_max_context,
            })
        } else {
            None
        };

        let version5 = if version >= 5 {
            let us_lower_optical_point_size = ctxt.read::<U16Be>()?;
            let us_upper_optical_point_size = ctxt.read::<U16Be>()?;
            Some(Version5 {
                us_lower_optical_point_size,
                us_upper_optical_point_size,
            })
        } else {
            None
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
            version0,
            version1,
            version2to4,
            version5,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;
    use crate::tables::{FontTableProvider, OpenTypeFile};
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    #[cfg(feature = "prince")]
    fn test_read() {
        let buffer = read_fixture("../../../tests/data/fonts/HardGothicNormal.ttf");
        let opentype_file = ReadScope::new(&buffer).read::<OpenTypeFile<'_>>().unwrap();
        let provider = opentype_file.font_provider(0).unwrap();
        let os_2_data = provider.read_table_data(tag::OS_2).unwrap();

        let os_2 = ReadScope::new(&os_2_data)
            .read_dep::<Os2>(os_2_data.len())
            .expect("unable to parse OS/2 table");
        assert_eq!(os_2.version, 1);
        assert!(os_2.version0.is_some());
        assert!(os_2.version1.is_some());
        assert!(os_2.version2to4.is_none());
        assert!(os_2.version5.is_none());
    }
}
