use crate::layout::GDEFTable;

pub fn gdef_is_mark(opt_gdef_table: Option<&GDEFTable>, glyph_index: u16) -> bool {
    glyph_class(opt_gdef_table, glyph_index) == 3
}

pub fn glyph_class(opt_gdef_table: Option<&GDEFTable>, glyph: u16) -> u16 {
    match opt_gdef_table {
        Some(ref gdef_table) => match gdef_table.opt_glyph_classdef {
            Some(ref glyph_classdef) => glyph_classdef.glyph_class_value(glyph),
            None => 0,
        },
        None => 0,
    }
}

pub fn mark_attach_class(opt_gdef_table: Option<&GDEFTable>, glyph: u16) -> u16 {
    match opt_gdef_table {
        Some(ref gdef_table) => match gdef_table.opt_mark_attach_classdef {
            Some(ref mark_attach_classdef) => mark_attach_classdef.glyph_class_value(glyph),
            None => 0,
        },
        None => 0,
    }
}
