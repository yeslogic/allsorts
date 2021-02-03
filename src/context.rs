//! Utilities for performing contextual lookup in gpos and gsub.

use core::marker::PhantomData;
use alloc::rc::Rc;

use crate::gdef;
use crate::layout::{ClassDef, Coverage, GDEFTable};

#[derive(Copy, Clone)]
pub struct LookupFlag(pub u16);

#[derive(Copy, Clone, PartialEq)]
pub enum IgnoreMarks {
    NoIgnoreMarks,
    IgnoreAllMarks,
    IgnoreMarksExcept(u8),
}

#[derive(Copy, Clone)]
pub struct MatchType {
    ignore_bases: bool,
    ignore_ligatures: bool,
    ignore_marks: IgnoreMarks,
}

pub enum GlyphTable<'a> {
    Empty,
    ById(&'a [u16]),
    ByClassDef(Rc<ClassDef>, &'a [u16]),
    ByCoverage(&'a [Rc<Coverage>]),
}

impl<'a> GlyphTable<'a> {
    pub fn len(&self) -> usize {
        match self {
            GlyphTable::Empty => 0,
            GlyphTable::ById(ref arr) => arr.len(),
            GlyphTable::ByClassDef(_, ref arr) => arr.len(),
            GlyphTable::ByCoverage(ref vec) => vec.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub struct MatchContext<'a> {
    pub backtrack_table: GlyphTable<'a>,
    pub input_table: GlyphTable<'a>,
    pub lookahead_table: GlyphTable<'a>,
}

pub struct ContextLookupHelper<'a, T> {
    pub match_context: MatchContext<'a>,
    pub lookup_array: &'a [(u16, u16)],
    phantom: PhantomData<T>,
}

impl<'a, T> ContextLookupHelper<'a, T> {
    pub fn new(
        match_context: MatchContext<'a>,
        lookup_array: &'a [(u16, u16)],
    ) -> ContextLookupHelper<'a, T> {
        ContextLookupHelper {
            match_context,
            lookup_array,
            phantom: PhantomData,
        }
    }
}

pub trait Glyph {
    fn get_glyph_index(&self) -> u16;
}

impl LookupFlag {
    pub fn get_rtl(self) -> bool {
        (self.0 & 0x0001) != 0
    }

    pub fn get_ignore_bases(self) -> bool {
        (self.0 & 0x0002) != 0
    }

    pub fn get_ignore_ligatures(self) -> bool {
        (self.0 & 0x0004) != 0
    }

    pub fn get_ignore_marks(self) -> IgnoreMarks {
        if (self.0 & 0x8) != 0 {
            IgnoreMarks::IgnoreAllMarks
        } else if self.0 & 0xFF00 != 0 {
            IgnoreMarks::IgnoreMarksExcept((self.0 >> 8) as u8)
        } else {
            IgnoreMarks::NoIgnoreMarks
        }
    }
}

impl MatchType {
    pub fn ignore_marks() -> MatchType {
        MatchType {
            ignore_bases: false,
            ignore_ligatures: false,
            ignore_marks: IgnoreMarks::IgnoreAllMarks,
        }
    }

    pub fn marks_only() -> MatchType {
        MatchType {
            ignore_bases: true,
            ignore_ligatures: true,
            ignore_marks: IgnoreMarks::NoIgnoreMarks,
        }
    }

    pub fn from_lookup_flag(lookup_flag: LookupFlag) -> MatchType {
        MatchType {
            ignore_bases: lookup_flag.get_ignore_bases(),
            ignore_ligatures: lookup_flag.get_ignore_ligatures(),
            ignore_marks: lookup_flag.get_ignore_marks(),
        }
    }

    pub fn match_glyph<G: Glyph>(self, opt_gdef_table: Option<&GDEFTable>, glyph: &G) -> bool {
        if !self.ignore_bases
            && !self.ignore_ligatures
            && self.ignore_marks == IgnoreMarks::NoIgnoreMarks
        {
            // fast path that doesn't require checking glyph_class
            return true;
        }
        let glyph_class = gdef::glyph_class(opt_gdef_table, glyph.get_glyph_index());
        if self.ignore_bases && glyph_class == 1 {
            return false;
        }
        if self.ignore_ligatures && glyph_class == 2 {
            return false;
        }
        match self.ignore_marks {
            IgnoreMarks::NoIgnoreMarks => true,
            IgnoreMarks::IgnoreAllMarks => glyph_class != 3,
            IgnoreMarks::IgnoreMarksExcept(keep_class) => {
                let mark_attach_class =
                    gdef::mark_attach_class(opt_gdef_table, glyph.get_glyph_index());
                (glyph_class != 3) || (mark_attach_class == u16::from(keep_class))
            }
        }
    }

    // searches backwards from glyphs[index-1]
    pub fn find_prev<G: Glyph>(
        self,
        opt_gdef_table: Option<&GDEFTable>,
        glyphs: &[G],
        mut index: usize,
    ) -> Option<usize> {
        while index > 0 {
            index -= 1;
            if self.match_glyph(opt_gdef_table, &glyphs[index]) {
                return Some(index);
            }
        }
        None
    }

    // searches forwards from glyphs[index+1]
    pub fn find_next<G: Glyph>(
        self,
        opt_gdef_table: Option<&GDEFTable>,
        glyphs: &[G],
        mut index: usize,
    ) -> Option<usize> {
        while index + 1 < glyphs.len() {
            index += 1;
            if self.match_glyph(opt_gdef_table, &glyphs[index]) {
                return Some(index);
            }
        }
        None
    }

    // count == 0 will return current index
    pub fn find_nth<G: Glyph>(
        self,
        opt_gdef_table: Option<&GDEFTable>,
        glyphs: &[G],
        mut index: usize,
        count: usize,
    ) -> Option<usize> {
        for _ in 0..count {
            match self.find_next(opt_gdef_table, glyphs, index) {
                Some(next_index) => index = next_index,
                None => return None,
            }
        }
        Some(index)
    }

    pub fn find_first<G: Glyph>(
        self,
        opt_gdef_table: Option<&GDEFTable>,
        glyphs: &[G],
    ) -> Option<usize> {
        for (index, glyph) in glyphs.iter().enumerate() {
            if self.match_glyph(opt_gdef_table, glyph) {
                return Some(index);
            }
        }
        None
    }

    // searches backwards from glyphs[index-1]
    pub fn match_back<G: Glyph>(
        self,
        opt_gdef_table: Option<&GDEFTable>,
        glyph_table: &GlyphTable<'_>,
        glyphs: &[G],
        mut index: usize,
    ) -> bool {
        for i in 0..glyph_table.len() {
            match self.find_prev(opt_gdef_table, glyphs, index) {
                Some(prev_index) => {
                    index = prev_index;
                    let glyph_index = glyphs[index].get_glyph_index();
                    if !check_glyph_table(glyph_table, i, glyph_index) {
                        return false;
                    }
                }
                None => return false,
            }
        }
        true
    }

    // searches forwards from glyphs[index+1]
    pub fn match_front<G: Glyph>(
        self,
        opt_gdef_table: Option<&GDEFTable>,
        glyph_table: &GlyphTable<'_>,
        glyphs: &[G],
        mut index: usize,
        last_index: &mut usize,
    ) -> bool {
        for i in 0..glyph_table.len() {
            match self.find_next(opt_gdef_table, glyphs, index) {
                Some(next_index) => {
                    index = next_index;
                    let glyph_index = glyphs[index].get_glyph_index();
                    if !check_glyph_table(glyph_table, i, glyph_index) {
                        return false;
                    }
                }
                None => return false,
            }
        }
        *last_index = index;
        true
    }
}

impl<'a> MatchContext<'a> {
    pub fn matches<G: Glyph>(
        &self,
        opt_gdef_table: Option<&GDEFTable>,
        match_type: MatchType,
        glyphs: &[G],
        index: usize,
    ) -> bool {
        let mut front_index = index;
        match_type.match_back(opt_gdef_table, &self.backtrack_table, glyphs, index)
            && match_type.match_front(
                opt_gdef_table,
                &self.input_table,
                glyphs,
                index,
                &mut front_index,
            )
            && match_type.match_front(
                opt_gdef_table,
                &self.lookahead_table,
                glyphs,
                front_index,
                &mut front_index,
            )
    }
}

fn check_glyph_table(glyph_table: &GlyphTable<'_>, i: usize, glyph_index: u16) -> bool {
    match *glyph_table {
        GlyphTable::Empty => false,
        GlyphTable::ById(ref table) => table[i] == glyph_index,
        GlyphTable::ByClassDef(ref classdef, ref table) => {
            classdef.glyph_class_value(glyph_index) == table[i]
        }
        GlyphTable::ByCoverage(ref vec) => vec[i].glyph_coverage_value(glyph_index).is_some(),
    }
}
