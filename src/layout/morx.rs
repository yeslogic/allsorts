//! `morx` layout transformations.

use std::cmp;
use std::convert::TryFrom;
use tinyvec::{tiny_vec, TinyVec};

use crate::error::ParseError;
use crate::gsub::{FeatureMask, Features, GlyphOrigin, RawGlyph, RawGlyphFlags};
use crate::tables::morx::{
    self, Chain, ClassLookupTable, ContextualEntryFlags, ContextualSubtable, LigatureEntryFlags,
    LigatureSubtable, LookupTable, MorxTable, NonContextualSubtable, RearrangementSubtable,
    RearrangementVerb, Subtable, SubtableType,
};

/// End of text.
///
/// This class should not appear in the class array.
const CLASS_CODE_EOT: u16 = 0;

/// Out of bounds.
///
/// All glyph indexes that are less than firstGlyph, or greater than or equal to firstGlyph plus
/// nGlyphs will automatically be assigned class code 1. Class code 1 may also appear in the class
/// array.
const CLASS_CODE_OOB: u16 = 1;

/// Deleted glyph.
///
/// Sometimes contextual processing removes a glyph from the glyph array by changing its glyph
/// index to the deleted glyph index, 0xFFFF. This glyph code is automatically assigned class
/// "deleted," which should not appear in the class array.
const CLASS_CODE_DELETED: u16 = 2;
const DELETED_GLYPH: u16 = 0xFFFF;

/// Perform a lookup in a class lookup table.
fn lookup(glyph: u16, lookup_table: &ClassLookupTable<'_>) -> Option<u16> {
    match &lookup_table.lookup_table {
        LookupTable::Format0(lookup_values) => lookup_values.get_item(usize::from(glyph)),
        LookupTable::Format2(lookup_segments) => {
            lookup_segments.iter().find_map(|lookup_segment| {
                lookup_segment
                    .contains(glyph)
                    .then_some(lookup_segment.lookup_value)
            })
        }
        LookupTable::Format4(lookup_segments) => {
            for lookup_segment in lookup_segments {
                // The segments are meant to be non-overlapping so if a segment contains the glyph
                // then we always return a result.
                if lookup_segment.contains(glyph) {
                    let index = usize::from(glyph - lookup_segment.first_glyph);
                    return lookup_segment.lookup_values.get_item(index);
                }
            }
            None
        }
        LookupTable::Format6(lookup_entries) => lookup_entries.iter().find_map(|lookup_entry| {
            (lookup_entry.glyph == glyph).then_some(lookup_entry.lookup_value)
        }),
        LookupTable::Format8(lookup_table) => lookup_table.lookup(glyph),
        LookupTable::Format10(lookup_table) => lookup_table.lookup(glyph),
    }
}

fn glyph_class(glyph: u16, class_table: &ClassLookupTable<'_>) -> u16 {
    if glyph == DELETED_GLYPH {
        CLASS_CODE_DELETED
    } else {
        lookup(glyph, class_table).unwrap_or(CLASS_CODE_OOB)
    }
}

pub struct RearrangementTransformation<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    mark_first_index: usize,
    mark_last_index: usize,
}

impl<'a> RearrangementTransformation<'a> {
    fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> RearrangementTransformation<'a> {
        RearrangementTransformation {
            glyphs,
            next_state: 0,
            mark_first_index: 0,
            mark_last_index: 0,
        }
    }

    fn process_glyphs(
        &mut self,
        rearrangement_subtable: &RearrangementSubtable<'_>,
    ) -> Result<(), ParseError> {
        let len = self.glyphs.len();
        let mut i = 0;
        while i <= len {
            let class = if i < len {
                let glyph_index = self.glyphs[i].glyph_index;
                glyph_class(glyph_index, &rearrangement_subtable.class_table)
            } else {
                CLASS_CODE_EOT
            };

            let entry_table_index = rearrangement_subtable
                .state_array
                .get(self.next_state)
                .and_then(|s| s.get_item(class as usize))
                .ok_or(ParseError::BadIndex)?;

            let entry = rearrangement_subtable
                .entry_table
                .rearrangement_entries
                .get(entry_table_index as usize)
                .ok_or(ParseError::BadIndex)?;

            self.next_state = entry.next_state;

            if entry.mark_first() {
                self.mark_first_index = i;
            }

            if entry.mark_last() {
                self.mark_last_index = cmp::min(i + 1, len);
            }

            if self.mark_first_index < self.mark_last_index {
                let seq = &mut self.glyphs[self.mark_first_index..self.mark_last_index];
                rearrange_glyphs(entry.verb(), seq);
            }

            // Guard against infinite loop during end-of-text processing, which is caused by the
            // presence of the DONT_ADVANCE flag.
            if class == CLASS_CODE_EOT {
                break;
            }

            if !entry.dont_advance() {
                i += 1;
            }
        }

        Ok(())
    }
}

fn rearrange_glyphs<T>(verb: RearrangementVerb, seq: &mut [T]) {
    use RearrangementVerb::*;

    let len = seq.len();
    match verb {
        Verb1 if len > 1 => seq.rotate_left(1),
        Verb2 if len > 1 => seq.rotate_right(1),
        Verb3 if len > 1 => seq.swap(0, len - 1),
        Verb4 if len > 2 => seq.rotate_left(2),
        Verb5 if len > 1 => {
            seq.swap(0, 1);
            seq.rotate_left(2);
        }
        Verb6 if len > 2 => seq.rotate_right(2),
        Verb7 if len > 1 => {
            seq.rotate_right(2);
            seq.swap(0, 1);
        }
        Verb8 if len > 2 => {
            seq.rotate_right(2);
            seq[2..].rotate_left(1);
        }
        Verb9 if len > 2 => {
            seq.rotate_right(2);
            seq.swap(0, 1);
            seq[2..].rotate_left(1);
        }
        Verb10 if len > 2 => {
            seq.rotate_right(1);
            seq[1..].rotate_left(2);
        }
        Verb11 if len > 2 => {
            seq.swap(0, 1);
            seq.rotate_right(1);
            seq[1..].rotate_left(2);
        }
        Verb12 if len > 3 => {
            seq.rotate_right(2);
            seq[2..].rotate_left(2);
        }
        Verb13 if len > 3 => {
            seq.swap(0, 1);
            seq.rotate_right(2);
            seq[2..].rotate_left(2);
        }
        Verb14 if len > 3 => {
            seq.rotate_right(2);
            seq.swap(0, 1);
            seq[2..].rotate_left(2);
        }
        Verb15 if len > 3 => {
            seq.swap(0, 1);
            seq.rotate_right(2);
            seq.swap(0, 1);
            seq[2..].rotate_left(2);
        }
        _ => {}
    }
}

pub struct ContextualSubstitution<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    mark_index: Option<usize>,
}

impl<'a> ContextualSubstitution<'a> {
    fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> ContextualSubstitution<'a> {
        ContextualSubstitution {
            glyphs,
            next_state: 0,
            mark_index: None,
        }
    }

    fn process_glyphs(
        &mut self,
        contextual_subtable: &ContextualSubtable<'_>,
    ) -> Result<(), ParseError> {
        let mut i = 0;
        while i <= self.glyphs.len() {
            // It appears that no substitutions occur if mark isn't set prior to end-of-text.
            if i == self.glyphs.len() && self.mark_index.is_none() {
                return Ok(());
            }

            let class = if i < self.glyphs.len() {
                let current_glyph = self.glyphs[i].glyph_index;
                glyph_class(current_glyph, &contextual_subtable.class_table)
            } else {
                CLASS_CODE_EOT
            };

            let entry_table_index = contextual_subtable
                .state_array
                .get(self.next_state)
                .and_then(|s| s.get_item(class as usize))
                .ok_or(ParseError::BadIndex)?;

            let entry = contextual_subtable
                .get_entry(entry_table_index)
                .ok_or(ParseError::BadIndex)?;

            self.next_state = entry.next_state;

            if entry.mark_index != 0xFFFF {
                if let Some(mark_index) = self.mark_index {
                    let lookup_table = contextual_subtable
                        .substitution_subtables
                        .get(usize::from(entry.mark_index))
                        .ok_or(ParseError::BadIndex)?;

                    let mark_glyph = self.glyphs[mark_index].glyph_index;
                    if let Some(mark_glyph_subst) = lookup(mark_glyph, lookup_table) {
                        self.glyphs[mark_index].glyph_index = mark_glyph_subst;
                        self.glyphs[mark_index].glyph_origin = GlyphOrigin::Direct;
                    }
                }
            }

            if entry.current_index != 0xFFFF {
                // End-of-text substitutions appear to operate on the end glyph.
                let j = cmp::min(i, self.glyphs.len() - 1);

                let lookup_table = contextual_subtable
                    .substitution_subtables
                    .get(usize::from(entry.current_index))
                    .ok_or(ParseError::BadIndex)?;

                let current_glyph = self.glyphs[j].glyph_index;
                if let Some(current_glyph_subst) = lookup(current_glyph, lookup_table) {
                    self.glyphs[j].glyph_index = current_glyph_subst;
                    self.glyphs[j].glyph_origin = GlyphOrigin::Direct;
                }
            }

            if entry.flags.contains(ContextualEntryFlags::SET_MARK) {
                self.mark_index = Some(i);
            }

            if class == CLASS_CODE_EOT {
                break;
            }

            if !entry.flags.contains(ContextualEntryFlags::DONT_ADVANCE) {
                i += 1;
            }
        }

        Ok(())
    }
}

pub struct LigatureSubstitution<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    component_stack: TinyVec<[usize; 32]>,
}

impl<'a> LigatureSubstitution<'a> {
    fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> LigatureSubstitution<'a> {
        let len = glyphs.len();
        LigatureSubstitution {
            glyphs,
            next_state: 0,
            component_stack: TinyVec::with_capacity(len),
        }
    }

    fn process_glyphs(
        &mut self,
        ligature_subtable: &LigatureSubtable<'_>,
    ) -> Result<(), ParseError> {
        let mut i = 0;
        while i <= self.glyphs.len() {
            let class = if i < self.glyphs.len() {
                let glyph_index = self.glyphs[i].glyph_index;
                glyph_class(glyph_index, &ligature_subtable.class_table)
            } else {
                CLASS_CODE_EOT
            };

            let entry_table_index: u16 = ligature_subtable
                .state_array
                .get(self.next_state)
                .ok_or(ParseError::BadIndex)
                .and_then(|s| s.read_item(class as usize))?;

            let entry = ligature_subtable
                .entry_table
                .lig_entries
                .get(entry_table_index as usize)
                .ok_or(ParseError::BadIndex)?;

            self.next_state = entry.next_state_index;

            if entry.flags.contains(LigatureEntryFlags::SET_COMPONENT) {
                if class == CLASS_CODE_EOT {
                    // `i` points to one past the buffer, so don't push it.
                } else if self.component_stack.last() == Some(&i) {
                    // When DONT_ADVANCE == true, avoid pushing the same index twice.
                } else {
                    self.component_stack.push(i);
                }
            }

            if entry.flags.contains(LigatureEntryFlags::PERFORM_ACTION) {
                let mut action_index = entry.lig_action_index as usize;
                let mut ligature_list_index = 0;

                let mut unicodes = tiny_vec!([char; 32]);
                let mut end_i = None;
                'stack: loop {
                    let popped_i = match self.component_stack.pop() {
                        Some(popped_i) => popped_i,
                        None => break 'stack, // Stack underflow.
                    };
                    if end_i.is_none() {
                        end_i = Some(popped_i);
                    }

                    let glyph = &mut self.glyphs[popped_i];
                    let glyph_index = glyph.glyph_index;
                    let variation = glyph.variation;

                    // Mark glyph for deletion; copy its `.unicodes` content into a temp buffer.
                    glyph.glyph_index = DELETED_GLYPH;
                    for &u in glyph.unicodes.iter().rev() {
                        unicodes.push(u);
                    }

                    let action = &ligature_subtable.action_table.actions[action_index];
                    action_index += 1;

                    let component_index = (glyph_index as i32) + action.offset();
                    let component_index = usize::try_from(component_index)?;

                    ligature_list_index += &ligature_subtable
                        .component_table
                        .component_array
                        .read_item(component_index)?;

                    // `last` implies `store`.
                    if action.last() || action.store() {
                        let ligature_glyph_index = ligature_subtable
                            .ligature_list
                            .get(ligature_list_index)
                            .ok_or(ParseError::BadIndex)?;

                        *glyph = RawGlyph {
                            unicodes: unicodes.iter().rev().copied().collect(),
                            glyph_index: ligature_glyph_index,
                            liga_component_pos: 0,
                            glyph_origin: GlyphOrigin::Direct,
                            flags: RawGlyphFlags::empty(),
                            extra_data: (),
                            variation,
                        };

                        // Push ligature onto stack, only when the next state is non-zero.
                        if self.next_state != 0 {
                            self.component_stack.push(popped_i);
                        }

                        i = end_i.ok_or(ParseError::BadIndex)?;
                    }

                    if action.last() {
                        break 'stack;
                    }
                }
            }

            if class == CLASS_CODE_EOT {
                break;
            }

            if !entry.flags.contains(LigatureEntryFlags::DONT_ADVANCE) {
                i += 1;
            }
        }

        Ok(())
    }
}

fn noncontextual_substitution(
    glyphs: &mut Vec<RawGlyph<()>>,
    noncontextual_subtable: &NonContextualSubtable<'_>,
) -> Result<(), ParseError> {
    for glyph in glyphs.iter_mut() {
        match lookup(glyph.glyph_index, &noncontextual_subtable.lookup_table) {
            Some(subst) if subst != glyph.glyph_index => {
                glyph.glyph_index = subst;
                glyph.glyph_origin = GlyphOrigin::Direct;
            }
            Some(_) | None => (),
        }
    }
    Ok(())
}

pub fn apply(
    morx_table: &MorxTable<'_>,
    glyphs: &mut Vec<RawGlyph<()>>,
    features: &Features,
) -> Result<(), ParseError> {
    for chain in morx_table.chains.iter() {
        apply_chain(chain, features, glyphs)?;
    }
    remove_deleted_glyphs(glyphs);
    Ok(())
}

fn apply_chain(
    chain: &Chain<'_>,
    features: &Features,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ParseError> {
    let subfeatureflags: u32 = subfeatureflags(chain, features)?;

    for subtable in chain.subtables.iter() {
        if subfeatureflags & subtable.subtable_header.sub_feature_flags != 0 {
            apply_subtable(subtable, glyphs)?;
        }
    }

    Ok(())
}

fn apply_subtable(
    subtable: &Subtable<'_>,
    glyphs: &mut Vec<RawGlyph<()>>,
) -> Result<(), ParseError> {
    match (
        subtable.subtable_header.coverage & 0xFF,
        &subtable.subtable_body,
    ) {
        // Rearrangement subtable.
        (0, SubtableType::Rearrangement(rearrangement_subtable)) => {
            let mut rearrangement_trans = RearrangementTransformation::new(glyphs);
            rearrangement_trans.process_glyphs(rearrangement_subtable)?;
        }
        (0, _) => return Err(ParseError::BadValue),
        // Contextual subtable.
        (1, SubtableType::Contextual(contextual_subtable)) => {
            let mut contextual_subst = ContextualSubstitution::new(glyphs);
            contextual_subst.next_state = 0;
            contextual_subst.process_glyphs(contextual_subtable)?;
        }
        (1, _) => return Err(ParseError::BadValue),
        // Ligature subtable.
        (2, SubtableType::Ligature(ligature_subtable)) => {
            let mut liga_subst = LigatureSubstitution::new(glyphs);
            liga_subst.next_state = 0;
            liga_subst.component_stack.clear();
            liga_subst.process_glyphs(ligature_subtable)?;
        }
        (2, _) => return Err(ParseError::BadValue),
        // (Reserved)
        (3, _) => {}
        // Noncontextual (“swash”) subtable.
        (4, SubtableType::NonContextual(noncontextual_subtable)) => {
            noncontextual_substitution(glyphs, noncontextual_subtable)?;
        }
        (4, _) => return Err(ParseError::BadValue),
        // Insertion subtable (not implemented)
        (5, _) => {}
        _ => {}
    }

    Ok(())
}

fn remove_deleted_glyphs(glyphs: &mut Vec<RawGlyph<()>>) {
    glyphs.retain(|g| g.glyph_index != DELETED_GLYPH);
}

fn subfeatureflags(chain: &Chain<'_>, features: &Features) -> Result<u32, ParseError> {
    let mut subfeature_flags = chain.chain_header.default_flags;

    for entry in chain.feature_array.iter() {
        match features {
            Features::Custom(_features_list) => {
                return Ok(subfeature_flags);
            }
            Features::Mask(feature_mask) => {
                if should_apply_feature(entry, feature_mask) {
                    subfeature_flags =
                        (subfeature_flags & entry.disable_flags) | entry.enable_flags;
                }
            }
        }
    }
    Ok(subfeature_flags)
}

fn should_apply_feature(entry: morx::Feature, mask: &FeatureMask) -> bool {
    // Feature type:
    const LIGATURE_TYPE: u16 = 1;
    // Feature selectors:
    const COMMON_LIGATURES_ON: u16 = 2;
    const COMMON_LIGATURES_OFF: u16 = 3;
    const CONTEXTUAL_LIGATURES_ON: u16 = 18;
    const CONTEXTUAL_LIGATURES_OFF: u16 = 19;
    const HISTORICAL_LIGATURES_ON: u16 = 20;
    const HISTORICAL_LIGATURES_OFF: u16 = 21;

    // Feature type:
    const NUMBER_CASE_TYPE: u16 = 21;
    // Feature selectors:
    const OLD_STYLE_NUMBERS: u16 = 0;
    const LINING_NUMBERS: u16 = 1;

    // Feature type:
    const NUMBER_SPACING_TYPE: u16 = 6;
    // Feature selectors:
    const TABULAR_NUMBERS: u16 = 0;
    const PROPORTIONAL_NUMBERS: u16 = 1;

    // Feature type:
    const FRACTION_TYPE: u16 = 11;
    // Feature selectors:
    const NO_FRACTIONS: u16 = 0;
    const FRACTIONS_STACKED: u16 = 1;
    const FRACTIONS_DIAGONAL: u16 = 2;

    // Feature type:
    const VERTICAL_POSITION_TYPE: u16 = 10;
    // Feature selectors:
    const ORDINALS: u16 = 3;

    // Feature type:
    const TYPOGRAPHIC_EXTRAS_TYPE: u16 = 14;
    // Feature selectors:
    const SLASHED_ZERO_ON: u16 = 4;
    const SLASHED_ZERO_OFF: u16 = 5;

    // Feature type:
    const LOWERCASE_TYPE: u16 = 37;
    // Feature selectors:
    const LOWERCASE_SMALL_CAPS: u16 = 1;

    // Feature type:
    const UPPERCASE_TYPE: u16 = 38;
    // Feature selectors:
    const UPPERCASE_SMALL_CAPS: u16 = 1;

    match (entry.feature_type, entry.feature_setting) {
        (NUMBER_CASE_TYPE, LINING_NUMBERS) => mask.contains(FeatureMask::LNUM),
        (NUMBER_CASE_TYPE, OLD_STYLE_NUMBERS) => mask.contains(FeatureMask::ONUM),
        (NUMBER_SPACING_TYPE, PROPORTIONAL_NUMBERS) => mask.contains(FeatureMask::PNUM),
        (NUMBER_SPACING_TYPE, TABULAR_NUMBERS) => mask.contains(FeatureMask::TNUM),
        (FRACTION_TYPE, FRACTIONS_DIAGONAL) => mask.contains(FeatureMask::FRAC),
        (FRACTION_TYPE, FRACTIONS_STACKED) => mask.contains(FeatureMask::AFRC),
        (FRACTION_TYPE, NO_FRACTIONS) => {
            !mask.contains(FeatureMask::FRAC) && !mask.contains(FeatureMask::AFRC)
        }
        (VERTICAL_POSITION_TYPE, ORDINALS) => mask.contains(FeatureMask::ORDN),
        (TYPOGRAPHIC_EXTRAS_TYPE, SLASHED_ZERO_ON) => mask.contains(FeatureMask::ZERO),
        (TYPOGRAPHIC_EXTRAS_TYPE, SLASHED_ZERO_OFF) => !mask.contains(FeatureMask::ZERO),
        (LOWERCASE_TYPE, LOWERCASE_SMALL_CAPS) => {
            mask.contains(FeatureMask::SMCP) || mask.contains(FeatureMask::C2SC)
        }
        (UPPERCASE_TYPE, UPPERCASE_SMALL_CAPS) => mask.contains(FeatureMask::C2SC),
        (LIGATURE_TYPE, COMMON_LIGATURES_ON) => mask.contains(FeatureMask::LIGA),
        (LIGATURE_TYPE, COMMON_LIGATURES_OFF) => !mask.contains(FeatureMask::LIGA),
        (LIGATURE_TYPE, HISTORICAL_LIGATURES_ON) => mask.contains(FeatureMask::HLIG),
        (LIGATURE_TYPE, HISTORICAL_LIGATURES_OFF) => !mask.contains(FeatureMask::HLIG),
        (LIGATURE_TYPE, CONTEXTUAL_LIGATURES_ON) => mask.contains(FeatureMask::CLIG),
        (LIGATURE_TYPE, CONTEXTUAL_LIGATURES_OFF) => !mask.contains(FeatureMask::CLIG),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::MatchingPresentation;
    use crate::tables::{FontTableProvider, MaxpTable, OpenTypeFont};
    use crate::tests::read_fixture;
    use crate::{binary::read::ReadScope, tag, Font};

    mod rearrangement {
        use super::*;
        use RearrangementVerb::*;

        #[test]
        fn test_verb1() {
            let mut seq = ['A', 'x'];
            rearrange_glyphs(Verb1, &mut seq);
            assert_eq!(['x', 'A'], seq);

            let mut seq = ['A'];
            rearrange_glyphs(Verb1, &mut seq);
            assert_eq!(['A'], seq);
        }

        #[test]
        fn test_verb2() {
            let mut seq = ['x', 'D'];
            rearrange_glyphs(Verb2, &mut seq);
            assert_eq!(['D', 'x'], seq);

            let mut seq = ['D'];
            rearrange_glyphs(Verb2, &mut seq);
            assert_eq!(['D'], seq);
        }

        #[test]
        fn test_verb3() {
            let mut seq = ['A', 'x', 'D'];
            rearrange_glyphs(Verb3, &mut seq);
            assert_eq!(['D', 'x', 'A'], seq);

            let mut seq = ['A', 'D'];
            rearrange_glyphs(Verb3, &mut seq);
            assert_eq!(['D', 'A'], seq);
        }

        #[test]
        fn test_verb4() {
            let mut seq = ['A', 'B', 'x'];
            rearrange_glyphs(Verb4, &mut seq);
            assert_eq!(['x', 'A', 'B'], seq);

            let mut seq = ['A', 'B'];
            rearrange_glyphs(Verb4, &mut seq);
            assert_eq!(['A', 'B'], seq);
        }

        #[test]
        fn test_verb5() {
            let mut seq = ['A', 'B', 'x'];
            rearrange_glyphs(Verb5, &mut seq);
            assert_eq!(['x', 'B', 'A'], seq);

            let mut seq = ['A', 'B'];
            rearrange_glyphs(Verb5, &mut seq);
            assert_eq!(['B', 'A'], seq);
        }

        #[test]
        fn test_verb6() {
            let mut seq = ['x', 'C', 'D'];
            rearrange_glyphs(Verb6, &mut seq);
            assert_eq!(['C', 'D', 'x'], seq);

            let mut seq = ['C', 'D'];
            rearrange_glyphs(Verb6, &mut seq);
            assert_eq!(['C', 'D'], seq);
        }

        #[test]
        fn test_verb7() {
            let mut seq = ['x', 'C', 'D'];
            rearrange_glyphs(Verb7, &mut seq);
            assert_eq!(['D', 'C', 'x'], seq);

            let mut seq = ['C', 'D'];
            rearrange_glyphs(Verb7, &mut seq);
            assert_eq!(['D', 'C'], seq);
        }

        #[test]
        fn test_verb8() {
            let mut seq = ['A', 'x', 'C', 'D'];
            rearrange_glyphs(Verb8, &mut seq);
            assert_eq!(['C', 'D', 'x', 'A'], seq);

            let mut seq = ['A', 'C', 'D'];
            rearrange_glyphs(Verb8, &mut seq);
            assert_eq!(['C', 'D', 'A'], seq);
        }

        #[test]
        fn test_verb9() {
            let mut seq = ['A', 'x', 'C', 'D'];
            rearrange_glyphs(Verb9, &mut seq);
            assert_eq!(['D', 'C', 'x', 'A'], seq);

            let mut seq = ['A', 'C', 'D'];
            rearrange_glyphs(Verb9, &mut seq);
            assert_eq!(['D', 'C', 'A'], seq);
        }

        #[test]
        fn test_verb10() {
            let mut seq = ['A', 'B', 'x', 'D'];
            rearrange_glyphs(Verb10, &mut seq);
            assert_eq!(['D', 'x', 'A', 'B'], seq);

            let mut seq = ['A', 'B', 'D'];
            rearrange_glyphs(Verb10, &mut seq);
            assert_eq!(['D', 'A', 'B'], seq);
        }

        #[test]
        fn test_verb11() {
            let mut seq = ['A', 'B', 'x', 'D'];
            rearrange_glyphs(Verb11, &mut seq);
            assert_eq!(['D', 'x', 'B', 'A'], seq);

            let mut seq = ['A', 'B', 'D'];
            rearrange_glyphs(Verb11, &mut seq);
            assert_eq!(['D', 'B', 'A'], seq);
        }

        #[test]
        fn test_verb12() {
            let mut seq = ['A', 'B', 'x', 'C', 'D'];
            rearrange_glyphs(Verb12, &mut seq);
            assert_eq!(['C', 'D', 'x', 'A', 'B'], seq);

            let mut seq = ['A', 'B', 'C', 'D'];
            rearrange_glyphs(Verb12, &mut seq);
            assert_eq!(['C', 'D', 'A', 'B'], seq);
        }

        #[test]
        fn test_verb13() {
            let mut seq = ['A', 'B', 'x', 'C', 'D'];
            rearrange_glyphs(Verb13, &mut seq);
            assert_eq!(['C', 'D', 'x', 'B', 'A'], seq);

            let mut seq = ['A', 'B', 'C', 'D'];
            rearrange_glyphs(Verb13, &mut seq);
            assert_eq!(['C', 'D', 'B', 'A'], seq);
        }

        #[test]
        fn test_verb14() {
            let mut seq = ['A', 'B', 'x', 'C', 'D'];
            rearrange_glyphs(Verb14, &mut seq);
            assert_eq!(['D', 'C', 'x', 'A', 'B'], seq);

            let mut seq = ['A', 'B', 'C', 'D'];
            rearrange_glyphs(Verb14, &mut seq);
            assert_eq!(['D', 'C', 'A', 'B'], seq);
        }

        #[test]
        fn test_verb15() {
            let mut seq = ['A', 'B', 'x', 'C', 'D'];
            rearrange_glyphs(Verb15, &mut seq);
            assert_eq!(['D', 'C', 'x', 'B', 'A'], seq);

            let mut seq = ['A', 'B', 'C', 'D'];
            rearrange_glyphs(Verb15, &mut seq);
            assert_eq!(['D', 'C', 'B', 'A'], seq);
        }
    }

    #[test]
    #[cfg(feature = "prince")]
    fn zapfino() -> Result<(), ParseError> {
        let buffer = read_fixture("../../../tests/data/fonts/morx/Zapfino.ttf");
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let table_provider = otf.table_provider(0).expect("error reading font file");

        let maxp_data = table_provider
            .read_table_data(tag::MAXP)
            .expect("unable to read maxp table data");
        let maxp = ReadScope::new(&maxp_data).read::<MaxpTable>().unwrap();
        let morx_data = table_provider
            .read_table_data(tag::MORX)
            .expect("unable to read morx data");
        let morx = ReadScope::new(&morx_data)
            .read_dep::<MorxTable<'_>>(maxp.num_glyphs)
            .expect("unable to parse morx table");

        let provider = otf.table_provider(0).expect("error reading font file");
        let mut font = Font::new(provider)?;

        // Map text to glyphs and then apply font shaping
        let script = tag!(b"latn");
        let mut glyphs = font.map_glyphs("ptgffigpfl", script, MatchingPresentation::NotRequired);
        let features = Features::Mask(FeatureMask::default());
        apply(&morx, &mut glyphs, &features)?;

        let expected = [
            (585, "p"),
            (604, "t"),
            (541, "g"),
            (1086, "ffi"),
            (541, "g"),
            (1108, "pf"),
            (565, "l"),
        ];
        let actual = glyphs
            .iter()
            .map(|glyph| (glyph.glyph_index, glyph.unicodes.iter().collect::<String>()))
            .collect::<Vec<_>>();
        let actual = actual
            .iter()
            .map(|(gid, text)| (*gid, text.as_str()))
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);

        let mut glyphs = font.map_glyphs("ptpfgffigpfl", script, MatchingPresentation::NotRequired);
        let features = Features::Mask(FeatureMask::default());
        apply(&morx, &mut glyphs, &features)?;

        let expected = [
            (585, "p"),
            (604, "t"),
            (1108, "pf"),
            (541, "g"),
            (1086, "ffi"),
            (541, "g"),
            (1108, "pf"),
            (565, "l"),
        ];
        let actual = glyphs
            .iter()
            .map(|glyph| (glyph.glyph_index, glyph.unicodes.iter().collect::<String>()))
            .collect::<Vec<_>>();
        let actual = actual
            .iter()
            .map(|(gid, text)| (*gid, text.as_str()))
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);

        // There is a ligature for the whole string Zapfino
        let mut glyphs = font.map_glyphs("Zapfino", script, MatchingPresentation::NotRequired);
        let features = Features::Mask(FeatureMask::default());
        apply(&morx, &mut glyphs, &features)?;

        let expected = [(1059, "Zapfino")];
        let actual = glyphs
            .iter()
            .map(|glyph| (glyph.glyph_index, glyph.unicodes.iter().collect::<String>()))
            .collect::<Vec<_>>();
        let actual = actual
            .iter()
            .map(|(gid, text)| (*gid, text.as_str()))
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);

        Ok(())
    }
}
