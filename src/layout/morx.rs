//! `morx` layout transformations.

use std::convert::TryFrom;
use tinyvec::tiny_vec;

use crate::error::ParseError;
use crate::gsub::{FeatureMask, Features, GlyphOrigin, RawGlyph, RawGlyphFlags};
use crate::tables::morx::{
    Chain, ClassLookupTable, ContextualSubtable, LigatureSubtable, LookupTable, MorxTable,
    NonContextualSubtable, SubtableType,
};

/// Out of bounds.
///
/// All glyph indexes that are less than firstGlyph, or greater than or equal to firstGlyph plus nGlyphs will automatically be assigned class code 1. Class code 1 may also appear in the class array.
const CLASS_CODE_OOB: u16 = 1;

/// Deleted glyph.
///
/// Sometimes contextual processing removes a glyph from the glyph array by changing its glyph index to the deleted glyph index, 0xFFFF. This glyph code is automatically assigned class "deleted," which should not appear in the class array.
const CLASS_CODE_DELETED: u16 = 2;

/// Perform a lookup in a class lookup table.
fn lookup<'a>(glyph: u16, lookup_table: &ClassLookupTable<'a>) -> Option<u16> {
    if glyph == 0xFFFF {
        return Some(0xFFFF);
    }

    match &lookup_table.lookup_table {
        LookupTable::Format0 { lookup_values } => lookup_values.get(usize::from(glyph)).copied(),
        LookupTable::Format2 { lookup_segments } => {
            lookup_segments.iter().find_map(|lookup_segment| {
                lookup_segment
                    .contains(glyph)
                    .then_some(lookup_segment.lookup_value)
            })
        }
        LookupTable::Format4 { lookup_segments } => {
            for lookup_segment in lookup_segments {
                // The segments are meant to be non-overlapping so if a segment contains the glyph
                // then we always return a result.
                if lookup_segment.contains(glyph) {
                    let index = usize::from(glyph - lookup_segment.first_glyph);
                    return (index < lookup_segment.lookup_values.len())
                        .then(|| lookup_segment.lookup_values.get_item(index));
                }
            }
            None
        }
        LookupTable::Format6 { lookup_entries } => lookup_entries.iter().find_map(|lookup_entry| {
            (lookup_entry.glyph == glyph).then_some(lookup_entry.lookup_value)
        }),
        LookupTable::Format8(lookup_table) => lookup_table.lookup(glyph),
        LookupTable::Format10(lookup_table) => lookup_table.lookup(glyph),
    }
}

fn glyph_class<'a>(glyph: u16, class_table: &ClassLookupTable<'a>) -> u16 {
    lookup(glyph, class_table).map_or(CLASS_CODE_OOB, |class_code| {
        if class_code == 0xFFFF {
            // FIXME: Is this right? Seems like it should be mapping a glyph index of 0xFFFF to deleted
            CLASS_CODE_DELETED
        } else {
            class_code
        }
    })
}

pub struct ContextualSubstitution<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    // Records marked glyph and its position: (position, mark_glyph)
    mark: Option<(usize, u16)>,
}

impl<'a> ContextualSubstitution<'a> {
    fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> ContextualSubstitution<'a> {
        ContextualSubstitution {
            glyphs: glyphs,
            next_state: 0,
            mark: None,
        }
    }

    fn process_glyphs<'b>(
        &mut self,
        contextual_subtable: &ContextualSubtable<'b>,
    ) -> Result<(), ParseError> {
        const SET_MARK: u16 = 0x8000;
        const DONT_ADVANCE: u16 = 0x4000;
        let mut old_glyph: u16;
        let mut new_glyph: u16;

        // Loop through glyphs:
        for i in 0..self.glyphs.len() {
            let current_glyph: u16 = self.glyphs[i].glyph_index;
            old_glyph = self.glyphs[i].glyph_index;
            new_glyph = self.glyphs[i].glyph_index;

            let mut class = glyph_class(current_glyph, &contextual_subtable.class_table);

            'glyph: loop {
                let index_to_entry_table;
                let entry;

                if let Some(state_row) = contextual_subtable
                    .state_array
                    .state_array
                    .get(usize::from(self.next_state))
                {
                    index_to_entry_table = state_row.read_item(usize::from(class))?;
                } else {
                    return Err(ParseError::BadIndex);
                }

                if let Some(contxt_entry) = contextual_subtable
                    .entry_table
                    .contextual_entries
                    .get(usize::from(index_to_entry_table))
                {
                    entry = contxt_entry;
                } else {
                    return Err(ParseError::BadIndex);
                }

                self.next_state = entry.next_state;

                // If there is a marked glyph on record and the entry is providing a mark_index to
                // the substitution table for it, then make the substitution for the marked glyph.
                if entry.mark_index != 0xFFFF {
                    if let Some((mark_pos, mark_glyph)) = self.mark {
                        if let Some(mark_glyph_subst) = lookup(
                            mark_glyph,
                            &contextual_subtable.substitution_subtables
                                [usize::from(entry.mark_index)],
                        ) {
                            self.glyphs[mark_pos].glyph_index = mark_glyph_subst;
                            self.glyphs[mark_pos].glyph_origin = GlyphOrigin::Direct;
                        }
                    }
                }

                // If the entry is providing a current_index to the substitution table for the
                // current glyph, then make the substitution for the current glyph.
                if entry.current_index != 0xFFFF {
                    if let Some(current_glyph_subst) = lookup(
                        current_glyph,
                        &contextual_subtable.substitution_subtables
                            [usize::from(entry.current_index)],
                    ) {
                        self.glyphs[i].glyph_index = current_glyph_subst;
                        self.glyphs[i].glyph_origin = GlyphOrigin::Direct;
                        new_glyph = current_glyph_subst;
                    }
                }

                // If entry.flags says SET_MARK, then make the current glyph the marked glyph.
                if entry.flags & SET_MARK != 0 {
                    self.mark = Some((i, self.glyphs[i].glyph_index));
                }

                // Exit the loop 'glyph unless entry.flags says DONT_ADVANCE.
                if entry.flags & DONT_ADVANCE == 0 {
                    break 'glyph;
                }

                // If the entry.flags says DONT_ADVANCE, then keep looping in loop 'glyph, but the
                // class may have to be re-calculated if the current glyph has been substituted.
                if new_glyph != old_glyph {
                    class = glyph_class(new_glyph, &contextual_subtable.class_table);
                    old_glyph = new_glyph;
                }
            }
            // end of loop 'glyph
        }

        Ok(())
    }
}

pub struct LigatureSubstitution<'a> {
    glyphs: &'a mut Vec<RawGlyph<()>>,
    next_state: u16,
    component_stack: Vec<RawGlyph<()>>,
}

impl<'a> LigatureSubstitution<'a> {
    fn new(glyphs: &'a mut Vec<RawGlyph<()>>) -> LigatureSubstitution<'a> {
        LigatureSubstitution {
            glyphs: glyphs,
            next_state: 0,
            component_stack: Vec::new(),
        }
    }

    fn process_glyphs<'b>(
        &mut self,
        ligature_subtable: &LigatureSubtable<'b>,
    ) -> Result<(), ParseError> {
        const SET_COMPONENT: u16 = 0x8000;
        const DONT_ADVANCE: u16 = 0x4000;
        const PERFORM_ACTION: u16 = 0x2000;
        const LAST: u32 = 0x80000000;
        const STORE: u32 = 0x40000000;

        let mut i: usize = 0;
        let mut start_pos: usize = 0;
        let mut end_pos: usize;

        // Loop through glyphs:
        while let Some(glyph) = self.glyphs.get(i) {
            let glyph = glyph.clone();
            let class = glyph_class(glyph.glyph_index, &ligature_subtable.class_table);

            'glyph: loop {
                let index_to_entry_table;
                let entry;

                if let Some(state_row) = ligature_subtable
                    .state_array
                    .state_array
                    .get(usize::from(self.next_state))
                {
                    index_to_entry_table = state_row.read_item(usize::from(class))?;
                } else {
                    return Err(ParseError::BadIndex);
                }

                if let Some(lig_entry) = ligature_subtable
                    .entry_table
                    .lig_entries
                    .get(usize::from(index_to_entry_table))
                {
                    entry = lig_entry;
                } else {
                    return Err(ParseError::BadIndex);
                }

                self.next_state = entry.next_state_index;

                let entry_flags: u16 = entry.entry_flags;

                if entry_flags & SET_COMPONENT != 0 {
                    // Set Component: push this glyph onto the component stack
                    self.component_stack.push(glyph.clone());
                    if self.component_stack.len() == 1 {
                        // Mark the position in the buffer for the first glyph in a ligature group.
                        start_pos = i;
                    }
                }

                if entry_flags & PERFORM_ACTION != 0 {
                    // Perform Action: use the ligActionIndex to process a ligature group.

                    // Mark the position in the buffer for the last glyph in a ligature group.
                    end_pos = i;
                    let mut action_index: usize = usize::from(entry.lig_action_index);
                    let mut index_to_ligature: u16 = 0;
                    let mut ligature: RawGlyph<()> = RawGlyph {
                        unicodes: tiny_vec![[char; 1]],
                        glyph_index: 0x0000,
                        liga_component_pos: 0,
                        glyph_origin: GlyphOrigin::Direct,
                        flags: RawGlyphFlags::empty(),
                        extra_data: (),
                        variation: None,
                    };

                    // Loop through stack
                    'stack: loop {
                        let glyph_popped: u16;

                        match self.component_stack.pop() {
                            Some(val) => {
                                glyph_popped = val.glyph_index;

                                let mut unicodes = val.unicodes;
                                unicodes.append(&mut ligature.unicodes);
                                ligature.unicodes = unicodes;

                                ligature.variation = val.variation;
                            }
                            None => return Err(ParseError::MissingValue),
                        };

                        let action: u32 = ligature_subtable.action_table.actions[action_index];
                        action_index += 1;

                        let mut offset = action & 0x3FFFFFFF; // Take 30 bits

                        if offset & 0x20000000 != 0 {
                            offset |= 0xC0000000; // Sign-extend it to 32 bits
                        }
                        // TODO: check cast
                        let offset = offset as i32; // Convert to signed integer

                        let index_to_components = glyph_popped as i32 + offset;

                        if index_to_components < 0 {
                            return Err(ParseError::BadValue);
                        }

                        let index_to_component_table: usize =
                            match usize::try_from(index_to_components) {
                                Ok(index) => index,
                                Err(_err) => return Err(ParseError::BadValue),
                            };

                        index_to_ligature += &ligature_subtable
                            .component_table
                            .component_array
                            .read_item(index_to_component_table)?;

                        if (action & LAST != 0) || (action & STORE != 0) {
                            // Storage when LAST or STORE is seen

                            let ligature_glyph = ligature_subtable
                                .ligature_list
                                .ligature_list
                                .read_item(usize::from(index_to_ligature))?;

                            ligature.glyph_index = ligature_glyph;

                            // Subsitute glyphs[start_pos..(end_pos+1)] with ligature
                            self.glyphs.drain(start_pos..(end_pos + 1));

                            self.glyphs.insert(start_pos, ligature.clone());
                            i -= end_pos - start_pos; //make adjustment to i after substitution

                            // Push ligature onto stack, only when the next state is non-zero
                            if self.next_state != 0 {
                                self.component_stack.push(ligature.clone());
                            }

                            // "ligature" has been inserted at start_pos in glyphs array and the
                            // next glyph in glyphs array will be processed.
                        }

                        if action & LAST != 0 {
                            // This is the last action, so exit the loop 'stack
                            break 'stack;
                        }
                    }
                    // End of loop 'stack
                }
                // End of PERFORM_ACTION

                if entry_flags & DONT_ADVANCE == 0 {
                    break 'glyph; // Exit the loop 'glyph unless entry_flags says DONT_ADVANCE
                } else {
                    // If the entry_flags does say DONT_ADVANCE, then keep looping with the same
                    // glyph. clear the stack
                    self.component_stack.clear();
                }
            }
            //end of loop 'glyph

            i += 1; //advance to the next glyph
        }
        //end of loop 'glyphs

        Ok(())
    }
}

/// Look up and returns the noncontexutal substitute of glyph.
///
/// Returns 0xFFFF for a glyph index out of bounds of the lookup value array indices.
fn noncontextual_lookup<'a>(glyph: u16, lookup_table: &ClassLookupTable<'a>) -> u16 {
    match lookup(glyph, lookup_table) {
        None => {
            return 0xFFFF;
        }
        Some(val) => {
            return val;
        }
    }
}

fn noncontextual_substitution<'a>(
    glyphs: &mut Vec<RawGlyph<()>>,
    noncontextual_subtable: &NonContextualSubtable<'a>,
) -> Result<(), ParseError> {
    let mut glyph: u16;
    let mut subst: u16;
    for i in 0..glyphs.len() {
        glyph = glyphs[i].glyph_index;

        subst = noncontextual_lookup(glyph, &noncontextual_subtable.lookup_table);

        if (subst != 0xFFFF) && (subst != glyph) {
            glyphs[i].glyph_index = subst;
            glyphs[i].glyph_origin = GlyphOrigin::Direct;
        }
    }
    Ok(())
}

pub fn apply<'a>(
    morx_table: &MorxTable<'a>,
    glyphs: &mut Vec<RawGlyph<()>>,
    features: &Features,
) -> Result<(), ParseError> {
    for chain in morx_table.chains.iter() {
        let subfeatureflags: u32 = subfeatureflags(chain, features)?;
        for subtable in chain.subtables.iter() {
            if subfeatureflags & subtable.subtable_header.sub_feature_flags != 0 {
                match subtable.subtable_header.coverage & 0xFF {
                    1 => {
                        let mut contextual_subst: ContextualSubstitution<'_> =
                            ContextualSubstitution::new(glyphs);

                        if let SubtableType::Contextual {
                            contextual_subtable,
                        } = &subtable.subtable_body
                        {
                            contextual_subst.next_state = 0;
                            contextual_subst.process_glyphs(contextual_subtable)?;
                        } else {
                            return Err(ParseError::BadValue);
                        }
                    }
                    2 => {
                        let mut liga_subst: LigatureSubstitution<'_> =
                            LigatureSubstitution::new(glyphs);

                        if let SubtableType::Ligature { ligature_subtable } =
                            &subtable.subtable_body
                        {
                            liga_subst.next_state = 0;
                            liga_subst.component_stack.clear();
                            liga_subst.process_glyphs(ligature_subtable)?;
                        } else {
                            return Err(ParseError::BadValue);
                        }
                    }
                    4 => {
                        if let SubtableType::NonContextual {
                            noncontextual_subtable,
                        } = &subtable.subtable_body
                        {
                            noncontextual_substitution(glyphs, noncontextual_subtable)?;
                        } else {
                            return Err(ParseError::BadValue);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    Ok(())
}

fn subfeatureflags<'a>(chain: &Chain<'a>, features: &Features) -> Result<u32, ParseError> {
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

    let mut subfeature_flags = chain.chain_header.default_flags;

    for entry in chain.feature_array.iter() {
        match features {
            Features::Custom(_features_list) => {
                return Ok(subfeature_flags);
            }
            Features::Mask(feature_mask) => {
                let apply = match (entry.feature_type, entry.feature_setting) {
                    (NUMBER_CASE_TYPE, LINING_NUMBERS) => feature_mask.contains(FeatureMask::LNUM),
                    (NUMBER_CASE_TYPE, OLD_STYLE_NUMBERS) => {
                        feature_mask.contains(FeatureMask::ONUM)
                    }
                    (NUMBER_SPACING_TYPE, PROPORTIONAL_NUMBERS) => {
                        feature_mask.contains(FeatureMask::PNUM)
                    }
                    (NUMBER_SPACING_TYPE, TABULAR_NUMBERS) => {
                        feature_mask.contains(FeatureMask::TNUM)
                    }
                    (FRACTION_TYPE, FRACTIONS_DIAGONAL) => feature_mask.contains(FeatureMask::FRAC),
                    (FRACTION_TYPE, FRACTIONS_STACKED) => feature_mask.contains(FeatureMask::AFRC),
                    (FRACTION_TYPE, NO_FRACTIONS) => {
                        !feature_mask.contains(FeatureMask::FRAC)
                            && !feature_mask.contains(FeatureMask::AFRC)
                    }
                    (VERTICAL_POSITION_TYPE, ORDINALS) => feature_mask.contains(FeatureMask::ORDN),
                    (TYPOGRAPHIC_EXTRAS_TYPE, SLASHED_ZERO_ON) => {
                        feature_mask.contains(FeatureMask::ZERO)
                    }
                    (TYPOGRAPHIC_EXTRAS_TYPE, SLASHED_ZERO_OFF) => {
                        !feature_mask.contains(FeatureMask::ZERO)
                    }
                    (LOWERCASE_TYPE, LOWERCASE_SMALL_CAPS) => {
                        feature_mask.contains(FeatureMask::SMCP)
                            || feature_mask.contains(FeatureMask::C2SC)
                    }
                    (UPPERCASE_TYPE, UPPERCASE_SMALL_CAPS) => {
                        feature_mask.contains(FeatureMask::C2SC)
                    }
                    (LIGATURE_TYPE, COMMON_LIGATURES_ON) => {
                        feature_mask.contains(FeatureMask::LIGA)
                    }
                    (LIGATURE_TYPE, COMMON_LIGATURES_OFF) => {
                        !feature_mask.contains(FeatureMask::LIGA)
                    }
                    (LIGATURE_TYPE, HISTORICAL_LIGATURES_ON) => {
                        feature_mask.contains(FeatureMask::HLIG)
                    }
                    (LIGATURE_TYPE, HISTORICAL_LIGATURES_OFF) => {
                        !feature_mask.contains(FeatureMask::HLIG)
                    }
                    (LIGATURE_TYPE, CONTEXTUAL_LIGATURES_ON) => {
                        feature_mask.contains(FeatureMask::CLIG)
                    }
                    (LIGATURE_TYPE, CONTEXTUAL_LIGATURES_OFF) => {
                        !feature_mask.contains(FeatureMask::CLIG)
                    }
                    _ => false,
                };

                if apply {
                    subfeature_flags =
                        (subfeature_flags & entry.disable_flags) | entry.enable_flags;
                }
            }
        }
    }
    Ok(subfeature_flags)
}

#[cfg(test)]
mod tests {
    use tinyvec::tiny_vec;

    use super::*;
    use crate::font::MatchingPresentation;
    use crate::tables::{FontTableProvider, OpenTypeFont};
    use crate::tests::read_fixture;
    use crate::{
        binary::read::ReadScope, gsub::RawGlyphFlags, tables::morx::SubtableType, tag, Font,
    };

    fn morx_ligature_test(morx_table: MorxTable<'_>) -> Result<(), ParseError> {
        // string: "ptgffigpfl" (for Ayuthaya.ttf)
        // let mut glyphs: Vec<u16> = vec![197, 201, 188, 187, 187, 190, 188, 197, 187, 193];

        // string: ""\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F467}"" (for emoji.ttf)
        // let mut glyphs:  Vec<u16> = vec![1062, 43, 1164, 43, 1056, 43, 1056];

        // string: "U+1F1E6 U+1F1FA" (for emoji.ttf)
        // let mut glyphs:  Vec<u16> = vec![16, 36];

        let glyph1: RawGlyph<()> = RawGlyph {
            unicodes: tiny_vec![[char; 1]],
            glyph_index: 16,
            liga_component_pos: 0,
            glyph_origin: GlyphOrigin::Direct,
            flags: RawGlyphFlags::empty(),
            extra_data: (),
            variation: None,
        };

        let glyph2: RawGlyph<()> = RawGlyph {
            unicodes: tiny_vec![[char; 1]],
            glyph_index: 36,
            liga_component_pos: 0,
            glyph_origin: GlyphOrigin::Direct,
            flags: RawGlyphFlags::empty(),
            extra_data: (),
            variation: None,
        };

        let mut glyphs: Vec<RawGlyph<()>> = vec![glyph1, glyph2];

        let mut liga_subst: LigatureSubstitution<'_> = LigatureSubstitution::new(&mut glyphs);

        for chain in morx_table.chains.iter() {
            for subtable in chain.subtables.iter() {
                if subtable.subtable_header.coverage & 0xFF == 2 {
                    // liga_subtable_no += 1;
                    // println!("Ligature subtable No: {}", liga_subtable_no);

                    if let SubtableType::Ligature { ligature_subtable } = &subtable.subtable_body {
                        liga_subst.next_state = 0;
                        liga_subst.component_stack.clear();
                        liga_subst.process_glyphs(ligature_subtable)?;
                    }
                }
            }
        }

        // println!("The glyphs array after ligature substitutions: {:?}", glyphs);

        Ok(())
    }

    #[test]
    #[cfg(feature = "prince")]
    fn zapfino() -> Result<(), ParseError> {
        let buffer = read_fixture("../../../tests/data/fonts/morx/Zapfino.ttf");
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let table_provider = otf.table_provider(0).expect("error reading font file");

        let morx_data = table_provider
            .read_table_data(tag::MORX)
            .expect("unable to read morx data");
        let morx = ReadScope::new(&morx_data)
            .read::<MorxTable<'_>>()
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
