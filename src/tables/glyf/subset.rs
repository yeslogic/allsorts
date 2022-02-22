use super::{GlyfRecord, GlyfTable, Glyph, GlyphData, ParseError};
use crate::subset::SubsetGlyphs;

#[derive(Clone)]
pub struct SubsetGlyph<'a> {
    pub old_id: u16,
    pub record: GlyfRecord<'a>,
}

impl<'a> GlyfTable<'a> {
    /// Returns a copy of this table that only contains the glyphs specified by `glyph_ids`.
    pub fn subset(&self, glyph_ids: &[u16]) -> Result<Vec<SubsetGlyph<'a>>, ParseError> {
        let mut glyph_ids = glyph_ids.to_vec();
        let mut records = Vec::with_capacity(glyph_ids.len());

        let mut i = 0;
        while i < glyph_ids.len() {
            let glyph_id = glyph_ids[i];
            let mut record = self
                .records
                .get(usize::from(glyph_id))
                .ok_or(ParseError::BadIndex)?
                .clone();
            if record.is_composite() {
                record.parse()?;
                add_glyph(&mut glyph_ids, &mut record);
            }
            records.push(SubsetGlyph {
                old_id: glyph_id,
                record,
            });
            i += 1;
        }
        Ok(records)
    }
}

impl<'a> SubsetGlyphs for Vec<SubsetGlyph<'a>> {
    fn len(&self) -> usize {
        self.len()
    }

    fn old_id(&self, new_id: u16) -> u16 {
        self[usize::from(new_id)].old_id
    }

    fn new_id(&self, old_id: u16) -> u16 {
        // Cast should be safe as there must be less than u16::MAX glyphs in a font
        self.iter().position(|glyph| glyph.old_id == old_id).unwrap_or(0) as u16
    }
}

impl<'a> From<Vec<SubsetGlyph<'a>>> for GlyfTable<'a> {
    fn from(subset_glyphs: Vec<SubsetGlyph<'a>>) -> Self {
        let records = subset_glyphs
            .into_iter()
            .map(|subset_record| subset_record.record)
            .collect();

        GlyfTable { records }
    }
}

fn add_glyph(glyph_ids: &mut Vec<u16>, record: &mut GlyfRecord<'_>) {
    match record {
        GlyfRecord::Parsed(Glyph {
            data: GlyphData::Composite { glyphs, .. },
            ..
        }) => {
            for composite_glyph in glyphs.iter_mut() {
                let new_id = glyph_ids
                    .iter()
                    .position(|&id| id == composite_glyph.glyph_index)
                    .unwrap_or_else(|| {
                        let new_id = glyph_ids.len();
                        glyph_ids.push(composite_glyph.glyph_index);
                        new_id
                    });
                composite_glyph.glyph_index = new_id as u16;
            }
        }
        _ => unreachable!(),
    }
}
