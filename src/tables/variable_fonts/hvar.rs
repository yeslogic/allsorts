//! `HVAR` — Horizontal Metrics Variations Table
//!
//! Optional table in variable fonts to provide horizontal metrics variations. If absent then
//! `gvar` deltas much be used to determine adjustments to metrics.
//!
//! https://learn.microsoft.com/en-us/typography/opentype/spec/hvar

use crate::binary::read::{ReadBinary, ReadCtxt, ReadScope};
use crate::error::ParseError;
use crate::tables::variable_fonts::{
    DeltaSetIndexMap, DeltaSetIndexMapEntry, ItemVariationStore, OwnedTuple,
};
use crate::SafeFrom;

/// `HVAR` — Horizontal Metrics Variations Table.
pub struct HvarTable<'a> {
    /// Major version number of the horizontal metrics variations table.
    pub major_version: u16,
    /// Minor version number of the horizontal metrics variations table.
    pub minor_version: u16,
    /// Offset in bytes from the start of this table to the item variation store table.
    item_variation_store: ItemVariationStore<'a>,
    /// Offset in bytes from the start of this table to the delta-set index mapping for advance
    /// widths.
    advance_width_mapping: Option<DeltaSetIndexMap<'a>>,
    /// Offset in bytes from the start of this table to the delta-set index mapping for left side
    /// bearings.
    lsb_mapping: Option<DeltaSetIndexMap<'a>>,
    /// Offset in bytes from the start of this table to the delta-set index mapping for right side
    /// bearings.
    rsb_mapping: Option<DeltaSetIndexMap<'a>>,
}

impl<'a> HvarTable<'a> {
    /// Calculate the delta for the advance of the supplied `glyph_id`.
    pub fn advance_delta(&self, instance: &OwnedTuple, glyph_id: u16) -> Result<f32, ParseError> {
        // If there is no delta-set index mapping table then glyph IDs implicitly provide the
        // indices: for a given glyph ID, the delta-set outer-level index is zero, and the glyph ID
        // is the delta-set inner-level index.
        let delta_set_entry =
            Self::delta_set_entry_for_glyph(glyph_id, self.advance_width_mapping.as_ref())?;
        self.item_variation_store
            .adjustment(delta_set_entry, instance)
    }

    /// Calculate the delta for the left-side bearing of the supplied `glyph_id`.
    pub fn left_side_bearing_delta(
        &self,
        instance: &OwnedTuple,
        glyph_id: u16,
    ) -> Result<f32, ParseError> {
        let delta_set_entry = Self::delta_set_entry_for_glyph(glyph_id, self.lsb_mapping.as_ref())?;
        self.item_variation_store
            .adjustment(delta_set_entry, instance)
    }

    /// Calculate the delta for the right-side bearing of the supplied `glyph_id`.
    pub fn right_side_bearing_delta(
        &self,
        instance: &OwnedTuple,
        glyph_id: u16,
    ) -> Result<f32, ParseError> {
        let delta_set_entry = Self::delta_set_entry_for_glyph(glyph_id, self.rsb_mapping.as_ref())?;
        self.item_variation_store
            .adjustment(delta_set_entry, instance)
    }

    fn delta_set_entry_for_glyph(
        glyph_id: u16,
        delta_set_index_map: Option<&DeltaSetIndexMap<'_>>,
    ) -> Result<DeltaSetIndexMapEntry, ParseError> {
        Ok(delta_set_index_map
            .map(|mapping| mapping.entry(u32::from(glyph_id)))
            .transpose()?
            .unwrap_or_else(|| DeltaSetIndexMapEntry {
                outer_index: 0,
                inner_index: glyph_id,
            }))
    }
}

impl ReadBinary for HvarTable<'_> {
    type HostType<'a> = HvarTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let major_version = ctxt.read_u16be()?;
        ctxt.check_version(major_version == 1)?;
        let minor_version = ctxt.read_u16be()?;
        let item_variation_store_offset = ctxt.read_u32be()?;
        let advance_width_mapping_offset = ctxt.read_u32be()?;
        let lsb_mapping_offset = ctxt.read_u32be()?;
        let rsb_mapping_offset = ctxt.read_u32be()?;

        let item_variation_store = scope
            .offset(usize::safe_from(item_variation_store_offset))
            .read::<ItemVariationStore<'_>>()?;
        let advance_width_mapping = read_optional_index_map(scope, advance_width_mapping_offset)?;
        let lsb_mapping = read_optional_index_map(scope, lsb_mapping_offset)?;
        let rsb_mapping = read_optional_index_map(scope, rsb_mapping_offset)?;

        Ok(HvarTable {
            major_version,
            minor_version,
            item_variation_store,
            advance_width_mapping,
            lsb_mapping,
            rsb_mapping,
        })
    }
}

fn read_optional_index_map(
    scope: ReadScope<'_>,
    offset: u32,
) -> Result<Option<DeltaSetIndexMap<'_>>, ParseError> {
    (offset > 0)
        .then(|| {
            scope
                .offset(usize::safe_from(offset))
                .read::<DeltaSetIndexMap<'_>>()
        })
        .transpose()
}
