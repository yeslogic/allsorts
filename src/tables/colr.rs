// #![deny(missing_docs)]

//! `COLR` table parsing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/colr>

use crate::binary::{U24Be, U32Be};
use crate::tables::variable_fonts::{
    DeltaSetIndexMap, DeltaSetIndexMapEntry, ItemVariationStore, OwnedTuple,
};
use crate::SafeFrom;
use crate::{
    binary::{
        read::{ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope},
        U16Be,
    },
    error::ParseError,
};

/// `COLR` — Color Table
pub enum ColrTable<'a> {
    V0(ColrV0<'a>),
    V1(ColrV1<'a>),
}

/// `COLR` version 0.
pub struct ColrV0<'a> {
    base_glyph_records: ReadArray<'a, BaseGlyph>,
    layer_records: ReadArray<'a, Layer>,
}

/// `COLR` version 1.
pub struct ColrV1<'a> {
    base_glyph_records: Option<ReadArray<'a, BaseGlyph>>,
    layer_records: Option<ReadArray<'a, Layer>>,
    base_glyph_list: Option<BaseGlyphList<'a>>,
    layer_list: Option<LayerList<'a>>,
    clip_list: Option<ClipList<'a>>,
    var_index_map: Option<DeltaSetIndexMap<'a>>,
    item_variation_store: Option<ItemVariationStore<'a>>,
}

impl ReadBinary for ColrTable<'_> {
    type HostType<'a> = ColrTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let start = ctxt.scope();
        let version = ctxt.read_u16be()?;
        match version {
            0 => {
                let v0 = ctxt.read_dep::<ColrV0<'a>>(start)?;
                Ok(ColrTable::V0(v0))
            }
            1 => {
                let v1 = ctxt.read_dep::<ColrV1<'a>>(start)?;
                Ok(ColrTable::V1(v1))
            }
            _ => Err(ParseError::BadVersion),
        }
    }
}

impl ReadBinaryDep for ColrV0<'_> {
    type HostType<'a> = ColrV0<'a>;
    type Args<'a> = ReadScope<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        colr_scope: ReadScope<'a>,
    ) -> Result<Self::HostType<'a>, ParseError> {
        // Number of BaseGlyph records.
        let num_base_glyph_records = ctxt.read_u16be()?;
        // Offset to baseGlyphRecords array, from beginning of COLR table.
        let base_glyph_records_offset = ctxt.read_u32be()?;
        // Offset to layerRecords array, from beginning of COLR table.
        let layer_records_offset = ctxt.read_u32be()?;
        // Number of Layer records.
        let num_layer_records = ctxt.read_u16be()?;

        let base_glyph_records = colr_scope
            .offset(usize::safe_from(base_glyph_records_offset))
            .ctxt()
            .read_array(usize::from(num_base_glyph_records))?;

        let layer_records = colr_scope
            .offset(usize::safe_from(layer_records_offset))
            .ctxt()
            .read_array(usize::from(num_layer_records))?;

        Ok(ColrV0 {
            base_glyph_records,
            layer_records,
        })
    }
}

impl ReadBinaryDep for ColrV1<'_> {
    type HostType<'a> = ColrV1<'a>;
    type Args<'a> = ReadScope<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        colr_scope: ReadScope<'a>,
    ) -> Result<Self::HostType<'a>, ParseError> {
        // Number of BaseGlyph records; may be 0 in a version 1 table.
        let num_base_glyph_records = ctxt.read_u16be()?;
        // Offset to baseGlyphRecords array, from beginning of COLR table (may be NULL).
        let base_glyph_records_offset = ctxt.read_u32be()?;
        // Offset to layerRecords array, from beginning of COLR table (may be NULL).
        let layer_records_offset = ctxt.read_u32be()?;
        // Number of Layer records; may be 0 in a version 1 table.
        let num_layer_records = ctxt.read_u16be()?;
        // Offset to BaseGlyphList table, from beginning of COLR table.
        let base_glyph_list_offset = ctxt.read_u32be()?;
        // Offset to LayerList table, from beginning of COLR table (may be NULL).
        let layer_list_offset = ctxt.read_u32be()?;
        // Offset to ClipList table, from beginning of COLR table (may be NULL).
        let clip_list_offset = ctxt.read_u32be()?;
        // Offset to DeltaSetIndexMap table, from beginning of COLR table (may be NULL).
        let var_index_map_offset = ctxt.read_u32be()?;
        // Offset to ItemVariationStore, from beginning of COLR table (may be NULL).
        let item_variation_store_offset = ctxt.read_u32be()?;

        let base_glyph_records = (num_base_glyph_records == 0 || base_glyph_records_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(base_glyph_records_offset))
                    .ctxt()
                    .read_array(usize::from(num_base_glyph_records))
            })
            .transpose()?;

        let layer_records = (num_layer_records == 0 || layer_records_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(layer_records_offset))
                    .ctxt()
                    .read_array(usize::from(num_layer_records))
            })
            .transpose()?;

        let base_glyph_list = (base_glyph_list_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(base_glyph_list_offset))
                    .ctxt()
                    .read::<BaseGlyphList<'_>>()
            })
            .transpose()?;

        let layer_list = (layer_list_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(layer_list_offset))
                    .ctxt()
                    .read::<LayerList<'_>>()
            })
            .transpose()?;

        let clip_list = (clip_list_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(clip_list_offset))
                    .ctxt()
                    .read::<ClipList<'_>>()
            })
            .transpose()?;

        let var_index_map = (var_index_map_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(var_index_map_offset))
                    .ctxt()
                    .read::<DeltaSetIndexMap<'_>>()
            })
            .transpose()?;

        let item_variation_store = (item_variation_store_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(item_variation_store_offset))
                    .ctxt()
                    .read::<ItemVariationStore<'_>>()
            })
            .transpose()?;

        Ok(ColrV1 {
            base_glyph_records,
            layer_records,
            base_glyph_list,
            layer_list,
            clip_list,
            var_index_map,
            item_variation_store,
        })
    }
}

/// BaseGlyph record.
struct BaseGlyph {
    /// Glyph ID of the base glyph.
    glyph_id: u16,
    /// Index (base 0) into the layerRecords array.
    first_layer_index: u16,
    /// Number of color layers associated with this glyph.
    num_layers: u16,
}

/// Layer record
struct Layer {
    /// Glyph ID of the glyph used for a given layer.
    ///
    /// The glyphID in a Layer record must be less than the numGlyphs value in the `maxp` table.
    glyph_id: u16,
    /// Index (base 0) for a palette entry in the `CPAL` table.
    ///
    /// The paletteIndex value must be less than the numPaletteEntries value in the `CPAL` table. A
    /// paletteIndex value of 0xFFFF is a special case, indicating that the text foreground color
    /// (as determined by the application) is to be used.
    palette_index: u16,
}

impl ReadFrom for BaseGlyph {
    type ReadType = (U16Be, U16Be, U16Be);

    fn read_from((glyph_id, first_layer_index, num_layers): (u16, u16, u16)) -> Self {
        BaseGlyph {
            glyph_id,
            first_layer_index,
            num_layers,
        }
    }
}

impl ReadFrom for Layer {
    type ReadType = (U16Be, U16Be);

    fn read_from((glyph_id, palette_index): (u16, u16)) -> Self {
        Layer {
            glyph_id,
            palette_index,
        }
    }
}

struct BaseGlyphList<'a> {
    scope: ReadScope<'a>,
    records: ReadArray<'a, BaseGlyphPaintRecord>,
}

impl ReadBinary for BaseGlyphList<'_> {
    type HostType<'a> = BaseGlyphList<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let num_base_glyph_paint_records = ctxt.read_u32be()?;
        let base_glyph_paint_records =
            ctxt.read_array(usize::safe_from(num_base_glyph_paint_records))?;

        Ok(BaseGlyphList {
            scope,
            records: base_glyph_paint_records,
        })
    }
}

struct BaseGlyphPaintRecord {
    /// Glyph ID of the base glyph.
    glyph_id: u16,
    /// Offset to a Paint table, from beginning of BaseGlyphList table.
    paint_offset: u32,
}

impl ReadFrom for BaseGlyphPaintRecord {
    type ReadType = (U16Be, U32Be);

    fn read_from((glyph_id, paint_offset): (u16, u32)) -> Self {
        BaseGlyphPaintRecord {
            glyph_id,
            paint_offset,
        }
    }
}

struct LayerList<'a> {
    scope: ReadScope<'a>,
    paint_offsets: ReadArray<'a, U32Be>,
}

impl ReadBinary for LayerList<'_> {
    type HostType<'a> = LayerList<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let num_layers = ctxt.read_u32be()?;
        let paint_offsets = ctxt.read_array(usize::safe_from(num_layers))?;

        Ok(LayerList {
            scope,
            paint_offsets,
        })
    }
}

struct ClipList<'a> {
    scope: ReadScope<'a>,
    /// Clip list format.
    ///
    /// Currently only format 1 is supported.
    format: u8,
    /// Clip records. Sorted by startGlyphID.
    clips: ReadArray<'a, Clip>,
}

impl ReadBinary for ClipList<'_> {
    type HostType<'a> = ClipList<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        ctxt.check(format == 1)?;
        let num_clips = ctxt.read_u32be()?;
        let clips = ctxt.read_array(usize::safe_from(num_clips))?;

        Ok(ClipList {
            scope,
            format,
            clips,
        })
    }
}

/// Clip record
struct Clip {
    /// First glyph ID in the range.
    start_glyph_id: u16,
    /// Last glyph ID in the range.
    end_glyph_id: u16,
    /// Offset to a ClipBox table, from beginning of ClipList table.
    clip_box_offset: u32, // This is read from a 24-bit value
}

impl ReadFrom for Clip {
    type ReadType = (U16Be, U16Be, U24Be);

    fn read_from((start_glyph_id, end_glyph_id, clip_box_offset): (u16, u16, u32)) -> Self {
        Clip {
            start_glyph_id,
            end_glyph_id,
            clip_box_offset,
        }
    }
}
