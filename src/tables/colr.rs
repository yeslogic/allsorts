// #![deny(missing_docs)]

//! `COLR` table parsing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/colr>

use std::convert::TryFrom;
use std::fmt;

use super::{cpal, F2Dot14, Fixed};
use crate::binary::{U24Be, U32Be};
use crate::tables::cpal::{ColorRecord, CpalTable, Palette};
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
    /// A COLRv0 table
    V0(ColrV0<'a>),
    /// A COLRv1 table
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

impl<'data> ColrTable<'data> {
    /// Look up the color information for the supplied glyph.
    pub fn lookup<'a: 'data>(
        &'a self,
        glyph_id: u16,
    ) -> Result<Option<ColrV1Glyph<'a, 'data>>, ParseError> {
        match self {
            ColrTable::V0(_colr0) => todo!(),
            ColrTable::V1(colr1) => colr1.lookup(glyph_id),
        }
    }
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

struct ColrV1Glyph<'a, 'data> {
    table: &'a ColrV1<'data>,
    paint: Paint<'data>,
}

// Try to map this to concepts in: Quartz, Canvas, Cairo
pub trait Painter {
    fn fill(&self, color: Color);

    fn linear_gradient(&self, gradient: LinearGradient<'_>);
    fn radial_gradient(&self, gradient: RadialGradient<'_>);

    fn conic_gradient(&self, gradient: ConicGradient<'_>);

    fn push_clip(&self);
    fn pop_clip(&self);
    fn clip_path(&self);

    // compose the graphics state
    fn compose(&self);

    fn push_state(&self);
    fn pop_state(&self);

    fn transform(&self);
    fn translate(&self, dx: i16, dy: i16);
    fn scale(&self);
    fn rotate(&self);

    fn composite(&self, mode: CompositeMode);
}

struct PaintStack {}

impl PaintStack {
    fn push(&mut self, _paint: &Paint<'_>) {
        // TODO: work out how to identify a paint table
    }

    fn pop(&mut self) {
        // TODO
    }
}

impl ColrV1Glyph<'_, '_> {
    pub fn visit<P>(&self, painter: &P, palette: Palette<'_, '_>) -> Result<(), ParseError>
    where
        P: Painter,
    {
        self.paint.visit(painter, palette, &mut PaintStack {})
    }
}

impl Paint<'_> {
    fn visit<P>(
        &self,
        painter: &P,
        palette: Palette<'_, '_>,
        stack: &mut PaintStack,
    ) -> Result<(), ParseError>
    where
        P: Painter,
    {
        match self {
            Paint::ColrLayers(paint_colr_layers) => todo!(),
            Paint::Solid(paint_solid) => {
                if let Some(color) = paint_solid.color(palette) {
                    painter.fill(color)
                } else {
                    // TODO: How to handle a bad color reference
                    // We still need to call fill, as this is a 'closing' operation that resets
                    // graphics state
                }
            }
            Paint::LinearGradient(paint_linear_gradient) => {
                let color_line = paint_linear_gradient.color_line()?;
                let gradient = LinearGradient {
                    color_line,
                    start_point: (paint_linear_gradient.x0, paint_linear_gradient.y0),
                    end_point: (paint_linear_gradient.x1, paint_linear_gradient.y1),
                    rotation_point: (paint_linear_gradient.x2, paint_linear_gradient.y2),
                };
                painter.linear_gradient(gradient)
            }
            Paint::RadialGradient(paint_radial_gradient) => {
                let color_line = paint_radial_gradient.color_line()?;
                let gradient = RadialGradient {
                    color_line,
                    start_circle: Circle {
                        x: paint_radial_gradient.x0,
                        y: paint_radial_gradient.y0,
                        radius: paint_radial_gradient.radius0,
                    },
                    end_circle: Circle {
                        x: paint_radial_gradient.x1,
                        y: paint_radial_gradient.y1,
                        radius: paint_radial_gradient.radius1,
                    },
                };
                painter.radial_gradient(gradient)
            }
            Paint::SweepGradient(paint_sweep_gradient) => {
                let color_line = paint_sweep_gradient.color_line()?;
                let gradient = ConicGradient {
                    color_line,
                    center: (paint_sweep_gradient.center_x, paint_sweep_gradient.center_y),
                    start_angle: paint_sweep_gradient.start_angle.into(),
                    end_angle: paint_sweep_gradient.end_angle.into(),
                };
                painter.conic_gradient(gradient)
            }
            Paint::Glyph(paint_glyph) => todo!(),
            Paint::ColrGlyph(paint_colr_glyph) => todo!(),
            Paint::Transform(paint_transform) => todo!(),
            Paint::Translate(paint_translate) => {
                let paint = paint_translate.subpaint()?;
                painter.push_state();
                painter.translate(paint_translate.dx, paint_translate.dy);
                stack.push(&self);
                paint.visit(painter, palette, stack)?;
                stack.pop();
                painter.pop_state();
            }
            Paint::Scale(paint_scale) => todo!(),
            Paint::Rotate(paint_rotate) => todo!(),
            Paint::Skew(paint_skew) => todo!(),
            Paint::Composite(paint_composite) => todo!(),
        }

        Ok(())
    }
}

impl<'data> ColrV1<'data> {
    fn lookup<'a: 'data>(
        &'a self,
        glyph_id: u16,
    ) -> Result<Option<ColrV1Glyph<'a, 'data>>, ParseError> {
        Ok(self
            .base_glyph_list
            .as_ref()
            .unwrap()
            .record(glyph_id)?
            .map(|paint| ColrV1Glyph { table: self, paint }))
    }
}

impl ReadBinaryDep for ColrV1<'_> {
    type Args<'a> = ReadScope<'a>;
    type HostType<'a> = ColrV1<'a>;

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

        let base_glyph_records = (num_base_glyph_records > 0 && base_glyph_records_offset != 0)
            .then(|| {
                colr_scope
                    .offset(usize::safe_from(base_glyph_records_offset))
                    .ctxt()
                    .read_array(usize::from(num_base_glyph_records))
            })
            .transpose()?;

        let layer_records = (num_layer_records > 0 && layer_records_offset != 0)
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
#[derive(Debug, Copy, Clone)]
struct BaseGlyph {
    /// Glyph ID of the base glyph.
    glyph_id: u16,
    /// Index (base 0) into the layerRecords array.
    first_layer_index: u16,
    /// Number of color layers associated with this glyph.
    num_layers: u16,
}

/// Layer record
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
struct BaseGlyphList<'a> {
    scope: ReadScope<'a>,
    records: ReadArray<'a, BaseGlyphPaintRecord>,
}

impl BaseGlyphList<'_> {
    pub fn record(&self, glyph_id: u16) -> Result<Option<Paint<'_>>, ParseError> {
        let Some(record_index) = self
            .records
            .binary_search_by(|record| record.glyph_id.cmp(&glyph_id))
            .ok()
        else {
            return Ok(None);
        };
        let record = self
            .records
            .get_item(record_index)
            .ok_or(ParseError::BadIndex)?;
        self.scope
            .offset(usize::safe_from(record.paint_offset))
            .read::<Paint<'_>>()
            .map(Some)
    }
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

#[derive(Debug, Copy, Clone)]
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

#[derive(Debug)]
struct LayerList<'a> {
    scope: ReadScope<'a>,
    paint_offsets: ReadArray<'a, U32Be>,
}

impl<'a> LayerList<'a> {
    pub fn layers(&self) -> impl Iterator<Item = Result<Paint<'a>, ParseError>> + '_ {
        self.paint_offsets.iter().map(move |offset| {
            self.scope
                .offset(usize::safe_from(offset))
                .read::<Paint<'a>>()
        })
    }
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

#[derive(Debug)]
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
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
struct ClipBox {
    /// Minimum x of clip box.
    ///
    /// For variation, use varIndexBase + 0.
    x_min: i16,
    /// Minimum y of clip box.
    ///
    /// For variation, use varIndexBase + 1.
    y_min: i16,
    /// Maximum x of clip box.
    ///
    /// For variation, use varIndexBase + 2.
    x_max: i16,
    /// Maximum y of clip box.
    ///
    /// For variation, use varIndexBase + 3.
    y_max: i16,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

impl ReadBinary for ClipBox {
    type HostType<'a> = ClipBox;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let format = ctxt.read_u16be()?;
        ctxt.check(format == 1 || format == 2)?;
        let x_min = ctxt.read_i16be()?;
        let y_min = ctxt.read_i16be()?;
        let x_max = ctxt.read_i16be()?;
        let y_max = ctxt.read_i16be()?;
        let var_index_base = (format == 2).then(|| ctxt.read_u32be()).transpose()?;

        Ok(ClipBox {
            x_min,
            y_min,
            x_max,
            y_max,
            var_index_base,
        })
    }
}

#[derive(Debug, Clone, Copy)]
struct ColorStop {
    /// Position on a color line.
    stop_offset: F2Dot14,
    /// Index for a `CPAL` palette entry.
    palette_index: u16,
    /// Alpha value.
    alpha: F2Dot14,
}

impl ReadFrom for ColorStop {
    type ReadType = (F2Dot14, U16Be, F2Dot14);

    fn read_from((stop_offset, palette_index, alpha): (F2Dot14, u16, F2Dot14)) -> Self {
        ColorStop {
            stop_offset,
            palette_index,
            alpha,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct VarColorStop {
    /// Position on a color line.
    ///
    /// For variation, use varIndexBase + 0.
    stop_offset: F2Dot14,
    /// Index for a `CPAL` palette entry.
    palette_index: u16,
    /// Alpha value.
    ///
    /// For variation, use varIndexBase + 1.
    alpha: F2Dot14,
    /// Base index into DeltaSetIndexMap.
    var_index_base: u32,
}

impl ReadFrom for VarColorStop {
    type ReadType = (F2Dot14, U16Be, F2Dot14, U32Be);

    fn read_from(
        (stop_offset, palette_index, alpha, var_index_base): (F2Dot14, u16, F2Dot14, u32),
    ) -> Self {
        VarColorStop {
            stop_offset,
            palette_index,
            alpha,
            var_index_base,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ColorLine<'a> {
    /// An Extend enum value.
    extend: Extend,
    /// ColorStop records.
    color_stops: ReadArray<'a, ColorStop>,
}

impl ColorLine<'_> {
    pub fn extend(&self) -> Extend {
        self.extend
    }

    pub fn color_stops(&self) -> impl Iterator<Item = ColorStop> + '_ {
        self.color_stops.iter()
    }
}

impl ReadBinary for ColorLine<'_> {
    type HostType<'a> = ColorLine<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let extend = ctxt.read_u8()?;
        // If a ColorLine in a font has an unrecognized extend value,
        // applications should use EXTEND_PAD by default.
        let extend = Extend::try_from(extend).unwrap_or(Extend::Pad);
        let num_stops = ctxt.read_u16be()?;
        let color_stops = ctxt.read_array(usize::from(num_stops))?;
        Ok(ColorLine {
            extend,
            color_stops,
        })
    }
}

#[derive(Debug, Clone)]
struct VarColorLine<'a> {
    /// An Extend enum value.
    extend: Extend,
    /// Number of ColorStop records.
    num_stops: u16,
    /// Allows for variations.
    color_stops: ReadArray<'a, VarColorStop>,
}

#[derive(Debug, Clone, Copy)]
enum Extend {
    /// Use nearest color stop.
    Pad,
    /// Repeat from farthest color stop.
    Repeat,
    /// Mirror color line from nearest end.
    Reflect,
}

impl TryFrom<u8> for Extend {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Extend::Pad),
            1 => Ok(Extend::Repeat),
            2 => Ok(Extend::Reflect),
            _ => Err(ParseError::BadValue),
        }
    }
}

#[derive(Debug)]
enum Paint<'a> {
    ColrLayers(PaintColrLayers),
    Solid(PaintSolid),
    LinearGradient(PaintLinearGradient<'a>),
    RadialGradient(PaintRadialGradient<'a>),
    SweepGradient(PaintSweepGradient<'a>),
    Glyph(PaintGlyph<'a>),
    ColrGlyph(PaintColrGlyph),
    Transform(PaintTransform<'a>),
    Translate(PaintTranslate<'a>),
    Scale(PaintScale<'a>),
    Rotate(PaintRotate<'a>),
    Skew(PaintSkew<'a>),
    Composite(PaintComposite<'a>),
}

macro_rules! subpaint {
    ($t:ty) => {
        impl $t {
            fn subpaint(&self) -> Result<Paint<'_>, ParseError> {
                self.scope
                    .offset(usize::safe_from(self.paint_offset))
                    .ctxt()
                    .read::<Paint<'_>>()
            }
        }
    };
}

subpaint!(PaintGlyph<'_>);
subpaint!(PaintTransform<'_>);
subpaint!(PaintTranslate<'_>);
subpaint!(PaintScale<'_>);
subpaint!(PaintRotate<'_>);
subpaint!(PaintSkew<'_>);

#[derive(Debug)]
struct PaintColrLayers {
    /// Number of offsets to paint tables to read from LayerList.
    num_layers: u8,
    /// Index (base 0) into the LayerList.
    first_layer_index: u32,
}

#[derive(Debug)]
struct PaintSolid {
    /// Index for a CPAL palette entry.
    palette_index: u16,
    /// Alpha value.
    alpha: F2Dot14,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

trait Gradient {
    fn scope(&self) -> ReadScope<'_>;

    fn color_line_offset(&self) -> u32;

    fn color_line(&self) -> Result<ColorLine<'_>, ParseError> {
        self.scope()
            .offset(usize::safe_from(self.color_line_offset()))
            .ctxt()
            .read::<ColorLine<'_>>()
    }
}

#[derive(Debug)]
struct PaintLinearGradient<'a> {
    scope: ReadScope<'a>,
    /// Offset to ColorLine table, from beginning of PaintLinearGradient table.
    color_line_offset: u32, // Offset24,
    /// Start point (p₀) x coordinate.
    x0: i16,
    /// Start point (p₀) y coordinate.
    y0: i16,
    /// End point (p₁) x coordinate.
    x1: i16,
    /// End point (p₁) y coordinate.
    y1: i16,
    /// Rotation point (p₂) x coordinate.
    x2: i16,
    /// Rotation point (p₂) y coordinate.
    y2: i16,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
pub struct LinearGradient<'a> {
    pub color_line: ColorLine<'a>,
    /// Start point (p₀)
    pub start_point: (i16, i16),
    /// End point (p₁)
    pub end_point: (i16, i16),
    /// Rotation point (p₂)
    pub rotation_point: (i16, i16),
}

#[derive(Debug)]
struct PaintRadialGradient<'a> {
    scope: ReadScope<'a>,
    /// Offset to VarColorLine table, from beginning of PaintVarRadialGradient table.
    color_line_offset: u32, // Offset24,
    /// Start circle center x coordinate.
    ///
    /// For variation, use varIndexBase + 0.
    x0: i16,
    /// Start circle center y coordinate.
    ///
    /// For variation, use varIndexBase + 1.
    y0: i16,
    /// Start circle radius.
    ///
    /// For variation, use varIndexBase + 2.
    radius0: u16,
    /// End circle center x coordinate.
    ///
    /// For variation, use varIndexBase + 3.
    x1: i16,
    /// End circle center y coordinate.
    ///
    /// For variation, use varIndexBase + 4.
    y1: i16,
    /// End circle radius.
    ///
    /// For variation, use varIndexBase + 5.
    radius1: u16,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
pub struct RadialGradient<'a> {
    pub color_line: ColorLine<'a>,
    /// Start circle
    pub start_circle: Circle,
    /// End circle
    pub end_circle: Circle,
}

#[derive(Debug, Copy, Clone)]
pub struct Circle {
    pub x: i16,
    pub y: i16,
    pub radius: u16,
}

#[derive(Debug)]
struct PaintSweepGradient<'a> {
    scope: ReadScope<'a>,
    /// Offset to VarColorLine table, from beginning of PaintVarSweepGradient table.
    color_line_offset: u32, // Offset24,
    /// Center x coordinate.
    ///
    /// For variation, use varIndexBase + 0.
    center_x: i16,
    /// Center y coordinate.
    ///
    /// For variation, use varIndexBase + 1.
    center_y: i16,
    /// Start of the angular range of the gradient: add 1.0 and multiply by 180° to retrieve counter-clockwise degrees.
    ///
    /// For variation, use varIndexBase + 2.
    start_angle: F2Dot14,
    /// End of the angular range of the gradient: add 1.0 and multiply by 180° to retrieve counter-clockwise degrees.
    ///
    /// For variation, use varIndexBase + 3.
    end_angle: F2Dot14,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
pub struct ConicGradient<'a> {
    pub color_line: ColorLine<'a>,
    pub center: (i16, i16),
    pub start_angle: f32,
    pub end_angle: f32,
}

#[derive(Debug)]
struct PaintGlyph<'a> {
    scope: ReadScope<'a>,
    /// Offset to a Paint table, from beginning of PaintGlyph table.
    paint_offset: u32, // Offset24,
    /// Glyph ID for the source outline.
    glyph_id: u16,
}

#[derive(Debug)]
struct PaintColrGlyph {
    /// Glyph ID for a BaseGlyphList base glyph.
    glyph_id: u16,
}

#[derive(Debug)]
struct PaintTransform<'a> {
    scope: ReadScope<'a>,
    /// Offset to a Paint table, from beginning of PaintGlyph table.
    paint_offset: u32, // Offset24,
    /// Offset to an Affine2x3 table, from beginning of PaintTransform table.
    transform: Affine2x3,
}

#[derive(Debug)]
struct Affine2x3 {
    /// x-component of transformed x-basis vector.
    ///
    /// For variation, use varIndexBase + 0.
    xx: Fixed,
    /// y-component of transformed x-basis vector.
    ///
    /// For variation, use varIndexBase + 1.
    yx: Fixed,
    /// x-component of transformed y-basis vector.
    ///
    /// For variation, use varIndexBase + 2.
    xy: Fixed,
    /// y-component of transformed y-basis vector.
    ///
    /// For variation, use varIndexBase + 3.
    yy: Fixed,
    /// Translation in x direction.
    ///
    /// For variation, use varIndexBase + 4.
    dx: Fixed,
    /// Translation in y direction.
    ///
    /// For variation, use varIndexBase + 5.
    dy: Fixed,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
struct PaintTranslate<'a> {
    scope: ReadScope<'a>,
    /// Offset to a Paint subtable, from beginning of PaintVarTranslate table.
    paint_offset: u32, // Offset24,
    /// Translation in x direction.
    ///
    /// For variation, use varIndexBase + 0.
    dx: i16,
    /// Translation in y direction.
    ///
    /// For variation, use varIndexBase + 1.
    dy: i16,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
struct PaintScale<'a> {
    scope: ReadScope<'a>,
    /// Offset to a Paint subtable, from beginning of PaintVarScale table.
    paint_offset: u32, // Offset24,
    /// Scale factor in (x, y) directions.
    ///
    /// For variation, use varIndexBase + 0 for x, varIndexBase + 1 for y.
    scale: (F2Dot14, F2Dot14),
    /// Coordinates for the center of scaling (x, y).
    ///
    /// For variation, use varIndexBase + 2 for x, varIndexBase + 3 for y.
    center: Option<(i16, i16)>,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
struct PaintRotate<'a> {
    scope: ReadScope<'a>,
    /// Offset to a Paint subtable, from beginning of PaintVarRotate table.
    paint_offset: u32, // Offset24,
    /// Rotation angle, 180° in counter-clockwise degrees per 1.0 of value.
    ///
    /// For variation, use varIndexBase + 0.
    angle: F2Dot14,
    /// Coordinates for the center of rotation (x, y).
    ///
    /// For variation, use varIndexBase + 1 for x and varIndexBase + 2 for y.
    center: Option<(i16, i16)>,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
struct PaintSkew<'a> {
    scope: ReadScope<'a>,
    /// Offset to a Paint subtable, from beginning of PaintVarSkew table.
    paint_offset: u32, // Offset24,
    /// Angle of skew (x, y)
    ///
    /// 180° in counter-clockwise degrees per 1.0 of value.
    ///
    /// For variation, use varIndexBase + 0 for x-axis and varIndexBase + 1 for y-axis.
    skew_angle: (F2Dot14, F2Dot14),
    /// Coordinates for the center of rotation (x, y).
    ///
    /// For variation, use varIndexBase + 2 for x and varIndexBase + 3 for y.
    center: Option<(i16, i16)>,
    /// Base index into DeltaSetIndexMap.
    var_index_base: Option<u32>,
}

#[derive(Debug)]
struct PaintComposite<'a> {
    scope: ReadScope<'a>,
    /// Offset to a source Paint table, from beginning of PaintComposite table.
    source_paint_offset: u32, // Offset24,
    /// A CompositeMode enumeration value.
    composite_mode: CompositeMode,
    /// Offset to a backdrop Paint table, from beginning of PaintComposite table.
    backdrop_paint_offset: u32, // Offset24,
}

/// Mode to use for compositing paint format.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CompositeMode {
    // Porter-Duff modes
    /// Clear
    Clear,
    /// Source (“Copy” in Composition & Blending Level 1)
    Src,
    /// Destination
    Dest,
    /// Source Over
    SrcOver,
    /// Destination Over
    DestOver,
    /// Source In
    SrcIn,
    /// Destination In
    DestIn,
    /// Source Out
    SrcOut,
    /// Destination Out
    DestOut,
    /// Source Atop
    SrcAtop,
    /// Destination Atop
    DestAtop,
    /// XOR
    Xor,
    /// Plus (“Lighter” in Composition & Blending Level 1)
    Plus,
    // Separable color blend modes:
    /// screen
    Screen,
    /// overlay
    Overlay,
    /// darken
    Darken,
    /// lighten
    Lighten,
    /// color-dodge
    ColorDodge,
    /// color-burn
    ColorBurn,
    /// hard-light
    HardLight,
    /// soft-light
    SoftLight,
    /// difference
    Difference,
    /// exclusion
    Exclusion,
    /// multiply
    Multiply,
    // Non-separable color blend modes:
    /// hue
    HslHue,
    /// saturation
    HslSaturation,
    /// color
    HslColor,
    /// luminosity
    HslLuminosity,
}

impl TryFrom<u8> for CompositeMode {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(CompositeMode::Clear),
            1 => Ok(CompositeMode::Src),
            2 => Ok(CompositeMode::Dest),
            3 => Ok(CompositeMode::SrcOver),
            4 => Ok(CompositeMode::DestOver),
            5 => Ok(CompositeMode::SrcIn),
            6 => Ok(CompositeMode::DestIn),
            7 => Ok(CompositeMode::SrcOut),
            8 => Ok(CompositeMode::DestOut),
            9 => Ok(CompositeMode::SrcAtop),
            10 => Ok(CompositeMode::DestAtop),
            11 => Ok(CompositeMode::Xor),
            12 => Ok(CompositeMode::Plus),
            13 => Ok(CompositeMode::Screen),
            14 => Ok(CompositeMode::Overlay),
            15 => Ok(CompositeMode::Darken),
            16 => Ok(CompositeMode::Lighten),
            17 => Ok(CompositeMode::ColorDodge),
            18 => Ok(CompositeMode::ColorBurn),
            19 => Ok(CompositeMode::HardLight),
            20 => Ok(CompositeMode::SoftLight),
            21 => Ok(CompositeMode::Difference),
            22 => Ok(CompositeMode::Exclusion),
            23 => Ok(CompositeMode::Multiply),
            24 => Ok(CompositeMode::HslHue),
            25 => Ok(CompositeMode::HslSaturation),
            26 => Ok(CompositeMode::HslColor),
            27 => Ok(CompositeMode::HslLuminosity),
            _ => Err(ParseError::BadValue),
        }
    }
}

impl ReadBinary for Paint<'_> {
    type HostType<'a> = Paint<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        // Peek the format to determine paint type to read
        let format = ctxt.scope().ctxt().read_u8()?;
        match format {
            1 => Ok(Paint::ColrLayers(ctxt.read::<PaintColrLayers>()?)),
            2 | 3 => Ok(Paint::Solid(ctxt.read::<PaintSolid>()?)),
            4 | 5 => Ok(Paint::LinearGradient(ctxt.read::<PaintLinearGradient>()?)),
            6 | 7 => Ok(Paint::RadialGradient(ctxt.read::<PaintRadialGradient>()?)),
            8 | 9 => Ok(Paint::SweepGradient(ctxt.read::<PaintSweepGradient>()?)),
            10 => Ok(Paint::Glyph(ctxt.read::<PaintGlyph<'_>>()?)),
            11 => Ok(Paint::ColrGlyph(ctxt.read::<PaintColrGlyph>()?)),
            12 | 13 => Ok(Paint::Transform(ctxt.read::<PaintTransform<'_>>()?)),
            14 | 15 => Ok(Paint::Translate(ctxt.read::<PaintTranslate<'_>>()?)),
            16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 => {
                Ok(Paint::Scale(ctxt.read::<PaintScale<'_>>()?))
            }
            24 | 25 | 26 | 27 => Ok(Paint::Rotate(ctxt.read::<PaintRotate<'_>>()?)),
            28 | 29 | 30 | 31 => Ok(Paint::Skew(ctxt.read::<PaintSkew<'_>>()?)),
            32 => Ok(Paint::Composite(ctxt.read::<PaintComposite<'_>>()?)),
            _ => Err(ParseError::BadValue),
        }
    }
}

impl ReadBinary for PaintColrLayers {
    type HostType<'a> = PaintColrLayers;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let format = ctxt.read_u8()?;
        ctxt.check(format == 1)?;
        let num_layers = ctxt.read_u8()?;
        let first_layer_index = ctxt.read_u32be()?;

        Ok(PaintColrLayers {
            num_layers,
            first_layer_index,
        })
    }
}

impl PaintSolid {
    fn color(&self, palette: Palette<'_, '_>) -> Option<Color> {
        // TODO: A palette entry index value of 0xFFFF is a special case
        // indicating that the text foreground color (defined by the application) should be used,
        // and must not be treated as an actual index into the CPAL ColorRecord array.

        // The alpha value in the COLR structure is multiplied into the alpha value given in the
        // CPAL color entry. If the palette entry index is 0xFFFF, the alpha value in the COLR
        // structure is multiplied into the alpha value of the text foreground color.
        let color = palette.color(self.palette_index)?;
        Some(Color::new_with_alpha(color, self.alpha))
    }
}

impl ReadBinary for PaintSolid {
    type HostType<'a> = PaintSolid;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let format = ctxt.read_u8()?;
        let palette_index = ctxt.read_u16be()?;
        let alpha = ctxt.read::<F2Dot14>()?;
        let var_index_base = match format {
            2 => None,
            3 => ctxt.read_u32be().map(Some)?,
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintSolid {
            palette_index,
            alpha,
            var_index_base,
        })
    }
}

macro_rules! gradient {
    ($t:ty) => {
        impl Gradient for $t {
            fn scope(&self) -> ReadScope<'_> {
                self.scope
            }

            fn color_line_offset(&self) -> u32 {
                self.color_line_offset
            }
        }
    };
}

gradient!(PaintLinearGradient<'_>);
gradient!(PaintRadialGradient<'_>);
gradient!(PaintSweepGradient<'_>);

impl ReadBinary for PaintLinearGradient<'_> {
    type HostType<'a> = PaintLinearGradient<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let color_line_offset = ctxt.read::<U24Be>()?;
        let x0 = ctxt.read_i16be()?;
        let y0 = ctxt.read_i16be()?;
        let x1 = ctxt.read_i16be()?;
        let y1 = ctxt.read_i16be()?;
        let x2 = ctxt.read_i16be()?;
        let y2 = ctxt.read_i16be()?;
        let var_index_base = match format {
            4 => None,
            5 => ctxt.read_u32be().map(Some)?,
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintLinearGradient {
            scope,
            color_line_offset,
            x0,
            y0,
            x1,
            y1,
            x2,
            y2,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintRadialGradient<'_> {
    type HostType<'a> = PaintRadialGradient<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let color_line_offset = ctxt.read::<U24Be>()?;
        let x0 = ctxt.read_i16be()?;
        let y0 = ctxt.read_i16be()?;
        let radius0 = ctxt.read_u16be()?;
        let x1 = ctxt.read_i16be()?;
        let y1 = ctxt.read_i16be()?;
        let radius1 = ctxt.read_u16be()?;
        let var_index_base = match format {
            6 => None,
            7 => ctxt.read_u32be().map(Some)?,
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintRadialGradient {
            scope,
            color_line_offset,
            x0,
            y0,
            radius0,
            x1,
            y1,
            radius1,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintSweepGradient<'_> {
    type HostType<'a> = PaintSweepGradient<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let color_line_offset = ctxt.read::<U24Be>()?;
        let center_x = ctxt.read_i16be()?;
        let center_y = ctxt.read_i16be()?;
        let start_angle = ctxt.read::<F2Dot14>()?;
        let end_angle = ctxt.read::<F2Dot14>()?;
        let var_index_base = match format {
            8 => None,
            9 => ctxt.read_u32be().map(Some)?,
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintSweepGradient {
            scope,
            color_line_offset,
            center_x,
            center_y,
            start_angle,
            end_angle,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintGlyph<'_> {
    type HostType<'a> = PaintGlyph<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        ctxt.check(format == 10)?;
        let paint_offset = ctxt.read::<U24Be>()?;
        let glyph_id = ctxt.read_u16be()?;

        Ok(PaintGlyph {
            scope,
            paint_offset,
            glyph_id,
        })
    }
}

impl ReadBinary for PaintColrGlyph {
    type HostType<'a> = PaintColrGlyph;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let format = ctxt.read_u8()?;
        ctxt.check(format == 11)?;
        let glyph_id = ctxt.read_u16be()?;

        Ok(PaintColrGlyph { glyph_id })
    }
}

impl ReadBinary for PaintTransform<'_> {
    type HostType<'a> = PaintTransform<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let paint_offset = ctxt.read::<U24Be>()?;
        let transform_offset = ctxt.read::<U24Be>().map(SafeFrom::safe_from)?;
        let variable = match format {
            12 => false,
            13 => true,
            _ => return Err(ParseError::BadValue),
        };
        let transform = scope
            .offset(transform_offset)
            .ctxt()
            .read_dep::<Affine2x3>(variable)?;

        Ok(PaintTransform {
            scope,
            paint_offset,
            transform,
        })
    }
}

impl ReadBinaryDep for Affine2x3 {
    type HostType<'a> = Affine2x3;
    type Args<'a> = bool;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        variable: bool,
    ) -> Result<Self::HostType<'a>, ParseError> {
        let xx = ctxt.read::<Fixed>()?;
        let yx = ctxt.read::<Fixed>()?;
        let xy = ctxt.read::<Fixed>()?;
        let yy = ctxt.read::<Fixed>()?;
        let dx = ctxt.read::<Fixed>()?;
        let dy = ctxt.read::<Fixed>()?;
        let var_index_base = variable.then(|| ctxt.read_u32be()).transpose()?;

        Ok(Affine2x3 {
            xx,
            yx,
            xy,
            yy,
            dx,
            dy,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintTranslate<'_> {
    type HostType<'a> = PaintTranslate<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let paint_offset = ctxt.read::<U24Be>()?;
        let dx = ctxt.read_i16be()?;
        let dy = ctxt.read_i16be()?;
        let var_index_base = match format {
            14 => None,
            15 => ctxt.read_u32be().map(Some)?,
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintTranslate {
            scope,
            paint_offset,
            dx,
            dy,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintScale<'_> {
    type HostType<'a> = PaintScale<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let paint_offset = ctxt.read::<U24Be>()?;
        let (scale, center, var_index_base) = match format {
            // PaintScale and PaintVarScale
            16 | 17 => {
                let scale_x = ctxt.read::<F2Dot14>()?;
                let scale_y = ctxt.read::<F2Dot14>()?;
                let var_index_base = (format == 17).then(|| ctxt.read_u32be()).transpose()?;
                ((scale_x, scale_y), None, var_index_base)
            }
            // PaintScaleAroundCenter and PaintVarScaleAroundCenter
            18 | 19 => {
                let scale_x = ctxt.read::<F2Dot14>()?;
                let scale_y = ctxt.read::<F2Dot14>()?;
                let center_x = ctxt.read_i16be()?;
                let center_y = ctxt.read_i16be()?;
                let var_index_base = (format == 19).then(|| ctxt.read_u32be()).transpose()?;
                (
                    (scale_x, scale_y),
                    Some((center_x, center_y)),
                    var_index_base,
                )
            }
            // PaintScaleUniform and PaintVarScaleUniform
            20 | 21 => {
                let scale = ctxt.read::<F2Dot14>()?;
                let var_index_base = (format == 21).then(|| ctxt.read_u32be()).transpose()?;
                ((scale, scale), None, var_index_base)
            }
            // PaintScaleUniformAroundCenter and PaintVarScaleUniformAroundCenter
            22 | 23 => {
                let scale = ctxt.read::<F2Dot14>()?;
                let center_x = ctxt.read_i16be()?;
                let center_y = ctxt.read_i16be()?;
                let var_index_base = (format == 23).then(|| ctxt.read_u32be()).transpose()?;
                ((scale, scale), Some((center_x, center_y)), var_index_base)
            }
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintScale {
            scope,
            paint_offset,
            scale,
            center,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintRotate<'_> {
    type HostType<'a> = PaintRotate<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let paint_offset = ctxt.read::<U24Be>()?;
        let angle = ctxt.read::<F2Dot14>()?;
        let (center, var_index_base) = match format {
            // PaintRotate
            24 => (None, None),
            // PaintVarRotate
            25 => {
                let var_index_base = ctxt.read_u32be()?;
                (None, Some(var_index_base))
            }
            // PaintRotateAroundCenter and PaintVarRotateAroundCenter
            26 | 27 => {
                let center_x = ctxt.read_i16be()?;
                let center_y = ctxt.read_i16be()?;
                let var_index_base = (format == 27).then(|| ctxt.read_u32be()).transpose()?;
                (Some((center_x, center_y)), var_index_base)
            }
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintRotate {
            scope,
            paint_offset,
            angle,
            center,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintSkew<'_> {
    type HostType<'a> = PaintSkew<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        let paint_offset = ctxt.read::<U24Be>()?;
        let x_skew_angle = ctxt.read::<F2Dot14>()?;
        let y_skew_angle = ctxt.read::<F2Dot14>()?;
        let (center, var_index_base) = match format {
            // PaintSkew
            28 => (None, None),
            // PaintVarSkew
            29 => {
                let var_index_base = ctxt.read_u32be()?;
                (None, Some(var_index_base))
            }
            // PaintSkewAroundCenter and PaintVarSkewAroundCenter
            30 | 31 => {
                let center_x = ctxt.read_i16be()?;
                let center_y = ctxt.read_i16be()?;
                let var_index_base = (format == 31).then(|| ctxt.read_u32be()).transpose()?;
                (Some((center_x, center_y)), var_index_base)
            }
            _ => return Err(ParseError::BadValue),
        };

        Ok(PaintSkew {
            scope,
            paint_offset,
            skew_angle: (x_skew_angle, y_skew_angle),
            center,
            var_index_base,
        })
    }
}

impl ReadBinary for PaintComposite<'_> {
    type HostType<'a> = PaintComposite<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let format = ctxt.read_u8()?;
        ctxt.check(format == 32)?;
        let source_paint_offset = ctxt.read::<U24Be>()?;
        let composite_mode = ctxt.read::<CompositeMode>()?;
        let backdrop_paint_offset = ctxt.read::<U24Be>()?;

        Ok(PaintComposite {
            scope,
            source_paint_offset,
            composite_mode,
            backdrop_paint_offset,
        })
    }
}

impl ReadBinary for CompositeMode {
    type HostType<'a> = CompositeMode;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        ctxt.read_u8()
            .map_err(ParseError::from)
            .and_then(TryFrom::try_from)
    }
}

/// An RGBA color
#[derive(Copy, Clone, Debug)]
pub struct Color(pub f32, pub f32, pub f32, pub f32);

impl Color {
    fn new_with_alpha(color: ColorRecord, alpha: F2Dot14) -> Self {
        // "The alpha indicated in this record is multiplied with the alpha component of the CPAL
        // entry (converted to float—divide by 255)."
        //
        // "Values for alpha outside the range [0., 1.] (inclusive) are reserved; values outside
        // this range must be clamped."
        let alpha = ((f32::from(color.alpha) / 255.0) * f32::from(alpha)).clamp(0.0, 1.0);
        Color(
            f32::from(color.red) / 255.0,
            f32::from(color.green) / 255.0,
            f32::from(color.blue) / 255.0,
            alpha,
        )
    }
}

impl fmt::Debug for ColrV1<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ColrV1")
            .field("base_glyph_records", &self.base_glyph_records)
            .field("layer_records", &self.layer_records)
            .field("base_glyph_list", &self.base_glyph_list)
            .field("layer_list", &self.layer_list)
            .field("clip_list", &self.clip_list)
            .field("var_index_map", &self.var_index_map)
            .field("item_variation_store", &"ItemVariationStore")
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        tables::{FontTableProvider, OpenTypeFont},
        tag,
        tests::read_fixture,
    };

    #[test]
    fn test_read_colr_v1_variable() {
        let buffer = read_fixture(
            "tests/fonts/colr/SixtyfourConvergence-Regular-VariableFont_BLED,SCAN,XELA,YELA.ttf",
        );
        let otf = ReadScope::new(&buffer).read::<OpenTypeFont<'_>>().unwrap();
        let table_provider = otf.table_provider(0).expect("error reading font file");

        let colr_data = table_provider
            .read_table_data(tag::COLR)
            .expect("unable to read COLR data");
        let colr = ReadScope::new(&colr_data)
            .read::<ColrTable<'_>>()
            .expect("unable to parse COLR table");

        let ColrTable::V1(v1) = colr else {
            panic!("expected COLRv1 table");
        };

        assert!(v1.base_glyph_records.is_none());
        assert!(v1.layer_records.is_none());
        assert!(v1.layer_list.is_none());
        assert!(v1.clip_list.is_none());
        assert_eq!(v1.var_index_map.as_ref().map(|map| map.len()), Some(8));
        assert_eq!(
            v1.base_glyph_list.as_ref().map(|list| list.records.len()),
            Some(481)
        );
    }
}
