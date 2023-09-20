//! Variable font instancing.

#![deny(missing_docs)]

use std::convert::TryInto;
use std::fmt;

use pathfinder_geometry::rect::RectI;
use pathfinder_geometry::vector::vec2i;

use crate::binary::read::{ReadArrayCow, ReadScope};
use crate::error::{ParseError, ReadWriteError, WriteError};
use crate::post::PostTable;
use crate::subset::FontBuilder;
use crate::tables::glyf::{BoundingBox, GlyfRecord, GlyfTable};
use crate::tables::loca::LocaTable;
use crate::tables::os2::Os2;
use crate::tables::variable_fonts::gvar::GvarTable;
use crate::tables::variable_fonts::hvar::HvarTable;
use crate::tables::variable_fonts::mvar::MvarTable;
use crate::tables::variable_fonts::OwnedTuple;
use crate::tables::{FontTableProvider, HeadTable, HheaTable, HmtxTable, MaxpTable};
use crate::tag;

/// Error type returned from instancing a variable font.
#[derive(Debug)]
pub enum VariationError {
    /// An error occurred reading or parsing data.
    Parse(ParseError),
    /// An error occurred serializing data.
    Write(WriteError),
    /// The font is not a variable font.
    NotVariableFont,
    /// The font is a variable font but support for its format is not implemented.
    ///
    /// Encountered for variable CFF fonts.
    NotImplemented,
}

/// Create a static instance of a variable font according to the variation instance `instance`.
pub fn instance(
    provider: &impl FontTableProvider,
    instance: &OwnedTuple,
) -> Result<Vec<u8>, VariationError> {
    is_supported_variable_font(provider)?;

    // We need to create a font with at least these tables:
    //
    // cmap 	Character to glyph mapping
    // head 	Font header
    // hhea 	Horizontal header
    // hmtx 	Horizontal metrics
    // maxp 	Maximum profile
    // name 	Naming table
    // OS/2 	OS/2 and Windows specific metrics
    // post 	PostScript information
    //
    // https://learn.microsoft.com/en-us/typography/opentype/spec/otff#required-tables

    let mut head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
    let maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
    let loca_data = provider.read_table_data(tag::LOCA)?;
    let loca = ReadScope::new(&loca_data)
        .read_dep::<LocaTable<'_>>((usize::from(maxp.num_glyphs), head.index_to_loc_format))?;
    let glyf_data = provider.read_table_data(tag::GLYF)?;
    let mut glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
    let mut hhea = ReadScope::new(&provider.read_table_data(tag::HHEA)?).read::<HheaTable>()?;
    let hmtx_data = provider.read_table_data(tag::HMTX)?;
    let hmtx = ReadScope::new(&hmtx_data).read_dep::<HmtxTable<'_>>((
        usize::from(maxp.num_glyphs),
        usize::from(hhea.num_h_metrics),
    ))?;
    let vhea_data = provider.table_data(tag::VHEA)?;
    let vhea = vhea_data
        .as_ref()
        .map(|vhea_data| ReadScope::new(&vhea_data).read::<HheaTable>())
        .transpose()?;
    let vmtx_data = provider.table_data(tag::VMTX)?;
    let vmtx = vhea
        .and_then(|vhea| {
            vmtx_data.as_ref().map(|vmtx_data| {
                ReadScope::new(&vmtx_data).read_dep::<HmtxTable<'_>>((
                    usize::from(maxp.num_glyphs),
                    usize::from(vhea.num_h_metrics),
                ))
            })
        })
        .transpose()?;

    let os2_data = provider.read_table_data(tag::OS_2)?;
    let mut os2 = ReadScope::new(&os2_data).read_dep::<Os2>(os2_data.len())?; // FIXME: Is this optional?
    let post_data = provider.read_table_data(tag::POST)?;
    let mut post = ReadScope::new(&post_data).read::<PostTable<'_>>()?;
    let hvar_data = provider.table_data(tag::HVAR)?;
    let hvar = hvar_data
        .as_ref()
        .map(|hvar_data| ReadScope::new(hvar_data).read::<HvarTable<'_>>())
        .transpose()?;
    let gvar_data = provider.table_data(tag::GVAR)?;
    let gvar = gvar_data
        .as_ref()
        .map(|gvar_data| ReadScope::new(gvar_data).read::<GvarTable<'_>>())
        .transpose()?;
    let mvar_data = provider.table_data(tag::MVAR)?;
    let mvar = mvar_data
        .as_ref()
        .map(|mvar_data| ReadScope::new(mvar_data).read::<MvarTable<'_>>())
        .transpose()?;

    // Apply deltas to glyphs to build a new glyf table
    // TODO: Defer this until later if hvar is present
    // Perhaps there can be a new variant on GlyphRecord to indicate variable?
    if let Some(gvar) = &gvar {
        glyf = apply_gvar(
            glyf,
            &gvar,
            &hmtx,
            vmtx.as_ref(),
            Some(&os2),
            &hhea,
            &instance,
        )?;

        // Update head
        let mut bbox = RectI::default();
        glyf.records().iter().for_each(|glyph| match glyph {
            GlyfRecord::Empty => {}
            GlyfRecord::Present { .. } => {}
            GlyfRecord::Parsed(glyph) => bbox = union_rect(bbox, glyph.bounding_box.into()),
        });
        head.x_min = bbox.min_x().try_into().ok().unwrap_or(i16::MIN);
        head.y_min = bbox.min_y().try_into().ok().unwrap_or(i16::MIN);
        head.x_max = bbox.max_x().try_into().ok().unwrap_or(i16::MAX);
        head.y_max = bbox.max_y().try_into().ok().unwrap_or(i16::MAX);
    }

    // Build new hmtx table
    let hmtx = create_hmtx_table(&hmtx, hvar.as_ref(), &instance, maxp.num_glyphs)?;

    // Update hhea
    hhea.num_h_metrics = maxp.num_glyphs; // there's now metrics for each glyph
    hhea.advance_width_max = hmtx
        .h_metrics
        .iter()
        .map(|m| m.advance_width)
        .max()
        .unwrap_or(0);

    // Apply deltas to OS/2, hhea, vhea, post
    if let Some(mvar) = &mvar {
        process_mvar(mvar, &instance, &mut os2, &mut hhea, &mut None, &mut post);
    }

    // Get the remaining tables
    let cvt = provider.table_data(tag::CVT)?; // TODO: apply CVAR
    let cmap = provider.read_table_data(tag::CMAP)?;
    let fpgm = provider.table_data(tag::FPGM)?;
    let name = provider.table_data(tag::NAME)?;
    let prep = provider.table_data(tag::PREP)?;

    // Build the new font

    let mut builder = FontBuilder::new(0x00010000_u32);
    // if let Some(cmap) = cmap {
    builder.add_table::<_, ReadScope<'_>>(tag::CMAP, ReadScope::new(&cmap), ())?;
    // }
    if let Some(cvt) = cvt {
        builder.add_table::<_, ReadScope<'_>>(tag::CVT, ReadScope::new(&cvt), ())?;
    }
    if let Some(fpgm) = fpgm {
        builder.add_table::<_, ReadScope<'_>>(tag::FPGM, ReadScope::new(&fpgm), ())?;
    }
    builder.add_table::<_, HheaTable>(tag::HHEA, &hhea, ())?;
    builder.add_table::<_, HmtxTable<'_>>(tag::HMTX, &hmtx, ())?;
    builder.add_table::<_, MaxpTable>(tag::MAXP, &maxp, ())?;
    if let Some(name) = name {
        builder.add_table::<_, ReadScope<'_>>(tag::NAME, ReadScope::new(&name), ())?;
    }
    builder.add_table::<_, Os2>(tag::OS_2, &os2, ())?;
    builder.add_table::<_, PostTable<'_>>(tag::POST, &post, ())?;
    if let Some(prep) = prep {
        builder.add_table::<_, ReadScope<'_>>(tag::PREP, ReadScope::new(&prep), ())?;
    }
    // TODO: Some fields in head might need updating
    // TODO: Update the name of the font using STAT
    let mut builder = builder.add_head_table(&head)?;
    builder.add_glyf_table(glyf)?;
    builder.data().map_err(VariationError::from)
}

fn process_mvar(
    mvar: &MvarTable<'_>,
    instance: &OwnedTuple,
    os2: &mut Os2,
    hhea: &mut HheaTable,
    vhea: &mut Option<HheaTable>,
    post: &mut PostTable<'_>,
) {
    for value_record in mvar.value_records() {
        let Some(delta) = mvar.lookup(value_record.value_tag, instance) else {
            continue;
        };

        match value_record.value_tag {
            // horizontal ascender 	OS/2.sTypoAscender
            tag::HASC => {
                if let Some(v0) = &mut os2.version0 {
                    v0.s_typo_ascender = add_delta_i16(v0.s_typo_ascender, delta);
                }
            }
            // horizontal descender 	OS/2.sTypoDescender
            tag::HDSC => {
                if let Some(v0) = &mut os2.version0 {
                    v0.s_typo_descender = add_delta_i16(v0.s_typo_descender, delta);
                }
            }
            // horizontal line gap 	OS/2.sTypoLineGap
            tag::HLGP => {
                if let Some(v0) = &mut os2.version0 {
                    v0.s_typo_line_gap = add_delta_i16(v0.s_typo_line_gap, delta);
                }
            }
            // horizontal clipping ascent 	OS/2.usWinAscent
            tag::HCLA => {
                if let Some(v0) = &mut os2.version0 {
                    v0.us_win_ascent = add_delta_u16(v0.us_win_ascent, delta);
                }
            }
            // horizontal clipping descent 	OS/2.usWinDescent
            tag::HCLD => {
                if let Some(v0) = &mut os2.version0 {
                    v0.us_win_descent = add_delta_u16(v0.us_win_descent, delta);
                }
            }
            // vertical ascender 	vhea.ascent
            tag::VASC => {
                if let Some(vhea) = vhea {
                    vhea.ascender = add_delta_i16(vhea.ascender, delta);
                }
            }
            // vertical descender 	vhea.descent
            tag::VDSC => {
                if let Some(vhea) = vhea {
                    vhea.descender = add_delta_i16(vhea.descender, delta);
                }
            }
            // vertical line gap 	vhea.lineGap
            tag::VLGP => {
                if let Some(vhea) = vhea {
                    vhea.line_gap = add_delta_i16(vhea.line_gap, delta);
                }
            }
            // horizontal caret rise 	hhea.caretSlopeRise
            tag::HCRS => {
                hhea.caret_slope_rise = add_delta_i16(hhea.caret_slope_rise, delta);
            }
            // horizontal caret run 	hhea.caretSlopeRun
            tag::HCRN => {
                hhea.caret_slope_run = add_delta_i16(hhea.caret_slope_run, delta);
            }
            // horizontal caret offset 	hhea.caretOffset
            tag::HCOF => {
                hhea.caret_offset = add_delta_i16(hhea.caret_offset, delta);
            }
            // vertical caret rise 	vhea.caretSlopeRise
            tag::VCRS => {
                if let Some(vhea) = vhea {
                    vhea.caret_slope_rise = add_delta_i16(vhea.caret_slope_rise, delta);
                }
            }
            // vertical caret run 	vhea.caretSlopeRun
            tag::VCRN => {
                if let Some(vhea) = vhea {
                    vhea.caret_slope_run = add_delta_i16(vhea.caret_slope_run, delta);
                }
            }
            // vertical caret offset 	vhea.caretOffset
            tag::VCOF => {
                if let Some(vhea) = vhea {
                    vhea.caret_offset = add_delta_i16(vhea.caret_offset, delta);
                }
            }
            // x height 	OS/2.sxHeight
            tag::XHGT => {
                if let Some(version) = &mut os2.version2to4 {
                    version.sx_height = add_delta_i16(version.sx_height, delta);
                }
            }
            // cap height 	OS/2.sCapHeight
            tag::CPHT => {
                if let Some(version) = &mut os2.version2to4 {
                    version.s_cap_height = add_delta_i16(version.s_cap_height, delta);
                }
            }
            // subscript em x size 	OS/2.ySubscriptXSize
            tag::SBXS => {
                os2.y_subscript_x_size = add_delta_i16(os2.y_subscript_x_size, delta);
            }
            // subscript em y size 	OS/2.ySubscriptYSize
            tag::SBYS => {
                os2.y_subscript_y_size = add_delta_i16(os2.y_subscript_y_size, delta);
            }
            // subscript em x offset 	OS/2.ySubscriptXOffset
            tag::SBXO => {
                os2.y_subscript_x_offset = add_delta_i16(os2.y_subscript_x_offset, delta);
            }
            // subscript em y offset 	OS/2.ySubscriptYOffset
            tag::SBYO => {
                os2.y_subscript_y_offset = add_delta_i16(os2.y_subscript_y_offset, delta);
            }
            // superscript em x size 	OS/2.ySuperscriptXSize
            tag::SPXS => {
                os2.y_superscript_x_size = add_delta_i16(os2.y_superscript_x_size, delta);
            }
            // superscript em y size 	OS/2.ySuperscriptYSize
            tag::SPYS => {
                os2.y_superscript_y_size = add_delta_i16(os2.y_superscript_y_size, delta);
            }
            // superscript em x offset 	OS/2.ySuperscriptXOffset
            tag::SPXO => {
                os2.y_superscript_x_offset = add_delta_i16(os2.y_superscript_x_offset, delta);
            }
            // superscript em y offset 	OS/2.ySuperscriptYOffset
            tag::SPYO => {
                os2.y_superscript_y_offset = add_delta_i16(os2.y_superscript_y_offset, delta);
            }
            // strikeout size 	OS/2.yStrikeoutSize
            tag::STRS => {
                os2.y_strikeout_size = add_delta_i16(os2.y_strikeout_size, delta);
            }
            // strikeout offset 	OS/2.yStrikeoutPosition
            tag::STRO => {
                os2.y_strikeout_position = add_delta_i16(os2.y_strikeout_position, delta);
            }
            // underline size 	post.underlineThickness
            tag::UNDS => {
                post.header.underline_thickness =
                    add_delta_i16(post.header.underline_thickness, delta);
            }
            // underline offset 	post.underlinePosition
            tag::UNDO => {
                post.header.underline_position =
                    add_delta_i16(post.header.underline_position, delta);
            }
            // gaspRange[0] 	gasp.gaspRange[0..9].rangeMaxPPEM
            // We know about these but ignore them since the gasp table doesn't make it into subset
            // fonts.
            tag::GSP0
            | tag::GSP1
            | tag::GSP2
            | tag::GSP3
            | tag::GSP4
            | tag::GSP5
            | tag::GSP6
            | tag::GSP7
            | tag::GSP8
            | tag::GSP9 => (),
            // Skip/ignore unknown value tags
            _ => (),
        }
    }
}

fn add_delta_i16(value: i16, delta: f32) -> i16 {
    (value as f32 + delta)
        .round()
        .clamp(i16::MIN as f32, i16::MAX as f32) as i16
}

fn add_delta_u16(value: u16, delta: f32) -> u16 {
    (value as f32 + delta).round().clamp(0., u16::MAX as f32) as u16
}

fn is_supported_variable_font(provider: &impl FontTableProvider) -> Result<(), VariationError> {
    // Two tables are required in all variable fonts:
    //
    // * A font variations ('fvar') table is required to describe the variations supported by the font.
    // * A style attributes (STAT) table is required and is used to establish relationships between different fonts belonging to a family and to provide some degree of compatibility with legacy applications by allowing platforms to project variation instances involving many axes into older font-family models that assume a limited set of axes.
    //
    // https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#vartables
    if provider.has_table(tag::FVAR) && provider.has_table(tag::STAT) {
        // Variable CFF fonts are currently not supported
        if provider.has_table(tag::CFF2) {
            Err(VariationError::NotImplemented)
        } else {
            Ok(())
        }
    } else {
        Err(VariationError::NotVariableFont)
    }
}

fn create_hmtx_table<'b>(
    hmtx: &HmtxTable<'_>,
    hvar: Option<&HvarTable<'_>>,
    // glyf: &GlyfTable<'_>,
    instance: &OwnedTuple,
    num_glyphs: u16,
) -> Result<HmtxTable<'b>, ReadWriteError> {
    let mut h_metrics = Vec::with_capacity(usize::from(num_glyphs));

    for glyph_id in 0..num_glyphs {
        let mut metric = hmtx.metric(glyph_id)?;

        // Now apply deltas
        match hvar {
            Some(hvar) => {
                let delta = hvar.advance_delta(instance, glyph_id)?;
                let new = (metric.advance_width as f32 + delta).round();
                metric.advance_width = new.clamp(0., u16::MAX as f32) as u16;

                if let Some(delta) = hvar.left_side_bearing_delta(instance, glyph_id)? {
                    metric.lsb = (metric.lsb as f32 + delta)
                        .round()
                        .clamp(i16::MIN as f32, i16::MAX as f32)
                        as i16;
                }
            }
            None => {
                // TODO: Calculate from glyph deltas
                eprintln!("no hvar");

                // Take note that, in a variable font with TrueType outlines, the left side bearing for each glyph must equal xMin, and bit 1 in the flags field of the 'head' table must be set.

                return Err(ParseError::NotImplemented.into());
            }
        }
        h_metrics.push(metric)
    }

    // TODO: Can we apply the optimisation if they're all the same at the end

    Ok(HmtxTable {
        h_metrics: ReadArrayCow::Owned(h_metrics),
        left_side_bearings: ReadArrayCow::Owned(vec![]),
    })
}

/// Applies glyph deltas from the `gvar` table to glyphs in the `glyf` table.
///
/// Takes ownership of the `glyf` table as placeholder values are swapped in during processing
/// (see not in body of function) and returning early would leave the `glyf` table in an incorrect
/// state. So we consume it and return the modified, valid result only on success.
fn apply_gvar<'a>(
    mut glyf: GlyfTable<'a>,
    gvar: &GvarTable<'a>,
    hmtx: &HmtxTable<'a>,
    vmtx: Option<&HmtxTable<'a>>,
    os2: Option<&Os2>,
    hhea: &HheaTable,
    instance: &OwnedTuple,
) -> Result<GlyfTable<'a>, ReadWriteError> {
    for (glyph_id, glyph_record) in glyf.records_mut().iter_mut().enumerate() {
        // NOTE(cast): Safe as num_glyphs is u16
        let glyph_id = glyph_id as u16;
        glyph_record.parse()?;
        match glyph_record {
            GlyfRecord::Empty => {}
            GlyfRecord::Parsed(glyph) => {
                glyph.apply_variations(glyph_id, instance, gvar, hmtx, vmtx, os2, hhea)?;
            }
            GlyfRecord::Present { .. } => unreachable!("glyph should be parsed"),
        }
    }

    // Do a pass to update the bounding boxes and phantom points of composite glyphs
    for glyph_id in 0..glyf.num_glyphs() {
        // We do a little take/replace dance here to work within Rust's unique (mut) access
        // constraints: we need to mutate the glyph but also pass an immutable reference to the
        // glyf table that holds it. To work around this we swap the glyph we're processing with
        // an empty glyph in the glyf table and then put it back afterwards. This works because
        // the glyf table is required for `apply_variations` to resolve child components in
        // composite glyphs to calculate the bounding box, and a composite glyph can't refer to
        // itself so should never encounter the empty replacement.
        let mut glyph_record = glyf.take(glyph_id).ok_or(ParseError::BadIndex)?; // should not happen
        match &mut glyph_record {
            GlyfRecord::Empty => {}
            GlyfRecord::Parsed(glyph) => {
                if glyph.is_composite() {
                    // Calculate the new bounding box for this composite glyph
                    let bbox = glyph.calculate_bounding_box(&glyf)?;
                    glyph.bounding_box = BoundingBox {
                        x_min: bbox.min_x() as i16, // FIXME: casts
                        x_max: bbox.max_x() as i16,
                        y_min: bbox.min_y() as i16,
                        y_max: bbox.max_y() as i16,
                    };

                    // Now that is assigned we can update the phantom points, the apply_variations
                    // call stashed the phantom point deltas in the phantom_points field
                    let phantom_points =
                        glyph.calculate_phantom_points(glyph_id, hmtx, vmtx, os2, hhea)?;
                    match glyph.phantom_points.as_deref_mut() {
                        Some(deltas) => deltas
                            .iter_mut()
                            .zip(phantom_points.iter().copied())
                            .for_each(|(point, phantom)| {
                                *point = phantom + *point;
                            }),
                        // There were no deltas for the phantom points so we can just move them in
                        None => glyph.phantom_points = Some(Box::new(phantom_points)),
                    }
                }
            }
            GlyfRecord::Present { .. } => unreachable!("glyph should be parsed"),
        }
        glyf.replace(glyph_id, glyph_record)?;
    }

    Ok(glyf)
}

fn union_rect(rect: RectI, other: RectI) -> RectI {
    RectI::from_points(
        rect.origin().min(other.origin()),
        rect.lower_right().max(other.lower_right()),
    )
}

impl From<BoundingBox> for RectI {
    fn from(bbox: BoundingBox) -> Self {
        RectI::from_points(
            vec2i(bbox.x_min.into(), bbox.y_min.into()),
            vec2i(bbox.x_max.into(), bbox.y_max.into()),
        )
    }
}

impl From<ParseError> for VariationError {
    fn from(err: ParseError) -> VariationError {
        VariationError::Parse(err)
    }
}

impl From<WriteError> for VariationError {
    fn from(err: WriteError) -> VariationError {
        VariationError::Write(err)
    }
}

impl From<ReadWriteError> for VariationError {
    fn from(err: ReadWriteError) -> VariationError {
        match err {
            ReadWriteError::Read(err) => VariationError::Parse(err),
            ReadWriteError::Write(err) => VariationError::Write(err),
        }
    }
}

impl fmt::Display for VariationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VariationError::Parse(err) => write!(f, "variation: parse error: {}", err),
            VariationError::Write(err) => write!(f, "variation: write error: {}", err),
            VariationError::NotVariableFont => write!(f, "variation: not a variable font"),
            VariationError::NotImplemented => {
                write!(f, "variation: unsupported variable font format")
            }
        }
    }
}

impl std::error::Error for VariationError {}
