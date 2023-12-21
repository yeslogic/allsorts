//! Variable font instancing.

#![deny(missing_docs)]

use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fmt::Write;
use std::str::FromStr;

use pathfinder_geometry::rect::RectI;
use pathfinder_geometry::vector::vec2i;

use crate::binary::read::{ReadArrayCow, ReadScope};
use crate::error::{ParseError, ReadWriteError, WriteError};
use crate::post::PostTable;
use crate::subset::FontBuilder;
use crate::tables::glyf::{BoundingBox, GlyfRecord, GlyfTable, Glyph};
use crate::tables::loca::LocaTable;
use crate::tables::os2::Os2;
use crate::tables::variable_fonts::avar::AvarTable;
use crate::tables::variable_fonts::cvar::CvarTable;
use crate::tables::variable_fonts::fvar::FvarTable;
use crate::tables::variable_fonts::gvar::GvarTable;
use crate::tables::variable_fonts::hvar::HvarTable;
use crate::tables::variable_fonts::mvar::MvarTable;
use crate::tables::variable_fonts::stat::{ElidableName, StatTable};
use crate::tables::variable_fonts::OwnedTuple;
use crate::tables::{
    owned, CvtTable, Fixed, FontTableProvider, HeadTable, HheaTable, HmtxTable, LongHorMetric,
    MaxpTable, NameTable,
};
use crate::tag;
use crate::tag::DisplayTag;

/// Error type returned from instancing a variable font.
#[derive(Debug)]
pub enum VariationError {
    /// An error occurred reading or parsing data.
    Parse(ParseError),
    /// An error occurred serializing data.
    Write(WriteError),
    /// The font is not a variable font.
    NotVariableFont,
    /// The font is a variable font but support for its format is not
    /// implemented.
    ///
    /// Encountered for variable CFF fonts.
    NotImplemented,
    /// The font did not contain a `name` table entry for the family name in a
    /// usable encoding.
    NameError,
}

/// Create a static instance of a variable font according to the variation
/// instance `instance`.
///
/// Currently only TrueType variable fonts with a `gvar` table are supported.
/// I.e. CFF variable fonts and fonts without a `gvar` table will return
/// [VariationError::NotImplemented].
pub fn instance(
    provider: &impl FontTableProvider,
    user_instance: &[Fixed],
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
        .map(|vhea_data| ReadScope::new(vhea_data).read::<HheaTable>())
        .transpose()?;
    let vmtx_data = provider.table_data(tag::VMTX)?;
    let vmtx = vhea
        .and_then(|vhea| {
            vmtx_data.as_ref().map(|vmtx_data| {
                ReadScope::new(vmtx_data).read_dep::<HmtxTable<'_>>((
                    usize::from(maxp.num_glyphs),
                    usize::from(vhea.num_h_metrics),
                ))
            })
        })
        .transpose()?;

    let os2_data = provider.read_table_data(tag::OS_2)?;
    let mut os2 = ReadScope::new(&os2_data).read_dep::<Os2>(os2_data.len())?;
    let post_data = provider.read_table_data(tag::POST)?;
    let mut post = ReadScope::new(&post_data).read::<PostTable<'_>>()?;
    let fvar_data = provider.read_table_data(tag::FVAR)?;
    let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>()?;
    let avar_data = provider.table_data(tag::AVAR)?;
    let avar = avar_data
        .as_ref()
        .map(|avar_data| ReadScope::new(avar_data).read::<AvarTable<'_>>())
        .transpose()?;
    let cvt_data = provider.table_data(tag::CVAR)?;
    let mut cvt = cvt_data
        .as_ref()
        .map(|cvt_data| ReadScope::new(cvt_data).read_dep::<CvtTable<'_>>(cvt_data.len() as u32))
        .transpose()?;
    let cvar_data = provider.table_data(tag::CVAR)?;
    let cvar = cvt
        .as_ref()
        .and_then(|cvt| {
            cvar_data.as_ref().map(|cvar_data| {
                ReadScope::new(cvar_data)
                    .read_dep::<CvarTable<'_>>((fvar.axis_count(), cvt.values.len() as u32))
            })
        })
        .transpose()?;
    let gvar_data = provider.table_data(tag::GVAR)?;
    let gvar = gvar_data
        .as_ref()
        .map(|gvar_data| ReadScope::new(gvar_data).read::<GvarTable<'_>>())
        .transpose()?;
    let hvar_data = provider.table_data(tag::HVAR)?;
    let hvar = hvar_data
        .as_ref()
        .map(|hvar_data| ReadScope::new(hvar_data).read::<HvarTable<'_>>())
        .transpose()?;
    let mvar_data = provider.table_data(tag::MVAR)?;
    let mvar = mvar_data
        .as_ref()
        .map(|mvar_data| ReadScope::new(mvar_data).read::<MvarTable<'_>>())
        .transpose()?;
    let stat_data = provider.read_table_data(tag::STAT)?;
    let stat = ReadScope::new(&stat_data).read::<StatTable<'_>>()?;
    let name_data = provider.read_table_data(tag::NAME)?;
    let name = ReadScope::new(&name_data).read::<NameTable<'_>>()?;

    let instance = fvar.normalize(user_instance.iter().copied(), avar.as_ref())?;

    // Apply deltas to glyphs to build a new glyf table
    match &gvar {
        Some(gvar) => {
            glyf = apply_gvar(
                glyf,
                gvar,
                &hmtx,
                vmtx.as_ref(),
                Some(&os2),
                &hhea,
                &instance,
            )?;

            // Update head
            let mut bbox = RectI::default();
            glyf.records().iter().for_each(|glyph| match glyph {
                GlyfRecord::Present { .. } => {}
                GlyfRecord::Parsed(glyph) => {
                    if let Some(bounding_box) = glyph.bounding_box() {
                        bbox = union_rect(bbox, bounding_box.into())
                    }
                }
            });
            head.x_min = bbox.min_x().try_into().ok().unwrap_or(i16::MIN);
            head.y_min = bbox.min_y().try_into().ok().unwrap_or(i16::MIN);
            head.x_max = bbox.max_x().try_into().ok().unwrap_or(i16::MAX);
            head.y_max = bbox.max_y().try_into().ok().unwrap_or(i16::MAX);
        }
        // It is possible for a TrueType variable font to exist without a gvar table. The most
        // likely place this would be encountered would be a COLRv1 font that varies the colour
        // information but not the glyph contours. We don't currently support COLRv1. There are
        // other ways such a font might exist but it should be uncommon. For now these are
        // unsupported.
        None => return Err(VariationError::NotImplemented),
    }

    // Build new hmtx table
    let hmtx = create_hmtx_table(&hmtx, hvar.as_ref(), &glyf, &instance, maxp.num_glyphs)?;

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

    // If one of the axes is wght or wdth then when need to update the corresponding
    // fields in OS/2
    for (axis, value) in fvar.axes().zip(user_instance.iter().copied()) {
        if value == axis.default_value {
            continue;
        }

        match axis.axis_tag {
            tag::WGHT => {
                // Map the value to one of the weight classes. Weight can be 1 to 1000 but
                // weight classes are only defined for 100, 200, 300... 900.
                os2.us_weight_class = ((f32::from(value).clamp(1., 1000.) / 100.0).round() as u16
                    * 100)
                    .clamp(100, 900);
            }
            tag::WDTH => {
                os2.us_width_class = Os2::value_to_width_class(value);
            }
            _ => {}
        }
    }

    if let (Some(cvt), Some(cvar)) = (cvt.as_mut(), cvar) {
        *cvt = cvar.apply(&instance, cvt)?;
    }

    // Get the remaining tables
    let cmap = provider.read_table_data(tag::CMAP)?;
    let fpgm = provider.table_data(tag::FPGM)?;
    let prep = provider.table_data(tag::PREP)?;

    // Update name
    let names = typographic_subfamily_name(user_instance, &fvar, &stat, &name, "Regular")?;
    let typographic_family = name
        .string_for_id(NameTable::TYPOGRAPHIC_FAMILY_NAME)
        .or_else(|| name.string_for_id(NameTable::FONT_FAMILY_NAME))
        .ok_or(VariationError::NameError)?;
    let postscript_prefix = name.string_for_id(NameTable::VARIATIONS_POSTSCRIPT_NAME_PREFIX);
    let mut name = owned::NameTable::try_from(&name)?;

    // Remove name_id entries 1 & 2 and then populate 16 & 17, replacing an existing
    // entries
    let full_name = format!("{} {}", typographic_family, names);
    let postscript_name = generate_postscript_name(
        &postscript_prefix,
        &typographic_family,
        user_instance,
        &fvar,
    );
    let unique_id = generate_unique_id(&head, &os2, &postscript_name);
    name.remove_entries(NameTable::FONT_FAMILY_NAME);
    name.remove_entries(NameTable::FONT_SUBFAMILY_NAME);
    name.replace_entries(NameTable::UNIQUE_FONT_IDENTIFIER, &unique_id);
    name.replace_entries(NameTable::FULL_FONT_NAME, &full_name);
    name.replace_entries(NameTable::POSTSCRIPT_NAME, &postscript_name);
    name.replace_entries(NameTable::TYPOGRAPHIC_FAMILY_NAME, &typographic_family);
    name.replace_entries(NameTable::TYPOGRAPHIC_SUBFAMILY_NAME, &names);

    // Build the new font
    let mut builder = FontBuilder::new(0x00010000_u32);
    builder.add_table::<_, ReadScope<'_>>(tag::CMAP, ReadScope::new(&cmap), ())?;
    if let Some(cvt) = cvt {
        builder.add_table::<_, CvtTable<'_>>(tag::CVT, &cvt, ())?;
    }
    if let Some(fpgm) = fpgm {
        builder.add_table::<_, ReadScope<'_>>(tag::FPGM, ReadScope::new(&fpgm), ())?;
    }
    builder.add_table::<_, HheaTable>(tag::HHEA, &hhea, ())?;
    builder.add_table::<_, HmtxTable<'_>>(tag::HMTX, &hmtx, ())?;
    builder.add_table::<_, MaxpTable>(tag::MAXP, &maxp, ())?;
    builder.add_table::<_, owned::NameTable<'_>>(tag::NAME, &name, ())?;
    builder.add_table::<_, Os2>(tag::OS_2, &os2, ())?;
    builder.add_table::<_, PostTable<'_>>(tag::POST, &post, ())?;
    if let Some(prep) = prep {
        builder.add_table::<_, ReadScope<'_>>(tag::PREP, ReadScope::new(&prep), ())?;
    }
    let mut builder = builder.add_head_table(&head)?;
    builder.add_glyf_table(glyf)?;
    builder.data().map_err(VariationError::from)
}

fn typographic_subfamily_name<'a>(
    user_instance: &[Fixed],
    fvar: &FvarTable<'a>,
    stat: &'a StatTable<'a>,
    name: &NameTable<'a>,
    default: &str,
) -> Result<String, VariationError> {
    let mut names = Vec::new();
    for (axis, value) in fvar.axes().zip(user_instance.iter().copied()) {
        for (i, rec) in stat.design_axes().enumerate() {
            let rec = rec?;
            if rec.axis_tag == axis.axis_tag {
                if let Some(name_id) =
                    stat.name_for_axis_value(i as u16, value, ElidableName::Exclude)
                {
                    names.push((name_id, rec.axis_ordering));
                }
            }
        }
    }
    // Sort by axis_ordering
    names.sort_by_key(|res| res.1);
    let names = if names.is_empty() {
        // names might be empty if all the axis values names were elidable, fall back on
        // elidedFallbackNameID if present
        let name = stat
            .elided_fallback_name_id
            .and_then(|name_id| name.string_for_id(name_id))
            .unwrap_or_else(|| default.to_string());
        vec![name]
    } else {
        names
            .into_iter()
            .filter_map(|(name_id, _)| name.string_for_id(name_id))
            .collect::<Vec<_>>()
    };
    Ok(names.join(" "))
}

// https://web.archive.org/web/20190705180831/https://wwwimages2.adobe.com/content/dam/acom/en/devnet/font/pdfs/5902.AdobePSNameGeneration.pdf
fn generate_postscript_name(
    prefix: &Option<String>,
    typographic_family: &str,
    user_tuple: &[Fixed],
    fvar: &FvarTable<'_>,
) -> String {
    // Remove any characters other than ASCII-range uppercase Latin
    // letters, lowercase Latin letters, and digits.
    let mut prefix: String = prefix
        .as_deref()
        .unwrap_or(typographic_family)
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .collect();
    let mut postscript_name = prefix.clone();
    fvar.axes()
        .zip(user_tuple.iter().copied())
        .for_each(|(axis, value)| {
            if value != axis.default_value {
                // NOTE(unwrap): Should always succeed when writing to a String (I/O error not
                // possible)
                let tag = DisplayTag(axis.axis_tag).to_string();
                write!(
                    postscript_name,
                    "_{}{}",
                    fixed_to_min_float(value),
                    tag.trim()
                )
                .unwrap();
            }
        });

    if postscript_name.len() > 63 {
        // Too long, construct "last resort" name
        let crc = crc32fast::hash(postscript_name.as_bytes());
        let hash = format!("-{:X}...", crc);
        // Ensure prefix is short enough when prepended to hash. Truncate is safe as
        // prefix is ASCII only.
        prefix.truncate(63 - hash.len());
        postscript_name = prefix + &hash;
    }

    postscript_name
}

fn generate_unique_id(head: &HeadTable, os2: &Os2, postscript_name: &str) -> String {
    let version = head.font_revision;
    let vendor = DisplayTag(os2.ach_vend_id).to_string();
    format!(
        "{:.3};{};{}",
        f32::from(version),
        vendor.trim(),
        postscript_name
    )
}

/// Format [Fixed] using minimal decimals (as specified for generating
/// postscript names)
fn fixed_to_min_float(fixed: Fixed) -> f64 {
    // Implementation ported from:
    // https://web.archive.org/web/20190705180831/https://wwwimages2.adobe.com/content/dam/acom/en/devnet/font/pdfs/5902.AdobePSNameGeneration.pdf
    let scale = (1 << 16) as f64;
    let value = fixed.raw_value() as f64 / scale;
    let eps = 0.5 / scale;
    let lo = value - eps;
    let hi = value + eps;
    // If the range of valid choices spans an integer, return the integer.
    if lo as i32 != hi as i32 {
        return value.round();
    }

    let lo = format!("{:.8}", lo);
    let hi = format!("{:.8}", hi);
    debug_assert!(
        lo.len() == hi.len() && lo != hi,
        "lo = {}, hi = {}, eps = {}",
        lo,
        hi,
        eps
    );
    let mut i = lo.len() - 1;
    for (index, (l, h)) in lo.bytes().zip(hi.bytes()).enumerate() {
        if l != h {
            i = index;
            break;
        }
    }
    let period = lo.bytes().position(|b| b == b'.').unwrap();
    debug_assert!(period < i);
    f64::from_str(&format!("{:.digits$}", value, digits = i - period)).unwrap()
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
    // * A font variations ('fvar') table is required to describe the variations
    //   supported by the font.
    // * A style attributes (STAT) table is required and is used to establish
    //   relationships between different fonts belonging to a family and to provide
    //   some degree of compatibility with legacy applications by allowing platforms
    //   to project variation instances involving many axes into older font-family
    //   models that assume a limited set of axes.
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
    glyf: &GlyfTable<'_>,
    instance: &OwnedTuple,
    num_glyphs: u16,
) -> Result<HmtxTable<'b>, ReadWriteError> {
    let mut h_metrics = Vec::with_capacity(usize::from(num_glyphs));

    match hvar {
        // Apply deltas to hmtx
        Some(hvar) => {
            for glyph_id in 0..num_glyphs {
                let mut metric = hmtx.metric(glyph_id)?;
                let delta = hvar.advance_delta(instance, glyph_id)?;
                let new = (metric.advance_width as f32 + delta).round();
                metric.advance_width = new.clamp(0., u16::MAX as f32) as u16;

                if let Some(delta) = hvar.left_side_bearing_delta(instance, glyph_id)? {
                    metric.lsb = (metric.lsb as f32 + delta)
                        .round()
                        .clamp(i16::MIN as f32, i16::MAX as f32)
                        as i16;
                    h_metrics.push(metric)
                }
            }
        }
        // Calculate from glyph deltas/phantom points
        None => {
            // Take note that, in a variable font with TrueType outlines, the left side
            // bearing for each glyph must equal xMin, and bit 1 in the flags
            // field of the 'head' table must be set.
            //
            // If a glyph has no contours, xMax/xMin are not defined. The left side bearing
            // indicated in the 'hmtx' table for such glyphs should be zero.
            for glyph_record in glyf.records().iter() {
                let metric = match glyph_record {
                    GlyfRecord::Parsed(glyph) => {
                        let bounding_box = glyph.bounding_box().unwrap_or_else(BoundingBox::empty);
                        // NOTE(unwrap): Phantom points are populated by apply_gvar
                        let phantom_points = glyph.phantom_points().unwrap();
                        let pp1 = phantom_points[0].0;
                        let pp2 = phantom_points[1].0;
                        // pp1 = xMin - lsb
                        // pp2 = pp1 + aw
                        let lsb = bounding_box.x_min - pp1;
                        let advance_width = u16::try_from(pp2 - pp1).unwrap_or(0);
                        LongHorMetric { advance_width, lsb }
                    }
                    _ => unreachable!("glyph should be parsed with phantom points present"),
                };
                h_metrics.push(metric);
            }
        }
    }

    // TODO: Can we apply the optimisation if they're all the same at the end

    Ok(HmtxTable {
        h_metrics: ReadArrayCow::Owned(h_metrics),
        left_side_bearings: ReadArrayCow::Owned(vec![]),
    })
}

/// Applies glyph deltas from the `gvar` table to glyphs in the `glyf` table.
///
/// Takes ownership of the `glyf` table as placeholder values are swapped in
/// during processing (see not in body of function) and returning early would
/// leave the `glyf` table in an incorrect state. So we consume it and return
/// the modified, valid result only on success.
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
            GlyfRecord::Parsed(glyph) => {
                glyph.apply_variations(glyph_id, instance, gvar, hmtx, vmtx, os2, hhea)?;
            }
            GlyfRecord::Present { .. } => unreachable!("glyph should be parsed"),
        }
    }

    // Do a pass to update the bounding boxes of composite glyphs
    for glyph_id in 0..glyf.num_glyphs() {
        // We do a little take/replace dance here to work within Rust's unique (mut)
        // access constraints: we need to mutate the glyph but also pass an
        // immutable reference to the glyf table that holds it. To work around
        // this we swap the glyph we're processing with an empty glyph in the
        // glyf table and then put it back afterwards. This works because
        // the glyf table is required for `apply_variations` to resolve child components
        // in composite glyphs to calculate the bounding box, and a composite
        // glyph can't refer to itself so should never encounter the empty
        // replacement.
        if glyf.records()[usize::from(glyph_id)].is_composite() {
            // NOTE(unwrap): should not panic as glyph_id < num_glyphs
            let mut glyph_record = glyf.take(glyph_id).unwrap();
            let GlyfRecord::Parsed(Glyph::Composite(ref mut composite)) = glyph_record else {
                unreachable!("expected parsed composite glyph")
            };
            // Calculate the new bounding box for this composite glyph
            let bbox = composite
                .calculate_bounding_box(&glyf)?
                .round_out()
                .to_i32();
            composite.bounding_box = BoundingBox {
                x_min: bbox
                    .min_x()
                    .try_into()
                    .map_err(|_| ParseError::LimitExceeded)?,
                x_max: bbox
                    .max_x()
                    .try_into()
                    .map_err(|_| ParseError::LimitExceeded)?,
                y_min: bbox
                    .min_y()
                    .try_into()
                    .map_err(|_| ParseError::LimitExceeded)?,
                y_max: bbox
                    .max_y()
                    .try_into()
                    .map_err(|_| ParseError::LimitExceeded)?,
            };
            glyf.replace(glyph_id, glyph_record)?;
        }
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
            VariationError::NameError => write!(f, "font did not contain a `name` table entry for the family name in a usable encoding")
        }
    }
}

impl std::error::Error for VariationError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_close;
    use crate::font_data::FontData;
    use crate::tests::read_fixture;

    #[test]
    fn test_generate_postscript_name_with_postscript_prefix() {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let fvar_data = table_provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();

        // Display SemiCondensed Thin: [100.0, 87.5, 100.0]
        let user_tuple = [Fixed::from(100.0), Fixed::from(87.5), Fixed::from(100.0)];
        let typographic_family = "Family";
        let postscript_prefix = Some(String::from("PSPrefix"));
        let postscript_name =
            generate_postscript_name(&postscript_prefix, typographic_family, &user_tuple, &fvar);
        assert_eq!(postscript_name, "PSPrefix_100wght_87.5wdth_100CTGR");
    }

    #[test]
    fn test_generate_postscript_name_without_postscript_prefix() {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let fvar_data = table_provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();

        // Display SemiCondensed Thin: [100.0, 87.5, 100.0]
        let user_tuple = [Fixed::from(100.0), Fixed::from(87.5), Fixed::from(100.0)];
        let typographic_family = "Family";
        let postscript_prefix = None;
        let postscript_name =
            generate_postscript_name(&postscript_prefix, typographic_family, &user_tuple, &fvar);
        assert_eq!(postscript_name, "Family_100wght_87.5wdth_100CTGR");
    }

    #[test]
    fn test_generate_postscript_name_omit_defaults() {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let fvar_data = table_provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();

        let user_tuple = [Fixed::from(400.0), Fixed::from(87.5), Fixed::from(0.0)];
        let typographic_family = "Family";
        let postscript_prefix = Some(String::from("PSPrefix"));
        let postscript_name =
            generate_postscript_name(&postscript_prefix, typographic_family, &user_tuple, &fvar);
        assert_eq!(postscript_name, "PSPrefix_87.5wdth");
    }

    #[test]
    fn test_generate_postscript_name_strip_forbidden_chars() {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let fvar_data = table_provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();

        let user_tuple = [Fixed::from(100.0), Fixed::from(87.5), Fixed::from(100.0)];
        let typographic_family = "These aren't allowed []<>!";
        let postscript_name =
            generate_postscript_name(&None, typographic_family, &user_tuple, &fvar);
        assert_eq!(
            postscript_name,
            "Thesearentallowed_100wght_87.5wdth_100CTGR"
        );
    }

    #[test]
    fn test_generate_postscript_name_truncate() {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope
            .read::<FontData<'_>>()
            .expect("unable to parse font file");
        let table_provider = font_file
            .table_provider(0)
            .expect("unable to create font provider");
        let fvar_data = table_provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();

        let user_tuple = [Fixed::from(100.0), Fixed::from(87.5), Fixed::from(100.0)];
        let typographic_family = "IfAfterConstructingThePostScriptNameInThisWayTheLengthIsGreaterThan127CharactersThenConstructTheLastResortPostScriptName";
        let postscript_name =
            generate_postscript_name(&None, typographic_family, &user_tuple, &fvar);
        assert!(postscript_name.len() <= 63);
        assert_eq!(
            postscript_name,
            "IfAfterConstructingThePostScriptNameInThisWayTheLen-189E39CF..."
        );
    }

    #[test]
    fn typographic_subfamily_name_non_elidable() -> Result<(), ReadWriteError> {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<FontData<'_>>()?;
        let table_provider = font_file.table_provider(0)?;
        let fvar_data = table_provider.read_table_data(tag::FVAR)?;
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>()?;
        let stat_data = table_provider.read_table_data(tag::STAT)?;
        let stat = ReadScope::new(&stat_data).read::<StatTable<'_>>()?;
        let name_data = table_provider.read_table_data(tag::NAME)?;
        let name = ReadScope::new(&name_data).read::<NameTable<'_>>()?;

        let user_tuple = [Fixed::from(100.0), Fixed::from(87.5), Fixed::from(100.0)];
        let name = typographic_subfamily_name(&user_tuple, &fvar, &stat, &name, "Default").unwrap();
        assert_eq!(name, "Thin SemiCondensed Display");
        Ok(())
    }

    #[test]
    fn typographic_subfamily_name_elidable() -> Result<(), ReadWriteError> {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<FontData<'_>>()?;
        let table_provider = font_file.table_provider(0)?;
        let fvar_data = table_provider.read_table_data(tag::FVAR)?;
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>()?;
        let stat_data = table_provider.read_table_data(tag::STAT)?;
        let stat = ReadScope::new(&stat_data).read::<StatTable<'_>>()?;
        let name_data = table_provider.read_table_data(tag::NAME)?;
        let name = ReadScope::new(&name_data).read::<NameTable<'_>>()?;

        // - wght = min: 100, max: 900, default: 400
        // - wdth = min: 62.5, max: 100, default: 100
        // - CTGR = min: 0, max: 100, default: 0

        // Use default values to trigger elidable fallback
        let user_tuple = [Fixed::from(400.0), Fixed::from(100.0), Fixed::from(0.0)];
        let name = typographic_subfamily_name(&user_tuple, &fvar, &stat, &name, "Default").unwrap();
        assert_eq!(name, "Regular");
        Ok(())
    }

    #[test]
    fn test_fixed_to_float() {
        assert_close!(fixed_to_min_float(Fixed::from(900)), 900., f64::EPSILON);
        assert_close!(fixed_to_min_float(Fixed::from(5.5)), 5.5, f64::EPSILON);
        assert_close!(fixed_to_min_float(Fixed::from(2.9)), 2.9, f64::EPSILON);
        assert_close!(fixed_to_min_float(Fixed::from(-1.4)), -1.4, f64::EPSILON);
        assert_close!(
            fixed_to_min_float(Fixed::from(-1. + (1. / 65536.))),
            -0.99998,
            f64::EPSILON
        );
    }
}
