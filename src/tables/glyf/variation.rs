use std::collections::BTreeMap;
use std::ops::RangeInclusive;
use tinyvec::{tiny_vec, TinyVec};

use crate::error::ParseError;
use crate::tables::glyf::{
    CompositeGlyph, CompositeGlyphArgument, CompositeGlyphFlag, Glyph, GlyphData, Point,
    SimpleGlyph,
};
use crate::tables::variable_fonts::avar::AvarTable;
use crate::tables::variable_fonts::fvar::{FvarTable, VariationAxisRecord};
use crate::tables::variable_fonts::gvar::{GvarTable, NumPoints};
use crate::tables::variable_fonts::{Gvar, Tuple, TupleVariationHeader, TupleVariationStore};
use crate::tables::{F2Dot14, Fixed};
use crate::SafeFrom;

type OwnedTuple = TinyVec<[F2Dot14; 4]>;

enum Coordinates<'a> {
    Tuple(Tuple<'a>),
    Array(OwnedTuple),
}

struct CoordinatesIter<'a, 'data> {
    coords: &'a Coordinates<'data>,
    index: usize,
}

impl<'data> Coordinates<'data> {
    pub fn iter(&self) -> CoordinatesIter<'_, 'data> {
        CoordinatesIter {
            coords: self,
            index: 0,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Coordinates::Tuple(coords) => coords.0.len(),
            Coordinates::Array(coords) => coords.len(),
        }
    }
}

impl Iterator for CoordinatesIter<'_, '_> {
    type Item = F2Dot14;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.coords.len() {
            return None;
        }

        let index = self.index;
        self.index += 1;
        match self.coords {
            Coordinates::Tuple(coords) => Some(coords.0.get_item(index)),
            Coordinates::Array(coords) => Some(coords[index]),
        }
    }
}

/// Apply glyph variation to the supplied glyph according to the variation instance `instance`.
///
/// Returns the deltas for each coordinate.
fn apply<'a>(
    glyph: &Glyph<'a>,
    glyph_index: u16,
    user_instance: impl ExactSizeIterator<Item = Fixed>,
    gvar: &GvarTable<'a>,
    fvar: &FvarTable<'a>,
    avar: Option<&AvarTable<'a>>,
) -> Result<Glyph<'a>, ParseError> {
    let deltas = glyph_deltas(glyph, glyph_index, user_instance, gvar, fvar, avar)?;

    match glyph {
        Glyph {
            data: GlyphData::Simple(simple_glyph),
            ..
        } => {
            // Apply the deltas to the coordinates of the glyph
            let adjusted = simple_glyph
                .coordinates
                .iter()
                .copied()
                .zip(deltas.iter().copied())
                .map(|(point, delta)| {
                    Point(
                        (point.0 as f32 + delta.0).round() as i16,
                        (point.1 as f32 + delta.1) as i16,
                    ) // FIXME: Handle overflow
                })
                .collect::<Vec<_>>();
            let new_data = SimpleGlyph {
                end_pts_of_contours: simple_glyph.end_pts_of_contours.clone(),
                instructions: simple_glyph.instructions,
                flags: simple_glyph.flags.clone(),
                coordinates: adjusted,
            };

            Ok(Glyph {
                number_of_contours: glyph.number_of_contours,
                bounding_box: glyph.bounding_box, // FIXME: Adjust bounding box
                data: GlyphData::Simple(new_data),
            })
        }
        Glyph {
            data:
                GlyphData::Composite {
                    glyphs,
                    instructions,
                },
            ..
        } => {
            // Use the deltas to reposition the sub-glyphs of the composite glyph
            let adjusted = glyphs
                .iter()
                .zip(deltas.iter().copied())
                .map(|(composite_glyph, delta)| CompositeGlyph {
                    flags: composite_glyph.flags,
                    glyph_index,
                    argument1: add_delta(composite_glyph.flags, composite_glyph.argument1, delta.0),
                    argument2: add_delta(composite_glyph.flags, composite_glyph.argument2, delta.1),
                    scale: composite_glyph.scale,
                })
                .collect::<Vec<_>>();

            Ok(Glyph {
                number_of_contours: glyph.number_of_contours,
                bounding_box: glyph.bounding_box, // FIXME: Adjust bounding box
                data: GlyphData::Composite {
                    glyphs: adjusted,
                    instructions,
                },
            })
        }
    }
}

fn add_delta(
    flag: CompositeGlyphFlag,
    arg: CompositeGlyphArgument,
    delta: f32,
) -> CompositeGlyphArgument {
    // > if ARGS_ARE_XY_VALUES (bit 1) is set, then X and Y offsets are used; if that bit is clear,
    // > then point numbers are used. If the position of a component is represented using X and Y
    // > offsets — the ARGS_ARE_XY_VALUES flag is set — then adjustment deltas can be applied to
    // > those offsets. However, if the position of a component is represented using point numbers —
    // > the ARGS_ARE_XY_VALUES flag is not set — then adjustment deltas have no effect on that
    // > component and should not be specified.
    //
    // https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#point-numbers-and-processing-for-composite-glyphs
    if flag.args_are_xy_values() {
        let adjusted = match arg {
            CompositeGlyphArgument::U8(val) => val as f32 + delta,
            CompositeGlyphArgument::I8(val) => val as f32 + delta,
            CompositeGlyphArgument::U16(val) => val as f32 + delta,
            CompositeGlyphArgument::I16(val) => val as f32 + delta,
        };
        let adjusted = adjusted.round();

        // FIXME: handle overflow and use smaller types when appropriate
        if adjusted >= 0. {
            CompositeGlyphArgument::U16(adjusted as u16)
        } else {
            CompositeGlyphArgument::I16(adjusted as i16)
        }
    } else {
        arg
    }
}

/// Calculate the point deltas for the supplied glyph according to the variation instance `instance`.
fn glyph_deltas(
    glyph: &Glyph<'_>,
    glyph_index: u16,
    user_instance: impl ExactSizeIterator<Item = Fixed>,
    gvar: &GvarTable<'_>,
    fvar: &FvarTable<'_>,
    avar: Option<&AvarTable<'_>>,
) -> Result<Vec<(f32, f32)>, ParseError> {
    if user_instance.len() != usize::from(fvar.axis_count) {
        return Err(ParseError::MissingValue);
    }

    let instance = fvar.normalize(user_instance, avar)?;
    let num_points = NumPoints::new(glyph.number_of_coordinates()?);
    let variations = gvar.glyph_variation_data(glyph_index, num_points)?;
    let applicable = determine_applicable(gvar, &instance, &variations);
    dbg!(&applicable);

    // Now the deltas need to be calculated for each point.
    // The delta is multiplied by the scalar. The sum of deltas is applied to the default position
    // TODO: Use f32 or 16.16 fixed point?
    let mut final_deltas = vec![(0., 0.); usize::safe_from(num_points.get())];
    let mut region_deltas = vec![(0., 0.); usize::safe_from(num_points.get())];
    for (scale, region) in &applicable {
        let scale = f32::from(*scale);
        let variation_data =
            region.variation_data(num_points, variations.shared_point_numbers())?;
        // This is the output for this region, by the end every point needs to have a delta assigned.
        // Either explicitly or inferred. This buffer is reused between regions so we re-fill it
        // with zeros for each new region.
        region_deltas.fill((0., 0.));

        // This maps point numbers to deltas, in order. It allows direct lookup of deltas for a
        // point as well as navigating between explicit points.
        let explicit_deltas = variation_data.iter().collect::<BTreeMap<_, _>>();

        // Fill in the explicit deltas
        for (number, delta) in &explicit_deltas {
            // FIXME: use .get()?
            region_deltas[usize::safe_from(*number)] = (delta.0 as f32, delta.1 as f32);
        }

        // > Calculation of inferred deltas is done for a given glyph and a given region on a contour-by-contour basis.
        // >
        // > For a given contour, if the point number list does not include any of the points in that contour, then none of the points in the contour are affected and no inferred deltas need to be computed.
        // >
        // > If the point number list includes some but not all of the points in a given contour, then inferred deltas must be derived for the points that were not included in the point number list, as follows.

        // Only need to do this for simple glyphs
        match glyph {
            Glyph {
                data: GlyphData::Simple(simple_glyph),
                ..
            } => {
                // Deltas need to be inferred if not all points were assigned explicit deltas
                if explicit_deltas.len() != usize::safe_from(num_points.get()) {
                    infer_unreferenced_points(&mut region_deltas, &explicit_deltas, simple_glyph);
                }
            }
            _ => {}
        }

        // Scale and accumulate the deltas from this variation region onto the final deltas
        final_deltas
            .iter_mut()
            .zip(region_deltas.iter())
            .for_each(|(out, delta)| {
                let (delta_x, delta_y) = delta;
                out.0 += scale * delta_x;
                out.1 += scale * delta_y;
            })
    }

    // Now all the deltas need to be applied to the glyph points
    dbg!(&final_deltas);
    Ok(final_deltas)
}

fn infer_unreferenced_points(
    deltas: &mut [(f32, f32)],
    raw_deltas: &BTreeMap<u32, (i16, i16)>,
    simple_glyph: &SimpleGlyph<'_>,
) {
    // Iterate over the contours of the glyph and ensure that all points of the contour have a delta
    let mut begin = 0;
    for end in simple_glyph.end_pts_of_contours.iter().copied() {
        let start = begin;
        let end = u32::from(end);
        begin = end + 1;
        let range = start..=end;

        let explicit_count = raw_deltas.range(range.clone()).count();
        match explicit_count {
            0 => {
                // No points in this contour were referenced; no inferred deltas need to
                // be computed.
                continue;
            }
            1 => {
                // If exactly one point from the contour is referenced in the point number list, then every point in that contour uses the same X and Y delta values as that point.
                // find the one referenced point and use it to update the others
                // NOTE(unwrap): Safe as we confirmed we have one delta to get in this block
                let (_referenced_point_number, reference_delta) = raw_deltas
                    .range(range.clone())
                    .next()
                    .map(|(n, (x, y))| (*n, (*x as f32, *y as f32)))
                    .unwrap();
                // Get the delta for this point
                let usize_range = usize::safe_from(*range.start())..=usize::safe_from(*range.end());
                // Set all the deltas in this contour to `reference_delta`
                deltas[usize_range].fill(reference_delta);
                continue;
            }
            // FIXME: .count()?
            n if n == range.clone().count() => {
                // All points in this contour were referenced; no inferred deltas need to
                // be computed.
                continue;
            }
            _ => {
                // If the point number list includes some but not all of the points in a given contour, then inferred deltas must be derived for the points that were not included in the point number list.
                infer_contour(&range, deltas, raw_deltas, simple_glyph)
            }
        }
    }
}

fn infer_contour(
    contour_range: &RangeInclusive<u32>,
    deltas: &mut [(f32, f32)],
    explicit_deltas: &BTreeMap<u32, (i16, i16)>,
    simple_glyph: &SimpleGlyph<'_>,
) {
    // FIXME: How do you do this?
    // ...the inferred deltas can be pre-computed before any processing for a specific instance is done.
    for target in contour_range.clone() {
        if explicit_deltas.contains_key(&target) {
            continue;
        }

        // This is an unreferenced point
        // > First, for any un-referenced point, identify the nearest points before and after, in point number order, that are referenced. Note that the same referenced points will be used for calculating both X and Y inferred deltas. If there is no lower point number from that contour that was referenced, then the highest, referenced point number from that contour is used. Similarly, if no higher point number from that contour was referenced, then the lowest, referenced point number is used.
        // NOTE(unwrap): Due to checks above regarding the number of referenced points we should always find a next/prev point
        let next = explicit_deltas
            .range(target..*contour_range.end())
            .chain(explicit_deltas.range(*contour_range.start()..target))
            .next()
            .unwrap();
        let prev = explicit_deltas
            .range(target..*contour_range.end())
            .chain(explicit_deltas.range(*contour_range.start()..target))
            .rev()
            .next()
            .unwrap();

        let target = usize::safe_from(target);
        deltas[target] = infer_delta(target, prev, next, &simple_glyph.coordinates)
    }
}

// > Once the adjacent, referenced points are identified, then inferred-delta calculation is done separately for X and Y directions.
//
// Next, the (X or Y) grid coordinate values of the adjacent, referenced points are compared. If these coordinates are the same, then the delta values for the adjacent points are compared: if the delta values are the same, then this value is used as the inferred delta for the target, un-referenced point. If the delta values are different, then the inferred delta for the target point is zero.

// prev and next are indexes into the points of the current coordinate
// they need to be translated to indexes into the the whole glyph
// coordinate array.
fn infer_delta(
    target: usize,
    (prev_number, prev_delta): (&u32, &(i16, i16)),
    (next_number, next_delta): (&u32, &(i16, i16)),
    coordinates: &[Point],
) -> (f32, f32) {
    // https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#inferred-deltas-for-un-referenced-point-numbers
    let prev_coord = coordinates[usize::safe_from(*prev_number)]; // FIXME: can these panic
    let target_coord = coordinates[target];
    let next_coord = coordinates[usize::safe_from(*next_number)];

    let delta_x = do_infer(
        prev_coord.0,
        target_coord.0,
        next_coord.0,
        prev_delta.0,
        next_delta.0,
    );
    let delta_y = do_infer(
        prev_coord.1,
        target_coord.1,
        next_coord.1,
        prev_delta.1,
        next_delta.1,
    );
    (delta_x, delta_y)
}

fn do_infer(
    prev_coord: i16,
    target_coord: i16,
    next_coord: i16,
    prev_delta: i16,
    next_delta: i16,
) -> f32 {
    let prev_delta = prev_delta;
    let next_delta = next_delta;
    if prev_coord == next_coord {
        if prev_delta == next_delta {
            prev_delta as f32
        } else {
            0.
        }
    } else {
        if target_coord <= prev_coord.min(next_coord) {
            if prev_coord < next_coord {
                prev_delta as f32
            } else {
                next_delta as f32
            }
        } else if target_coord >= prev_coord.max(next_coord) {
            if prev_coord > next_coord {
                prev_delta as f32
            } else {
                next_delta as f32
            }
        } else {
            // But if the coordinate of the target point is not between the coordinates of the adjacent points, then the inferred delta is the delta for whichever of the adjacent points is closer in the given direction.
            // If the coordinate of the target point is between the coordinates of the adjacent points, then a delta is interpolated
            /* target point delta is derived from the adjacent point deltas using linear interpolation */
            // Note: The logical flow of the algorithm to this point implies that the coordinates of the two adjacent points are different. This avoids a division by zero in the following calculations that would otherwise occur.
            let proportion =
                (target_coord as f32 - prev_coord as f32) / (next_coord as f32 - prev_coord as f32);
            (1. - proportion) * prev_delta as f32 + proportion * next_delta as f32
        }
    }
}

fn determine_applicable<'a, 'data>(
    gvar: &GvarTable<'data>,
    instance: &OwnedTuple,
    variations: &'a TupleVariationStore<'data, Gvar>,
) -> Vec<(f32, &'a TupleVariationHeader<'data, Gvar>)> {
    // Ok, now we have our tuple we need to get the relevant glyph variation records
    //
    // > The tuple variation headers within the selected glyph variation data table will each
    // > specify a particular region of applicability within the font’s variation space. These will
    // > be compared with the coordinates for the selected variation instance to determine which of
    // > the tuple-variation data tables are applicable, and to calculate a scalar value for each.
    // > These comparisons and scalar calculations are done using normalized-scale coordinate values.
    // >
    // > The tuple variation headers within the selected glyph variation data table will each
    // > specify a particular region of applicability within the font’s variation space. These will
    // > be compared with the coordinates for the selected variation instance to determine which of
    // > the tuple-variation data tables are applicable, and to calculate a scalar value for each.
    // > These comparisons and scalar calculations are done using normalized-scale coordinate
    // > values.For each of the tuple-variation data tables that are applicable, the point number and
    // > delta data will be unpacked and processed. The data for applicable regions can be processed
    // > in any order. Derived delta values will correspond to particular point numbers derived from
    // > the packed point number data. For a given point number, the computed scalar is applied to
    // > the X coordinate and Y coordinate deltas as a coefficient, and then resulting delta
    // > adjustments applied to the X and Y coordinates of the point.

    // Determine which ones are applicable and return the scalar value for each one
    variations
        .headers()
        .filter_map(|header| {
            // https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#algorithm-for-interpolation-of-instance-values
            let peak_coords = header.peak_tuple(gvar).expect("FIXME");
            let (start_coords, end_coords) = match header.intermediate_region() {
                // NOTE(clone): Cheap as Tuple just contains ReadArray
                Some((start, end)) => (
                    Coordinates::Tuple(start.clone()),
                    Coordinates::Tuple(end.clone()),
                ),
                None => {
                    let mut start_coords = tiny_vec!();
                    let mut end_coords = tiny_vec!();
                    for peak in peak_coords.0.iter() {
                        match peak.raw_value().signum() {
                            // region is from peak to zero
                            -1 => {
                                start_coords.push(peak);
                                end_coords.push(F2Dot14::from(0));
                            }
                            // When a delta is provided for a region defined by n-tuples that have a peak value of 0 for some axis, then that axis does not factor into scalar calculations.
                            0 => {
                                start_coords.push(peak);
                                end_coords.push(peak);
                            }
                            // region is from zero to peak
                            1 => {
                                start_coords.push(F2Dot14::from(0));
                                end_coords.push(peak);
                            }
                            _ => unreachable!("unknown value from signum"),
                        }
                    }
                    (
                        Coordinates::Array(start_coords),
                        Coordinates::Array(end_coords),
                    )
                }
            };

            // Now determine the scalar
            //
            // >  In calculation of scalars (S, AS) and of interpolated values (scaledDelta, netAjustment, interpolatedValue), at least 16 fractional bits of precision should be maintained.
            let scalar = start_coords
                .iter()
                .zip(end_coords.iter())
                .zip(instance.iter().copied())
                .zip(peak_coords.0.iter())
                .map(|(((start, end), instance), peak)| {
                    // If peak is zero or not contained by the region of applicability then it does not
                    if peak == F2Dot14::from(0) {
                        // If the peak is zero for some axis, then ignore the axis.
                        1.
                    } else if (start..=end).contains(&instance) {
                        // The region is applicable: calculate a per-axis scalar as a proportion
                        // of the proximity of the instance to the peak within the region.
                        if instance == peak {
                            1.
                        } else if instance < peak {
                            (f32::from(instance) - f32::from(start))
                                / (f32::from(peak) - f32::from(start))
                        } else {
                            // instance > peak
                            (f32::from(end) - f32::from(instance))
                                / (f32::from(end) - f32::from(peak))
                        }
                    } else {
                        // If the instance coordinate is out of range for some axis, then the region and its
                        // associated deltas are not applicable.
                        0.
                    }
                })
                .fold(1., |scalar, axis_scalar| scalar * axis_scalar);

            (scalar != 0.).then(|| (scalar, header))
        })
        .collect::<Vec<_>>() // FIXME?
}

impl FvarTable<'_> {
    // I counted the number of axes in 399 variable fonts in Google Fonts and this was the
    // result:
    //
    // | Axis Count | Number |
    // |------------|--------|
    // | 1          | 279    |
    // | 2          | 108    |
    // | 3          | 2      |
    // | 4          | 5      |
    // | 5          | 1      |
    // | 13         | 2      |
    // | 15         | 2      |
    //
    // With this in mind the majority of fonts are handled with two axes. However, the minimum size
    // of a TinyVec is 24 bytes due to the Vec it can also hold, so we choose 4 since it doesn't
    // use any more space than when set to two.
    fn normalize(
        &self,
        user_tuple: impl ExactSizeIterator<Item = Fixed>,
        avar: Option<&AvarTable<'_>>,
    ) -> Result<OwnedTuple, ParseError> {
        // FIXME: user_tuple needs to be the right length
        let mut tuple = TinyVec::with_capacity(user_tuple.len());
        let mut avar_iter = avar.map(|avar| avar.segment_maps());
        for (axis, user_value) in self.axes().zip(user_tuple) {
            let axis = axis?;
            let mut normalized_value = default_normalize(&axis, user_value);

            // If avar table is present do more normalization with it
            if let Some(avar) = avar_iter.as_mut() {
                let segment_map = avar.next().ok_or(ParseError::BadIndex)?;
                normalized_value = segment_map.normalize(normalized_value);
                // Do the -1..1 clamping again to ensure the value remains in range
                normalized_value = normalized_value.clamp(Fixed::from(-1), Fixed::from(1));
            }

            // Convert the final, normalized 16.16 coordinate value to 2.14.
            tuple.push(F2Dot14::from(normalized_value));
        }
        Ok(tuple)
    }
}

fn default_normalize(axis: &VariationAxisRecord, coord: Fixed) -> Fixed {
    // Clamp
    let coord = coord.clamp(axis.min_value, axis.max_value);

    // Interpolate
    // TODO: convert to if-else
    let normalised_value = match coord {
        _ if coord < axis.default_value => {
            -(axis.default_value - coord) / (axis.default_value - axis.min_value)
        }
        _ if coord > axis.default_value => {
            (coord - axis.default_value) / (axis.max_value - axis.default_value)
        }
        _ => Fixed::from(0),
    };

    // After the default normalization calculation is performed, some results may be slightly outside the range [-1, +1]. Values must be clamped to this range.
    normalised_value.clamp(Fixed::from(-1), Fixed::from(1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;
    use crate::error::ReadWriteError;
    use crate::font_data::FontData;
    use crate::tables::glyf::GlyfTable;
    use crate::tables::loca::LocaTable;
    use crate::tables::variable_fonts::fvar::{FvarTable, VariationAxisRecord};
    use crate::tables::{FontTableProvider, HeadTable, MaxpTable, NameTable};
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    fn apply_variations() -> Result<(), ReadWriteError> {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<FontData<'_>>()?;
        let provider = font_file.table_provider(0)?;
        let head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
        let maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
        let loca_data = provider.read_table_data(tag::LOCA)?;
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((usize::from(maxp.num_glyphs), head.index_to_loc_format))?;
        let glyf_data = provider.read_table_data(tag::GLYF)?;
        let mut glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
        let fvar_data = provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();
        let avar_data = provider.table_data(tag::AVAR)?;
        let avar = avar_data
            .as_ref()
            .map(|avar_data| ReadScope::new(avar_data).read::<AvarTable<'_>>())
            .transpose()?;
        let gvar_data = provider.read_table_data(tag::GVAR)?;
        let gvar = ReadScope::new(&gvar_data).read::<GvarTable<'_>>().unwrap();
        let name_table_data = provider
            .read_table_data(tag::NAME)
            .expect("unable to read name table data");
        let name_table = ReadScope::new(&name_table_data)
            .read::<NameTable<'_>>()
            .unwrap();

        // Pick a glyph. Glyph 2 is 'b'
        let glyph_index = 2u16;
        let glyph = glyf.get_parsed_glyph(glyph_index)?.unwrap();

        // Pick an instance
        let mut instance = None;
        for inst in fvar.instances() {
            let inst = inst?;
            let subfamily = name_table.english_string_for_id(inst.subfamily_name_id);
            if subfamily.as_deref() == Some("Display Condensed Thin") {
                // - wght = min: 100, max: 900, default: 400
                // - wdth = min: 62.5, max: 100, default: 100
                // - CTGR = min: 0, max: 100, default: 0
                //
                // Coordinates: [100.0, 62.5, 100.0]
                instance = Some(inst);
                break;
            }
        }
        let instance = instance.unwrap();

        // The instance is a UserTuple record that needs be normalised into a Tuple record
        // let axes = fvar.axes().collect::<Result<Vec<_>, _>>()?;

        let varied = glyph_deltas(
            glyph,
            glyph_index,
            instance.coordinates.iter(),
            &gvar,
            &fvar,
            avar.as_ref(),
        )?;

        Ok(())
    }

    #[test]
    #[cfg(feature = "prince")]
    fn apply_skia_variations() -> Result<(), ReadWriteError> {
        let buffer = read_fixture("../../../tests/data/fonts/Skia.hyphen.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<FontData<'_>>()?;
        let provider = font_file.table_provider(0)?;
        let head = ReadScope::new(&provider.read_table_data(tag::HEAD)?).read::<HeadTable>()?;
        let maxp = ReadScope::new(&provider.read_table_data(tag::MAXP)?).read::<MaxpTable>()?;
        let loca_data = provider.read_table_data(tag::LOCA)?;
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((usize::from(maxp.num_glyphs), head.index_to_loc_format))?;
        let glyf_data = provider.read_table_data(tag::GLYF)?;
        let mut glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
        let fvar_data = provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();
        let avar_data = provider.table_data(tag::AVAR)?;
        let avar = avar_data
            .as_ref()
            .map(|avar_data| ReadScope::new(avar_data).read::<AvarTable<'_>>())
            .transpose()?;
        let gvar_data = provider.read_table_data(tag::GVAR)?;
        let gvar = ReadScope::new(&gvar_data).read::<GvarTable<'_>>().unwrap();

        // Pick a glyph. Glyph 45 is '-', this is chosen to replicate the example in the spec:
        // https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#interpolation-example
        let glyph_index = 45u16;
        let glyph = glyf.get_parsed_glyph(glyph_index)?.unwrap();

        // (0.2, 0.7) — a slight weight increase and a large width increase. The example gives
        // these are normalised values but we need to supply user values
        let instance = &[Fixed::from(1.44), Fixed::from(1.21)];

        let varied = glyph_deltas(
            glyph,
            glyph_index,
            instance.iter().copied(),
            &gvar,
            &fvar,
            avar.as_ref(),
        )?;

        // FIXME: I think the actual deltas should have a smaller epsilon than this
        fn assert_close(actual: f32, expected: f32) {
            let epsilon = 0.005;
            assert!(
                (actual - expected).abs() < epsilon,
                "{:?} != {:?} ± {}",
                actual,
                expected,
                epsilon
            );
        }
        let expected_deltas = &[
            (162.3, -28.4),
            (8.8, -28.4),
            (8.8, 36.4),
            (162.3, 36.4),
            (0., 0.),
            (172.7, 0.),
        ];
        for (expected, actual) in expected_deltas.iter().copied().zip(varied.iter().copied()) {
            assert_close(actual.0, expected.0);
            assert_close(actual.1, expected.1);
        }

        Ok(())
    }

    #[test]
    fn test_fvar_normalization() -> Result<(), ReadWriteError> {
        let buffer = read_fixture("tests/fonts/opentype/NotoSans-VF.abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<FontData<'_>>()?;
        let provider = font_file.table_provider(0)?;
        let fvar_data = provider
            .read_table_data(tag::FVAR)
            .expect("unable to read fvar table data");
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();
        let avar_data = provider.table_data(tag::AVAR)?;
        let avar = avar_data
            .as_ref()
            .map(|avar_data| ReadScope::new(avar_data).read::<AvarTable<'_>>())
            .transpose()?;
        let name_table_data = provider
            .read_table_data(tag::NAME)
            .expect("unable to read name table data");
        let name_table = ReadScope::new(&name_table_data)
            .read::<NameTable<'_>>()
            .unwrap();

        // Pick an instance
        let mut instance = None;
        for inst in fvar.instances() {
            let inst = inst?;
            let subfamily = name_table.english_string_for_id(inst.subfamily_name_id);
            if subfamily.as_deref() == Some("Display Condensed Thin") {
                // - wght = min: 100, max: 900, default: 400
                // - wdth = min: 62.5, max: 100, default: 100
                // - CTGR = min: 0, max: 100, default: 0
                //
                // Coordinates: [100.0, 62.5, 100.0]
                instance = Some(inst);
                break;
            }
        }
        let instance = instance.unwrap();

        // The instance is a UserTuple record that needs be normalised into a Tuple record
        let tuple = fvar.normalize(instance.coordinates.iter(), avar.as_ref())?;
        assert_eq!(
            tuple.as_slice(),
            &[
                F2Dot14::from(-1.0),
                F2Dot14::from(-0.7000122),
                F2Dot14::from(1.0)
            ]
        );

        Ok(())
    }

    #[test]
    fn test_default_normalization() {
        // https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#avar-normalization-example
        let axis = VariationAxisRecord {
            axis_tag: tag!(b"wght"),
            min_value: Fixed::from(100),
            default_value: Fixed::from(400),
            max_value: Fixed::from(900),
            flags: 0,
            axis_name_id: 0,
        };
        let user_coord = Fixed::from(250);
        assert_eq!(default_normalize(&axis, user_coord), Fixed::from(-0.5))
    }
}
