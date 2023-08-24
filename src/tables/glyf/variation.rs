use rustc_hash::FxHashSet;
use std::collections::BTreeSet;
use std::env::var;
use tinyvec::{tiny_vec, TinyVec};

use crate::error::ParseError;
use crate::tables::glyf::{GlyfRecord, Glyph, GlyphData, Point, SimpleGlyph};
use crate::tables::variable_fonts::avar::{AvarTable, AxisValueMap};
use crate::tables::variable_fonts::fvar::{FvarTable, VariationAxisRecord};
use crate::tables::variable_fonts::gvar::{GvarTable, NumPoints};
use crate::tables::variable_fonts::{Gvar, GvarVariationData, Tuple, TupleVariationHeader, TupleVariationStore, UserTuple};
use crate::tables::{F2Dot14, Fixed};
use crate::SafeFrom;

type OwnedTuple = TinyVec<[F2Dot14; 4]>;

enum Coordinates<'a> {
    Tuple(Tuple<'a>),
    Array(TinyVec<[F2Dot14; 4]>),
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

/// Apple glyph variation to the supplied glyph according to the variation instance `instance`.
fn apply(
    glyph: &Glyph,
    glyph_index: u16,
    user_instance: &UserTuple,
    gvar: &GvarTable<'_>,
    fvar: &FvarTable,
    avar: Option<&AvarTable>,
) -> Result<(), ParseError> {
    if user_instance.0.len() != usize::from(fvar.axis_count) {
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
    let mut referenced = BTreeSet::new(); // TODO: Can this be done with just a Vec or bool or options?
    let mut final_deltas = vec![None; usize::safe_from(num_points.get())];
    for (scale, region) in &applicable {
        let scale = f32::from(*scale);
        let variation_data =
            region.variation_data(num_points, variations.shared_point_numbers())?;
        referenced.clear();
        let mut deltas = vec![None; usize::safe_from(num_points.get())]; // FIXME: Can we reuse this
        variation_data
            .iter()
            .for_each(|(number, (delta_x, delta_y))| {
                let delta = deltas
                    .get_mut(usize::safe_from(number))
                    .ok_or_else(|| {
                        format!(
                            "unable to get point number {} in glyph with {} points",
                            number, num_points
                        )
                    })
                    .unwrap() // FIXME
                    .get_or_insert((0., 0.));
                delta.0 = delta_x as f32;
                delta.1 = delta_y as f32;
                referenced.insert(number);
            });

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
                post_process(num_points, &mut deltas, &variation_data, &referenced, scale, simple_glyph);
            }
            _ => {}
        }


        final_deltas.iter_mut().zip(deltas.iter()).for_each(|(out, delta)| {
            // NOTE(unwrap): All points should have deltas by the time we get here
            let (delta_x, delta_y) = delta.unwrap();
            let out = out.get_or_insert((0.,0.));
            out.0 += scale * delta_x;
            out.1 += scale * delta_y;
        })
    }

    // Now all the deltas need to be applied to the glyph points
    dbg!(&final_deltas);

    Err(ParseError::NotImplemented)
}

fn post_process(num_points: NumPoints, deltas: &mut Vec<Option<(f32, f32)>>, variation_data: &GvarVariationData, referenced: &BTreeSet<u32>, scale: f32, simple_glyph: &SimpleGlyph) {
    if variation_data.len() == usize::safe_from(num_points.get()) {
        // All points in all contours were referenced, no need to do further checks
        return;
    }

    let mut begin = 0;
    // let mut contour_referenced = FxHashSet::default();
    for end in simple_glyph.end_pts_of_contours.iter().copied() {
        // contour_referenced.clear();
        let start = begin;
        let end = u32::from(end);
        begin = end + 1;
        let range = start..=end;

        // TODO: Can we do better than count here?
        // Perhaps store the count alongside the BTree
        let referenced_count = referenced.range(range.clone()).count();
        match referenced_count {
            0 => {
                // No points in this contour were referenced; no inferred deltas need to
                // be computed.
                continue;
            }
            1 => {
                // If exactly one point from the contour is referenced in the point number list, then every point in that contour uses the same X and Y delta values as that point.
                // find the one referenced point and use it to update the others
                // NOTE(unwrap): Safe as we confirmed we have one delta to get in this block
                let referenced_point_number = referenced.range(range.clone()).copied().next().unwrap();
                // Get the delta for this point
                let reference_delta = deltas[usize::safe_from(referenced_point_number)];
                debug_assert!(reference_delta.is_some());
                let usize_range =
                    usize::safe_from(*range.start())..=usize::safe_from(*range.end());
                // let contour_deltas = deltas.get_mut(usize_range.clone()).unwrap();
                for delta in deltas.get_mut(usize_range.clone()).unwrap() {
                    // Is there a more efficient way to set them all in bulk?
                    *delta = reference_delta;
                }
                continue
            }
            // FIXME: .count()
            n if n == range.clone().count() => {
                // All points in this contour were referenced; no inferred deltas need to
                // be computed.
                continue;
            }
            _ => {
                // If the point number list includes some but not all of the points in a given contour, then inferred deltas must be derived for the points that were not included in the point number list.
            }
        }

        // FIXME: How do you do this?
        // ...the inferred deltas can be pre-computed before any processing for a specific instance is done.

        // FIXME: unwrap
        let usize_range =
            usize::safe_from(*range.start())..=usize::safe_from(*range.end());
        let contour_deltas = deltas.get_mut(usize_range.clone()).unwrap();
        let contour_deltas_len = contour_deltas.len();
        // for (number, delta) in contour_deltas.iter_mut().enumerate() /*.map(|(i, delta)| (usize_range.start() + i, delta))*/ {
        for number in 0..contour_deltas_len {
            if contour_deltas[number].is_none() {
                // This is an unreferenced point
                // > First, for any un-referenced point, identify the nearest points before and after, in point number order, that are referenced. Note that the same referenced points will be used for calculating both X and Y inferred deltas. If there is no lower point number from that contour that was referenced, then the highest, referenced point number from that contour is used. Similarly, if no higher point number from that contour was referenced, then the lowest, referenced point number is used.
                let number_u32 = number as u32;
                // NOTE(unwrap): Due to checks above regarding the number of referenced points we should always find a next/prev point
                let next: usize = referenced
                    .range(number_u32..)
                    .chain(referenced.range(0..number_u32))
                    .copied()
                    .map(SafeFrom::safe_from)
                    .next()
                    .unwrap();
                let prev: usize = referenced
                    .range(number_u32..)
                    .chain(referenced.range(0..number_u32))
                    .rev()
                    .copied()
                    .map(SafeFrom::safe_from)
                    .next()
                    .unwrap();

                infer_delta(
                    number,
                    prev,
                    next,
                    &simple_glyph.coordinates,
                    usize::safe_from(start),
                    contour_deltas,
                )
            }
        }
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
    prev: usize,
    next: usize,
    coordinates: &[Point],
    contour_start: usize,
    contour_deltas: &mut [Option<(f32, f32)>],
) {
    // https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#inferred-deltas-for-un-referenced-point-numbers
    let prev_coord = coordinates[contour_start + prev]; // FIXME: can these panic
    let target_coord = coordinates[contour_start + target];
    let next_coord = coordinates[contour_start + next];
    // These are Options but are assumed to be Some since they came via the referenced set
    let prev_delta = contour_deltas[prev].unwrap();
    let next_delta = contour_deltas[next].unwrap();

    // X
    let delta_x = do_infer(prev_coord.0, target_coord.0, next_coord.0, prev_delta.0, next_delta.0);
    let delta_y = do_infer(prev_coord.1, target_coord.1, next_coord.1, prev_delta.1, next_delta.1);
    contour_deltas[target] = Some((delta_x, delta_y));
}

fn do_infer(prev_coord: i16, target_coord: i16, next_coord: i16, prev_delta: f32, next_delta: f32) -> f32 {
    if prev_coord == next_coord {
        if prev_delta == next_delta {
            prev_delta
        } else {
            0.
        }
    } else {
        if target_coord <= prev_coord.min(next_coord) {
            if prev_coord < next_coord {
                prev_delta
            } else {
                next_delta
            }
        } else if target_coord >= prev_coord.max(next_coord) {
            if prev_coord > next_coord {
                prev_delta
            } else {
                next_delta
            }
        } else {
            // But if the coordinate of the target point is not between the coordinates of the adjacent points, then the inferred delta is the delta for whichever of the adjacent points is closer in the given direction.
            // If the coordinate of the target point is between the coordinates of the adjacent points, then a delta is interpolated
            /* target point delta is derived from the adjacent point deltas using linear interpolation */
            // Note: The logical flow of the algorithm to this point implies that the coordinates of the two adjacent points are different. This avoids a division by zero in the following calculations that would otherwise occur.
            let proportion = (target_coord as f32 - prev_coord as f32)
                / (next_coord as f32 - prev_coord as f32);
            (1. - proportion) * prev_delta + proportion * next_delta
        }
    }
}


struct ContourPoint {
    contour: u16,
    x: f32,
    y: f32,
    state: ContourPointState,
}

enum ContourPointState {
    Untouched,
    Touched,
}

impl ContourPoint {
    fn new(contour: u16, point: Point) -> Self {
        ContourPoint {
            contour,
            x: point.0 as f32,
            y: point.1 as f32,
            state: ContourPointState::Untouched,
        }
    }

    fn update(&mut self, delta: (f32, f32)) {
        self.state = ContourPointState::Touched;
        self.x = self.x + delta.0;
        self.y = self.y + delta.1;
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
        user_tuple: &UserTuple,
        avar: Option<&AvarTable>,
    ) -> Result<OwnedTuple, ParseError> {
        let mut tuple = TinyVec::with_capacity(user_tuple.0.len());
        let mut avar_iter = avar.map(|avar| avar.segment_maps());
        for (i, (axis, user_value)) in self.axes().zip(user_tuple.0.iter()).enumerate() {
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
    use crate::tables::variable_fonts::avar;
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

        let varied = apply(
            glyph,
            glyph_index,
            &instance.coordinates,
            &gvar,
            &fvar,
            avar.as_ref(),
        )?;

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
        let tuple = fvar.normalize(&instance.coordinates, avar.as_ref())?;
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

    #[test]
    fn test_prev_next_referenced_point() {
        let referenced: BTreeSet<_> = IntoIterator::into_iter([5u32, 9, 10, 13, 18]).collect();

        let number = 2;
        let next = referenced
            .range(number..)
            .chain(referenced.range(0..number))
            .copied()
            .next();
        let prev = referenced
            .range(number..)
            .chain(referenced.range(0..number))
            .rev()
            .copied()
            .next(); // Does this work?
        assert_eq!(next, Some(5u32));
        assert_eq!(prev, Some(18u32));

        let number = 7;
        let next = referenced
            .range(number..)
            .chain(referenced.range(0..number))
            .copied()
            .next();
        let prev = referenced
            .range(number..)
            .chain(referenced.range(0..number))
            .rev()
            .copied()
            .next(); // Does this work?
        assert_eq!(next, Some(9u32));
        assert_eq!(prev, Some(5u32));

        let number = 20;
        let next = referenced
            .range(number..)
            .chain(referenced.range(0..number))
            .copied()
            .next();
        let prev = referenced
            .range(number..)
            .chain(referenced.range(0..number))
            .rev()
            .copied()
            .next(); // Does this work?
        assert_eq!(next, Some(5u32));
        assert_eq!(prev, Some(18u32));
    }
}
