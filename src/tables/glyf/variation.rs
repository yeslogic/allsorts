use tinyvec::{tiny_vec, TinyVec};

use crate::error::ParseError;
use crate::tables::glyf::GlyfRecord;
use crate::tables::variable_fonts::avar::{AvarTable, AxisValueMap};
use crate::tables::variable_fonts::fvar::{FvarTable, VariationAxisRecord};
use crate::tables::variable_fonts::gvar::GvarTable;
use crate::tables::variable_fonts::{
    Gvar, Tuple, TupleVariationHeader, TupleVariationStore, UserTuple,
};
use crate::tables::{F2Dot14, Fixed};

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
    glyph: &GlyfRecord,
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
    let variations = gvar.glyph_variation_data(glyph_index, glyph.number_of_coordinates()?)?;
    let applicable = determine_applicable(gvar, &instance, &variations);
    dbg!(applicable);

    // Now the deltas need to be calculated for each point.
    // The delta is multiplied by the scalar. The sum of deltas is applied to the default position

    Err(ParseError::NotImplemented)
}

fn determine_applicable<'a, 'data>(
    gvar: &GvarTable<'data>,
    instance: &OwnedTuple,
    variations: &'a TupleVariationStore<'data, Gvar>,
) -> Vec<(F2Dot14, &'a TupleVariationHeader<'data, Gvar>)> {
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
            let scalar = start_coords
                .iter()
                .zip(end_coords.iter())
                .zip(instance.iter().copied())
                .zip(peak_coords.0.iter())
                .map(|(((start, end), instance), peak)| {
                    // If peak is zero or not contained by the region of applicability then it does not
                    if peak == F2Dot14::from(0) {
                        // If the peak is zero for some axis, then ignore the axis.
                        F2Dot14::from(1)
                    } else if (start..=end).contains(&instance) {
                        // The region is applicable: calculate a per-axis scalar as a proportion
                        // of the proximity of the instance to the peak within the region.
                        if instance == peak {
                            F2Dot14::from(1)
                        } else if instance < peak {
                            (instance - start) / (peak - start)
                        } else {
                            // instance > peak
                            (end - instance) / (end - peak)
                        }
                    } else {
                        // If the instance coordinate is out of range for some axis, then the region and its
                        // associated deltas are not applicable.
                        F2Dot14::from(0)
                    }
                })
                .fold(F2Dot14::from(1), |scalar, axis_scalar| scalar * axis_scalar);

            (scalar != F2Dot14::from(0)).then(|| (scalar, header))
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
        let glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
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
        let glyph = &glyf.records[usize::from(glyph_index)];

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
}
