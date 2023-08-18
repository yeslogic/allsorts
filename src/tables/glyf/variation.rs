use crate::error::ParseError;
use crate::tables::variable_fonts::avar::AvarTable;
use crate::tables::variable_fonts::fvar::{FvarTable, VariationAxisRecord};
use crate::tables::variable_fonts::UserTuple;
use crate::tables::{F2Dot14, Fixed};
use tinyvec::TinyVec;

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
        user_tuple: &UserTuple<'_>,
        avar: Option<&AvarTable<'_>>,
    ) -> Result<TinyVec<[F2Dot14; 4]>, ParseError> {
        let mut tuple = TinyVec::with_capacity(user_tuple.0.len());
        let mut avar_iter = avar.map(|avar| avar.segment_maps());
        for (_, (axis, user_value)) in self.axes().zip(user_tuple.0.iter()).enumerate() {
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
    use crate::tables::variable_fonts::fvar::{FvarTable, VariationAxisRecord};
    use crate::tables::{FontTableProvider, NameTable};
    use crate::tag;
    use crate::tests::read_fixture;

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
