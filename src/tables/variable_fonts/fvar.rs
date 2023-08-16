#![deny(missing_docs)]

//! `fvar` Font Variations Table
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/fvar>

use crate::binary::read::{
    ReadArray, ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope, ReadUnchecked,
};
use crate::binary::{U16Be, U32Be};
use crate::error::ParseError;
use crate::tables::Fixed;

/// `fvar` font Variations Table
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#fvar-header>
pub struct FvarTable<'a> {
    /// Major version number of the font variations table
    pub major_version: u16,
    /// Minor version number of the font variations table
    pub minor_version: u16,
    /// The number of variation axes in the font (the number of records in the axes array).
    axis_count: u16,
    /// The size in bytes of each VariationAxisRecord
    axis_size: u16,
    axes_array: &'a [u8],
    /// The number of named instances defined in the font (the number of records in the instances array).
    instance_count: u16,
    /// The size in bytes of each InstanceRecord
    instance_size: u16,
    instance_array: &'a [u8],
}

/// Variation axis
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#variationaxisrecord>
#[derive(Eq, PartialEq, Debug)]
pub struct VariationAxisRecord {
    /// Tag identifying the design variation for the axis.
    pub axis_tag: u32,
    /// The minimum coordinate value for the axis.
    pub min_value: Fixed,
    /// The default coordinate value for the axis.
    pub default_value: Fixed,
    /// The maximum coordinate value for the axis.
    pub max_value: Fixed,
    /// Axis qualifiers.
    pub flags: u16,
    /// The name ID for entries in the `name` table that provide a display name for this axis.
    pub axis_name_id: u16,
}

/// Variation instance record
///
/// Instances are like named presets for a variable font. Each instance has name and a value
/// for each variation axis.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#instancerecord>
#[derive(Debug)]
pub struct InstanceRecord<'a> {
    /// The name ID for entries in the `name` table that provide subfamily names for this instance.
    pub subfamily_name_id: u16,
    /// Flags
    pub flags: u16,
    /// The coordinates array for this instance.
    pub coordinates: ReadArray<'a, Fixed>,
    /// Optional. The name ID for entries in the `name` table that provide PostScript names for this instance.
    pub post_script_name_id: Option<u16>,
}

impl FvarTable<'_> {
    /// Returns an iterator over the variation axes of the font.
    pub fn axes(&self) -> impl Iterator<Item = Result<VariationAxisRecord, ParseError>> + '_ {
        let axis_size = usize::from(self.axis_size);
        (0..usize::from(self.axis_count)).map(move |i| {
            let offset = i * axis_size;
            self.axes_array
                .get(offset..(offset + axis_size))
                .ok_or_else(|| ParseError::BadIndex)
                .and_then(|data| ReadScope::new(data).read::<VariationAxisRecord>())
        })
    }

    /// Returns an iterator over the pre-defined instances in the font.
    pub fn instances(&self) -> impl Iterator<Item = Result<InstanceRecord<'_>, ParseError>> + '_ {
        let instance_size = usize::from(self.instance_size);
        (0..usize::from(self.instance_count)).map(move |i| {
            let offset = i * instance_size;
            self.instance_array
                .get(offset..(offset + instance_size))
                .ok_or_else(|| ParseError::BadIndex)
                .and_then(|data| {
                    ReadScope::new(data)
                        .read_dep::<InstanceRecord<'_>>((instance_size, self.axis_count))
                })
        })
    }
}

impl<'b> ReadBinary for FvarTable<'b> {
    type HostType<'a> = FvarTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let scope = ctxt.scope();
        let major_version = ctxt.read_u16be()?;
        ctxt.check_version(major_version == 1)?;
        let minor_version = ctxt.read_u16be()?;
        let axes_array_offset = ctxt.read_u16be()?;
        let _reserved = ctxt.read_u16be()?;
        let axis_count = ctxt.read_u16be()?;
        let axis_size = ctxt.read_u16be()?;
        let instance_count = ctxt.read_u16be()?;
        let instance_size = ctxt.read_u16be()?;

        let axes_length = usize::from(axis_count) * usize::from(axis_size);
        let instance_length = usize::from(instance_count) * usize::from(instance_size);
        let mut data_ctxt = scope.offset(usize::from(axes_array_offset)).ctxt();
        let axes_array = data_ctxt.read_slice(axes_length)?;
        let instance_array = data_ctxt.read_slice(instance_length)?;

        Ok(FvarTable {
            major_version,
            minor_version,
            axis_count,
            axis_size,
            axes_array,
            instance_count,
            instance_size,
            instance_array,
        })
    }
}

impl ReadFrom for VariationAxisRecord {
    type ReadType = ((U32Be, Fixed, Fixed), (Fixed, U16Be, U16Be));

    fn from(
        ((axis_tag, min_value, default_value), (max_value, flags, axis_name_id)): (
            (u32, Fixed, Fixed),
            (Fixed, u16, u16),
        ),
    ) -> Self {
        VariationAxisRecord {
            axis_tag,
            min_value,
            default_value,
            max_value,
            flags,
            axis_name_id,
        }
    }
}

impl ReadBinaryDep for InstanceRecord<'_> {
    type Args<'a> = (usize, u16);
    type HostType<'a> = InstanceRecord<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        (record_size, axis_count): (usize, u16),
    ) -> Result<Self::HostType<'a>, ParseError> {
        // TODO: Accept axis_count as usize too
        let axis_count = usize::from(axis_count);

        let subfamily_name_id = ctxt.read_u16be()?;
        let flags = ctxt.read_u16be()?;
        let coordinates = ctxt.read_array(axis_count)?;
        // If the record size is larger than the size of the subfamily_name_id, flags,
        // and coordinates then the optional post_script_name_id is present.
        let post_script_name_id = (record_size > axis_count * Fixed::SIZE + 4)
            .then(|| ctxt.read_u16be())
            .transpose()?;

        Ok(InstanceRecord {
            subfamily_name_id,
            flags,
            coordinates,
            post_script_name_id,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font_data::FontData;
    use crate::tables::{FontTableProvider, NameTable};
    use crate::tag;
    use crate::tests::read_fixture;

    #[test]
    fn fvar() {
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
        let name_table_data = table_provider
            .read_table_data(tag::NAME)
            .expect("unable to read name table data");
        let name_table = ReadScope::new(&name_table_data)
            .read::<NameTable<'_>>()
            .unwrap();

        let expected = [
            VariationAxisRecord {
                axis_tag: tag!(b"wght"),
                min_value: <Fixed as From<i32>>::from(100),
                default_value: <Fixed as From<i32>>::from(400),
                max_value: <Fixed as From<i32>>::from(900),
                flags: 0,
                axis_name_id: 279,
            },
            VariationAxisRecord {
                axis_tag: tag!(b"wdth"),
                min_value: <Fixed as From<f32>>::from(62.5),
                default_value: <Fixed as From<i32>>::from(100),
                max_value: <Fixed as From<i32>>::from(100),
                flags: 0,
                axis_name_id: 280,
            },
            VariationAxisRecord {
                axis_tag: tag!(b"CTGR"),
                min_value: <Fixed as From<i32>>::from(0),
                default_value: <Fixed as From<i32>>::from(0),
                max_value: <Fixed as From<i32>>::from(100),
                flags: 0,
                axis_name_id: 281,
            },
        ];
        assert_eq!(
            fvar.axes().collect::<Result<Vec<_>, _>>().unwrap(),
            expected
        );

        let instances = fvar.instances().collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(instances.len(), 72);
        let first = instances.first().unwrap();
        let subfamily_name = name_table
            .english_string_for_id(first.subfamily_name_id)
            .unwrap();
        assert_eq!(subfamily_name, "Thin");
        // axis="wght" value="100.0", axis="wdth" value="100.0", axis="CTGR" value="0.0"
        let coordinates = [
            <Fixed as From<f32>>::from(100.),
            <Fixed as From<f32>>::from(100.),
            <Fixed as From<f32>>::from(0.),
        ];
        assert_eq!(first.coordinates.iter().collect::<Vec<_>>(), coordinates);

        let last = instances.last().unwrap();
        let subfamily_name = name_table
            .english_string_for_id(last.subfamily_name_id)
            .unwrap();
        assert_eq!(subfamily_name, "Display ExtraCondensed Black");
        //  axis="wght" value="900.0", axis="wdth" value="62.5", axis="CTGR" value="100.0"
        let coordinates = [
            <Fixed as From<f32>>::from(900.),
            <Fixed as From<f32>>::from(62.5),
            <Fixed as From<f32>>::from(100.),
        ];
        assert_eq!(last.coordinates.iter().collect::<Vec<_>>(), coordinates);
    }
}
