#![deny(missing_docs)]

//! `kern` table parsing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/kern>

use crate::{
    binary::{
        read::{ReadArray, ReadBinary, ReadCtxt, ReadFrom, ReadScope},
        I16Be, U16Be, U8,
    },
    error::ParseError,
};
use std::convert::TryFrom;

/// `kern` Kerning Table.
#[derive(Clone, Copy)]
pub struct KernTable<'a> {
    version: KernTableVersion,
    /// Number of subtables in the kerning table.
    table_count: u32,
    data: &'a [u8],
}

#[derive(Clone, Copy)]
enum KernTableVersion {
    /// Version of the kerning table, as defined in OpenType.
    KernTableVersion0,
    /// Version of the kerning table, as defined in Apple Advanced Typography.
    /// Contains extensions that are not supported in OpenType.
    KernTableVersion1,
}

/// Kerning data.
pub enum KernData<'a> {
    /// Format 0 kerning data (pairs).
    Format0(KernFormat0<'a>),
    /// Format 2 kerning data (2D array).
    Format2(KernFormat2<'a>),
    /// Format 3 kerning data (2D array).
    Format3(KernFormat3<'a>),
}

/// Format 0 kerning data (pairs).
pub struct KernFormat0<'a> {
    /// Array of KernPair records.
    kern_pairs: ReadArray<'a, KernPair>, // [nPairs]: KernPair,
}

/// Format 2 kerning data (2D array).
pub struct KernFormat2<'a> {
    left_table: ClassTable<'a>,
    right_table: ClassTable<'a>,
    kerning_array: &'a [u8], // ReadArray<'a, I16Be>,
}

/// Format 3 kerning data (2D array).
pub struct KernFormat3<'a> {
    kern_value_table: ReadArray<'a, I16Be>,
    left_class_table: ReadArray<'a, U8>,
    right_class_table: ReadArray<'a, U8>,
    right_class_count: usize,
    kern_index_table: ReadArray<'a, U8>,
}

/// Kerning value for glyph pair.
pub struct KernPair {
    /// The glyph index for the left-hand glyph in the kerning pair.
    left: u16,
    /// The glyph index for the right-hand glyph in the kerning pair.
    right: u16,
    /// The kerning value for the above pair, in font design units. If this value is greater than
    /// zero, the characters will be moved apart. If this value is less than zero, the character
    /// will be moved closer together.
    value: i16,
}

/// Glyph class table.
pub struct ClassTable<'a> {
    /// First glyph in class range.
    first_glyph: u16,
    values: ReadArray<'a, U16Be>,
}

/// Sub-table within `kern` table.
pub struct KernSubtable<'a> {
    coverage: KernCoverage,
    data: KernData<'a>,
}

enum KernCoverage {
    KernCoverageVersion0(u16),
    KernCoverageVersion1(u16),
}

impl ReadBinary for KernTable<'_> {
    type HostType<'a> = KernTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let version = match ctxt.read_u16be()? {
            0 => KernTableVersion::KernTableVersion0,
            1 => {
                let _padding = ctxt.read_u16be()?;
                KernTableVersion::KernTableVersion1
            }
            _ => return Err(ParseError::BadVersion),
        };
        let table_count = match version {
            KernTableVersion::KernTableVersion0 => u32::from(ctxt.read_u16be()?),
            KernTableVersion::KernTableVersion1 => ctxt.read_u32be()?,
        };
        let data = ctxt.scope().data();
        let kern = KernTable {
            version,
            table_count,
            data,
        };

        // Validate the sub-tables can be read.
        // Note that the sub-table `length` field can't be trusted as the very widely used
        // OpenSans font has an invalid value for this field. Avoid its use where possible.
        // https://github.com/fonttools/fonttools/issues/314#issuecomment-118116527
        kern.sub_tables().try_for_each(|table| table.map(drop))?;

        Ok(kern)
    }
}

impl<'a> KernTable<'a> {
    /// Iterate over the sub-tables of this `kern` table.
    pub fn sub_tables(&self) -> impl Iterator<Item = Result<KernSubtable<'a>, ParseError>> + 'a {
        let mut ctxt = ReadScope::new(self.data).ctxt();
        let version = self.version;

        (0..self.table_count).map(move |_| {
            let start = ctxt.scope();
            match version {
                KernTableVersion::KernTableVersion0 => {
                    let _version = ctxt.read_u16be()?;
                    let length = usize::from(ctxt.read_u16be()?);
                    let coverage = ctxt.read_u16be()?;
                    let format = coverage >> 8;
                    let data = match format {
                        0 => Self::read_format0(&mut ctxt).map(KernData::Format0)?,
                        2 => Self::read_format2(&mut ctxt, start, length).map(KernData::Format2)?,
                        _ => return Err(ParseError::BadValue),
                    };

                    Ok(KernSubtable {
                        coverage: KernCoverage::KernCoverageVersion0(coverage),
                        data,
                    })
                }
                KernTableVersion::KernTableVersion1 => {
                    let length = usize::try_from(ctxt.read_u32be()?)?;
                    let coverage = ctxt.read_u16be()?;
                    let _tuple_index = ctxt.read_u16be()?;
                    let format = coverage & 0x00FF;
                    let data = match format {
                        0 => Self::read_format0(&mut ctxt).map(KernData::Format0)?,
                        2 => Self::read_format2(&mut ctxt, start, length).map(KernData::Format2)?,
                        3 => Self::read_format3(&mut ctxt, length).map(KernData::Format3)?,
                        _ => return Err(ParseError::BadValue),
                    };

                    Ok(KernSubtable {
                        coverage: KernCoverage::KernCoverageVersion1(coverage),
                        data,
                    })
                }
            }
        })
    }

    // Format 0 is the only sub-table format supported by Windows.
    fn read_format0(ctxt: &mut ReadCtxt<'a>) -> Result<KernFormat0<'a>, ParseError> {
        let n_pairs = ctxt.read_u16be()?;
        let _search_range = ctxt.read_u16be()?;
        let _entry_selector = ctxt.read_u16be()?;
        let _range_shift = ctxt.read_u16be()?;
        let kern_pairs = ctxt.read_array(usize::from(n_pairs))?; // [nPairs]: KernPair,

        Ok(KernFormat0 { kern_pairs })
    }

    fn read_format2(
        ctxt: &mut ReadCtxt<'a>,
        start: ReadScope<'a>,
        length: usize,
    ) -> Result<KernFormat2<'a>, ParseError> {
        let _row_width = ctxt.read_u16be()?;
        let left_class_offset = ctxt.read_u16be()?;
        let right_class_offset = ctxt.read_u16be()?;
        let kerning_array_offset = usize::from(ctxt.read_u16be()?);

        let left_table = start
            .offset(usize::from(left_class_offset))
            .read::<ClassTable<'_>>()?;
        let right_table = start
            .offset(usize::from(right_class_offset))
            .read::<ClassTable<'_>>()?;
        // The kerning array is a 2-dimensional array of kerning values, with each row in the array
        // representing one left-hand glyph class, and each column representing one right-hand glyph
        // class.
        //
        // In order to compute the size of the kerning array without the (possibly unreliable)
        // subtable `length` field, we need to multiply the number of left-hand classes by the
        // number of right-hand classes. The `row_width` field presumably gives us the number of
        // right-hand classes, but there isn't a way to obtain the number of left-hand classes
        // without scanning the left-hand class table for the largest class number.
        //
        // As such, use the subtable `length` field. There appear to be _very_ few fonts in the
        // wild that use format 2 any way.
        let kerning_array = start
            .offset(kerning_array_offset)
            .ctxt()
            .read_slice(length - kerning_array_offset)?;

        Ok(KernFormat2 {
            left_table,
            right_table,
            kerning_array,
        })
    }

    fn read_format3(ctxt: &mut ReadCtxt<'a>, length: usize) -> Result<KernFormat3<'a>, ParseError> {
        // Assume that `length` can be trusted. Subtable format 3 is specific to `kern` version 1;
        // unlike version 0, it stores its subtable length as a uint32. As such, the issues that
        // affect version 0 (see comment on OpenSans above) shouldn't occur.
        //
        // Use `length` to establish a sub-context from which the format 3 sub-subtables are read.
        // This is to guard against a situation where `length` > sum_length_of_subsubtables, which
        // occurs in Apple's Skia font and causes a mis-read of subsequent sub-tables.
        let sub_body_length = length.checked_sub(8).ok_or(ParseError::BadEof)?;
        let mut sub_ctxt = ctxt.read_scope(sub_body_length)?.ctxt();

        let glyph_count = usize::from(sub_ctxt.read_u16be()?);
        let kern_value_count = usize::from(sub_ctxt.read_u8()?);
        let left_class_count = usize::from(sub_ctxt.read_u8()?);
        let right_class_count = usize::from(sub_ctxt.read_u8()?);
        let _flags = sub_ctxt.read_u8()?;

        let kern_value_table = sub_ctxt.read_array(kern_value_count)?;
        let left_class_table = sub_ctxt.read_array(glyph_count)?;
        let right_class_table = sub_ctxt.read_array(glyph_count)?;
        let kern_index_table = sub_ctxt.read_array(left_class_count * right_class_count)?;

        Ok(KernFormat3 {
            kern_value_table,
            left_class_table,
            right_class_table,
            right_class_count,
            kern_index_table,
        })
    }

    /// Create an owned version of this `kern` table.
    pub fn to_owned(&self) -> owned::KernTable {
        owned::KernTable {
            version: self.version,
            table_count: self.table_count,
            data: Box::from(self.data),
        }
    }
}

impl<'a> From<&'a owned::KernTable> for KernTable<'a> {
    fn from(kern: &'a owned::KernTable) -> Self {
        KernTable {
            version: kern.version,
            table_count: kern.table_count,
            data: &kern.data,
        }
    }
}

impl<'a> KernSubtable<'a> {
    /// True if table has horizontal data, false if vertical.
    pub fn is_horizontal(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & 1 != 0,
            KernCoverage::KernCoverageVersion1(c) => c & 0x8000 == 0,
        }
    }

    /// If true the table has minimum values, otherwise the table has kerning values.
    pub fn is_minimum(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & (1 << 1) != 0,
            KernCoverage::KernCoverageVersion1(_) => false,
        }
    }

    /// Is kerning is perpendicular to the flow of the text.
    pub fn is_cross_stream(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & (1 << 2) != 0,
            KernCoverage::KernCoverageVersion1(c) => c & 0x4000 != 0,
        }
    }

    /// True if the value in this table should replace the value currently being accumulated.
    pub fn is_override(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(c) => c & (1 << 3) != 0,
            KernCoverage::KernCoverageVersion1(_) => false,
        }
    }

    /// True if table has variation kerning values.
    pub fn has_variation(&self) -> bool {
        match self.coverage {
            KernCoverage::KernCoverageVersion0(_) => false,
            KernCoverage::KernCoverageVersion1(c) => c & 0x2000 != 0,
        }
    }

    /// Access the kerning data of this sub table.
    pub fn data(&self) -> &KernData<'a> {
        &self.data
    }
}

impl KernPair {
    fn search_key(&self) -> u32 {
        (u32::from(self.left) << 16) | u32::from(self.right)
    }
}

impl ReadFrom for KernPair {
    type ReadType = (U16Be, U16Be, I16Be);

    fn read_from((left, right, value): (u16, u16, i16)) -> Self {
        KernPair { left, right, value }
    }
}

impl ReadBinary for ClassTable<'_> {
    type HostType<'a> = ClassTable<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let first_glyph = ctxt.read_u16be()?;
        let n_glyphs = ctxt.read_u16be()?;
        let values = ctxt.read_array(usize::from(n_glyphs))?;

        Ok(ClassTable {
            first_glyph,
            values,
        })
    }
}

impl<'a> KernData<'a> {
    /// Lookup the kerning for a pair of glyphs
    pub fn lookup(&self, left: u16, right: u16) -> Option<i16> {
        match self {
            KernData::Format0(x) => {
                // The KernPair records must be ordered by combining the left and right values to
                // form an unsigned 32-bit integer (left as the high-order word), then ordering
                // records numerically using these combined values.
                let needle = (u32::from(left) << 16) | u32::from(right);
                x.kern_pairs
                    .binary_search_by(|pair| pair.search_key().cmp(&needle))
                    .ok()
                    .and_then(|index| x.kern_pairs.get_item(index))
                    .map(|pair| pair.value)
            }
            KernData::Format2(x) => {
                // Get the class of the left/right glyphs, then lookup the kerning value
                let left_class = x.left_table.get(left)?;
                let right_class = x.right_table.get(right)?;

                // The values in the right class table are stored pre-multiplied by the number of
                // bytes in a single kerning value, and the values in the left class table are
                // stored pre-multiplied by the number of bytes in one row. This eliminates a need
                // to multiply the row and column values together to determine the location of the
                // kerning value.
                ReadScope::new(x.kerning_array)
                    .offset(usize::from(left_class) + usize::from(right_class))
                    .read::<I16Be>()
                    .ok()
            }
            KernData::Format3(x) => {
                // Suppose you have two glyphs, L and R, and you wish to determine the kerning
                // value. You can do so using this pseudo-expression:
                // value = kernValue[kernIndex[leftClass[L] * rightClassCount + rightClass[R]]].
                let left_class = x.left_class_table.get_item(usize::from(left))?;
                let right_class = x.right_class_table.get_item(usize::from(right))?;
                let kern_index = x.kern_index_table.get_item(
                    usize::from(left_class) * x.right_class_count + usize::from(right_class),
                )?;
                x.kern_value_table.get_item(usize::from(kern_index))
            }
        }
    }
}

impl<'a> ClassTable<'a> {
    fn get(&self, glyph_id: u16) -> Option<u16> {
        let index = glyph_id.checked_sub(self.first_glyph).map(usize::from)?;
        self.values.get_item(index)
    }
}

/// Version of `kern` table that holds owned data
pub mod owned {
    use super::KernTableVersion;

    /// `kern` Kerning Table (owned version).
    pub struct KernTable {
        pub(super) version: KernTableVersion,
        /// Number of subtables in the kerning table.
        pub(super) table_count: u32,
        pub(super) data: Box<[u8]>,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        tables::{FontTableProvider, OpenTypeFont},
        tag,
        tests::read_fixture,
    };

    use super::*;

    #[test]
    fn parse() {
        let font_buffer = read_fixture("tests/fonts/opentype/OpenSans-Regular.ttf");
        let otf = ReadScope::new(&font_buffer)
            .read::<OpenTypeFont<'_>>()
            .unwrap();
        let table_provider = otf.table_provider(0).expect("error reading font file");

        let kern_data = table_provider
            .read_table_data(tag::KERN)
            .expect("unable to read kern data");
        let kern = ReadScope::new(&kern_data)
            .read::<KernTable<'_>>()
            .expect("unable to parse kern table");

        let subtables = kern
            .sub_tables()
            .collect::<Result<Vec<_>, _>>()
            .expect("error iterating sub-tables");

        assert_eq!(subtables.len(), 1);
        let sub_table = &subtables[0];
        assert!(sub_table.is_horizontal());
        assert!(!sub_table.is_minimum());
        let sub_table_data = &sub_table.data;
        // 'W' and 'A'
        let kerning = sub_table_data.lookup(58, 36);
        assert_eq!(kerning, Some(-82));
    }
}
