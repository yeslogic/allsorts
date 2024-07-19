#![deny(missing_docs)]

//! `kern` table parsing.
//!
//! <https://learn.microsoft.com/en-us/typography/opentype/spec/kern>

use std::ops::Range;

use crate::{
    binary::{
        read::{ReadArray, ReadBinary, ReadCtxt, ReadFrom, ReadScope},
        I16Be, U16Be,
    },
    error::ParseError,
};

/// `kern` Kerning Table.
pub struct KernTable<'a> {
    /// Number of subtables in the kerning table.
    table_count: u16,
    data: &'a [u8],
}

/// Kerning data.
pub enum KernData<'a> {
    /// Format 0 kerning data (pairs).
    Format0(KernFormat0<'a>),
    /// Format 2 kerning data (2D array).
    Format2(KernFormat2<'a>),
}

/// Format 0 kerning data (pairs).
pub struct KernFormat0<'a> {
    /// The largest power of two less than or equal to the value of nPairs, multiplied by the size
    /// in bytes of an entry in the table.
    search_range: u16,
    /// This is calculated as log2 of the largest power of two less than or equal to the value of
    /// nPairs. This value indicates how many iterations of the search loop will have to be made.
    /// (For example, in a list of eight items, there would have to be three iterations of the
    /// loop).
    entry_selector: u16,
    /// The value of nPairs minus the largest power of two less than or equal to nPairs, and then
    /// multiplied by the size in bytes of an entry in the table.
    range_shift: u16,
    /// Array of KernPair records.
    kern_pairs: ReadArray<'a, KernPair>, // [nPairs]: KernPair,
}

/// Format 2 kerning data (2D array).
pub struct KernFormat2<'a> {
    row_width: u16,
    left_table: ClassTable<'a>,
    right_table: ClassTable<'a>,
    kerning_array: &'a [u8], // ReadArray<'a, I16Be>,
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
    coverage: u16,
    data: KernData<'a>,
}

impl ReadBinary for KernTable<'_> {
    type HostType<'a> = KernTable<'a>;

    fn read<'a>(
        ctxt: &mut crate::binary::read::ReadCtxt<'a>,
    ) -> Result<Self::HostType<'a>, ParseError> {
        let version = ctxt.read_u16be()?;
        ctxt.check_version(version == 0)?;
        let table_count = ctxt.read_u16be()?;

        // Do a pass to validate that there is enough data present to read all the subtables,
        // and determine a length to read.
        let start = ctxt.scope();
        let mut len = 0;
        for _ in 0..table_count {
            let version = ctxt.read_u16be()?;
            ctxt.check_version(version == 0)?;
            let subtable_length = ctxt.read_u16be().map(usize::from)?;
            let _ = ctxt.read_slice(subtable_length)?;
            len += subtable_length;
        }

        let data = start.ctxt().read_slice(len)?;

        Ok(KernTable { table_count, data })
    }
}

impl<'a> KernTable<'a> {
    /// Interate of the sub-tables of this `kern` table.
    pub fn sub_tables(&self) -> impl Iterator<Item = Result<KernSubtable<'a>, ParseError>> + 'a {
        let mut ctxt = ReadScope::new(self.data).ctxt();
        (0..self.table_count).map(move |_| {
            let start = ctxt.scope();
            let _version = ctxt.read_u16be()?;
            let _length = ctxt.read_u16be()?;
            let coverage = ctxt.read_u16be()?;
            let format = coverage >> 8;
            let data = match format {
                0 => Self::read_format0(&mut ctxt).map(KernData::Format0)?,
                2 => Self::read_format2(&mut ctxt, start).map(KernData::Format2)?,
                _ => return Err(ParseError::BadValue),
            };

            Ok(KernSubtable { coverage, data })
        })
    }

    // Format 0 is the only subtable format supported by Windows.
    fn read_format0(ctxt: &mut ReadCtxt<'a>) -> Result<KernFormat0<'a>, ParseError> {
        let n_pairs = ctxt.read_u16be()?;
        let search_range = ctxt.read_u16be()?;
        let entry_selector = ctxt.read_u16be()?;
        let range_shift = ctxt.read_u16be()?;
        let kern_pairs = ctxt.read_array(usize::from(n_pairs))?; // [nPairs]: KernPair,

        Ok(KernFormat0 {
            search_range,
            entry_selector,
            range_shift,
            kern_pairs,
        })
    }

    fn read_format2(
        ctxt: &mut ReadCtxt<'a>,
        start: ReadScope<'a>,
    ) -> Result<KernFormat2<'a>, ParseError> {
        let row_width = ctxt.read_u16be()?;
        let left_class_offset = ctxt.read_u16be()?;
        let right_class_offset = ctxt.read_u16be()?;
        let kerning_array_offset = ctxt.read_u16be()?;

        let left_table = start
            .offset(usize::from(left_class_offset))
            .read::<ClassTable<'_>>()?;
        let right_table = start
            .offset(usize::from(right_class_offset))
            .read::<ClassTable<'_>>()?;
        let kerning_array = start
            .offset(usize::from(kerning_array_offset))
            .ctxt()
            .read_slice(usize::from(row_width) * right_table.values.len())?;
        // .read_array(left_table.values.len() * right_table.values.len())?;

        Ok(KernFormat2 {
            row_width,
            left_table,
            right_table,
            kerning_array,
        })
    }
}

impl<'a> KernSubtable<'a> {
    /// True if table has horizontal data, false if vertical.
    pub fn is_horizontal(&self) -> bool {
        self.coverage & 1 != 0
    }

    /// If true the table has minimum values, otherwise the table has kerning values.
    pub fn is_minumum(&self) -> bool {
        self.coverage & (1 << 1) != 0
    }

    /// Is kerning is perpendicular to the flow of the text.
    pub fn is_cross_stream(&self) -> bool {
        self.coverage & (1 << 2) != 0
    }

    /// True if the the value in this table should replace the value currently being accumulated.
    pub fn is_override(&self) -> bool {
        self.coverage & (1 << 3) != 0
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
                    .map(|index| x.kern_pairs.get_item(index).value)
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
        }
    }
}

impl<'a> ClassTable<'a> {
    fn get(&self, glyph_id: u16) -> Option<u16> {
        let index = glyph_id.checked_sub(self.first_glyph).map(usize::from)?;
        if index < self.values.len() {
            Some(self.values.get_item(index))
        } else {
            None
        }
    }

    fn range(&self) -> Range<u16> {
        // NOTE(cast): values is contructed from a u16 length
        // FIXME: overflow
        self.first_glyph..(self.first_glyph + self.values.len() as u16)
    }
}
