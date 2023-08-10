//! Parsing and writing of the `loca` table.
//!
//! > The indexToLoc table stores the offsets to the locations of the glyphs in the font, relative
//! > to the beginning of the glyphData table.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/loca>

use crate::binary::read::{ReadArray, ReadBinaryDep, ReadCtxt};
use crate::binary::write::{WriteBinary, WriteContext};
use crate::binary::{U16Be, U32Be};
use crate::error::{ParseError, WriteError};
use crate::tables::IndexToLocFormat;

/// `loca` table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/loca>
#[derive(Clone, Debug)]
pub struct LocaTable<'a> {
    pub offsets: LocaOffsets<'a>,
}

#[derive(Clone, Debug)]
pub enum LocaOffsets<'a> {
    Short(ReadArray<'a, U16Be>),
    Long(ReadArray<'a, U32Be>),
}

pub struct LocaOffsetsIter<'a, 'b> {
    offsets: &'b LocaOffsets<'a>,
    index: usize,
}

impl<'b> ReadBinaryDep for LocaTable<'b> {
    type Args<'a> = (usize, IndexToLocFormat);
    type HostType<'a> = LocaTable<'a>;

    /// Read a `loca` table from `ctxt`
    ///
    /// * `num_glyphs` is the number of glyphs in the font. The value for `num_glyphs` is found in
    ///   the 'maxp' table.
    /// * `index_to_loc_format` specifies whether the offsets in the `loca` table are short or
    ///   long. This value can be read from the `head` table.
    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        (num_glyphs, index_to_loc_format): (usize, IndexToLocFormat),
    ) -> Result<Self::HostType<'a>, ParseError> {
        let offsets = match index_to_loc_format {
            IndexToLocFormat::Short => {
                // The actual local offset divided by 2 is stored. The value of n is numGlyphs + 1.
                LocaOffsets::Short(ctxt.read_array::<U16Be>(num_glyphs + 1)?)
            }
            IndexToLocFormat::Long => {
                // The actual local offset is stored. The value of n is numGlyphs + 1.
                LocaOffsets::Long(ctxt.read_array::<U32Be>(num_glyphs + 1)?)
            }
        };

        Ok(LocaTable { offsets })
    }
}

impl<'a> WriteBinary for LocaTable<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, loca: LocaTable<'a>) -> Result<(), WriteError> {
        match loca.offsets {
            LocaOffsets::Long(array) => ctxt.write_array(&array),
            LocaOffsets::Short(array) => ctxt.write_array(&array),
        }?;

        Ok(())
    }
}

impl<'a> LocaTable<'a> {
    pub fn empty() -> Self {
        LocaTable {
            offsets: LocaOffsets::Long(ReadArray::empty()),
        }
    }
}

impl<'a> LocaOffsets<'a> {
    /// Iterate the offsets in this table.
    pub fn iter<'b>(&'b self) -> LocaOffsetsIter<'a, 'b> {
        LocaOffsetsIter {
            offsets: self,
            index: 0,
        }
    }

    /// Returns the number of offsets in the table.
    pub fn len(&self) -> usize {
        match self {
            LocaOffsets::Short(array) => array.len(),
            LocaOffsets::Long(array) => array.len(),
        }
    }

    /// Get a specified offset from the table at `index`.
    pub fn get(&self, index: usize) -> Option<u32> {
        if index < self.len() {
            match self {
                LocaOffsets::Short(array) => Some(u32::from(array.get_item(index)) * 2),
                LocaOffsets::Long(array) => Some(array.get_item(index)),
            }
        } else {
            None
        }
    }

    /// Get the last offset in the table.
    ///
    /// Returns `None` if the table is empty.
    pub fn last(&self) -> Option<u32> {
        self.len().checked_sub(1).and_then(|index| self.get(index))
    }
}

impl<'a, 'b> Iterator for LocaOffsetsIter<'a, 'b> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: Use inspect when stable
        // https://github.com/rust-lang/rust/issues/91345
        self.offsets.get(self.index).map(|item| {
            self.index += 1;
            item
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.offsets.len();

        if self.index < len {
            let upper = len - self.index;
            (upper, Some(upper))
        } else {
            (0, Some(0))
        }
    }
}

pub mod owned {
    use std::convert::TryFrom;

    use super::{IndexToLocFormat, U16Be, U32Be, WriteContext, WriteError};
    use crate::binary::write::{WriteBinary, WriteBinaryDep};

    pub struct LocaTable {
        pub offsets: Vec<u32>,
    }

    impl WriteBinaryDep<Self> for LocaTable {
        type Output = ();
        type Args = IndexToLocFormat;

        fn write_dep<C: WriteContext>(
            ctxt: &mut C,
            loca: LocaTable,
            index_to_loc_format: Self::Args,
        ) -> Result<(), WriteError> {
            // 0 for short offsets (Offset16), 1 for long (Offset32).
            match index_to_loc_format {
                IndexToLocFormat::Short => {
                    match loca.offsets.last() {
                        Some(&last) if (last / 2) > u32::from(std::u16::MAX) => {
                            return Err(WriteError::BadValue)
                        }
                        _ => {}
                    }

                    // The actual loca offset divided by 2 is stored.
                    // https://docs.microsoft.com/en-us/typography/opentype/spec/loca#short-version
                    for offset in loca.offsets {
                        if offset & 1 == 1 {
                            // odd offsets can't use this format
                            return Err(WriteError::BadValue);
                        }
                        let short_offset = u16::try_from(offset / 2)?;
                        U16Be::write(ctxt, short_offset)?;
                    }

                    Ok(())
                }
                IndexToLocFormat::Long => ctxt.write_vec::<U32Be>(loca.offsets),
            }
        }
    }
}
