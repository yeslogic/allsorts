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

impl<'a> ReadBinaryDep<'a> for LocaTable<'a> {
    type Args = (usize, IndexToLocFormat);
    type HostType = Self;

    /// Read a `loca` table from `ctxt`
    ///
    /// * `num_glyphs` is the number of glyphs in the font. The value for `num_glyphs` is found in
    ///   the 'maxp' table.
    /// * `index_to_loc_format` specifies whether the offsets in the `loca` table are short or
    ///   long. This value can be read from the `head` table.
    fn read_dep(
        ctxt: &mut ReadCtxt<'a>,
        (num_glyphs, index_to_loc_format): (usize, IndexToLocFormat),
    ) -> Result<Self, ParseError> {
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
    pub fn iter<'b>(&'b self) -> LocaOffsetsIter<'a, 'b> {
        LocaOffsetsIter {
            offsets: &self,
            index: 0,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            LocaOffsets::Short(array) => array.len(),
            LocaOffsets::Long(array) => array.len(),
        }
    }
}

impl<'a, 'b> Iterator for LocaOffsetsIter<'a, 'b> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.offsets.len() {
            let item = match self.offsets {
                LocaOffsets::Short(array) => u32::from(array.get_item(self.index)) * 2,
                LocaOffsets::Long(array) => array.get_item(self.index),
            };

            self.index += 1;
            Some(item)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = match self.offsets {
            LocaOffsets::Short(array) => array.len(),
            LocaOffsets::Long(array) => array.len(),
        };

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

    impl<'a> WriteBinaryDep<Self> for LocaTable {
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
