#![deny(missing_docs)]

//! Common tables pertaining to variable fonts.

use crate::binary::read::{ReadArray, ReadBinaryDep, ReadCtxt, ReadScope};
use crate::binary::{I16Be, U16Be, I8, U8};
use crate::error::ParseError;
use crate::tables::F2Dot14;
use crate::SafeFrom;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::iter;
use std::marker::PhantomData;

pub mod cvar;
pub mod fvar;
pub mod gvar;
pub mod stat;

/// Flag indicating that some or all tuple variation tables reference a shared set of “point”
/// numbers.
///
/// These shared numbers are represented as packed point number data at the start of the serialized
/// data.
const SHARED_POINT_NUMBERS: u16 = 0x8000;
/// Mask for the low bits to give the number of tuple variation tables.
const COUNT_MASK: u16 = 0x0FFF;
/// Flag indicating the data type used for point numbers in this run.
///
/// If set, the point numbers are stored as unsigned 16-bit values (uint16); if clear, the point
/// numbers are stored as unsigned bytes (uint8).
const POINTS_ARE_WORDS: u8 = 0x80;
/// Mask for the low 7 bits of the control byte to give the number of point number elements, minus
/// 1.
const POINT_RUN_COUNT_MASK: u8 = 0x7F;

/// Flag indicating that this tuple variation header includes an embedded peak tuple record,
/// immediately after the tupleIndex field.
///
/// If set, the low 12 bits of the tupleIndex value are ignored.
///
/// Note that this must always be set within the `cvar` table.
const EMBEDDED_PEAK_TUPLE: u16 = 0x8000;
/// Flag indicating that this tuple variation table applies to an intermediate region within the
/// variation space.
///
/// If set, the header includes the two intermediate-region, start and end tuple records,
/// immediately after the peak tuple record (if present).
const INTERMEDIATE_REGION: u16 = 0x4000;
/// Flag indicating that the serialized data for this tuple variation table includes packed “point”
/// number data.
///
/// If set, this tuple variation table uses that number data; if clear, this tuple variation table
/// uses shared number data found at the start of the serialized data for this glyph variation data
/// or 'cvar' table.
const PRIVATE_POINT_NUMBERS: u16 = 0x2000;
/// Mask for the low 12 bits to give the shared tuple records index.
const TUPLE_INDEX_MASK: u16 = 0x0FFF;

/// Flag indicating that this run contains no data (no explicit delta values are stored), and that
/// the deltas for this run are all zero.
const DELTAS_ARE_ZERO: u8 = 0x80;
/// Flag indicating the data type for delta values in the run.
///
/// If set, the run contains 16-bit signed deltas (int16); if clear, the run contains 8-bit signed
/// deltas (int8).
const DELTAS_ARE_WORDS: u8 = 0x40;
/// Mask for the low 6 bits to provide the number of delta values in the run, minus one.
const DELTA_RUN_COUNT_MASK: u8 = 0x3F;

/// Coordinate array specifying a position within the font’s variation space.
///
/// The number of elements must match the axisCount specified in the `fvar` table.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#tuple-records>
pub type Tuple<'a> = ReadArray<'a, F2Dot14>;

/// Phantom type for TupleVariationStore from a `gvar` table.
pub enum Gvar {}
/// Phantom type for TupleVariationStore from a `CVT` table.
pub enum Cvar {}

/// Tuple Variation Store Header.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#tuple-variation-store-header>
pub struct TupleVariationStore<'a, T> {
    /// The number of points in the glyph this store is for
    num_points: u32,
    /// A packed field. The high 4 bits are flags (see below), and the low 12 bits are the number
    /// of tuple variation tables. The count can be any number between 1 and 4095.
    tuple_variation_flags_and_count: u16,
    /// Offset from the start of the table containing the tuple store to the serialized data.
    data_offset: u16,
    /// The serialized data block begins with shared “point” number data, followed by the variation
    /// data for the tuple variation tables.
    ///
    /// The shared point number data is optional: it is present if the corresponding flag is set in
    /// the `tuple_variation_flags_and_count` field of the header.
    shared_point_numbers: Option<PointNumbers>,
    /// Array of tuple variation headers.
    tuple_variation_headers: Vec<TupleVariationHeader<'a, T>>,
}

/// Tuple variation header.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#tuplevariationheader>
pub struct TupleVariationHeader<'a, T> {
    /// The size in bytes of the serialized data for this tuple variation table.
    variation_data_size: u16,
    /// A packed field. The high 4 bits are flags (see below). The low 12 bits are an index into a
    /// shared tuple records array.
    tuple_flags_and_index: u16,
    /// Peak tuple record for this tuple variation table — optional, determined by flags in the
    /// tupleIndex value.
    ///
    /// Note that this must always be included in the `cvar` table.
    peak_tuple: Option<Tuple<'a>>,
    /// The start and end tuples for the intermediate region.
    ///
    /// Presence determined by flags in the `tuple_flags_and_index` value.
    intermediate_region: Option<(Tuple<'a>, Tuple<'a>)>,
    /// The serialized data fro this Tuple Variation
    data: &'a [u8],
    variant: PhantomData<T>,
}

/// Glyph variation data.
///
/// (x, y) deltas for numbered points.
pub struct GvarVariationData<'a> {
    point_numbers: Cow<'a, PointNumbers>,
    x_coord_deltas: Vec<i16>,
    y_coord_deltas: Vec<i16>,
}

/// CVT variation data.
///
/// deltas for numbered CVTs.
pub struct CvarVariationData<'a> {
    point_numbers: Cow<'a, PointNumbers>,
    deltas: Vec<i16>,
}

#[derive(Clone)]
enum PointNumbers {
    All(u32),
    Specific(Vec<u16>),
}

struct PointNumbersIter<'a> {
    numbers: &'a PointNumbers,
    index: usize,
}

impl TupleVariationStore<'_, Gvar> {
    /// Retrieve the variation data for the variation tuple at the given index.
    // pub fn variation_data(&self, index: u16) -> Result<(Tuple<'_>, GvarVariationData<'_>), ParseError> {
    pub fn variation_data(&self, index: u16) -> Result<GvarVariationData<'_>, ParseError> {
        let header = self
            .tuple_variation_headers
            .get(usize::from(index))
            .ok_or(ParseError::BadIndex)?;
        // let tuple = self. header.tuple_index()

        // TODO: we need to return the Tuple with this
        // but they live in the parent table
        header.variation_data(self.num_points, self.shared_point_numbers.as_ref())
    }
}

impl<T> ReadBinaryDep for TupleVariationStore<'_, T> {
    type Args<'a> = (u16, u32);
    type HostType<'a> = TupleVariationStore<'a, T>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        (axis_count, num_points): (u16, u32),
    ) -> Result<Self::HostType<'a>, ParseError> {
        let axis_count = usize::from(axis_count);

        let scope = ctxt.scope();
        let tuple_variation_flags_and_count = ctxt.read_u16be()?;
        let tuple_variation_count = usize::from(tuple_variation_flags_and_count & COUNT_MASK);
        let data_offset = ctxt.read_u16be()?;

        // Now read the TupleVariationHeaders
        let mut tuple_variation_headers = (0..tuple_variation_count)
            .map(|_| ctxt.read_dep::<TupleVariationHeader<'_, T>>(axis_count))
            .collect::<Result<Vec<_>, _>>()?;

        // Read the serialized data for each tuple variation header
        let mut data_ctxt = scope.offset(data_offset.into()).ctxt(); // FIXME: into

        // Read shared point numbers if the flag indicates they are present
        let shared_point_numbers = ((tuple_variation_flags_and_count & SHARED_POINT_NUMBERS)
            == SHARED_POINT_NUMBERS)
            .then(|| read_packed_point_numbers(&mut data_ctxt, num_points))
            .transpose()?;

        // Populate the data slices on the headers
        for header in tuple_variation_headers.iter_mut() {
            header.data = data_ctxt.read_slice(header.variation_data_size.into())?;
        }

        Ok(TupleVariationStore {
            num_points,
            tuple_variation_flags_and_count,
            data_offset,
            shared_point_numbers,
            tuple_variation_headers,
        })
    }
}

impl PointNumbers {
    /// Returns the number of point numbers contained by this value
    pub fn len(&self) -> usize {
        match self {
            PointNumbers::All(n) => usize::safe_from(*n),
            PointNumbers::Specific(vec) => vec.len(),
        }
    }

    /// Iterate over the point numbers contained by this value.
    pub fn iter(&self) -> PointNumbersIter<'_> {
        PointNumbersIter {
            numbers: self,
            index: 0,
        }
    }
}

impl Iterator for PointNumbersIter<'_> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.numbers.len() {
            return None;
        }

        let index = self.index;
        self.index += 1;
        match self.numbers {
            PointNumbers::All(_n) => Some(index as u32),
            PointNumbers::Specific(numbers) => numbers.get(index).copied().map(u32::from),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.numbers.len() - self.index;
        (remaining, Some(remaining))
    }
}

/// Read packed point numbers for a glyph with `num_points` points.
///
/// `num_points` is expected to already have the four "phantom points" added to it.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#packed-point-numbers>
fn read_packed_point_numbers(
    ctxt: &mut ReadCtxt<'_>,
    num_points: u32,
) -> Result<PointNumbers, ParseError> {
    let count = read_count(ctxt)?;
    // If the first byte is 0, then a second count byte is not used. This value has a special
    // meaning: the tuple variation data provides deltas for all glyph points (including the
    // “phantom” points), or for all CVTs.
    if count == 0 {
        return Ok(PointNumbers::All(num_points));
    }

    let mut num_read = 0;
    let mut point_numbers = Vec::with_capacity(usize::from(count));
    while num_read < count {
        let control_byte = ctxt.read_u8()?;
        let point_run_count = u16::from(control_byte & POINT_RUN_COUNT_MASK) + 1;
        if (control_byte & POINTS_ARE_WORDS) == POINTS_ARE_WORDS {
            // Points are words (2 bytes)
            let array = ctxt.read_array::<U16Be>(point_run_count.into())?;
            point_numbers.extend(array.iter().scan(0u16, |prev, diff| {
                let number = *prev + diff;
                *prev = number;
                Some(number)
            }));
        } else {
            // Points are single bytes
            let array = ctxt.read_array::<U8>(point_run_count.into())?;
            point_numbers.extend(array.iter().scan(0u16, |prev, diff| {
                let number = *prev + u16::from(diff);
                *prev = number;
                Some(number)
            }));
        }
        num_read += point_run_count;
    }
    Ok(PointNumbers::Specific(point_numbers))
}

// The count may be stored in one or two bytes:
//
// * If the first byte is 0, then a second count byte is not used. This value has a special
//   meaning: the tuple variation data provides deltas for all glyph points (including the “phantom”
//   points), or for all CVTs.
// * If the first byte is non-zero and the high bit is clear (value is 1 to 127), then a second
//   count byte is not used. The point count is equal to the value of the first byte.
// * If the high bit of the first byte is set, then a second byte is used. The count is read from
//   interpreting the two bytes as a big-endian uint16 value with the high-order bit masked out.
fn read_count(ctxt: &mut ReadCtxt<'_>) -> Result<u16, ParseError> {
    let count1 = u16::from(ctxt.read_u8()?);
    let count = match count1 {
        0 => 0,
        1..=127 => count1,
        128.. => {
            let count2 = ctxt.read_u8()?;
            ((count1 & 0x7F) << 8) | u16::from(count2)
        }
    };
    Ok(count)
}

/// Read `num_deltas` packed deltas.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#packed-deltas>
fn read_packed_deltas(ctxt: &mut ReadCtxt<'_>, num_deltas: u32) -> Result<Vec<i16>, ParseError> {
    let mut deltas_read = 0;
    let mut deltas = Vec::with_capacity(usize::safe_from(num_deltas));

    while deltas_read < num_deltas {
        let control_byte = ctxt.read_u8()?;
        // FIXME: Handling of count (u16, u32, usize)
        let count = u16::from(control_byte & DELTA_RUN_COUNT_MASK) + 1; // value is stored - 1
        let ucount = usize::from(count);

        deltas.reserve(ucount);
        if (control_byte & DELTAS_ARE_ZERO) == DELTAS_ARE_ZERO {
            deltas.extend(iter::repeat(0).take(ucount));
        } else if (control_byte & DELTAS_ARE_WORDS) == DELTAS_ARE_WORDS {
            // Points are words (2 bytes)
            let array = ctxt.read_array::<I16Be>(ucount)?;
            deltas.extend(array.iter())
        } else {
            // Points are single bytes
            let array = ctxt.read_array::<I8>(ucount)?;
            deltas.extend(array.iter().map(i16::from));
        };
        deltas_read += u32::from(count);
    }

    Ok(deltas)
}

impl GvarVariationData<'_> {
    /// Iterates over the point numbers and (x, y) deltas.
    pub fn iter(&self) -> impl Iterator<Item = (u32, (i16, i16))> + '_ {
        let deltas = self
            .x_coord_deltas
            .iter()
            .copied()
            .zip(self.y_coord_deltas.iter().copied());
        self.point_numbers.iter().zip(deltas)
    }
}

impl<'data> TupleVariationHeader<'data, Gvar> {
    /// Read the variation data for `gvar`.
    ///
    /// `num_points` is the number of points in the glyph this variation relates to.
    fn variation_data<'a>(
        &'a self,
        num_points: u32,
        shared_point_numbers: Option<&'a PointNumbers>,
    ) -> Result<GvarVariationData<'_>, ParseError> {
        let mut ctxt = ReadScope::new(self.data).ctxt();

        let point_numbers = self.read_point_numbers(&mut ctxt, num_points, shared_point_numbers)?;
        let num_deltas = u32::try_from(point_numbers.len()).map_err(ParseError::from)?;

        // The deltas are stored X, followed by Y but the delta runs can span the boundary of the
        // two so they need to be read as a single span of packed deltas and then split.
        let mut x_coord_deltas = read_packed_deltas(&mut ctxt, 2 * num_deltas)?;
        let y_coord_deltas = x_coord_deltas.split_off(usize::safe_from(num_deltas));

        Ok(GvarVariationData {
            point_numbers,
            x_coord_deltas,
            y_coord_deltas,
        })
    }

    /// Returns the index of the shared tuple that this header relates to.
    ///
    /// The tuple index is an index into the shared tuples of the `Gvar` table. Pass this value
    /// to the [shared_tuple](gvar::Gvar::shared_tuple) method to retrieve the tuple.
    ///
    /// The value returned from this method will be `None` if the header has an embedded
    /// peak tuple.
    pub fn tuple_index(&self) -> Option<u16> {
        self.peak_tuple
            .is_none()
            .then(|| self.tuple_flags_and_index & TUPLE_INDEX_MASK)
    }

    /// Returns the embedded peak tuple if present.
    pub fn peak_tuple(&self) -> Option<&Tuple<'data>> {
        self.peak_tuple.as_ref()
    }
}

impl<'data> TupleVariationHeader<'data, Cvar> {
    /// Read the variation data for `cvar`.
    ///
    /// `num_cvts` is the number of CVTs in the CVT table.
    fn variation_data<'a>(
        &'a self,
        num_cvts: u32,
        shared_point_numbers: Option<&'a PointNumbers>,
    ) -> Result<CvarVariationData<'_>, ParseError> {
        let mut ctxt = ReadScope::new(self.data).ctxt();

        let point_numbers = self.read_point_numbers(&mut ctxt, num_cvts, shared_point_numbers)?;
        let num_deltas = u32::try_from(point_numbers.len()).map_err(ParseError::from)?;
        let deltas = read_packed_deltas(&mut ctxt, num_deltas)?;

        Ok(CvarVariationData {
            point_numbers,
            deltas,
        })
    }

    /// Returns the index of the shared tuple that this header relates to.
    ///
    /// The tuple index is an index into the shared tuples of the `Gvar` table. Pass this value
    /// to the [shared_tuple](gvar::Gvar::shared_tuple) method to retrieve the tuple.
    ///
    /// The value returned from this method will be `None` if the header has an embedded
    /// peak tuple.
    pub fn tuple_index(&self) -> Option<u16> {
        self.peak_tuple
            .is_none()
            .then(|| self.tuple_flags_and_index & TUPLE_INDEX_MASK)
    }

    // FIXME: This is mandatory for Cvar
    /// Returns the embedded peak tuple if present.
    pub fn peak_tuple(&self) -> Option<&Tuple<'data>> {
        self.peak_tuple.as_ref()
    }
}

impl<'data, T> TupleVariationHeader<'data, T> {
    /// Read the point numbers for this tuple.
    ///
    /// This method will return either the embedded private point numbers or the shared numbers
    /// if private points are not present.
    fn read_point_numbers<'a>(
        &'a self,
        ctxt: &mut ReadCtxt<'data>,
        num_points: u32,
        shared_point_numbers: Option<&'a PointNumbers>,
    ) -> Result<Cow<'_, PointNumbers>, ParseError> {
        // Read private point numbers if the flag indicates they are present
        let private_point_numbers =
            if (self.tuple_flags_and_index & PRIVATE_POINT_NUMBERS) == PRIVATE_POINT_NUMBERS {
                read_packed_point_numbers(ctxt, num_points).map(Some)?
            } else {
                None
            };

        // If there are private point numbers then we need to read that many points
        // otherwise we need to read as many points are specified by the shared points.
        //
        // Either private or shared point numbers should be present. If both are missing that's
        // invalid.
        private_point_numbers
            .map(Cow::Owned)
            .or_else(|| shared_point_numbers.map(Cow::Borrowed))
            .ok_or(ParseError::MissingValue)
    }
}

impl<T> ReadBinaryDep for TupleVariationHeader<'_, T> {
    type Args<'a> = usize;
    type HostType<'a> = TupleVariationHeader<'a, T>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        axis_count: usize,
    ) -> Result<Self::HostType<'a>, ParseError> {
        // The size in bytes of the serialized data for this tuple variation table.
        let variation_data_size = ctxt.read_u16be()?;
        // A packed field. The high 4 bits are flags (see below). The low 12 bits are an index into a
        // shared tuple records array.
        let tuple_flags_and_index = ctxt.read_u16be()?;
        // If this is absent then `tuple_flags_and_index` contains the index to one of the shared
        // tuple records to use instead:
        //
        // > Every tuple variation table has a peak n-tuple indicated either by an embedded tuple
        // > record (always true in the 'cvar' table) or by an index into a shared tuple records
        // > array (only in the 'gvar' table).
        // FIXME: This is not optional for Cvar
        let peak_tuple = ((tuple_flags_and_index & EMBEDDED_PEAK_TUPLE) == EMBEDDED_PEAK_TUPLE)
            .then(|| ctxt.read_array(axis_count))
            .transpose()?;
        let intermediate_region =
            if (tuple_flags_and_index & INTERMEDIATE_REGION) == INTERMEDIATE_REGION {
                let start = ctxt.read_array(axis_count)?;
                let end = ctxt.read_array(axis_count)?;
                Some((start, end))
            } else {
                None
            };
        Ok(TupleVariationHeader {
            variation_data_size,
            tuple_flags_and_index,
            peak_tuple,
            intermediate_region,
            data: &[], // filled in later
            variant: PhantomData,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::read::ReadScope;

    #[test]
    fn test_read_count() {
        let mut ctxt = ReadScope::new(&[0]).ctxt();
        assert_eq!(read_count(&mut ctxt).unwrap(), 0);
        let mut ctxt = ReadScope::new(&[0x32]).ctxt();
        assert_eq!(read_count(&mut ctxt).unwrap(), 50);
        let mut ctxt = ReadScope::new(&[0x81, 0x22]).ctxt();
        assert_eq!(read_count(&mut ctxt).unwrap(), 290);
    }

    #[test]
    fn test_read_packed_point_numbers() {
        let data = [0x0d, 0x0c, 1, 4, 4, 2, 1, 2, 3, 3, 2, 1, 1, 3, 4];
        let mut ctxt = ReadScope::new(&data).ctxt();

        let expected = vec![1, 5, 9, 11, 12, 14, 17, 20, 22, 23, 24, 27, 31];
        assert_eq!(
            read_packed_point_numbers(&mut ctxt, expected.len() as u32)
                .unwrap()
                .iter()
                .collect::<Vec<_>>(),
            expected
        );
    }

    #[test]
    fn test_read_packed_deltas() {
        let data = [
            0x03, 0x0A, 0x97, 0x00, 0xC6, 0x87, 0x41, 0x10, 0x22, 0xFB, 0x34,
        ];
        let mut ctxt = ReadScope::new(&data).ctxt();
        let expected = vec![10, -105, 0, -58, 0, 0, 0, 0, 0, 0, 0, 0, 4130, -1228];
        assert_eq!(
            read_packed_deltas(&mut ctxt, expected.len() as u32).unwrap(),
            expected
        );
    }
}
