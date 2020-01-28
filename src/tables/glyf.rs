//! Parsing and writing of the `glyf` table.
//!
//! > This table contains information that describes the glyphs in the font in the TrueType outline
//! > format. Information regarding the rasterizer (scaler) refers to the TrueType rasterizer.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/glyf>

use std::convert::TryFrom;
use std::iter;

use bitflags::bitflags;
use itertools::Itertools;

use crate::binary::read::{ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope};
use crate::binary::write::{WriteBinary, WriteBinaryDep, WriteContext};
use crate::binary::{word_align, I16Be, U16Be, I8, U8};
use crate::error::{ParseError, WriteError};
use crate::tables::loca::{owned, LocaTable};
use crate::tables::{F2Dot14, IndexToLocFormat};

bitflags! {
    #[rustfmt::skip]
    pub struct SimpleGlyphFlag: u8 {
        const ON_CURVE_POINT                       = 0b00000001;
        const X_SHORT_VECTOR                       = 0b00000010;
        const Y_SHORT_VECTOR                       = 0b00000100;
        const REPEAT_FLAG                          = 0b00001000;
        const X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR = 0b00010000;
        const Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR = 0b00100000;
    }
}

bitflags! {
    pub struct CompositeGlyphFlag: u16 {
        /// Bit 0: If this is set, the arguments are 16-bit (uint16 or int16); otherwise, they are
        /// bytes (uint8 or int8).
        const ARG_1_AND_2_ARE_WORDS = 0x0001;
        /// Bit 1: If this is set, the arguments are signed xy values; otherwise, they are unsigned
        /// point numbers.
        const ARGS_ARE_XY_VALUES = 0x0002;
        /// Bit 2: For the xy values if the preceding is true.
        const ROUND_XY_TO_GRID = 0x0004;
        /// Bit 3: This indicates that there is a simple scale for the component. Otherwise, scale = 1.0.
        const WE_HAVE_A_SCALE = 0x0008;
        /// Bit 4: Reserved, set to 0
        /// Bit 5: Indicates at least one more glyph after this one.
        const MORE_COMPONENTS = 0x0020;
        /// Bit 6: The x direction will use a different scale from the y direction.
        const WE_HAVE_AN_X_AND_Y_SCALE = 0x0040;
        /// Bit 7: There is a 2 by 2 transformation that will be used to scale the component.
        const WE_HAVE_A_TWO_BY_TWO = 0x0080;
        /// Bit 8: Following the last component are instructions for the composite character.
        const WE_HAVE_INSTRUCTIONS = 0x0100;
        /// Bit 9: If set, this forces the aw and lsb (and rsb) for the composite to be equal to
        /// those from this original glyph. This works for hinted and unhinted characters.
        const USE_MY_METRICS = 0x0200;
        /// Bit 10: If set, the components of the compound glyph overlap.
        ///
        /// Use of this flag is not required in OpenType — that is, it is valid to have components
        /// overlap without having this flag set. It may affect behaviors in some platforms,
        /// however. (See Apple’s specification for details regarding behavior in Apple platforms.)
        /// When used, it must be set on the flag word for the first component. See additional
        /// remarks, above, for the similar OVERLAP_SIMPLE flag used in simple-glyph descriptions.
        const OVERLAP_COMPOUND = 0x0400;
        /// Bit 11: The composite is designed to have the component offset scaled.
        const SCALED_COMPONENT_OFFSET = 0x0800;
        /// Bit 12: The composite is designed not to have the component offset scaled.
        const UNSCALED_COMPONENT_OFFSET = 0x1000;
        // 0xE010 	Reserved 	Bits 4, 13, 14 and 15 are reserved: set to 0.
    }
}

/// `glyf` table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/glyf>
#[derive(Debug, PartialEq)]
pub struct GlyfTable<'a> {
    pub records: Vec<GlyfRecord<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum GlyfRecord<'a> {
    Empty,
    Present(ReadScope<'a>),
    Parsed(Glyph<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Glyph<'a> {
    pub number_of_contours: i16,
    pub bounding_box: BoundingBox,
    pub data: GlyphData<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum GlyphData<'a> {
    Simple(SimpleGlyph),
    Composite {
        glyphs: Vec<CompositeGlyph>,
        instructions: &'a [u8],
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct SimpleGlyph {
    pub end_pts_of_contours: Vec<u16>,
    pub instructions: Vec<u8>,
    pub flags: Vec<SimpleGlyphFlag>,
    pub coordinates: Vec<Point>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompositeGlyph {
    pub flags: CompositeGlyphFlag,
    pub glyph_index: u16,
    pub argument1: CompositeGlyphArgument,
    pub argument2: CompositeGlyphArgument,
    pub scale: Option<CompositeGlyphScale>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompositeGlyphArgument {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompositeGlyphScale {
    Scale(F2Dot14),
    XY { x_scale: F2Dot14, y_scale: F2Dot14 },
    Matrix([[F2Dot14; 2]; 2]),
}

pub struct CompositeGlyphs {
    pub glyphs: Vec<CompositeGlyph>,
    pub have_instructions: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point(pub i16, pub i16);

#[derive(Debug, PartialEq, Clone)]
pub struct BoundingBox {
    pub x_min: i16,
    pub x_max: i16,
    pub y_min: i16,
    pub y_max: i16,
}

impl<'a> ReadBinaryDep<'a> for GlyfTable<'a> {
    type Args = &'a LocaTable<'a>;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, loca: Self::Args) -> Result<Self, ParseError> {
        if loca.offsets.len() < 2 {
            return Err(ParseError::BadIndex);
        }

        let glyph_records = loca
            .offsets
            .iter()
            .tuple_windows()
            .map(|(start, end)| match end.checked_sub(start) {
                Some(0) => Ok(GlyfRecord::Empty),
                Some(length) => {
                    let glyph_scope = ctxt
                        .scope()
                        .offset_length(usize::try_from(start)?, usize::try_from(length)?)?;
                    Ok(GlyfRecord::Present(glyph_scope))
                }
                None => Err(ParseError::BadOffset),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(GlyfTable {
            records: glyph_records,
        })
    }
}

impl<'a> WriteBinaryDep<Self> for GlyfTable<'a> {
    type Output = owned::LocaTable;
    type Args = IndexToLocFormat;

    /// Write this glyf table into `ctxt`.
    ///
    /// ## A Note About Padding
    ///
    /// On the [loca table documentation](https://docs.microsoft.com/en-us/typography/opentype/spec/loca#long-version)
    /// at the bottom it states:
    ///
    /// > Note that the local offsets should be 32-bit aligned. Offsets which are not 32-bit
    /// > aligned may seriously degrade performance of some processors.
    ///
    /// On the [Recommendations for OpenType Fonts](https://docs.microsoft.com/en-us/typography/opentype/spec/recom#loca-table)
    /// page it states:
    ///
    /// > We recommend that local offsets should be 16-bit aligned, in both the short and long
    /// > formats of this table.
    ///
    /// On [Apple's loca documentation](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6loca.html)
    /// it says:
    ///
    /// > The glyph data is always word aligned.
    ///
    /// Elsewhere in the Apple docs they refer to long as 32-bits, so assuming word here means
    /// 16-bits.
    ///
    /// [An issue](https://github.com/MicrosoftDocs/typography-issues/issues/241) was raised against
    /// Microsoft's docs regarding this.
    /// Behdad Esfahbod [commented](https://github.com/MicrosoftDocs/typography-issues/issues/241#issuecomment-495265379):
    ///
    /// > All the requirements should be removed since 2019.
    /// >
    /// > In reality, in the short format, you are forced to do 16-bit alignment because of how
    /// > offsets are stored. In the long format, use alignment 1. We've been doing that in
    /// > fonttools for years and never ever heard a complaint whatsoever.
    ///
    /// So with this in mind we implement 16-bit alignment when `index_to_loc_format` is 0,
    /// and no alignment/padding otherwise.
    fn write_dep<C: WriteContext>(
        ctxt: &mut C,
        table: GlyfTable<'a>,
        index_to_loc_format: IndexToLocFormat,
    ) -> Result<Self::Output, WriteError> {
        let mut offsets: Vec<u32> = Vec::with_capacity(table.records.len() + 1);

        let start = ctxt.bytes_written();
        for record in table.records {
            let offset = ctxt.bytes_written();

            offsets.push(u32::try_from(ctxt.bytes_written() - start)?);

            match record {
                GlyfRecord::Empty => (),
                GlyfRecord::Present(glyph) => ReadScope::write(ctxt, glyph)?,
                GlyfRecord::Parsed(glyph) => Glyph::write(ctxt, glyph)?,
            }

            if index_to_loc_format == IndexToLocFormat::Short {
                let length = ctxt.bytes_written() - offset;
                let padded_length = word_align(length);
                ctxt.write_zeros(padded_length - length)?;
            }
        }

        // Add the final loca entry
        offsets.push(u32::try_from(ctxt.bytes_written() - start)?);

        Ok(owned::LocaTable { offsets })
    }
}

impl<'a> ReadBinary<'a> for Glyph<'a> {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let number_of_contours = ctxt.read_i16be()?;
        let bounding_box = ctxt.read::<BoundingBox>()?;

        if number_of_contours >= 0 {
            // Simple glyph
            // Cast is safe as we've checked value is positive above
            let glyph = ctxt.read_dep::<SimpleGlyph>(number_of_contours as u16)?;

            Ok(Glyph {
                number_of_contours,
                bounding_box,
                data: GlyphData::Simple(glyph),
            })
        } else {
            // Composite glyph
            let glyphs = ctxt.read::<CompositeGlyphs>()?;

            let instruction_length = if glyphs.have_instructions {
                usize::from(ctxt.read::<U16Be>()?)
            } else {
                0
            };
            let instructions = ctxt.read_slice(instruction_length)?;

            Ok(Glyph {
                number_of_contours,
                bounding_box,
                data: GlyphData::Composite {
                    glyphs: glyphs.glyphs,
                    instructions,
                },
            })
        }
    }
}

impl<'a> WriteBinary for Glyph<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, glyph: Glyph<'a>) -> Result<(), WriteError> {
        I16Be::write(ctxt, glyph.number_of_contours)?;
        BoundingBox::write(ctxt, glyph.bounding_box)?;
        match glyph.data {
            GlyphData::Simple(simple_glyph) => SimpleGlyph::write(ctxt, simple_glyph)?,
            GlyphData::Composite {
                glyphs,
                instructions,
            } => {
                for glyph in glyphs {
                    CompositeGlyph::write(ctxt, glyph)?;
                }
                if !instructions.is_empty() {
                    U16Be::write(ctxt, u16::try_from(instructions.len())?)?;
                    ctxt.write_bytes(instructions)?;
                }
            }
        }

        Ok(())
    }
}

impl<'a> ReadBinaryDep<'a> for SimpleGlyph {
    type Args = u16;
    type HostType = Self;

    fn read_dep(
        ctxt: &mut ReadCtxt<'a>,
        number_of_contours: Self::Args,
    ) -> Result<Self, ParseError> {
        let number_of_contours = usize::from(number_of_contours);
        let end_pts_of_contours: Vec<u16> = ctxt
            .read_array::<U16Be>(number_of_contours)?
            .iter()
            .collect();
        let instruction_length = ctxt.read::<U16Be>()?;
        let instructions = ctxt.read_slice(usize::from(instruction_length))?;
        // end_pts_of_contours stores the index of the end points.
        // Therefore the number of coordinates is the last index + 1
        let number_of_coordinates =
            usize::from(*end_pts_of_contours.last().ok_or(ParseError::BadIndex)?) + 1;

        // Read all the flags
        let mut flags = Vec::with_capacity(number_of_contours);
        while flags.len() < number_of_coordinates {
            let flag = ctxt.read::<SimpleGlyphFlag>()?;
            if flag.is_repeated() {
                let count = usize::from(ctxt.read::<U8>()?) + 1; // + 1 to include the current entry
                let repeat = iter::repeat(flag).take(count);
                flags.extend(repeat)
            } else {
                flags.push(flag);
            }
        }

        // Read all the x coordinates
        let x_coordinates = flags
            .iter()
            .map(|flag| {
                if flag.x_is_short() {
                    ctxt.read::<U8>()
                        .map(|val| i16::from(val) * flag.x_short_sign())
                } else if flag.x_is_same_or_positive() {
                    Ok(0)
                } else {
                    ctxt.read::<I16Be>()
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        // Read all y coordinates
        let y_coordinates = flags
            .iter()
            .map(|flag| {
                if flag.y_is_short() {
                    ctxt.read::<U8>()
                        .map(|val| i16::from(val) * flag.y_short_sign())
                } else if flag.y_is_same_or_positive() {
                    Ok(0)
                } else {
                    ctxt.read::<I16Be>()
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        // The x and y coordinates are stored as deltas against the previous point, with the first
        // one being implicitly against (0, 0). Here we resolve these deltas into absolute (x, y)
        // values and combine them into Points.
        let coordinates = x_coordinates
            .into_iter()
            .zip(y_coordinates.into_iter())
            .scan(Point(0, 0), |prev_point, (x, y)| {
                *prev_point = Point(prev_point.0 + x, prev_point.1 + y);
                Some(*prev_point)
            })
            .collect();

        Ok(SimpleGlyph {
            end_pts_of_contours,
            instructions: instructions.to_vec(),
            flags,
            coordinates,
        })
    }
}

impl<'a> WriteBinary for SimpleGlyph {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, glyph: SimpleGlyph) -> Result<(), WriteError> {
        assert!(glyph.flags.len() == glyph.coordinates.len());

        ctxt.write_vec::<U16Be>(glyph.end_pts_of_contours)?;
        U16Be::write(ctxt, u16::try_from(glyph.instructions.len())?)?;
        ctxt.write_bytes(&glyph.instructions)?;

        // Flags and coordinates are written without any attempt to compact them using
        // smaller representation, use of REPEAT, or X/Y_IS_SAME.
        // TODO: try to compact the values written

        // flags
        let mask = SimpleGlyphFlag::ON_CURVE_POINT; // ON_CURVE_POINT is the only flag that needs to carry through
        for flag in glyph.flags {
            U8::write(ctxt, (flag & mask).bits())?;
        }

        // x coordinates
        let mut prev_x = 0;
        for Point(x, _) in &glyph.coordinates {
            let delta_x = x - prev_x;
            I16Be::write(ctxt, delta_x)?;
            prev_x = *x;
        }

        // y coordinates
        let mut prev_y = 0;
        for Point(_, y) in &glyph.coordinates {
            let delta_y = y - prev_y;
            I16Be::write(ctxt, delta_y)?;
            prev_y = *y;
        }

        Ok(())
    }
}

impl<'a> ReadFrom<'a> for SimpleGlyphFlag {
    type ReadType = U8;

    fn from(flag: u8) -> Self {
        SimpleGlyphFlag::from_bits_truncate(flag)
    }
}

impl<'a> ReadBinary<'a> for CompositeGlyphs {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'_>) -> Result<Self, ParseError> {
        let mut have_instructions = false;
        let mut glyphs = Vec::new();
        loop {
            let flags = ctxt.read::<CompositeGlyphFlag>()?;
            let data = ctxt.read_dep::<CompositeGlyph>(flags)?;

            if flags.we_have_instructions() {
                have_instructions = true;
            }

            glyphs.push(data);

            if !flags.more_components() {
                break;
            }
        }

        Ok(CompositeGlyphs {
            glyphs,
            have_instructions,
        })
    }
}

impl SimpleGlyphFlag {
    pub fn is_on_curve(self) -> bool {
        self & Self::ON_CURVE_POINT == Self::ON_CURVE_POINT
    }

    pub fn x_is_short(self) -> bool {
        self & Self::X_SHORT_VECTOR == Self::X_SHORT_VECTOR
    }

    pub fn y_is_short(self) -> bool {
        self & Self::Y_SHORT_VECTOR == Self::Y_SHORT_VECTOR
    }

    pub fn is_repeated(self) -> bool {
        self & Self::REPEAT_FLAG == Self::REPEAT_FLAG
    }

    pub fn x_short_sign(self) -> i16 {
        if self.x_is_same_or_positive() {
            1
        } else {
            -1
        }
    }

    pub fn y_short_sign(self) -> i16 {
        if self.y_is_same_or_positive() {
            1
        } else {
            -1
        }
    }

    pub fn x_is_same_or_positive(self) -> bool {
        self & Self::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
            == Self::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
    }

    pub fn y_is_same_or_positive(self) -> bool {
        self & Self::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR
            == Self::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR
    }
}

impl<'a> ReadFrom<'a> for CompositeGlyphFlag {
    type ReadType = U16Be;

    fn from(flag: u16) -> Self {
        CompositeGlyphFlag::from_bits_truncate(flag)
    }
}

impl<'a> ReadBinaryDep<'a> for CompositeGlyphArgument {
    type Args = CompositeGlyphFlag;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, flags: Self::Args) -> Result<Self, ParseError> {
        let arg = match (flags.arg_1_and_2_are_words(), flags.args_are_xy_values()) {
            (true, true) => CompositeGlyphArgument::I16(ctxt.read_i16be()?),
            (true, false) => CompositeGlyphArgument::U16(ctxt.read_u16be()?),
            (false, true) => CompositeGlyphArgument::I8(ctxt.read_i8()?),
            (false, false) => CompositeGlyphArgument::U8(ctxt.read_u8()?),
        };

        Ok(arg)
    }
}

impl<'a> WriteBinary for CompositeGlyphArgument {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, arg: CompositeGlyphArgument) -> Result<(), WriteError> {
        match arg {
            CompositeGlyphArgument::U8(val) => U8::write(ctxt, val),
            CompositeGlyphArgument::I8(val) => I8::write(ctxt, val),
            CompositeGlyphArgument::U16(val) => U16Be::write(ctxt, val),
            CompositeGlyphArgument::I16(val) => I16Be::write(ctxt, val),
        }
    }
}

impl<'a> ReadBinaryDep<'a> for CompositeGlyph {
    type Args = CompositeGlyphFlag;
    type HostType = Self;

    fn read_dep(ctxt: &mut ReadCtxt<'a>, flags: Self::Args) -> Result<Self, ParseError> {
        let glyph_index = ctxt.read_u16be()?;
        let argument1 = ctxt.read_dep::<CompositeGlyphArgument>(flags)?;
        let argument2 = ctxt.read_dep::<CompositeGlyphArgument>(flags)?;

        let scale = if flags.we_have_a_scale() {
            Some(CompositeGlyphScale::Scale(ctxt.read::<F2Dot14>()?))
        } else if flags.we_have_an_x_and_y_scale() {
            Some(CompositeGlyphScale::XY {
                x_scale: ctxt.read::<F2Dot14>()?,
                y_scale: ctxt.read::<F2Dot14>()?,
            })
        } else if flags.we_have_a_two_by_two() {
            Some(CompositeGlyphScale::Matrix([
                [ctxt.read::<F2Dot14>()?, ctxt.read::<F2Dot14>()?],
                [ctxt.read::<F2Dot14>()?, ctxt.read::<F2Dot14>()?],
            ]))
        } else {
            None
        };

        Ok(CompositeGlyph {
            flags,
            glyph_index,
            argument1,
            argument2,
            scale,
        })
    }
}

impl<'a> WriteBinary for CompositeGlyph {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, glyph: CompositeGlyph) -> Result<(), WriteError> {
        U16Be::write(ctxt, glyph.flags.bits())?;
        U16Be::write(ctxt, glyph.glyph_index)?;
        CompositeGlyphArgument::write(ctxt, glyph.argument1)?;
        CompositeGlyphArgument::write(ctxt, glyph.argument2)?;
        if let Some(scale) = glyph.scale {
            CompositeGlyphScale::write(ctxt, scale)?;
        }
        Ok(())
    }
}

impl<'a> WriteBinary for CompositeGlyphScale {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, scale: CompositeGlyphScale) -> Result<(), WriteError> {
        match scale {
            CompositeGlyphScale::Scale(scale) => F2Dot14::write(ctxt, scale)?,
            CompositeGlyphScale::XY { x_scale, y_scale } => {
                F2Dot14::write(ctxt, x_scale)?;
                F2Dot14::write(ctxt, y_scale)?;
            }
            CompositeGlyphScale::Matrix(matrix) => {
                F2Dot14::write(ctxt, matrix[0][0])?;
                F2Dot14::write(ctxt, matrix[0][1])?;
                F2Dot14::write(ctxt, matrix[1][0])?;
                F2Dot14::write(ctxt, matrix[1][1])?;
            }
        }

        Ok(())
    }
}

impl<'a> ReadBinary<'a> for BoundingBox {
    type HostType = Self;

    fn read(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let x_min = ctxt.read::<I16Be>()?;
        let y_min = ctxt.read::<I16Be>()?;
        let x_max = ctxt.read::<I16Be>()?;
        let y_max = ctxt.read::<I16Be>()?;

        Ok(BoundingBox {
            x_min,
            y_min,
            x_max,
            y_max,
        })
    }
}

impl<'a> WriteBinary for BoundingBox {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, bbox: BoundingBox) -> Result<(), WriteError> {
        I16Be::write(ctxt, bbox.x_min)?;
        I16Be::write(ctxt, bbox.y_min)?;
        I16Be::write(ctxt, bbox.x_max)?;
        I16Be::write(ctxt, bbox.y_max)?;
        Ok(())
    }
}

struct SubsetGlyph<'a> {
    old_id: u16,
    record: GlyfRecord<'a>,
}

fn add_glyph(glyph_ids: &mut Vec<u16>, record: &mut GlyfRecord<'_>) {
    match record {
        GlyfRecord::Parsed(Glyph {
            data: GlyphData::Composite { glyphs, .. },
            ..
        }) => {
            for composite_glyph in glyphs.iter_mut() {
                let new_id = glyph_ids
                    .iter()
                    .position(|&id| id == composite_glyph.glyph_index)
                    .unwrap_or_else(|| {
                        let new_id = glyph_ids.len();
                        glyph_ids.push(composite_glyph.glyph_index);
                        new_id
                    });
                composite_glyph.glyph_index = new_id as u16;
            }
        }
        _ => unreachable!(),
    }
}

impl<'a> GlyfTable<'a> {
    /// Returns a copy of this table that only contains the glyphs specified by `glyph_ids`.
    pub fn subset(&self, glyph_ids: &[u16]) -> Result<(GlyfTable<'a>, Vec<u16>), ParseError> {
        let mut glyph_ids = glyph_ids.to_vec();
        let mut records = Vec::with_capacity(glyph_ids.len());

        let mut i = 0;
        while i < glyph_ids.len() {
            let glyph_id = glyph_ids[i];
            let mut record = self
                .records
                .get(usize::from(glyph_id))
                .ok_or(ParseError::BadIndex)?
                .clone();
            if record.is_composite()? {
                record = record.parse()?;
                add_glyph(&mut glyph_ids, &mut record);
            }
            records.push(SubsetGlyph {
                old_id: glyph_id,
                record,
            });
            i += 1;
        }

        let mut new_to_old_id = vec![0u16; records.len()];
        let records = records
            .into_iter()
            .enumerate()
            .map(|(new_id, subset_record)| {
                new_to_old_id[new_id] = subset_record.old_id;
                subset_record.record
            })
            .collect();

        Ok((GlyfTable { records }, new_to_old_id))
    }
}

impl<'a> GlyfRecord<'a> {
    pub fn number_of_contours(&self) -> Result<i16, ParseError> {
        match self {
            GlyfRecord::Empty => Ok(0),
            GlyfRecord::Present(scope) => scope.read::<I16Be>(),
            GlyfRecord::Parsed(glyph) => Ok(glyph.number_of_contours),
        }
    }

    pub fn is_composite(&self) -> Result<bool, ParseError> {
        self.number_of_contours()
            .map(|number_of_contours| number_of_contours < 0)
    }

    pub fn parse(self) -> Result<Self, ParseError> {
        match self {
            GlyfRecord::Empty => Ok(GlyfRecord::Empty),
            GlyfRecord::Present(scope) => scope.read::<Glyph<'_>>().map(GlyfRecord::Parsed),
            GlyfRecord::Parsed(glyph) => Ok(GlyfRecord::Parsed(glyph)),
        }
    }
}

impl CompositeGlyphFlag {
    pub fn arg_1_and_2_are_words(self) -> bool {
        self & Self::ARG_1_AND_2_ARE_WORDS == Self::ARG_1_AND_2_ARE_WORDS
    }

    pub fn args_are_xy_values(self) -> bool {
        self & Self::ARGS_ARE_XY_VALUES == Self::ARGS_ARE_XY_VALUES
    }

    pub fn we_have_a_scale(self) -> bool {
        self & Self::WE_HAVE_A_SCALE == Self::WE_HAVE_A_SCALE
    }

    pub fn we_have_an_x_and_y_scale(self) -> bool {
        self & Self::WE_HAVE_AN_X_AND_Y_SCALE == Self::WE_HAVE_AN_X_AND_Y_SCALE
    }

    pub fn we_have_a_two_by_two(self) -> bool {
        self & Self::WE_HAVE_A_TWO_BY_TWO == Self::WE_HAVE_A_TWO_BY_TWO
    }

    pub fn more_components(self) -> bool {
        self & Self::MORE_COMPONENTS == Self::MORE_COMPONENTS
    }

    pub fn we_have_instructions(self) -> bool {
        self & Self::WE_HAVE_INSTRUCTIONS == Self::WE_HAVE_INSTRUCTIONS
    }
}

impl BoundingBox {
    /// Calculate xMin, xMax and yMin, yMax from a collection of `Points`
    ///
    /// Panics if `points` is empty.
    pub fn from_points(points: &[Point]) -> Self {
        assert!(!points.is_empty());

        let Point(initial_x, initial_y) = points[0];
        let initial = BoundingBox {
            x_min: initial_x,
            x_max: initial_x,
            y_min: initial_y,
            y_max: initial_y,
        };

        points
            .iter()
            .fold(initial, |mut bounding_box, &Point(x, y)| {
                if x < bounding_box.x_min {
                    bounding_box.x_min = x
                }
                if x > bounding_box.x_max {
                    bounding_box.x_max = x
                }
                if y < bounding_box.y_min {
                    bounding_box.y_min = y
                }
                if y > bounding_box.y_max {
                    bounding_box.y_max = y
                }

                bounding_box
            })
    }
}

impl SimpleGlyph {
    pub fn bounding_box(&self) -> BoundingBox {
        BoundingBox::from_points(&self.coordinates)
    }
}

#[cfg(test)]
mod tests {
    use super::{BoundingBox, GlyfRecord, GlyfTable, IndexToLocFormat, Point};
    use crate::binary::read::ReadScope;
    use crate::binary::write::{WriteBinary, WriteBinaryDep, WriteBuffer, WriteContext};
    use crate::tables::glyf::{
        CompositeGlyph, CompositeGlyphArgument, CompositeGlyphFlag, Glyph, GlyphData, SimpleGlyph,
        SimpleGlyphFlag,
    };
    use crate::tables::loca::{owned, LocaTable};

    #[test]
    fn test_point_bounding_box() {
        let points = [Point(1761, 565), Point(2007, 565), Point(1884, 1032)];

        let expected = BoundingBox {
            x_min: 1761,
            y_min: 565,
            x_max: 2007,
            y_max: 1032,
        };

        assert_eq!(BoundingBox::from_points(&points), expected);
    }

    #[test]
    fn write_glyf_table_loca_sanity_check() {
        let glyf = GlyfTable {
            records: vec![GlyfRecord::Empty, GlyfRecord::Empty],
        };
        let num_glyphs = glyf.records.len();
        let mut buffer = WriteBuffer::new();
        let loca = GlyfTable::write_dep(&mut buffer, glyf, IndexToLocFormat::Long).unwrap();
        assert_eq!(loca.offsets.len(), num_glyphs + 1);
    }

    #[test]
    fn write_composite_glyf_instructions() {
        let glyph = Glyph {
            number_of_contours: -1,
            bounding_box: BoundingBox {
                x_min: 205,
                x_max: 4514,
                y_min: 0,
                y_max: 1434,
            },
            data: GlyphData::Composite {
                glyphs: vec![
                    CompositeGlyph {
                        flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                            | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                            | CompositeGlyphFlag::ROUND_XY_TO_GRID
                            | CompositeGlyphFlag::MORE_COMPONENTS
                            | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                        glyph_index: 5,
                        argument1: CompositeGlyphArgument::I16(3453),
                        argument2: CompositeGlyphArgument::I16(0),
                        scale: None,
                    },
                    CompositeGlyph {
                        flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                            | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                            | CompositeGlyphFlag::ROUND_XY_TO_GRID
                            | CompositeGlyphFlag::MORE_COMPONENTS
                            | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                        glyph_index: 4,
                        argument1: CompositeGlyphArgument::I16(2773),
                        argument2: CompositeGlyphArgument::I16(0),
                        scale: None,
                    },
                    CompositeGlyph {
                        flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                            | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                            | CompositeGlyphFlag::ROUND_XY_TO_GRID
                            | CompositeGlyphFlag::MORE_COMPONENTS
                            | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET,
                        glyph_index: 3,
                        argument1: CompositeGlyphArgument::I16(1182),
                        argument2: CompositeGlyphArgument::I16(0),
                        scale: None,
                    },
                    CompositeGlyph {
                        flags: CompositeGlyphFlag::ARG_1_AND_2_ARE_WORDS
                            | CompositeGlyphFlag::ARGS_ARE_XY_VALUES
                            | CompositeGlyphFlag::ROUND_XY_TO_GRID
                            | CompositeGlyphFlag::UNSCALED_COMPONENT_OFFSET
                            | CompositeGlyphFlag::WE_HAVE_INSTRUCTIONS,
                        glyph_index: 2,
                        argument1: CompositeGlyphArgument::I16(205),
                        argument2: CompositeGlyphArgument::I16(0),
                        scale: None,
                    },
                ],
                instructions: &[1, 2, 3, 4],
            },
        };

        let mut buffer = WriteBuffer::new();
        Glyph::write(&mut buffer, glyph).unwrap();

        // Read it back and check the instructions are intact
        match ReadScope::new(buffer.bytes()).read::<Glyph<'_>>() {
            Ok(Glyph {
                data: GlyphData::Composite { instructions, .. },
                ..
            }) => assert_eq!(instructions, vec![1, 2, 3, 4].as_slice()),
            _ => panic!("did not read back expected instructions"),
        }
    }

    #[test]
    fn read_glyph_offsets_correctly() {
        // Test for a bug in which only the length relative to current ReadCtxt offset was used
        // to read a glyph out of the `glyf` table. It should have been using `start` and `end`
        // offsets read from `loca`. The bug was discovered when reading the Baekmuk Batang font
        // in which the glyph data starts at offset 366.
        let glyph = Glyph {
            number_of_contours: 1,
            bounding_box: BoundingBox {
                x_min: 60,
                x_max: 915,
                y_min: -105,
                y_max: 702,
            },
            data: GlyphData::Simple(SimpleGlyph {
                end_pts_of_contours: vec![103],
                instructions: vec![],
                flags: vec![
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                    SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    SimpleGlyphFlag::X_SHORT_VECTOR | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    SimpleGlyphFlag::X_SHORT_VECTOR | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR,
                ],
                coordinates: vec![
                    Point(433, 77),
                    Point(499, 30),
                    Point(625, 2),
                    Point(756, -27),
                    Point(915, -31),
                    Point(891, -47),
                    Point(862, -60),
                    Point(832, -73),
                    Point(819, -103),
                ],
            }),
        };

        // Write the glyph out
        let mut buffer = WriteBuffer::new();
        buffer.write_zeros(4).unwrap(); // Add some unused data at the start
        Glyph::write(&mut buffer, glyph).unwrap();
        let glyph_data = buffer.into_inner();

        let mut buffer = WriteBuffer::new();
        let loca = owned::LocaTable {
            offsets: vec![4, 4, glyph_data.len() as u32 - 4],
        };
        owned::LocaTable::write_dep(&mut buffer, loca, IndexToLocFormat::Long)
            .expect("unable to generate loca");
        let loca_data = buffer.into_inner();

        // Parse and verify
        let num_glyphs = 2;
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((num_glyphs, IndexToLocFormat::Long))
            .expect("unable to read loca");
        let glyf = ReadScope::new(&glyph_data)
            .read_dep::<GlyfTable<'_>>(&loca)
            .expect("unable to read glyf");
        assert_eq!(glyf.records.len(), 2);
        assert_eq!(&glyf.records[0], &GlyfRecord::Empty);
        let glyph = &glyf.records[1];

        // Before the fix num_contours was read as 0
        assert_eq!(glyph.number_of_contours().unwrap(), 1);
    }
}
