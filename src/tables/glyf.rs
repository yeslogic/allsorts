//! Parsing and writing of the `glyf` table.
//!
//! > This table contains information that describes the glyphs in the font in the TrueType outline
//! > format. Information regarding the rasterizer (scaler) refers to the TrueType rasterizer.
//!
//! — <https://docs.microsoft.com/en-us/typography/opentype/spec/glyf>

#[cfg(feature = "outline")]
mod outline;
mod subset;
pub mod variation;

use std::convert::{TryFrom, TryInto};
use std::iter;

use bitflags::bitflags;
use itertools::Itertools;
use log::warn;

use crate::binary::read::{
    ReadBinary, ReadBinaryDep, ReadCtxt, ReadFrom, ReadScope, ReadUnchecked,
};
use crate::binary::write::{WriteBinary, WriteBinaryDep, WriteContext};
use crate::binary::{word_align, I16Be, U16Be, I8, U8};
use crate::error::{ParseError, WriteError};
use crate::tables::loca::{owned, LocaTable};
use crate::tables::{F2Dot14, IndexToLocFormat};

pub use subset::SubsetGlyph;

/// Recursion limit for nested composite glyphs
///
/// "There is no minimum nesting depth that must be supported" so we use the same value as Harfbuzz.
const COMPOSITE_GLYPH_RECURSION_LIMIT: u8 = 6;

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
    records: Vec<GlyfRecord<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum GlyfRecord<'a> {
    Present {
        number_of_contours: i16,
        scope: ReadScope<'a>,
    },
    Parsed(Glyph<'a>),
}

pub type PhantomPoints = [Point; 4];

#[derive(Debug, PartialEq, Clone)]
pub enum Glyph<'a> {
    Empty(EmptyGlyph),
    Simple(SimpleGlyph<'a>),
    Composite(CompositeGlyph<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct EmptyGlyph {
    pub phantom_points: Option<PhantomPoints>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SimpleGlyph<'a> {
    pub bounding_box: BoundingBox,
    pub end_pts_of_contours: Vec<u16>,
    pub instructions: &'a [u8],
    pub coordinates: Vec<(SimpleGlyphFlag, Point)>,
    /// Phantom points, only populated when applying glyph variation deltas
    pub phantom_points: Option<Box<PhantomPoints>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompositeGlyph<'a> {
    pub bounding_box: BoundingBox,
    pub glyphs: Vec<CompositeGlyphComponent>,
    pub instructions: &'a [u8],
    /// Phantom points, only populated when applying glyph variation deltas
    pub phantom_points: Option<Box<PhantomPoints>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompositeGlyphComponent {
    pub flags: CompositeGlyphFlag,
    pub glyph_index: u16,
    pub argument1: CompositeGlyphArgument,
    pub argument2: CompositeGlyphArgument,
    pub scale: Option<CompositeGlyphScale>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum CompositeGlyphArgument {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum CompositeGlyphScale {
    Scale(F2Dot14),
    XY { x_scale: F2Dot14, y_scale: F2Dot14 },
    Matrix([[F2Dot14; 2]; 2]),
}

pub struct CompositeGlyphs {
    pub glyphs: Vec<CompositeGlyphComponent>,
    pub have_instructions: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Point(pub i16, pub i16);

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct BoundingBox {
    pub x_min: i16,
    pub x_max: i16,
    pub y_min: i16,
    pub y_max: i16,
}

impl<'b> ReadBinaryDep for GlyfTable<'b> {
    type Args<'a> = &'a LocaTable<'a>;
    type HostType<'a> = GlyfTable<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        loca: Self::Args<'a>,
    ) -> Result<Self::HostType<'a>, ParseError> {
        if loca.offsets.len() < 2 {
            return Err(ParseError::BadIndex);
        }

        let glyph_records = loca
            .offsets
            .iter()
            .tuple_windows()
            .map(|(start, end)| match end.checked_sub(start) {
                Some(0) => Ok(GlyfRecord::empty()),
                Some(length) => {
                    let offset = usize::try_from(start)?;
                    let glyph_scope = ctxt.scope().offset_length(offset, usize::try_from(length)?);
                    match glyph_scope {
                        Ok(scope) => {
                            let number_of_contours = scope.read::<I16Be>()?;
                            Ok(GlyfRecord::Present {
                                number_of_contours,
                                scope,
                            })
                        }
                        Err(ParseError::BadEof) => {
                            // The length specified by `loca` is beyond the end of the `glyf`
                            // table. Try parsing the glyph without a length limit to see if it's
                            // valid. This is a workaround for a font where the last `loca` offset
                            // was incorrectly 1 byte beyond the end of the `glyf` table but the
                            // actual glyph data was valid.
                            warn!("glyph length out of bounds, trying to parse");
                            let scope = ctxt.scope().offset(offset);
                            scope.read::<Glyph<'_>>().map(GlyfRecord::Parsed)
                        }
                        Err(err) => Err(err),
                    }
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
                GlyfRecord::Present { scope, .. } => ReadScope::write(ctxt, scope)?,
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

impl Glyph<'_> {
    fn number_of_coordinates(&self) -> Result<u32, ParseError> {
        match self {
            Glyph {
                data: GlyphData::Simple(glyph),
                ..
            } => Ok(glyph.coordinates.len().try_into()?),
            // The variation data for composite glyphs also use packed point number data
            // representing a series of numbers, but the numbers in this case, apart from the last
            // four “phantom” point numbers, refer to the components that make up the glyph rather
            // than to outline points.
            //
            // https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#point-numbers-and-processing-for-composite-glyphs
            Glyph {
                data: GlyphData::Composite { glyphs, .. },
                ..
            } => Ok(glyphs.len().try_into()?),
        }
    }
}

impl<'b> ReadBinary for Glyph<'b> {
    type HostType<'a> = Glyph<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let number_of_contours = ctxt.read_i16be()?;

        if number_of_contours >= 0 {
            // Simple glyph
            // Cast is safe as we've checked value is positive above
            let glyph = ctxt.read_dep::<SimpleGlyph<'_>>(number_of_contours as u16)?;
            Ok(Glyph::Simple(glyph))
        } else {
            // Composite glyph
            let glyph = ctxt.read::<CompositeGlyph<'_>>()?;
            Ok(Glyph::Composite(glyph))
        }
    }
}

impl<'a> WriteBinary for Glyph<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, glyph: Glyph<'a>) -> Result<(), WriteError> {
        match glyph {
            Glyph::Empty(_) => Ok(()),
            Glyph::Simple(simple_glyph) => SimpleGlyph::write(ctxt, simple_glyph),
            Glyph::Composite(composite) => CompositeGlyph::write(ctxt, composite),
        }
    }
}

impl<'a> SimpleGlyph<'a> {
    pub fn number_of_contours(&self) -> i16 {
        // TODO: Revisit this to see how we might enforce its validity
        self.end_pts_of_contours.len() as i16
    }

    pub fn contours(&self) -> impl Iterator<Item = &[(SimpleGlyphFlag, Point)]> {
        self.end_pts_of_contours.iter().scan(0, move |i, &end| {
            let start = *i;
            let end = usize::from(end);
            *i = end + 1;
            self.coordinates.get(start..=end)
        })
    }

    pub fn bounding_box(&self) -> BoundingBox {
        BoundingBox::from_points(self.coordinates.iter().copied().map(|(_flag, point)| point))
    }
}

impl<'b> ReadBinaryDep for SimpleGlyph<'b> {
    type Args<'a> = u16;
    type HostType<'a> = SimpleGlyph<'a>;

    fn read_dep<'a>(
        ctxt: &mut ReadCtxt<'a>,
        number_of_contours: Self::Args<'_>,
    ) -> Result<Self::HostType<'a>, ParseError> {
        let number_of_contours = usize::from(number_of_contours);
        let bounding_box = ctxt.read::<BoundingBox>()?;
        let end_pts_of_contours = ctxt.read_array::<U16Be>(number_of_contours)?.to_vec();
        let instruction_length = ctxt.read::<U16Be>()?;
        let instructions = ctxt.read_slice(usize::from(instruction_length))?;
        // end_pts_of_contours stores the index of the end points.
        // Therefore the number of coordinates is the last index + 1
        let number_of_coordinates = end_pts_of_contours
            .last()
            .map_or(0, |&last| usize::from(last) + 1);

        // Read all the flags
        let mut coordinates = Vec::with_capacity(number_of_coordinates);
        while coordinates.len() < number_of_coordinates {
            let flag = ctxt.read::<SimpleGlyphFlag>()?;
            if flag.is_repeated() {
                let count = usize::from(ctxt.read::<U8>()?) + 1; // + 1 to include the current entry
                let repeat = iter::repeat((flag, Point::zero())).take(count);
                coordinates.extend(repeat)
            } else {
                coordinates.push((flag, Point::zero()));
            }
        }

        // Read the x coordinates
        for (flag, Point(x, _y)) in coordinates.iter_mut() {
            *x = if flag.x_is_short() {
                ctxt.read::<U8>()
                    .map(|val| i16::from(val) * flag.x_short_sign())?
            } else if flag.x_is_same_or_positive() {
                0
            } else {
                ctxt.read::<I16Be>()?
            }
        }

        // Read y coordinates, updating the Points in `coordinates`
        let mut prev_point = Point::zero();
        for (flag, point) in coordinates.iter_mut() {
            let y = if flag.y_is_short() {
                ctxt.read::<U8>()
                    .map(|val| i16::from(val) * flag.y_short_sign())?
            } else if flag.y_is_same_or_positive() {
                0
            } else {
                ctxt.read::<I16Be>()?
            };

            // The x and y coordinates are stored as deltas against the previous point, with the
            // first one being implicitly against (0, 0). Here we resolve these deltas into
            // absolute (x, y) values.
            prev_point = Point(prev_point.0 + point.0, prev_point.1 + y);
            *point = prev_point
        }

        Ok(SimpleGlyph {
            bounding_box,
            end_pts_of_contours,
            instructions,
            coordinates,
            phantom_points: None,
        })
    }
}

impl<'a> WriteBinary for SimpleGlyph<'a> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, glyph: SimpleGlyph<'_>) -> Result<(), WriteError> {
        I16Be::write(ctxt, glyph.number_of_contours())?;
        BoundingBox::write(ctxt, glyph.bounding_box)?;
        ctxt.write_vec::<U16Be>(glyph.end_pts_of_contours)?;
        U16Be::write(ctxt, u16::try_from(glyph.instructions.len())?)?;
        ctxt.write_bytes(glyph.instructions)?;

        // Flags and coordinates are written without any attempt to compact them using
        // smaller representation, use of REPEAT, or X/Y_IS_SAME.
        // TODO: try to compact the values written

        // flags
        let mask = SimpleGlyphFlag::ON_CURVE_POINT; // ON_CURVE_POINT is the only flag that needs to carry through
        for flag in glyph.coordinates.iter().map(|(flag, _)| *flag) {
            U8::write(ctxt, (flag & mask).bits())?;
        }

        // x coordinates
        let mut prev_x = 0;
        for (_, Point(x, _)) in &glyph.coordinates {
            let delta_x = x - prev_x;
            I16Be::write(ctxt, delta_x)?;
            prev_x = *x;
        }

        // y coordinates
        let mut prev_y = 0;
        for (_, Point(_, y)) in &glyph.coordinates {
            let delta_y = y - prev_y;
            I16Be::write(ctxt, delta_y)?;
            prev_y = *y;
        }

        Ok(())
    }
}

impl ReadFrom for SimpleGlyphFlag {
    type ReadType = U8;

    fn read_from(flag: u8) -> Self {
        SimpleGlyphFlag::from_bits_truncate(flag)
    }
}

impl ReadBinary for CompositeGlyphs {
    type HostType<'a> = Self;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self, ParseError> {
        let mut have_instructions = false;
        let mut glyphs = Vec::new();
        loop {
            let flags = ctxt.read::<CompositeGlyphFlag>()?;
            let data = ctxt.read_dep::<CompositeGlyphComponent>(flags)?;

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

impl ReadBinary for CompositeGlyph<'_> {
    type HostType<'a> = CompositeGlyph<'a>;

    fn read<'a>(ctxt: &mut ReadCtxt<'a>) -> Result<Self::HostType<'a>, ParseError> {
        let bounding_box = ctxt.read::<BoundingBox>()?;
        let glyphs = ctxt.read::<CompositeGlyphs>()?;

        let instruction_length = if glyphs.have_instructions {
            usize::from(ctxt.read::<U16Be>()?)
        } else {
            0
        };
        let instructions = ctxt.read_slice(instruction_length)?;

        Ok(CompositeGlyph {
            bounding_box,
            glyphs: glyphs.glyphs,
            instructions,
            phantom_points: None,
        })
    }
}

impl WriteBinary for CompositeGlyph<'_> {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, composite: Self) -> Result<Self::Output, WriteError> {
        I16Be::write(ctxt, -1_i16)?; // number_of_contours
        BoundingBox::write(ctxt, composite.bounding_box)?;
        let mut has_instructions = false;
        for glyph in composite.glyphs {
            has_instructions |= glyph.flags.we_have_instructions();
            CompositeGlyphComponent::write(ctxt, glyph)?;
        }
        if has_instructions {
            U16Be::write(ctxt, u16::try_from(composite.instructions.len())?)?;
            ctxt.write_bytes(composite.instructions)?;
        }
        Ok(())
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

impl ReadFrom for CompositeGlyphFlag {
    type ReadType = U16Be;

    fn read_from(flag: u16) -> Self {
        CompositeGlyphFlag::from_bits_truncate(flag)
    }
}

impl ReadBinaryDep for CompositeGlyphArgument {
    type Args<'a> = CompositeGlyphFlag;
    type HostType<'a> = Self;

    fn read_dep<'a>(ctxt: &mut ReadCtxt<'a>, flags: Self::Args<'_>) -> Result<Self, ParseError> {
        let arg = match (flags.arg_1_and_2_are_words(), flags.args_are_xy_values()) {
            (true, true) => CompositeGlyphArgument::I16(ctxt.read_i16be()?),
            (true, false) => CompositeGlyphArgument::U16(ctxt.read_u16be()?),
            (false, true) => CompositeGlyphArgument::I8(ctxt.read_i8()?),
            (false, false) => CompositeGlyphArgument::U8(ctxt.read_u8()?),
        };

        Ok(arg)
    }
}

impl WriteBinary for CompositeGlyphArgument {
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

impl ReadBinaryDep for CompositeGlyphComponent {
    type Args<'a> = CompositeGlyphFlag;
    type HostType<'a> = Self;

    fn read_dep<'a>(ctxt: &mut ReadCtxt<'a>, flags: Self::Args<'_>) -> Result<Self, ParseError> {
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

        Ok(CompositeGlyphComponent {
            flags,
            glyph_index,
            argument1,
            argument2,
            scale,
        })
    }
}

impl WriteBinary for CompositeGlyphComponent {
    type Output = ();

    fn write<C: WriteContext>(
        ctxt: &mut C,
        glyph: CompositeGlyphComponent,
    ) -> Result<(), WriteError> {
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

impl WriteBinary for CompositeGlyphScale {
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

impl ReadFrom for BoundingBox {
    type ReadType = ((I16Be, I16Be), (I16Be, I16Be));

    fn read_from(((x_min, y_min), (x_max, y_max)): ((i16, i16), (i16, i16))) -> Self {
        BoundingBox {
            x_min,
            y_min,
            x_max,
            y_max,
        }
    }
}

impl WriteBinary for BoundingBox {
    type Output = ();

    fn write<C: WriteContext>(ctxt: &mut C, bbox: BoundingBox) -> Result<(), WriteError> {
        I16Be::write(ctxt, bbox.x_min)?;
        I16Be::write(ctxt, bbox.y_min)?;
        I16Be::write(ctxt, bbox.x_max)?;
        I16Be::write(ctxt, bbox.y_max)?;
        Ok(())
    }
}

impl<'a> GlyfTable<'a> {
    pub fn new(records: Vec<GlyfRecord<'a>>) -> Result<Self, ParseError> {
        if records.len() > usize::from(u16::MAX) {
            return Err(ParseError::LimitExceeded);
        }
        Ok(GlyfTable { records })
    }

    /// Returns the number of glyphs in this `glyf` table.
    ///
    /// Returns a `u16` for convenience of interacting with other parts of the code.
    pub fn num_glyphs(&self) -> u16 {
        // NOTE(cast): Safe as we check records length in `new` and `push`.
        self.records.len() as u16
    }

    pub fn records(&self) -> &[GlyfRecord<'a>] {
        &self.records
    }

    pub fn records_mut(&mut self) -> &mut [GlyfRecord<'a>] {
        &mut self.records
    }

    pub fn push(&mut self, record: GlyfRecord<'a>) -> Result<(), ParseError> {
        if self.num_glyphs() < u16::MAX {
            self.records.push(record);
            Ok(())
        } else {
            Err(ParseError::LimitExceeded)
        }
    }

    /// Returns a parsed glyph, converting [GlyfRecord::Present] into [GlyfRecord::Parsed] if
    /// necessary.
    pub fn get_parsed_glyph(&mut self, glyph_index: u16) -> Result<&Glyph<'_>, ParseError> {
        let record = self
            .records
            .get_mut(usize::from(glyph_index))
            .ok_or(ParseError::BadIndex)?;
        record.parse()?;
        match record {
            GlyfRecord::Parsed(glyph) => Ok(glyph),
            GlyfRecord::Present { .. } => unreachable!("glyph should be parsed"),
        }
    }
}

impl<'a> GlyfRecord<'a> {
    /// Construct an empty glyph record
    pub fn empty() -> Self {
        GlyfRecord::Parsed(Glyph::Empty(EmptyGlyph::new()))
    }

    pub fn number_of_contours(&self) -> i16 {
        match self {
            GlyfRecord::Present {
                number_of_contours, ..
            } => *number_of_contours,
            GlyfRecord::Parsed(glyph) => glyph.number_of_contours(),
        }
    }

    /// Returns the number of coordinates (or points) in this glyph.
    pub fn number_of_coordinates(&self) -> Result<u32, ParseError> {
        // The `maxp` table contains fields:
        //
        // * maxPoints            Maximum points in a non-composite glyph.
        // * maxCompositePoints   Maximum points in a composite glyph.
        //
        // Both of which are u16, since we have to add four for the phantom points this method
        // returns a u32.
        // FIXME: comment above
        match self {
            GlyfRecord::Empty => Ok(0),
            GlyfRecord::Present {
                scope,
                number_of_contours,
            } => {
                let mut ctxt = scope.ctxt();
                // skip glyph header: number_of_contours and the bounding box
                let _skip = ctxt.read_slice(U16Be::SIZE + BoundingBox::SIZE)?;
                if *number_of_contours >= 0 {
                    // Simple glyph
                    let end_pts_of_contours =
                        ctxt.read_array::<U16Be>(*number_of_contours as usize)?;
                    // end_pts_of_contours stores the index of the end points.
                    // Therefore the number of coordinates is the last index + 1
                    Ok(end_pts_of_contours
                        .last()
                        .map_or(0, |last| u32::from(last) + 1))
                } else {
                    // Composite glyph
                    let mut count = 0;
                    loop {
                        let flags = ctxt.read::<CompositeGlyphFlag>()?;
                        let _composite_glyph = ctxt.read_dep::<CompositeGlyph>(flags)?;
                        count += 1;
                        if !flags.more_components() {
                            break;
                        }
                    }
                    Ok(count)
                }
            }
            GlyfRecord::Parsed(glyph) => glyph.number_of_coordinates(),
        }
    }

    pub fn is_composite(&self) -> bool {
        self.number_of_contours() < 0
    }

    /// Turn self from GlyfRecord::Present into GlyfRecord::Parsed
    pub fn parse(&mut self) -> Result<(), ParseError> {
        if let GlyfRecord::Present { scope, .. } = self {
            *self = scope.read::<Glyph<'_>>().map(GlyfRecord::Parsed)?;
        }
        Ok(())
    }
}

impl<'a> Glyph<'a> {
    pub fn number_of_contours(&self) -> i16 {
        match self {
            Glyph::Empty(_) => 0,
            Glyph::Simple(simple) => simple.number_of_contours(),
            Glyph::Composite(_) => -1,
        }
    }

    /// Returns the bounding box of the glyph.
    ///
    /// Returns `None` if the glyph is an [EmptyGlyph].
    pub fn bounding_box(&self) -> Option<BoundingBox> {
        match self {
            Glyph::Empty(_) => None,
            Glyph::Simple(simple) => Some(simple.bounding_box),
            Glyph::Composite(composite) => Some(composite.bounding_box),
        }
    }
}

impl EmptyGlyph {
    pub fn new() -> Self {
        EmptyGlyph {
            phantom_points: None,
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

impl Point {
    pub fn zero() -> Self {
        Point(0, 0)
    }
}

impl BoundingBox {
    pub fn empty() -> Self {
        BoundingBox {
            x_min: 0,
            x_max: 0,
            y_min: 0,
            y_max: 0,
        }
    }

    /// Calculate xMin, xMax and yMin, yMax from a collection of `Points`
    ///
    /// Panics if `points` is empty.
    pub fn from_points(points: impl ExactSizeIterator<Item = Point>) -> Self {
        assert!(points.len() > 0);
        let mut points = points.peekable();

        // NOTE(unwrap): Safe as length is at least 1
        let &Point(initial_x, initial_y) = points.peek().unwrap();
        let initial = BoundingBox {
            x_min: initial_x,
            x_max: initial_x,
            y_min: initial_y,
            y_max: initial_y,
        };

        points.fold(initial, |mut bounding_box, Point(x, y)| {
            bounding_box.x_min = i16::min(x, bounding_box.x_min);
            bounding_box.x_max = i16::max(x, bounding_box.x_max);
            bounding_box.y_min = i16::min(y, bounding_box.y_min);
            bounding_box.y_max = i16::max(y, bounding_box.y_max);
            bounding_box
        })
    }
}

impl From<CompositeGlyphArgument> for i32 {
    fn from(arg: CompositeGlyphArgument) -> Self {
        match arg {
            CompositeGlyphArgument::U8(value) => i32::from(value),
            CompositeGlyphArgument::I8(value) => i32::from(value),
            CompositeGlyphArgument::U16(value) => i32::from(value),
            CompositeGlyphArgument::I16(value) => i32::from(value),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::write::WriteBuffer;
    use crate::error::ReadWriteError;

    pub(super) fn simple_glyph_fixture() -> SimpleGlyph<'static> {
        SimpleGlyph {
            bounding_box: BoundingBox {
                x_min: 60,
                x_max: 915,
                y_min: -105,
                y_max: 702,
            },
            end_pts_of_contours: vec![8],
            instructions: &[],
            coordinates: vec![
                (
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR,
                    Point(433, 77),
                ),
                (
                    SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    Point(499, 30),
                ),
                (
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    Point(625, 2),
                ),
                (
                    SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    Point(756, -27),
                ),
                (
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR
                        | SimpleGlyphFlag::X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR,
                    Point(915, -31),
                ),
                (
                    SimpleGlyphFlag::X_SHORT_VECTOR | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    Point(891, -47),
                ),
                (
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    Point(862, -60),
                ),
                (
                    SimpleGlyphFlag::X_SHORT_VECTOR | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    Point(832, -73),
                ),
                (
                    SimpleGlyphFlag::ON_CURVE_POINT
                        | SimpleGlyphFlag::X_SHORT_VECTOR
                        | SimpleGlyphFlag::Y_SHORT_VECTOR,
                    Point(819, -103),
                ),
            ],
            phantom_points: None,
        }
    }

    pub(super) fn composite_glyph_fixture(instructions: &'static [u8]) -> CompositeGlyph<'static> {
        CompositeGlyph {
            bounding_box: BoundingBox {
                x_min: 205,
                x_max: 4514,
                y_min: 0,
                y_max: 1434,
            },
            glyphs: vec![
                CompositeGlyphComponent {
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
                CompositeGlyphComponent {
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
                CompositeGlyphComponent {
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
                CompositeGlyphComponent {
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
            instructions,
            phantom_points: None,
        }
    }

    #[test]
    fn test_point_bounding_box() {
        let points = [Point(1761, 565), Point(2007, 565), Point(1884, 1032)];

        let expected = BoundingBox {
            x_min: 1761,
            y_min: 565,
            x_max: 2007,
            y_max: 1032,
        };

        assert_eq!(BoundingBox::from_points(points.iter().copied()), expected);
    }

    #[test]
    fn write_glyf_table_loca_sanity_check() {
        let glyf = GlyfTable {
            records: vec![GlyfRecord::empty(), GlyfRecord::empty()],
        };
        let num_glyphs = glyf.records.len();
        let mut buffer = WriteBuffer::new();
        let loca = GlyfTable::write_dep(&mut buffer, glyf, IndexToLocFormat::Long).unwrap();
        assert_eq!(loca.offsets.len(), num_glyphs + 1);
    }

    #[test]
    fn write_composite_glyf_instructions() {
        let glyph = Glyph::Composite(composite_glyph_fixture(&[1, 2, 3, 4]));

        let mut buffer = WriteBuffer::new();
        Glyph::write(&mut buffer, glyph).unwrap();

        // Read it back and check the instructions are intact
        match ReadScope::new(buffer.bytes()).read::<Glyph<'_>>() {
            Ok(Glyph::Composite(CompositeGlyph { instructions, .. })) => {
                assert_eq!(instructions, vec![1, 2, 3, 4].as_slice())
            }
            _ => panic!("did not read back expected instructions"),
        }
    }

    #[test]
    fn read_glyph_offsets_correctly() {
        // Test for a bug in which only the length relative to current ReadCtxt offset was used
        // to read a glyph out of the `glyf` table. It should have been using `start` and `end`
        // offsets read from `loca`. The bug was discovered when reading the Baekmuk Batang font
        // in which the glyph data starts at offset 366.
        let glyph = simple_glyph_fixture();

        // Write the glyph out
        let mut buffer = WriteBuffer::new();
        buffer.write_zeros(4).unwrap(); // Add some unused data at the start
        SimpleGlyph::write(&mut buffer, glyph).unwrap();
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
        assert_eq!(&glyf.records[0], &GlyfRecord::empty());
        let glyph = &glyf.records[1];

        // Before the fix num_contours was read as 0
        assert_eq!(glyph.number_of_contours(), 1);
    }

    // Regarding simple glyphs the OpenType spec says:
    // This is the table information needed if numberOfContours is greater than or equal to zero
    // https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#simple-glyph-description
    //
    // We previously rejected glyphs with zero contours.
    #[test]
    fn simple_glyph_with_zero_contours() {
        let glyph_data = &[
            0, 0, 0, 0, 0, 0, 0, 0, // bounding box
            0, 0, // instruction length
        ];
        let expected = SimpleGlyph {
            bounding_box: BoundingBox::empty(),
            end_pts_of_contours: vec![],
            instructions: &[],
            coordinates: vec![],
            phantom_points: None,
        };

        let glyph = ReadScope::new(glyph_data)
            .read_dep::<SimpleGlyph<'_>>(0)
            .unwrap();
        assert_eq!(glyph, expected);
    }

    #[test]
    fn write_simple_glyph_with_zero_contours() {
        let glyph = SimpleGlyph {
            bounding_box: BoundingBox::empty(),
            end_pts_of_contours: vec![],
            instructions: &[],
            coordinates: vec![],
            phantom_points: None,
        };

        let mut buffer = WriteBuffer::new();
        assert!(SimpleGlyph::write(&mut buffer, glyph).is_ok());
    }

    #[test]
    fn read_glyph_with_incorrect_loca_length() {
        // Write the glyph out
        let glyph = simple_glyph_fixture();
        let mut buffer = WriteBuffer::new();
        Glyph::write(&mut buffer, Glyph::Simple(glyph)).unwrap();
        let glyph_data = buffer.into_inner();

        let mut buffer = WriteBuffer::new();
        let loca = owned::LocaTable {
            offsets: vec![0, 0, glyph_data.len() as u32 + 1], // + 1 to go past end of glyf
        };
        owned::LocaTable::write_dep(&mut buffer, loca, IndexToLocFormat::Long)
            .expect("unable to generate loca");
        let loca_data = buffer.into_inner();

        // Parse and verify
        let num_glyphs = 2;
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((num_glyphs, IndexToLocFormat::Long))
            .expect("unable to read loca");
        assert!(ReadScope::new(&glyph_data)
            .read_dep::<GlyfTable<'_>>(&loca)
            .is_ok())
    }

    // This is a test for a bug in which a composite glyph read with has_instructions = yes, but
    // instruction length 0 would be written without an instruction length field. This resulting
    // font was invalid as parsers would see the has_instructions flag and attempt to read the
    // non-existent instruction length.
    #[test]
    fn write_composite_glyph_with_empty_instructions() {
        let glyph = composite_glyph_fixture(&[]);

        let mut buffer = WriteBuffer::new();
        Glyph::write(&mut buffer, Glyph::Composite(glyph)).unwrap();

        // Ensure we can read it back. Before this fix this failed.
        match ReadScope::new(buffer.bytes()).read::<Glyph<'_>>() {
            Ok(Glyph::Composite(CompositeGlyph { instructions, .. })) => {
                assert_eq!(instructions, &[])
            }
            Ok(_) => panic!("did not read back expected glyph"),
            Err(_) => panic!("unable to read back glyph"),
        }
    }

    #[test]
    fn test_number_of_coordinates_empty() {
        let glyph = GlyfRecord::Empty;
        assert_eq!(glyph.number_of_coordinates().unwrap(), 0);
    }

    #[test]
    fn test_number_of_coordinates_simple_parsed() {
        let glyph = GlyfRecord::Parsed(simple_glyph_fixture());
        assert_eq!(glyph.number_of_coordinates().unwrap(), 9);
    }

    #[test]
    fn test_number_of_coordinates_simple_present() -> Result<(), ReadWriteError> {
        // Serialize
        let glyph = GlyfRecord::Parsed(simple_glyph_fixture());
        let glyf = GlyfTable {
            records: vec![GlyfRecord::Empty, glyph],
        };
        let num_glyphs = glyf.records.len();
        let mut buffer = WriteBuffer::new();
        let loca = GlyfTable::write_dep(&mut buffer, glyf, IndexToLocFormat::Long).unwrap();
        let mut loca_buffer = WriteBuffer::new();
        owned::LocaTable::write_dep(&mut loca_buffer, loca, IndexToLocFormat::Long)?;
        let loca_data = loca_buffer.into_inner();
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((num_glyphs, IndexToLocFormat::Long))?;

        // Read back
        let glyf = ReadScope::new(&buffer.bytes())
            .read_dep::<GlyfTable<'_>>(&loca)
            .unwrap();
        let glyph = &glyf.records[1];
        assert!(matches!(glyph, GlyfRecord::Present { .. }));
        assert_eq!(glyph.number_of_coordinates().unwrap(), 9);
        Ok(())
    }

    #[test]
    fn test_number_of_coordinates_composite_parsed() {
        // Test parsed
        let glyph = GlyfRecord::Parsed(composite_glyph_fixture(&[]));
        assert_eq!(glyph.number_of_coordinates().unwrap(), 4);
    }

    #[test]
    fn test_number_of_coordinates_composite_present() -> Result<(), ReadWriteError> {
        // Serialize
        let glyph = GlyfRecord::Parsed(composite_glyph_fixture(&[]));
        let glyf = GlyfTable {
            records: vec![GlyfRecord::Empty, glyph],
        };
        let num_glyphs = glyf.records.len();
        let mut buffer = WriteBuffer::new();
        let loca = GlyfTable::write_dep(&mut buffer, glyf, IndexToLocFormat::Long).unwrap();
        let mut loca_buffer = WriteBuffer::new();
        owned::LocaTable::write_dep(&mut loca_buffer, loca, IndexToLocFormat::Long)?;
        let loca_data = loca_buffer.into_inner();
        let loca = ReadScope::new(&loca_data)
            .read_dep::<LocaTable<'_>>((num_glyphs, IndexToLocFormat::Long))?;

        // Read back
        let glyf = ReadScope::new(&buffer.bytes())
            .read_dep::<GlyfTable<'_>>(&loca)
            .unwrap();
        let glyph = &glyf.records[1];
        assert!(matches!(glyph, GlyfRecord::Present { .. }));
        assert_eq!(glyph.number_of_coordinates().unwrap(), 4);
        Ok(())
    }
}
