#![deny(missing_docs)]

//! Bitmap font handling.

pub mod cbdt;
pub mod sbix;

use num_traits as num;

/// Bit depth of bitmap data.
#[derive(Debug, PartialEq, Eq, Copy, Clone, PartialOrd)]
pub enum BitDepth {
    /// 1-bit per pixel.
    One = 1,
    /// 2-bits per pixel.
    Two = 2,
    /// 4-bits per pixel.
    Four = 4,
    /// 8-bits per pixel.
    Eight = 8,
    /// 32-bits per pixel (RGBA)
    ThirtyTwo = 32,
}

/// A bitmap glyph with metrics.
pub struct BitmapGlyph {
    /// Horizontal pixels per em.
    pub ppem_x: u16,
    /// Vertical pixels per em.
    pub ppem_y: u16,
    /// Glyph metrics in font units.
    pub metrics: Metrics,
    /// Bitmap data.
    pub bitmap: Bitmap,
    // ppi?
    // offset_origin_x
    // offset_origin_y
}

/// Bitmap data, either raw or encapsulated in a container format like PNG.
#[allow(missing_docs)]
pub enum Bitmap {
    Embedded(EmbeddedBitmap),
    Encapsulated(EncapsulatedBitmap),
}

/// Raw bitmap data.
pub struct EmbeddedBitmap {
    /// The width of the bitmap in pixels.
    pub width: u8,
    /// The height of the bitmap in pixels.
    pub height: u8,
    /// The format for the pixel data.
    pub format: BitDepth,
    /// Raw pixel data.
    pub data: Box<[u8]>,
}

/// Bitmap data encapsulated in a container format like PNG.
pub struct EncapsulatedBitmap {
    /// The container format used to hold the bitmap data.
    pub format: EncapsulatedFormat,
    /// Bitmap data.
    pub data: Box<[u8]>,
}

/// The container format of an `EncapsulatedBitmap`.
#[allow(missing_docs)]
pub enum EncapsulatedFormat {
    Jpeg,
    Png,
    Tiff,
    /// A format not part of the OpenType specification.
    Other(u32),
}

/// Bitmap glyph metrics either embedded or from `hmtx`/`vmtx`.
pub enum Metrics {
    /// Metrics were embedded with the bitmap.
    Embedded(EmbeddedMetrics),
    /// Metrics are available in the `hmtx` and `vmtx` tables.
    HmtxVmtx(OriginOffset),
}

/// Metrics embedded alongside the bitmap.
pub struct EmbeddedMetrics {
    /// Horizontal metrics.
    hori: Option<BitmapMetrics>,
    /// Vertical metrics.
    vert: Option<BitmapMetrics>,
}

/// Bitmap offset from glyph origin in font units.
pub struct OriginOffset {
    /// The horizontal (x-axis) offset from the left edge of the graphic to the glyph’s origin.
    pub x: i16,
    /// The vertical (y-axis) offset from the bottom edge of the graphic to the glyph’s origin.
    pub y: i16,
}

/// The actual embedded bitmap glyph metrics, normalised to font units.
pub struct BitmapMetrics {
    /// The offset from the glyph origin to the bottom left of the bitmap in font units.
    pub origin_offset: OriginOffset,
    // /// Distance in font units from the horizontal origin to the left edge of the bitmap.
    // pub bearing_x: i16,
    // /// Distance in font units from the horizontal origin to the top edge of the bitmap.
    // pub bearing_y: i16,
    /// advance width in font units.
    pub advance: u16,
    /// The spacing of the line before the baseline in font units.
    pub ascender: i16,
    ///The spacing of the line after the baseline in font units.
    pub descender: i16,
}

/// Returns true if `value` is closer to zero than `current_best`, favouring positive values even
/// if they're further away from zero.
fn bigger_or_closer_to_zero<V>(value: V, current_best: V) -> bool
where
    V: PartialOrd + num::Signed + num::Zero,
{
    if value == V::zero() {
        return true;
    } else if current_best == V::zero() {
        return false;
    }

    match (current_best.is_positive(), value.is_positive()) {
        (true, true) if value < current_best => true,
        (true, false) => false,
        (false, true) => true,
        (false, false) if value > current_best => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bigger_or_closer_to_zero() {
        // zero always wins
        assert!(bigger_or_closer_to_zero(0, -1));
        assert!(bigger_or_closer_to_zero(0, 0));
        assert!(bigger_or_closer_to_zero(0, 1));
        assert!(!bigger_or_closer_to_zero(-1, 0));
        assert!(!bigger_or_closer_to_zero(1, 0));

        // current best is negative
        assert!(bigger_or_closer_to_zero(10, -5)); // positive wins, even if further from zero
        assert!(bigger_or_closer_to_zero(-2, -5)); // negative wins if closer to zero
        assert!(!bigger_or_closer_to_zero(-7, -5));

        // current best is positive
        assert!(bigger_or_closer_to_zero(2, 5)); // positive wins if smaller
        assert!(!bigger_or_closer_to_zero(-2, 5)); // positive wins, even if further from zero
        assert!(!bigger_or_closer_to_zero(7, 5));
    }
}
