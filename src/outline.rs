use pathfinder_geometry::line_segment::LineSegment2F;
use pathfinder_geometry::transform2d::Matrix2x2F;
use pathfinder_geometry::vector::Vector2F;

use crate::tables::glyf::{CompositeGlyphScale, Point as GlyfPoint};

/// Trait for visiting a glyph outline and delivering drawing commands to an `OutlineSink`.
pub trait OutlineBuilder {
    type Error: std::error::Error;

    /// Visit the glyph outlines in `self`.
    fn visit<S: OutlineSink>(&mut self, glyph_index: u16, sink: &mut S) -> Result<(), Self::Error>;
}

// `OutlineSink` is from font-kit, font-kit/src/outline.rs:
//
// Copyright © 2020 The Pathfinder Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/// A trait for visiting a glyph outline
pub trait OutlineSink {
    /// Moves the pen to a point.
    fn move_to(&mut self, to: Vector2F);
    /// Draws a line to a point.
    fn line_to(&mut self, to: Vector2F);
    /// Draws a quadratic Bézier curve to a point.
    fn quadratic_curve_to(&mut self, ctrl: Vector2F, to: Vector2F);
    /// Draws a cubic Bézier curve to a point.
    fn cubic_curve_to(&mut self, ctrl: LineSegment2F, to: Vector2F);
    /// Closes the path, returning to the first point in it.
    fn close(&mut self);
}

impl From<GlyfPoint> for Vector2F {
    fn from(point: GlyfPoint) -> Self {
        Vector2F::new(point.0 as f32, point.1 as f32)
    }
}

impl From<CompositeGlyphScale> for Matrix2x2F {
    fn from(scale: CompositeGlyphScale) -> Self {
        match scale {
            CompositeGlyphScale::Scale(scale) => {
                let scale = f32::from(scale);
                Matrix2x2F::from_scale(scale)
            }
            CompositeGlyphScale::XY { x_scale, y_scale } => {
                let scale = Vector2F::new(f32::from(x_scale), f32::from(y_scale));
                Matrix2x2F::from_scale(scale)
            }
            CompositeGlyphScale::Matrix(matrix) => Matrix2x2F::row_major(
                f32::from(matrix[0][0]),
                f32::from(matrix[0][1]),
                f32::from(matrix[1][0]),
                f32::from(matrix[1][1]),
            ),
        }
    }
}
