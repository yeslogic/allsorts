use crate::error::ParseError;
use crate::tables::glyf::{CompositeGlyphScale, Point as GlyfPoint};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point(pub f32, pub f32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Matrix {
    x1: f32,
    y1: f32,
    x2: f32,
    y2: f32,
}

pub trait OutlineBuilder {
    fn visit<V: OutlineVisitor>(
        &mut self,
        glyph_index: u16,
        visitor: &mut V,
    ) -> Result<(), ParseError>;
}

/// A trait for visiting a glyph outline
pub trait OutlineVisitor {
    fn move_to(&mut self, point: Point);

    fn line_to(&mut self, point: Point);

    fn quad_to(&mut self, control: Point, point: Point);

    fn curve_to(&mut self, control1: Point, control2: Point, point: Point);

    fn close(&mut self);
}

impl Point {
    pub fn mid(self, other: Point) -> Point {
        let x = (self.0 + other.0) / 2.;
        let y = (self.1 + other.1) / 2.;
        Point(x, y)
    }

    pub fn offset(self, offset: Point) -> Point {
        Point(self.0 + offset.0, self.1 + offset.1)
    }

    pub fn scale(self, scale: Matrix) -> Point {
        Point(
            (self.0 * scale.x1) + (self.1 * scale.y1),
            (self.0 * scale.x2) + (self.1 * scale.y2),
        )
    }
}

impl Matrix {
    pub fn identity() -> Matrix {
        Matrix {
            x1: 1.,
            y1: 0.,
            x2: 0.,
            y2: 1.,
        }
    }
}

impl From<GlyfPoint> for Point {
    fn from(point: GlyfPoint) -> Self {
        Point(point.0 as f32, point.1 as f32)
    }
}

impl From<CompositeGlyphScale> for Matrix {
    fn from(scale: CompositeGlyphScale) -> Self {
        match scale {
            CompositeGlyphScale::Scale(scale) => {
                let scale = f32::from(scale);
                Matrix {
                    x1: scale,
                    y1: 0.,
                    x2: 0.,
                    y2: scale,
                }
            }
            CompositeGlyphScale::XY { x_scale, y_scale } => Matrix {
                x1: f32::from(x_scale),
                y1: 0.,
                x2: 0.,
                y2: f32::from(y_scale),
            },
            CompositeGlyphScale::Matrix(matrix) => Matrix {
                x1: f32::from(matrix[0][0]),
                y1: f32::from(matrix[0][1]),
                x2: f32::from(matrix[1][0]),
                y2: f32::from(matrix[1][1]),
            },
        }
    }
}
