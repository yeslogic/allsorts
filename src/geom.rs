//! 2D geometry primitives used by the outline / variations / COLR code paths.
//!
//! Replaces the pathfinder_geometry dependency with a small native module
//! covering only the types, operators, and methods that allsorts actually
//! uses. Behaviour is intended to match pathfinder_geometry exactly for the
//! API surface listed below; in particular, constructors that take two
//! corners (`RectF::from_points`, `RectI::from_points`) do **not** normalise
//! the inputs — callers that need a min/max bounding box must pre-order the
//! corners (which mirrors pathfinder's contract).
//!
//! API surface:
//!
//! * [`Vector2F`], [`Vector2I`] — 2D float / int vectors. Constructors
//!   `new`, `splat`, `zero`, plus `vec2f` / `vec2i` shorthand. Arithmetic
//!   is component-wise except `Mul<f32>` which is scalar.
//! * [`RectF`], [`RectI`] — axis-aligned rectangles built from two corner
//!   points. `from_points`, `min_x` / `min_y` / `max_x` / `max_y`,
//!   `contains_point`, `round_out` (RectF only), `union_point` /
//!   `union_rect` (RectF only), and `RectF + Vector2F` translation.
//! * [`LineSegment2F`] — pair of points exposed via `from()` / `to()`.
//! * [`Matrix2x2F`] — row-major 2×2 matrix with `from_scale` and
//!   `row_major` constructors.
//! * [`Transform2F`] — affine 2D transform = `Matrix2x2F` + translation
//!   `Vector2F`. Constructable as `Transform2F { matrix, vector }` or via
//!   `row_major(m11, m12, m21, m22, m31, m32)`. Applies as
//!   `(m11*x + m12*y + m31, m21*x + m22*y + m32)`. Multiplication with
//!   `Vector2F` transforms a point; multiplication with `RectF` transforms
//!   the four corners and returns the axis-aligned bounding box of the
//!   result.

use std::ops::{Add, AddAssign, Div, Mul, Sub};

// ---------------------------------------------------------------------------
// Vector2F
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct Vector2F {
    x: f32,
    y: f32,
}

impl Vector2F {
    #[inline]
    pub const fn new(x: f32, y: f32) -> Self {
        Vector2F { x, y }
    }

    #[inline]
    pub const fn splat(s: f32) -> Self {
        Vector2F { x: s, y: s }
    }

    #[inline]
    pub const fn zero() -> Self {
        Vector2F { x: 0.0, y: 0.0 }
    }

    #[inline]
    pub fn x(self) -> f32 {
        self.x
    }

    #[inline]
    pub fn y(self) -> f32 {
        self.y
    }

    #[inline]
    pub fn min(self, other: Self) -> Self {
        Vector2F {
            x: self.x.min(other.x),
            y: self.y.min(other.y),
        }
    }

    #[inline]
    pub fn max(self, other: Self) -> Self {
        Vector2F {
            x: self.x.max(other.x),
            y: self.y.max(other.y),
        }
    }

    #[inline]
    pub fn clamp(self, min: Self, max: Self) -> Self {
        self.max(min).min(max)
    }

    /// Component-wise rounding to the nearest integer (ties-to-even matches `f32::round`).
    #[inline]
    pub fn round(self) -> Self {
        Vector2F {
            x: self.x.round(),
            y: self.y.round(),
        }
    }

    /// Component-wise cast to integer, truncating toward zero (matches `f32 as i32`).
    #[inline]
    pub fn to_i32(self) -> Vector2I {
        Vector2I {
            x: self.x as i32,
            y: self.y as i32,
        }
    }

    /// Linear interpolation between `self` and `other`: `self + (other - self) * t`.
    #[inline]
    pub fn lerp(self, other: Self, t: f32) -> Self {
        self + (other - self) * t
    }
}

#[inline]
pub fn vec2f(x: f32, y: f32) -> Vector2F {
    Vector2F::new(x, y)
}

impl Add for Vector2F {
    type Output = Vector2F;
    #[inline]
    fn add(self, rhs: Self) -> Self {
        Vector2F::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl Sub for Vector2F {
    type Output = Vector2F;
    #[inline]
    fn sub(self, rhs: Self) -> Self {
        Vector2F::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl Mul for Vector2F {
    type Output = Vector2F;
    #[inline]
    fn mul(self, rhs: Self) -> Self {
        Vector2F::new(self.x * rhs.x, self.y * rhs.y)
    }
}

impl Mul<f32> for Vector2F {
    type Output = Vector2F;
    #[inline]
    fn mul(self, rhs: f32) -> Self {
        Vector2F::new(self.x * rhs, self.y * rhs)
    }
}

impl Div for Vector2F {
    type Output = Vector2F;
    #[inline]
    fn div(self, rhs: Self) -> Self {
        Vector2F::new(self.x / rhs.x, self.y / rhs.y)
    }
}

impl AddAssign for Vector2F {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

// ---------------------------------------------------------------------------
// Vector2I
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Vector2I {
    x: i32,
    y: i32,
}

impl Vector2I {
    #[inline]
    pub const fn new(x: i32, y: i32) -> Self {
        Vector2I { x, y }
    }

    #[inline]
    pub const fn zero() -> Self {
        Vector2I { x: 0, y: 0 }
    }

    #[inline]
    pub fn x(self) -> i32 {
        self.x
    }

    #[inline]
    pub fn y(self) -> i32 {
        self.y
    }

    /// Component-wise minimum.
    #[inline]
    pub fn min(self, other: Self) -> Self {
        Vector2I {
            x: self.x.min(other.x),
            y: self.y.min(other.y),
        }
    }

    /// Component-wise maximum.
    #[inline]
    pub fn max(self, other: Self) -> Self {
        Vector2I {
            x: self.x.max(other.x),
            y: self.y.max(other.y),
        }
    }
}

#[inline]
pub fn vec2i(x: i32, y: i32) -> Vector2I {
    Vector2I::new(x, y)
}

// ---------------------------------------------------------------------------
// RectF
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct RectF {
    origin: Vector2F,
    lower_right: Vector2F,
}

impl RectF {
    /// Construct a rectangle from two corner points.
    ///
    /// The inputs are stored verbatim — `from_points(a, b)` does not
    /// normalise. Callers that need a min/max bounding rect must pre-order
    /// the corners (this matches `pathfinder_geometry::RectF::from_points`).
    #[inline]
    pub fn from_points(origin: Vector2F, lower_right: Vector2F) -> Self {
        RectF {
            origin,
            lower_right,
        }
    }

    #[inline]
    pub fn min_x(self) -> f32 {
        self.origin.x
    }

    #[inline]
    pub fn min_y(self) -> f32 {
        self.origin.y
    }

    #[inline]
    pub fn max_x(self) -> f32 {
        self.lower_right.x
    }

    #[inline]
    pub fn max_y(self) -> f32 {
        self.lower_right.y
    }

    /// Return whether `point` is inside this rectangle (inclusive on the
    /// origin edges, exclusive on the far edges — matches pathfinder).
    #[inline]
    pub fn contains_point(self, point: Vector2F) -> bool {
        point.x >= self.origin.x
            && point.y >= self.origin.y
            && point.x < self.lower_right.x
            && point.y < self.lower_right.y
    }

    /// Round the origin down and the lower-right up to the nearest integer
    /// coordinates, producing a containing rectangle with integer corners.
    #[inline]
    pub fn round_out(self) -> RectF {
        RectF::from_points(
            Vector2F::new(self.origin.x.floor(), self.origin.y.floor()),
            Vector2F::new(self.lower_right.x.ceil(), self.lower_right.y.ceil()),
        )
    }

    /// Smallest rectangle containing both `self` and `point`.
    #[inline]
    pub fn union_point(self, point: Vector2F) -> RectF {
        RectF::from_points(self.origin.min(point), self.lower_right.max(point))
    }

    /// Smallest rectangle containing both `self` and `other`.
    #[inline]
    pub fn union_rect(self, other: RectF) -> RectF {
        RectF::from_points(
            self.origin.min(other.origin),
            self.lower_right.max(other.lower_right),
        )
    }

    /// Cast each corner component to an `i32`, truncating toward zero.
    #[inline]
    pub fn to_i32(self) -> RectI {
        RectI::from_points(self.origin.to_i32(), self.lower_right.to_i32())
    }

    /// X coordinate of the origin (alias of `min_x` matching pathfinder's API).
    #[inline]
    pub fn origin_x(self) -> f32 {
        self.origin.x
    }

    /// Y coordinate of the origin (alias of `min_y`).
    #[inline]
    pub fn origin_y(self) -> f32 {
        self.origin.y
    }

    /// `max_x - min_x`. Negative values are possible for non-normalised rects.
    #[inline]
    pub fn width(self) -> f32 {
        self.lower_right.x - self.origin.x
    }

    /// `max_y - min_y`. Negative values are possible for non-normalised rects.
    #[inline]
    pub fn height(self) -> f32 {
        self.lower_right.y - self.origin.y
    }
}

impl Add<Vector2F> for RectF {
    type Output = RectF;
    #[inline]
    fn add(self, v: Vector2F) -> RectF {
        RectF::from_points(self.origin + v, self.lower_right + v)
    }
}

// ---------------------------------------------------------------------------
// RectI
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct RectI {
    origin: Vector2I,
    lower_right: Vector2I,
}

impl RectI {
    /// Construct a rectangle from two corner points (no normalisation; see
    /// [`RectF::from_points`] for rationale).
    #[inline]
    pub fn from_points(origin: Vector2I, lower_right: Vector2I) -> Self {
        RectI {
            origin,
            lower_right,
        }
    }

    #[inline]
    pub fn min_x(self) -> i32 {
        self.origin.x
    }

    #[inline]
    pub fn min_y(self) -> i32 {
        self.origin.y
    }

    #[inline]
    pub fn max_x(self) -> i32 {
        self.lower_right.x
    }

    #[inline]
    pub fn max_y(self) -> i32 {
        self.lower_right.y
    }

    /// The origin (top-left) corner of the rectangle.
    #[inline]
    pub fn origin(self) -> Vector2I {
        self.origin
    }

    /// The lower-right corner of the rectangle.
    #[inline]
    pub fn lower_right(self) -> Vector2I {
        self.lower_right
    }
}

// ---------------------------------------------------------------------------
// LineSegment2F
// ---------------------------------------------------------------------------

#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct LineSegment2F {
    from: Vector2F,
    to: Vector2F,
}

impl LineSegment2F {
    #[inline]
    pub fn new(from: Vector2F, to: Vector2F) -> Self {
        LineSegment2F { from, to }
    }

    #[inline]
    pub fn from(self) -> Vector2F {
        self.from
    }

    #[inline]
    pub fn to(self) -> Vector2F {
        self.to
    }

    #[inline]
    pub fn from_x(self) -> f32 {
        self.from.x
    }

    #[inline]
    pub fn from_y(self) -> f32 {
        self.from.y
    }

    #[inline]
    pub fn to_x(self) -> f32 {
        self.to.x
    }

    #[inline]
    pub fn to_y(self) -> f32 {
        self.to.y
    }
}

// ---------------------------------------------------------------------------
// Matrix2x2F
// ---------------------------------------------------------------------------

/// Row-major 2×2 matrix.
///
/// `row_major(a, b, c, d)` represents:
///
/// ```text
/// [ a  b ]
/// [ c  d ]
/// ```
///
/// applied to a point `(x, y)` as `(a*x + b*y, c*x + d*y)`.
#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct Matrix2x2F {
    pub m11: f32,
    pub m12: f32,
    pub m21: f32,
    pub m22: f32,
}

impl Matrix2x2F {
    #[inline]
    pub const fn row_major(m11: f32, m12: f32, m21: f32, m22: f32) -> Self {
        Matrix2x2F { m11, m12, m21, m22 }
    }

    /// Build a uniform or non-uniform scale matrix from a scalar or vector.
    #[inline]
    pub fn from_scale<S: Into<Vector2F>>(scale: S) -> Self {
        let s = scale.into();
        Matrix2x2F::row_major(s.x(), 0.0, 0.0, s.y())
    }
}

impl From<f32> for Vector2F {
    #[inline]
    fn from(s: f32) -> Self {
        Vector2F::splat(s)
    }
}

// ---------------------------------------------------------------------------
// Transform2F
// ---------------------------------------------------------------------------

/// 2D affine transform.
///
/// Equivalent to `matrix` followed by translation `vector`:
/// `(x, y) ↦ (m11*x + m12*y + vector.x, m21*x + m22*y + vector.y)`.
#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct Transform2F {
    pub matrix: Matrix2x2F,
    pub vector: Vector2F,
}

impl Transform2F {
    /// Construct from the six row-major components of the augmented matrix.
    #[inline]
    pub const fn row_major(
        m11: f32,
        m12: f32,
        m21: f32,
        m22: f32,
        m31: f32,
        m32: f32,
    ) -> Self {
        Transform2F {
            matrix: Matrix2x2F::row_major(m11, m12, m21, m22),
            vector: Vector2F::new(m31, m32),
        }
    }
}

impl Mul<Vector2F> for Transform2F {
    type Output = Vector2F;
    #[inline]
    fn mul(self, v: Vector2F) -> Vector2F {
        Vector2F::new(
            self.matrix.m11 * v.x + self.matrix.m12 * v.y + self.vector.x,
            self.matrix.m21 * v.x + self.matrix.m22 * v.y + self.vector.y,
        )
    }
}

impl Mul<RectF> for Transform2F {
    type Output = RectF;
    #[inline]
    fn mul(self, r: RectF) -> RectF {
        // Transform each of the four corners and rebuild the AABB.
        let p0 = self * Vector2F::new(r.min_x(), r.min_y());
        let p1 = self * Vector2F::new(r.max_x(), r.min_y());
        let p2 = self * Vector2F::new(r.min_x(), r.max_y());
        let p3 = self * Vector2F::new(r.max_x(), r.max_y());
        let mn = p0.min(p1).min(p2).min(p3);
        let mx = p0.max(p1).max(p2).max(p3);
        RectF::from_points(mn, mx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vector_arithmetic() {
        let a = Vector2F::new(1.0, 2.0);
        let b = Vector2F::new(3.0, 4.0);
        assert_eq!(a + b, Vector2F::new(4.0, 6.0));
        assert_eq!(b - a, Vector2F::new(2.0, 2.0));
        assert_eq!(a * b, Vector2F::new(3.0, 8.0));
        assert_eq!(b / a, Vector2F::new(3.0, 2.0));
        assert_eq!(a * 2.0, Vector2F::new(2.0, 4.0));
        assert_eq!(a.min(b), a);
        assert_eq!(a.max(b), b);
        assert_eq!(
            Vector2F::new(5.0, -1.0).clamp(a, b),
            Vector2F::new(3.0, 2.0)
        );
    }

    #[test]
    fn rect_round_out_and_union() {
        let r = RectF::from_points(vec2f(0.5, 1.2), vec2f(2.7, 3.1));
        let r2 = r.round_out();
        assert_eq!(r2.min_x(), 0.0);
        assert_eq!(r2.min_y(), 1.0);
        assert_eq!(r2.max_x(), 3.0);
        assert_eq!(r2.max_y(), 4.0);

        let u = r.union_point(vec2f(-1.0, 5.0));
        assert_eq!(u.min_x(), -1.0);
        assert_eq!(u.max_y(), 5.0);

        let other = RectF::from_points(vec2f(2.0, 0.0), vec2f(4.0, 1.5));
        let u2 = r.union_rect(other);
        assert_eq!(u2.min_x(), 0.5);
        assert_eq!(u2.min_y(), 0.0);
        assert_eq!(u2.max_x(), 4.0);
        assert_eq!(u2.max_y(), 3.1);
    }

    #[test]
    fn rect_contains_point_and_translate() {
        let r = RectF::from_points(vec2f(0.0, 0.0), vec2f(10.0, 10.0));
        assert!(r.contains_point(vec2f(5.0, 5.0)));
        assert!(!r.contains_point(vec2f(10.0, 5.0))); // exclusive on far edge
        assert!(r.contains_point(vec2f(0.0, 0.0))); // inclusive on origin

        let t = r + vec2f(2.0, 3.0);
        assert_eq!(t.min_x(), 2.0);
        assert_eq!(t.max_y(), 13.0);
    }

    #[test]
    fn transform_applies_matrix_then_translation() {
        // Pure translation
        let t = Transform2F::row_major(1.0, 0.0, 0.0, 1.0, 5.0, 7.0);
        assert_eq!(t * vec2f(2.0, 3.0), vec2f(7.0, 10.0));

        // Pure scale
        let s = Transform2F {
            matrix: Matrix2x2F::from_scale(vec2f(2.0, 3.0)),
            vector: Vector2F::zero(),
        };
        assert_eq!(s * vec2f(4.0, 5.0), vec2f(8.0, 15.0));

        // Scale then translate
        let st = Transform2F {
            matrix: Matrix2x2F::from_scale(vec2f(2.0, 3.0)),
            vector: vec2f(1.0, 1.0),
        };
        assert_eq!(st * vec2f(4.0, 5.0), vec2f(9.0, 16.0));

        // row-major 90° rotation: (x, y) -> (-y, x)
        let r = Transform2F::row_major(0.0, -1.0, 1.0, 0.0, 0.0, 0.0);
        assert_eq!(r * vec2f(1.0, 0.0), vec2f(0.0, 1.0));
    }

    #[test]
    fn transform_of_rect_is_aabb_of_corners() {
        // 90° rotation around origin.
        let rot = Transform2F::row_major(0.0, -1.0, 1.0, 0.0, 0.0, 0.0);
        let r = RectF::from_points(vec2f(1.0, 2.0), vec2f(3.0, 4.0));
        let out = rot * r;
        // Corners (1,2) (3,2) (1,4) (3,4) -> (-2,1) (-2,3) (-4,1) (-4,3)
        assert_eq!(out.min_x(), -4.0);
        assert_eq!(out.min_y(), 1.0);
        assert_eq!(out.max_x(), -2.0);
        assert_eq!(out.max_y(), 3.0);
    }

    #[test]
    fn matrix_from_scalar_or_vector_scale() {
        let a = Matrix2x2F::from_scale(2.0);
        assert_eq!(a, Matrix2x2F::row_major(2.0, 0.0, 0.0, 2.0));
        let b = Matrix2x2F::from_scale(vec2f(2.0, 3.0));
        assert_eq!(b, Matrix2x2F::row_major(2.0, 0.0, 0.0, 3.0));
    }
}
