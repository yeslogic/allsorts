use pathfinder_geometry::transform2d::{Matrix2x2F, Transform2F};
use pathfinder_geometry::vector::Vector2F;

use crate::error::ParseError;
use crate::outline::{OutlineBuilder, OutlineSink};
use crate::tables::glyf::{CompositeGlyph, CompositeGlyphScale, GlyfTable, GlyphData, SimpleGlyph};

use contour::{Contour, CurvePoint};

// There's no limit in the OpenType documentation so we use the same value as Harfbuzz
const RECURSION_LIMIT: u8 = 6;

impl<'a> GlyfTable<'a> {
    fn visit_outline<S: OutlineSink>(
        &mut self,
        glyph_index: u16,
        sink: &mut S,
        offset: Vector2F,
        scale: Option<CompositeGlyphScale>,
        depth: u8,
    ) -> Result<(), ParseError> {
        if depth > RECURSION_LIMIT {
            return Err(ParseError::LimitExceeded);
        }

        let glyph = match self.get_parsed_glyph(glyph_index)? {
            Some(glyph) => glyph.clone(), // FIXME clone
            None => return Ok(()),
        };
        let scale = scale
            .map(|scale| Matrix2x2F::from(scale))
            .unwrap_or(Matrix2x2F::from_scale(1.0));
        let transform = Transform2F {
            vector: offset,
            matrix: scale,
        };

        match &glyph.data {
            GlyphData::Simple(simple_glyph) => {
                Self::visit_simple_glyph_outline(sink, transform, simple_glyph)
            }
            GlyphData::Composite { glyphs, .. } => {
                self.visit_composite_glyph_outline(sink, glyphs, depth)
            }
        }
    }

    fn visit_simple_glyph_outline<S: OutlineSink>(
        sink: &mut S,
        transform: Transform2F,
        simple_glyph: &SimpleGlyph,
    ) -> Result<(), ParseError> {
        let contours = simple_glyph.contours().zip(simple_glyph.contour_flags());
        for (points, flags) in contours {
            let contour = Contour::new(points, flags);

            // Determine origin of the contour and move to it
            let origin = contour.origin();
            sink.move_to(transform * origin);

            // Consume the stream of points...
            let mut points = contour.points();
            // It's assumed that the current location is on curve each time through this loop
            while let Some(next) = points.next() {
                match next {
                    CurvePoint::OnCurve(to) => {
                        sink.line_to(transform * to);
                    }
                    CurvePoint::Control(control) => {
                        match points.next() {
                            Some(CurvePoint::OnCurve(to)) => {
                                sink.quadratic_curve_to(transform * control, transform * to);
                            }
                            Some(CurvePoint::Control(_)) => {
                                // Can't happen as the Points iterator inserts on curve mid-points when two consecutive control points are encountered
                                unreachable!("consecutive control points")
                            }
                            None => {
                                // Wrap around to the first point
                                sink.quadratic_curve_to(transform * control, transform * origin);
                                break;
                            }
                        }
                    }
                }
            }

            sink.close();
        }

        Ok(())
    }

    fn visit_composite_glyph_outline<S: OutlineSink>(
        &mut self,
        sink: &mut S,
        glyphs: &[CompositeGlyph],
        depth: u8,
    ) -> Result<(), ParseError> {
        for composite_glyph in glyphs {
            if composite_glyph.flags.args_are_xy_values() {
                // NOTE: Casts are safe as max value of composite glyph is u16::MAX
                let offset = Vector2F::new(
                    i32::from(composite_glyph.argument1) as f32,
                    i32::from(composite_glyph.argument2) as f32,
                );
                self.visit_outline(
                    composite_glyph.glyph_index,
                    sink,
                    offset,
                    composite_glyph.scale,
                    depth + 1,
                )?;
            } else {
                unimplemented!("args as point numbers not implemented")
            }
        }

        Ok(())
    }
}

impl<'a> OutlineBuilder for GlyfTable<'a> {
    type Error = ParseError;

    // TODO: Rename this method to build_outline or visit outline or something
    fn visit<V: OutlineSink>(
        &mut self,
        glyph_index: u16,
        visitor: &mut V,
    ) -> Result<(), Self::Error> {
        self.visit_outline(glyph_index, visitor, Vector2F::new(0., 0.), None, 0)
    }
}

mod contour {
    use crate::tables::glyf::{Point, SimpleGlyphFlag};
    use pathfinder_geometry::vector::Vector2F;

    pub struct Contour<'points> {
        points: &'points [Point],
        flags: &'points [SimpleGlyphFlag],
    }

    #[derive(Debug, PartialEq)]
    pub enum CurvePoint {
        OnCurve(Vector2F),
        Control(Vector2F),
    }

    pub struct Points<'a, 'points> {
        contour: &'a Contour<'points>,
        i: usize,
        until: usize,
        mid: Option<Vector2F>,
    }

    impl<'points> Contour<'points> {
        pub fn new(points: &'points [Point], flags: &'points [SimpleGlyphFlag]) -> Self {
            assert!(points.len() > 0);
            assert_eq!(points.len(), flags.len());
            Contour { points, flags }
        }

        pub fn origin(&self) -> Vector2F {
            self.calculate_origin().0
        }

        pub fn calculate_origin(&self) -> (Vector2F, usize, usize) {
            match (self.first(), self.last()) {
                (CurvePoint::OnCurve(first), _) => {
                    // Origin is the first point, so start on the second point
                    (first, 1, self.len())
                }
                (CurvePoint::Control(_), CurvePoint::OnCurve(last)) => {
                    // Origin is the last point, so start on the first point and consider
                    // the last point already processed
                    (last, 0, self.len() - 1) // TODO: Test this
                }
                (CurvePoint::Control(first), CurvePoint::Control(last)) => {
                    // Origin is the mid-point between first and last control points.
                    // Start on the first point
                    (first.lerp(last, 0.5), 0, self.len())
                }
            }
        }

        pub fn points<'a>(&'a self) -> Points<'a, 'points> {
            let (_, start, until) = self.calculate_origin();
            Points {
                contour: self,
                i: start,
                until,
                mid: None,
            }
        }

        pub fn first(&self) -> CurvePoint {
            self.get(0)
        }

        pub fn last(&self) -> CurvePoint {
            self.get(self.points.len() - 1)
        }

        pub fn len(&self) -> usize {
            self.points.len()
        }

        fn get(&self, index: usize) -> CurvePoint {
            let point = self.points[index];
            let flags = self.flags[index];
            CurvePoint::new(point, flags.is_on_curve())
        }
    }

    impl<'a, 'points> Iterator for Points<'a, 'points> {
        type Item = CurvePoint;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(mid) = self.mid {
                self.mid = None;
                return Some(CurvePoint::OnCurve(mid));
            }

            if self.i >= self.until {
                return None;
            }

            let point = match self.contour.get(self.i) {
                point @ CurvePoint::OnCurve(_) => point,
                CurvePoint::Control(control) => {
                    // Check the next point, wrapping around if needed
                    match self.contour.get((self.i + 1) % self.contour.len()) {
                        CurvePoint::OnCurve(_) => CurvePoint::Control(control),
                        CurvePoint::Control(control2) => {
                            // Next point is a control point, yield mid point as on curve point
                            // after this one
                            self.mid = Some(control.lerp(control2, 0.5));
                            CurvePoint::Control(control)
                        }
                    }
                }
            };

            self.i += 1;
            Some(point)
        }
    }

    impl CurvePoint {
        fn new(point: Point, on_curve: bool) -> Self {
            if on_curve {
                CurvePoint::OnCurve(Vector2F::from(point))
            } else {
                CurvePoint::Control(Vector2F::from(point))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pathfinder_geometry::line_segment::LineSegment2F;
    use pathfinder_geometry::vector::vec2f;

    use crate::tables::glyf::tests::{composite_glyph_fixture, simple_glyph_fixture};
    use crate::tables::glyf::{GlyfRecord, Point, SimpleGlyphFlag};

    use super::*;

    struct TestVisitor {}

    impl OutlineSink for TestVisitor {
        fn move_to(&mut self, to: Vector2F) {
            println!("move_to({}, {})", to.x(), to.y());
        }

        fn line_to(&mut self, to: Vector2F) {
            println!("line_to({}, {})", to.x(), to.y());
        }

        fn quadratic_curve_to(&mut self, control: Vector2F, to: Vector2F) {
            println!(
                "quad_to({}, {}, {}, {})",
                control.x(),
                control.y(),
                to.x(),
                to.y()
            );
        }

        fn cubic_curve_to(&mut self, control: LineSegment2F, to: Vector2F) {
            println!(
                "curve_to({}, {}, {}, {}, {}, {})",
                control.from_x(),
                control.from_y(),
                control.to_x(),
                control.to_y(),
                to.x(),
                to.y()
            );
        }

        fn close(&mut self) {
            println!("close()");
        }
    }

    #[test]
    fn iter_simple_glyph_contours() {
        let glyph = simple_glyph_fixture();
        let simple_glyph = match glyph.data {
            GlyphData::Simple(simple) => simple,
            _ => unreachable!(),
        };
        let contours = simple_glyph.contours().collect::<Vec<_>>();
        let expected = &[&[
            Point(433, 77),
            Point(499, 30),
            Point(625, 2),
            Point(756, -27),
            Point(915, -31),
            Point(891, -47),
            Point(862, -60),
            Point(832, -73),
            Point(819, -103),
        ]];
        assert_eq!(&contours, expected);
    }

    #[test]
    fn iter_points() {
        let raw_points = &[Point(0, 0), Point(10, 40), Point(30, 40), Point(40, 10)];
        let flags = &[
            SimpleGlyphFlag::ON_CURVE_POINT,
            SimpleGlyphFlag::empty(), // control
            SimpleGlyphFlag::empty(), // control
            SimpleGlyphFlag::ON_CURVE_POINT,
        ];
        let contour = Contour::new(raw_points, flags);
        let points = contour.points().collect::<Vec<_>>();
        let expected = &[
            CurvePoint::Control(vec2f(10., 40.)),
            CurvePoint::OnCurve(vec2f(20., 40.)), // mid point
            CurvePoint::Control(vec2f(30., 40.)),
            CurvePoint::OnCurve(vec2f(40., 10.)),
        ];
        assert_eq!(contour.origin(), vec2f(0., 0.));
        assert_eq!(&points, expected);
    }

    #[test]
    fn outlines() {
        let mut glyf = GlyfTable {
            records: vec![
                GlyfRecord::Parsed(simple_glyph_fixture()),
                GlyfRecord::Parsed(composite_glyph_fixture(&[])),
                GlyfRecord::Parsed(simple_glyph_fixture()),
                GlyfRecord::Parsed(simple_glyph_fixture()),
                GlyfRecord::Parsed(simple_glyph_fixture()),
                GlyfRecord::Parsed(simple_glyph_fixture()),
            ],
        };
        let mut visitor = TestVisitor {};
        glyf.visit(1, &mut visitor)
            .expect("error visiting glyph outline");
    }
}
