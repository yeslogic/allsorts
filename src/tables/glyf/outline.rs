use pathfinder_geometry::transform2d::{Matrix2x2F, Transform2F};
use pathfinder_geometry::vector::Vector2F;

use crate::error::ParseError;
use crate::outline::{OutlineBuilder, OutlineSink};
use crate::tables::glyf::{
    CompositeGlyphComponent, CompositeGlyphScale, GlyfTable, Glyph, SimpleGlyph,
    COMPOSITE_GLYPH_RECURSION_LIMIT,
};

use contour::{Contour, CurvePoint};

impl<'a> GlyfTable<'a> {
    fn visit_outline<S: OutlineSink>(
        &mut self,
        glyph_index: u16,
        sink: &mut S,
        offset: Vector2F,
        scale: Option<CompositeGlyphScale>,
        depth: u8,
    ) -> Result<(), ParseError> {
        if depth > COMPOSITE_GLYPH_RECURSION_LIMIT {
            return Err(ParseError::LimitExceeded);
        }

        let glyph = self.get_parsed_glyph(glyph_index)?;
        let scale = scale.map_or(Matrix2x2F::from_scale(1.0), Matrix2x2F::from);
        let transform = Transform2F {
            vector: offset,
            matrix: scale,
        };

        match &glyph {
            Glyph::Empty(_) => Ok(()),
            Glyph::Simple(simple_glyph) => {
                Self::visit_simple_glyph_outline(sink, transform, simple_glyph)
            }
            Glyph::Composite(composite) => {
                // Have to clone glyphs otherwise glyph is mutably borrowed as &mut self as well
                // as borrowed via the `glyphs` argument.
                let glyphs = composite.glyphs.clone();
                self.visit_composite_glyph_outline(sink, &glyphs, depth)
            }
        }
    }

    fn visit_simple_glyph_outline<S: OutlineSink>(
        sink: &mut S,
        transform: Transform2F,
        simple_glyph: &SimpleGlyph<'_>,
    ) -> Result<(), ParseError> {
        for points_and_flags in simple_glyph.contours() {
            let contour = Contour::new(points_and_flags);

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
                                // Can't happen as the Points iterator inserts on curve mid-points
                                // when two consecutive control points are encountered
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
        glyphs: &[CompositeGlyphComponent],
        depth: u8,
    ) -> Result<(), ParseError> {
        for composite_glyph in glyphs {
            // Argument1 and argument2 can be either x and y offsets to be added to the glyph (the
            // ARGS_ARE_XY_VALUES flag is set), or two point numbers (the ARGS_ARE_XY_VALUES flag
            // is not set). In the latter case, the first point number indicates the point that is
            // to be matched to the new glyph. The second number indicates the new glyph’s
            // “matched” point. Once a glyph is added, its point numbers begin directly after the
            // last glyphs (endpoint of first glyph + 1).
            //
            // https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#composite-glyph-description
            let offset = if composite_glyph.flags.args_are_xy_values() {
                // NOTE: Casts are safe as max value of composite glyph is u16::MAX
                Vector2F::new(
                    i32::from(composite_glyph.argument1) as f32,
                    i32::from(composite_glyph.argument2) as f32,
                )
            } else {
                // TODO: support args as point numbers
                Vector2F::zero()
            };

            self.visit_outline(
                composite_glyph.glyph_index,
                sink,
                offset,
                composite_glyph.scale,
                depth + 1,
            )?;
        }

        Ok(())
    }
}

impl<'a> OutlineBuilder for GlyfTable<'a> {
    type Error = ParseError;

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
        points_and_flags: &'points [(SimpleGlyphFlag, Point)],
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
        pub fn new(points_and_flags: &'points [(SimpleGlyphFlag, Point)]) -> Self {
            assert!(points_and_flags.len() > 0);
            Contour { points_and_flags }
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
            self.get(self.points_and_flags.len() - 1)
        }

        pub fn len(&self) -> usize {
            self.points_and_flags.len()
        }

        fn get(&self, index: usize) -> CurvePoint {
            let (flags, point) = self.points_and_flags[index];
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
        let simple_glyph = simple_glyph_fixture();
        let contours = simple_glyph
            .contours()
            .map(|contour| contour.iter().map(|(_, point)| *point).collect::<Vec<_>>())
            .collect::<Vec<_>>();
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
        let points_and_flags = &[
            (SimpleGlyphFlag::ON_CURVE_POINT, Point::zero()),
            (SimpleGlyphFlag::empty(), Point(10, 40)), // control
            (SimpleGlyphFlag::empty(), Point(30, 40)), // control
            (SimpleGlyphFlag::ON_CURVE_POINT, Point(40, 10)),
        ];
        let contour = Contour::new(points_and_flags);
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
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Composite(composite_glyph_fixture(&[]))),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
            ],
        };
        let mut visitor = TestVisitor {};
        glyf.visit(1, &mut visitor)
            .expect("error visiting glyph outline");
    }
}
