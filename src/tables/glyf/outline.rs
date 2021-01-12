use pathfinder_geometry::transform2d::{Matrix2x2F, Transform2F};
use pathfinder_geometry::vector::Vector2F;

use crate::error::ParseError;
use crate::outline::{OutlineBuilder, OutlineSink};
use crate::tables::glyf::{CompositeGlyphScale, GlyfTable, GlyphData, Point, SimpleGlyphFlag};

#[derive(Debug, PartialEq)]
enum CurvePoint {
    OnCurve(Vector2F),
    Control(Vector2F),
}

struct Points<'a, 'p, 'f> {
    contour: &'a Contour<'p, 'f>,
    i: usize,
    mid: Option<Vector2F>,
}

struct Contour<'p, 'f> {
    points: &'p [Point],
    flags: &'f [SimpleGlyphFlag],
}

impl<'a> GlyfTable<'a> {
    fn visit_outline<S>(
        &mut self,
        glyph_index: u16,
        sink: &mut S,
        offset_x: f32,
        offset_y: f32,
        scale: Option<CompositeGlyphScale>,
    ) -> Result<(), ParseError>
    where
        S: OutlineSink,
    {
        let glyph = match self.get_parsed_glyph(glyph_index)? {
            Some(glyph) => glyph.clone(), // FIXME clone
            None => return Ok(()),
        };

        let offset = Vector2F::new(offset_x, offset_y); // TODO: Move to argument
        let scale = scale
            .map(|scale| Matrix2x2F::from(scale))
            .unwrap_or(Matrix2x2F::from_scale(1.0));
        let transform = Transform2F {
            vector: offset,
            matrix: scale,
        };

        match &glyph.data {
            GlyphData::Simple(simple_glyph) => {
                let contours = simple_glyph.contours().zip(simple_glyph.contour_flags());
                for (points, flags) in contours {
                    let contour = Contour::new(points, flags);

                    // Determine origin of the contour and move to it
                    let (origin, start, end) = match (contour.first(), contour.last()) {
                        (CurvePoint::OnCurve(first), _) => {
                            // Origin is the first point, so start on the point after this one
                            (first, 1, contour.len())
                        }
                        (CurvePoint::Control(first), CurvePoint::OnCurve(last)) => {
                            // Origin is the last point, so start on the first point and consider
                            // the last point already processed
                            ((last), 0, contour.len() - 1) // TODO: Test this
                        }
                        (CurvePoint::Control(first), CurvePoint::Control(last)) => {
                            // Origin is the mid-point between first and last control points.
                            // Start on the first point
                            ((first).lerp(last, 0.5), 0, contour.len())
                        }
                    };
                    sink.move_to(transform * origin);

                    // Consume the stream of points...
                    let mut points = contour.points().skip(start); // TODO: Handle ending early (with end)
                                                                   // let mut point = contour.get(start); // TODO: Consider making this a Point if it's always on curve
                                                                   // It's assumed that the current location is on curve each time through this loop
                    while let Some(next) = points.next() {
                        match next {
                            CurvePoint::OnCurve(dest) => {
                                sink.line_to(transform * dest);
                                // point = next;
                            }
                            CurvePoint::Control(control) => {
                                match points.next() {
                                    Some(CurvePoint::OnCurve(dest)) => {
                                        sink.quadratic_curve_to(
                                            transform * control,
                                            transform * dest,
                                        );
                                        // point = CurvePoint::OnCurve(dest);
                                    }
                                    Some(CurvePoint::Control(_)) => {
                                        unreachable!("consecutive control points")
                                    }
                                    None => {
                                        // Wrap around to the first point
                                        match contour.first() {
                                            CurvePoint::OnCurve(first) => {
                                                sink.quadratic_curve_to(
                                                    transform * control,
                                                    transform * first,
                                                );
                                            }
                                            CurvePoint::Control(control2) => {
                                                let mid = control.lerp(control2, 0.5);
                                                sink.quadratic_curve_to(
                                                    transform * control,
                                                    transform * mid,
                                                );
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    sink.close();
                }
            }
            GlyphData::Composite { glyphs, .. } => {
                for composite_glyph in glyphs {
                    // TODO: Impose recursion limit

                    if composite_glyph.flags.args_are_xy_values() {
                        // NOTE: Casts are safe as max value of composite glyph is u16::MAX
                        let offset_x = i32::from(composite_glyph.argument1) as f32;
                        let offset_y = i32::from(composite_glyph.argument2) as f32;
                        self.visit_outline(
                            composite_glyph.glyph_index,
                            sink,
                            offset_x,
                            offset_y,
                            composite_glyph.scale,
                        )?;
                    } else {
                        unimplemented!("args as point numbers not implemented")
                    }
                }
            }
        }

        Ok(())
    }
}

impl<'a> OutlineBuilder for GlyfTable<'a> {
    // TODO: Rename this method to build_outline or visit outline or something
    fn visit<V: OutlineSink>(
        &mut self,
        glyph_index: u16,
        visitor: &mut V,
    ) -> Result<(), ParseError> {
        self.visit_outline(glyph_index, visitor, 0., 0., None)
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

impl<'p, 'f> Contour<'p, 'f> {
    fn new(points: &'p [Point], flags: &'f [SimpleGlyphFlag]) -> Self {
        assert_eq!(points.len(), flags.len()); // TODO: Handle empty contour
        Contour { points, flags }
    }

    fn points<'a>(&'a self) -> Points<'a, 'p, 'f> {
        Points {
            contour: self,
            i: 0,
            mid: None,
        }
    }

    fn get(&self, index: usize) -> CurvePoint {
        // FIXME: Is this wrap around behaviour needed?
        let len = self.points.len();
        let point = self.points[index % len];
        let flags = self.flags[index % len];
        CurvePoint::new(point, flags.is_on_curve())
    }

    fn first(&self) -> CurvePoint {
        self.get(0)
    }

    fn last(&self) -> CurvePoint {
        self.get(self.points.len() - 1)
    }

    fn len(&self) -> usize {
        self.points.len()
    }
}

impl<'a, 'p, 'f> Iterator for Points<'a, 'p, 'f> {
    type Item = CurvePoint;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(mid) = self.mid {
            self.mid = None;
            return Some(CurvePoint::OnCurve(mid));
        }

        if self.i >= self.contour.len() {
            return None;
        }

        let point = match self.contour.get(self.i) {
            point @ CurvePoint::OnCurve(_) => point,
            CurvePoint::Control(control) => {
                match self.contour.get(self.i + 1) {
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

#[cfg(test)]
mod tests {
    use pathfinder_geometry::line_segment::LineSegment2F;
    use pathfinder_geometry::vector::vec2f;

    use crate::tables::glyf::tests::{composite_glyph_fixture, simple_glyph_fixture};
    use crate::tables::glyf::GlyfRecord;

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
