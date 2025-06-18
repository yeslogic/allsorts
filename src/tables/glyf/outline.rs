use std::borrow::Cow;

use pathfinder_geometry::transform2d::{Matrix2x2F, Transform2F};
use pathfinder_geometry::vector::Vector2F;

use crate::binary::read::ReadScope;
use crate::error::ParseError;
use crate::outline::{OutlineBuilder, OutlineSink};
use crate::tables::os2::Os2;
use crate::tables::variable_fonts::gvar::GvarTable;
use crate::tables::variable_fonts::OwnedTuple;
use crate::tables::{FontTableProvider, HheaTable, HmtxTable, MaxpTable};
use crate::tag;

use super::{
    CompositeGlyphComponent, CompositeGlyphScale, Glyph, LocaGlyf, SimpleGlyph,
    COMPOSITE_GLYPH_RECURSION_LIMIT,
};

use contour::{Contour, CurvePoint};

/// Context for visiting possibly variable outlines of glyphs from the `glyf` table
pub struct GlyfVisitorContext<'a, 'data> {
    glyf: &'a mut LocaGlyf,
    variable: Option<VariableGlyfContext<'data>>,
}

/// Tables required to visit variable glyphs
pub struct VariableGlyfContext<'data> {
    /// [gvar][crate::tables::variable::gvar::GvarTable] table
    gvar: GvarTable<'data>,
    /// [hmtx][crate::tables::HmtxTable] table
    hmtx: HmtxTable<'data>,
    /// [vmtx][crate::tables::HmtxTable] table
    vmtx: Option<HmtxTable<'data>>,
    /// [OS/2][crate::tables::Os2] table
    os2: Os2,
    /// [hhea][crate::tables::HmtxTable] table
    hhea: HheaTable,
}

/// Holds tables required to visit variable glyphs
///
/// This type is used in conjunction with [VariableGlyphContext]. It exists to hold the data
/// parsed and held by the context. In an ideal world this data could be held by the context
/// itself, but this required self-referencing types, which are annoying.
pub struct VariableGlyfContextStore<'a> {
    maxp: Cow<'a, [u8]>,
    gvar: Cow<'a, [u8]>,
    hhea: Cow<'a, [u8]>,
    hmtx: Cow<'a, [u8]>,
    vhea: Option<Cow<'a, [u8]>>,
    vmtx: Option<Cow<'a, [u8]>>,
    os2: Cow<'a, [u8]>,
}

#[derive(Copy, Clone)]
struct GlyfVisitorState {
    offset: Vector2F,
    scale: Option<CompositeGlyphScale>,
    depth: u8,
}

impl GlyfVisitorState {
    fn new() -> Self {
        GlyfVisitorState {
            offset: Vector2F::zero(),
            scale: None,
            depth: 0,
        }
    }

    fn transform(&self) -> Transform2F {
        let scale = self
            .scale
            .map_or_else(|| Matrix2x2F::from_scale(1.0), Matrix2x2F::from);
        Transform2F {
            vector: self.offset,
            matrix: scale,
        }
    }
}

impl<'a, 'data> GlyfVisitorContext<'a, 'data> {
    /// Construct a new context for visiting glyphs
    pub fn new(glyf: &'a mut LocaGlyf, variable: Option<VariableGlyfContext<'data>>) -> Self {
        GlyfVisitorContext {
            glyf,
            variable: variable,
        }
    }

    fn visit_outline<S: OutlineSink>(
        &mut self,
        glyph_index: u16,
        tuple: Option<&OwnedTuple>,
        state: GlyfVisitorState,
        sink: &mut S,
    ) -> Result<(), ParseError> {
        if state.depth > COMPOSITE_GLYPH_RECURSION_LIMIT {
            return Err(ParseError::LimitExceeded);
        }

        let glyph = self.glyf.glyph(glyph_index)?;
        let glyph = match (&self.variable, tuple) {
            (Some(var), Some(tuple)) => {
                // Get a copy of the glyph that can be mutated in order to apply the variations
                let mut glyph = Glyph::clone(&glyph);
                glyph.apply_variations(
                    glyph_index,
                    tuple,
                    &var.gvar,
                    &var.hmtx,
                    var.vmtx.as_ref(),
                    Some(&var.os2),
                    &var.hhea,
                )?;
                Cow::Owned(glyph)
            }
            _ => Cow::Borrowed(&*glyph),
        };

        match &*glyph {
            Glyph::Empty(_) => Ok(()),
            Glyph::Simple(simple_glyph) => {
                visit_simple_glyph_outline(sink, state.transform(), simple_glyph)
            }
            Glyph::Composite(composite) => {
                self.visit_composite_glyph_outline(sink, &composite.glyphs, tuple, state.depth)
            }
        }
    }

    fn visit_composite_glyph_outline<S: OutlineSink>(
        &mut self,
        sink: &mut S,
        glyphs: &[CompositeGlyphComponent],
        tuple: Option<&OwnedTuple>,
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
            let state = GlyfVisitorState {
                offset,
                scale: composite_glyph.scale,
                depth: depth + 1,
            };

            self.visit_outline(composite_glyph.glyph_index, tuple, state, sink)?;
        }

        Ok(())
    }
}

impl<'a> VariableGlyfContextStore<'a> {
    /// Read the required tables from the supplied [FontTableProvider][crate::tables::FontTableProvider]
    pub fn read<F: FontTableProvider>(provider: &'a F) -> Result<Self, ParseError> {
        let maxp = provider.read_table_data(tag::MAXP)?;
        let gvar = provider.read_table_data(tag::GVAR)?;
        let hhea = provider.read_table_data(tag::HHEA)?;
        let hmtx = provider.read_table_data(tag::HMTX)?;
        let vhea = provider.table_data(tag::VHEA)?;
        let vmtx = provider.table_data(tag::VMTX)?;
        let os2 = provider.read_table_data(tag::OS_2)?;

        Ok(VariableGlyfContextStore {
            maxp,
            gvar,
            hhea,
            hmtx,
            vhea,
            vmtx,
            os2,
        })
    }
}

impl<'data> VariableGlyfContext<'data> {
    /// TODO
    pub fn new<'a>(store: &'data VariableGlyfContextStore<'data>) -> Result<Self, ParseError> {
        let maxp = ReadScope::new(&store.maxp).read::<MaxpTable>()?;
        let gvar = ReadScope::new(&store.gvar).read::<GvarTable<'data>>()?;
        let hhea = ReadScope::new(&store.hhea).read::<HheaTable>()?;
        let hmtx = ReadScope::new(&store.hmtx).read_dep::<HmtxTable<'_>>((
            usize::from(maxp.num_glyphs),
            usize::from(hhea.num_h_metrics),
        ))?;
        let vhea = store
            .vhea
            .as_ref()
            .map(|vhea_data| ReadScope::new(vhea_data).read::<HheaTable>())
            .transpose()?;
        let vmtx = vhea
            .and_then(|vhea| {
                store.vmtx.as_ref().map(|vmtx_data| {
                    ReadScope::new(vmtx_data).read_dep::<HmtxTable<'_>>((
                        usize::from(maxp.num_glyphs),
                        usize::from(vhea.num_h_metrics),
                    ))
                })
            })
            .transpose()?;

        let os2 = ReadScope::new(&store.os2).read_dep::<Os2>(store.os2.len())?;

        Ok(VariableGlyfContext {
            gvar,
            hmtx,
            vmtx,
            os2,
            hhea,
        })
    }
}

impl<'a, 'data> OutlineBuilder for GlyfVisitorContext<'a, 'data> {
    type Error = ParseError;

    fn visit<V: OutlineSink>(
        &mut self,
        glyph_index: u16,
        tuple: Option<&OwnedTuple>,
        visitor: &mut V,
    ) -> Result<(), Self::Error> {
        self.visit_outline(glyph_index, tuple, GlyfVisitorState::new(), visitor)
    }
}

fn visit_simple_glyph_outline<S: OutlineSink>(
    sink: &mut S,
    transform: Transform2F,
    simple_glyph: &SimpleGlyph,
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

    impl Iterator for Points<'_, '_> {
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

    use crate::binary::read::ReadScope;
    use crate::binary::write::{WriteBinaryDep, WriteBuffer};
    use crate::tables::glyf::tests::{composite_glyph_fixture, simple_glyph_fixture};
    use crate::tables::glyf::{GlyfRecord, GlyfTable, Point, SimpleGlyphFlag};
    use crate::tables::variable_fonts::avar::AvarTable;
    use crate::tables::variable_fonts::fvar::FvarTable;
    use crate::tables::{Fixed, IndexToLocFormat, OpenTypeFont};
    use crate::tests::read_fixture;

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
        let glyphs = GlyfTable {
            records: vec![
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Composite(composite_glyph_fixture(&[]))),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
                GlyfRecord::Parsed(Glyph::Simple(simple_glyph_fixture())),
            ],
        };
        let mut buf = WriteBuffer::new();
        let loca = GlyfTable::write_dep(&mut buf, glyphs, IndexToLocFormat::Short)
            .expect("unable to write glyf table");
        let glyf = buf.into_inner().into_boxed_slice();
        let mut loca_glyf = LocaGlyf::loaded(loca, glyf);
        let mut visitor = TestVisitor {};
        let mut context = GlyfVisitorContext::new(&mut loca_glyf, None);
        context
            .visit(1, None, &mut visitor)
            .expect("error visiting glyph outline");
    }

    #[test]
    fn variable_outlines() -> Result<(), ParseError> {
        let buffer = read_fixture("tests/fonts/variable/Inter[slnt,wght].abc.ttf");
        let scope = ReadScope::new(&buffer);
        let font_file = scope.read::<OpenTypeFont<'_>>()?;
        let provider = font_file.table_provider(0)?;

        // Load the tables
        let fvar_data = provider.read_table_data(tag::FVAR)?;
        let fvar = ReadScope::new(&fvar_data).read::<FvarTable<'_>>().unwrap();
        let avar_data = provider.table_data(tag::AVAR)?;
        let avar = avar_data
            .as_ref()
            .map(|avar_data| ReadScope::new(avar_data).read::<AvarTable<'_>>())
            .transpose()?;
        let mut loca_glyf = LocaGlyf::load(&provider)?;
        let store = VariableGlyfContextStore::read(&provider)?;

        // Get the variation tuple
        //   Subfamily: ExtraBold Italic
        // Coordinates: [800.0, -10.0]
        let user_tuple = [Fixed::from(800), Fixed::from(-10)];
        let tuple = fvar.normalize(user_tuple.iter().copied(), avar.as_ref())?;

        // Visit the outlines
        let var_context = VariableGlyfContext::new(&store)?;
        let mut context = GlyfVisitorContext::new(&mut loca_glyf, Some(var_context));
        let mut visitor = TestVisitor {};

        context
            .visit(1, Some(&tuple), &mut visitor)
            .expect("error visiting glyph outline");

        Ok(())
    }
}
