use pathfinder_geometry::line_segment::LineSegment2F;
use pathfinder_geometry::transform2d::Transform2F;
use pathfinder_geometry::vector::Vector2F;
use svg::node::element::path;

use super::{Color, CompositeMode, ConicGradient, LinearGradient, Painter, RadialGradient};
use crate::outline::OutlineSink;
use crate::tables::cpal::Palette;

struct SvgPainter {
    doc: svg::Document,
    path: Option<path::Data>,
}

impl SvgPainter {
    pub fn new() -> Self {
        SvgPainter {
            doc: svg::Document::new(),
            path: Some(path::Data::new()),
        }
    }
}

impl Painter for SvgPainter {
    type Layer = ();

    fn fill(&self, color: Color) {
        todo!()
    }

    fn linear_gradient(&self, gradient: LinearGradient<'_>, palette: Palette<'_, '_>) {
        todo!()
    }

    fn radial_gradient(&self, gradient: RadialGradient<'_>, palette: Palette<'_, '_>) {
        todo!()
    }

    fn conic_gradient(&self, gradient: ConicGradient<'_>) {
        todo!()
    }

    fn clip(&self) {
        todo!()
    }

    fn begin_layer(&self) {
        todo!()
    }

    fn end_layer(&self) -> Self::Layer {
        todo!()
    }

    fn compose_layers(&self, backdrop: Self::Layer, source: Self::Layer, mode: CompositeMode) {
        todo!()
    }

    fn push_state(&self) {
        todo!()
    }

    fn pop_state(&self) {
        todo!()
    }

    fn transform(&self, transform: Transform2F) {
        todo!()
    }

    fn translate(&self, dx: i16, dy: i16) {
        todo!()
    }

    fn scale(&self, sx: f32, sy: f32, center: Option<(i16, i16)>) {
        todo!()
    }

    fn rotate(&self, angle: f32, center: Option<(i16, i16)>) {
        todo!()
    }

    fn skew(&self, angle_x: f32, angle_y: f32, center: Option<(i16, i16)>) {
        todo!()
    }
}

impl OutlineSink for SvgPainter {
    fn move_to(&mut self, to: Vector2F) {
        let path = self.path.take().unwrap();
        self.path.replace(path.move_to((to.x(), to.y())));
    }

    fn line_to(&mut self, to: Vector2F) {
        let path = self.path.take().unwrap();
        self.path.replace(path.line_to((to.x(), to.y())));
    }

    fn quadratic_curve_to(&mut self, ctrl: Vector2F, to: Vector2F) {
        let path = self.path.take().unwrap();
        self.path
            .replace(path.quadratic_curve_to((ctrl.x(), ctrl.y(), to.x(), to.y())));
    }

    fn cubic_curve_to(&mut self, ctrl: LineSegment2F, to: Vector2F) {
        todo!()
    }

    fn close(&mut self) {
        todo!()
    }
}
