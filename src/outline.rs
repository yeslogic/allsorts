//! Access glyphs outlines. Requires the `outline` cargo feature (enabled by default).
//!
//! This module is used to access the outlines of glyphs as a series of foundational drawing
//! instruction callbacks on implementors of the `OutlineSink` trait. Outlines from `glyf` and
//! `CFF` tables can be accessed.
//!
//! ### Example
//!
//! This is a fairly complete example of mapping some glyphs and then visiting their outlines with
//! support for TrueType and CFF fonts. It accumulates the drawing operations into a `String`.
//! In a real application you'd probably make calls to a graphics library instead.
//!
//! ```
//! use std::fmt::Write;
//!
//! use allsorts::binary::read::ReadScope;
//! use allsorts::cff::CFF;
//! use allsorts::font::{GlyphTableFlags, MatchingPresentation};
//! use allsorts::font_data::FontData;
//! use allsorts::gsub::RawGlyph;
//! use allsorts::outline::{OutlineBuilder, OutlineSink};
//! use allsorts::pathfinder_geometry::line_segment::LineSegment2F;
//! use allsorts::pathfinder_geometry::vector::Vector2F;
//! use allsorts::tables::glyf::GlyfTable;
//! use allsorts::tables::loca::LocaTable;
//! use allsorts::tables::{FontTableProvider, SfntVersion};
//! use allsorts::{tag, Font};
//!
//! struct DebugVisitor {
//!     outlines: String,
//! }
//!
//! impl OutlineSink for DebugVisitor {
//!     fn move_to(&mut self, to: Vector2F) {
//!         writeln!(&mut self.outlines, "move_to({}, {})", to.x(), to.y()).unwrap();
//!     }
//!
//!     fn line_to(&mut self, to: Vector2F) {
//!         writeln!(&mut self.outlines, "line_to({}, {})", to.x(), to.y()).unwrap();
//!     }
//!
//!     fn quadratic_curve_to(&mut self, control: Vector2F, to: Vector2F) {
//!         writeln!(
//!             &mut self.outlines,
//!             "quad_to({}, {}, {}, {})",
//!             control.x(),
//!             control.y(),
//!             to.x(),
//!             to.y()
//!         )
//!         .unwrap();
//!     }
//!
//!     fn cubic_curve_to(&mut self, control: LineSegment2F, to: Vector2F) {
//!         writeln!(
//!             &mut self.outlines,
//!             "curve_to({}, {}, {}, {}, {}, {})",
//!             control.from_x(),
//!             control.from_y(),
//!             control.to_x(),
//!             control.to_y(),
//!             to.x(),
//!             to.y()
//!         )
//!         .unwrap();
//!     }
//!
//!     fn close(&mut self) {
//!         writeln!(&mut self.outlines, "close()").unwrap();
//!     }
//! }
//!
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let script = tag::LATN;
//!     let buffer = std::fs::read("tests/fonts/opentype/Klei.otf")?;
//!     let scope = ReadScope::new(&buffer);
//!     let font_file = scope.read::<FontData<'_>>()?;
//!     let provider = font_file.table_provider(0)?;
//!     let mut font = Font::new(provider)?;
//!     let mut sink = DebugVisitor {
//!         outlines: String::new(),
//!     };
//!
//!     // Map text to glyphs
//!     let glyphs = font.map_glyphs("+", script, MatchingPresentation::NotRequired);
//!
//!     // Visit the outlines of each glyph. Read tables depending on the type of font
//!     if font.glyph_table_flags.contains(GlyphTableFlags::CFF)
//!         && font.font_table_provider.sfnt_version() == tag::OTTO
//!     {
//!         let cff_data = font.font_table_provider.read_table_data(tag::CFF)?;
//!         let mut cff = ReadScope::new(&cff_data).read::<CFF<'_>>()?;
//!         sink.glyphs_to_path(&mut cff, &glyphs)?;
//!     } else if font.glyph_table_flags.contains(GlyphTableFlags::GLYF) {
//!         let loca_data = font.font_table_provider.read_table_data(tag::LOCA)?;
//!         let loca = ReadScope::new(&loca_data).read_dep::<LocaTable<'_>>((
//!             usize::from(font.maxp_table.num_glyphs),
//!             font.head_table.index_to_loc_format,
//!         ))?;
//!         let glyf_data = font.font_table_provider.read_table_data(tag::GLYF)?;
//!         let mut glyf = ReadScope::new(&glyf_data).read_dep::<GlyfTable<'_>>(&loca)?;
//!         sink.glyphs_to_path(&mut glyf, &glyphs)?;
//!     } else {
//!         return Err("no glyf or CFF table".into());
//!     }
//!
//!     let expected = "move_to(225, 152)
//! line_to(225, 269)
//! curve_to(225, 274, 228, 276, 232, 276)
//! line_to(341, 276)
//! curve_to(346, 276, 347, 285, 347, 295)
//! curve_to(347, 307, 345, 320, 341, 320)
//! line_to(232, 320)
//! curve_to(226, 320, 226, 325, 226, 328)
//! line_to(226, 432)
//! curve_to(220, 435, 214, 437, 206, 437)
//! curve_to(198, 437, 190, 435, 181, 432)
//! line_to(181, 329)
//! curve_to(181, 326, 180, 320, 172, 320)
//! line_to(68, 320)
//! curve_to(62, 320, 59, 311, 59, 300)
//! curve_to(59, 289, 62, 278, 68, 276)
//! line_to(174, 276)
//! curve_to(179, 276, 181, 271, 181, 267)
//! line_to(181, 152)
//! curve_to(181, 147, 193, 144, 204, 144)
//! curve_to(215, 144, 225, 147, 225, 152)
//! close()
//! ";
//!     assert_eq!(sink.outlines, expected);
//!     Ok(())
//! }
//!
//! impl DebugVisitor {
//!     pub fn glyphs_to_path<T>(
//!         &mut self,
//!         builder: &mut T,
//!         glyphs: &[RawGlyph<()>],
//!     ) -> Result<(), Box<dyn std::error::Error>>
//!     where
//!         T: OutlineBuilder,
//!         <T as OutlineBuilder>::Error: 'static,
//!     {
//!         for glyph in glyphs {
//!             builder.visit(glyph.glyph_index, self)?;
//!         }
//!
//!         Ok(())
//!     }
//! }
//! ```

use crate::error::ParseError;
use crate::tables::glyf::Point as GlyfPoint;
use pathfinder_geometry::line_segment::LineSegment2F;
use pathfinder_geometry::rect::{RectF, RectI};
use pathfinder_geometry::vector::Vector2F;

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

pub trait BoundingBox {
    // TODO: Probably want a custom error type that captures the core failure modes
    // TODO: Is this the type we want to return?
    fn bounding_box(&self, glyph_id: u16) -> Result<RectI, ParseError>;
}

pub(crate) struct NullSink;

impl OutlineSink for NullSink {
    fn move_to(&mut self, _to: Vector2F) {}

    fn line_to(&mut self, _to: Vector2F) {}

    fn quadratic_curve_to(&mut self, _ctrl: Vector2F, _to: Vector2F) {}

    fn cubic_curve_to(&mut self, _ctrl: LineSegment2F, _to: Vector2F) {}

    fn close(&mut self) {}
}

impl From<GlyfPoint> for Vector2F {
    fn from(point: GlyfPoint) -> Self {
        Vector2F::new(point.0 as f32, point.1 as f32)
    }
}
