#![warn(rust_2018_idioms)]
#![doc(html_logo_url = "https://github.com/yeslogic/allsorts/raw/master/allsorts.svg?sanitize=1")]

//! # Font parser, shaping engine, and subsetter
//!
//! Allsorts is a font parser, shaping engine, and subsetter for OpenType, WOFF, and WOFF2
//! written entirely in Rust. It was extracted from
//! [Prince](https://www.princexml.com/), a tool that typesets and lays out HTML
//! and CSS documents into PDF.
//!
//! The Allsorts shaping engine was developed in conjunction with [a specification
//! for OpenType shaping](https://github.com/n8willis/opentype-shaping-documents/),
//! which aims to specify OpenType font shaping behaviour.
//!
//! ## Features
//!
//! * **Parse** TrueType (`ttf`), OpenType (`otf`), WOFF, and WOFF2 files.
//! * **Shape** Arabic, Latin, [Indic scripts](https://en.wikipedia.org/wiki/Languages_of_India)
//!   (Bengali, Devanagari, Gujarati, Gurmukhi, Kannada, Malayalam, Oriya, Tamil, Telugu).
//! * **Subset** from TrueType, OpenType, WOFF, and WOFF2 files into OpenType.
//!
//! ## What is font shaping?
//!
//! Font shaping is the process of taking text in the form of Unicode codepoints
//! and a font, and laying out glyphs from the font according to the text. This
//! involves honouring kerning, ligatures, and substitutions specified by the font.
//! For some languages this is relatively straightforward. For others, such as
//! Indic scripts it is quite complex. After shaping, another library such as
//! [Pathfinder](https://github.com/servo/pathfinder) or
//! [FreeType](https://www.freetype.org/) is responsible for rendering the glyphs.
//! To learn more about text rendering, Andrea Cognolato has a good [overview of
//! modern font rending on
//! Linux](https://mrandri19.github.io/2019/07/24/modern-text-rendering-linux-overview.html).
//! The concepts remain similar on other platforms.
//!
//! ## Examples
//!
//! Refer to the [Allsorts Tools repository](https://github.com/yeslogic/allsorts-tools) for
//! a trio of tools that exercise Allsorts font parsing, shaping, and subsetting.
//!
//! ## Unimplemented Features / Known Issues
//!
//! We don't currently support:
//!
//! * Shaping Hebrew, Tibetan, and Mongolian.
//! * Apple's [morx table](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html).
//! * Emoji.
//! * Unicode normalisation.
//!
//! Known limitations:
//!
//! * The crate is not well documented yet ([#5](https://github.com/yeslogic/allsorts/issues/5)).
//! * Allsorts does not do font lookup/matching. For this something like
//!   [font-kit](https://github.com/pcwalton/font-kit) is recommended.
//! * The subsetting implementation is tailored towards PDF font embedding (mostly
//!   the `cmap0` argument to
//!   [the subset function](https://docs.rs/allsorts/latest/allsorts/subset/fn.subset.html))
//!   at the moment.
//!
//! ## Development Status
//!
//! Allsorts is still under active development but has reached its first release
//! milestone with its inclusion in Prince 13. In Prince it is responsible for
//! all font loading, and font shaping.
//!
//! Currently the font parsing code is handwritten. It is planned for this to
//! eventually be replaced by machine generated code via our [declarative data
//! definition language project](https://github.com/yeslogic/ddl/).
//!
//! ## Platform Support
//!
//! Allsorts CI runs tests on Linux, macOS, and Windows. Via Prince it is also
//! built for FreeBSD.
//!
//! ## Building and Testing
//!
//! **Minimum Supported Rust Version:** 1.38.0
//!
//! To build the crate ensure you have [Rust 1.38.0 or newer installed](https://www.rust-lang.org/tools/install).
//!
//! Build with `cargo build` and run the tests with `cargo test`.
//!
//! ## License
//!
//! Allsorts is distributed under the terms of the Apache License (Version 2.0).
//!
//! See [LICENSE](https://github.com/yeslogic/allsorts/blob/master/LICENSE) for details.

pub mod big5;
pub mod binary;
pub mod cbdt;
pub mod cff;
pub mod checksum;
pub mod context;
pub mod error;
pub mod font_data_impl;
pub mod fontfile;
pub mod gdef;
pub mod get_name;
pub mod glyph_info;
pub mod gpos;
pub mod gsub;
pub mod layout;
pub mod macroman;
pub mod opentype;
pub mod post;
pub mod scripts;
pub mod size;
pub mod subset;
pub mod tables;
pub mod tag;
#[cfg(test)]
pub mod tests;
pub mod unicode;
pub mod woff;
pub mod woff2;

// Note from rental docs:
// NOTE for Rust 2018: Relying on implicit crate imports may cause compile errors in code generated
// by this macro. To avoid this, import the crate manually like so
#[macro_use]
extern crate rental;

pub use tinyvec;

#[macro_export]
macro_rules! read_table {
    ($source:expr, $tag:path, $t:ty, $index:expr) => {
        $source
            .read_table($tag, $index)?
            .ok_or(ParseError::MissingValue)?
            .scope()
            .read::<$t>()
    };
}
