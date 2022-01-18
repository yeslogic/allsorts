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
//! * **Shape** Arabic, Cyrillic, Greek, Hebrew,
//!   [Indic scripts](https://en.wikipedia.org/wiki/Languages_of_India)
//!   (Bengali, Devanagari, Gujarati, Gurmukhi, Kannada, Malayalam, Oriya, Sinhala, Tamil, Telugu),
//!   Khmer, Lao, Latin, Syriac, Thai, and other scripts.
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
//! a set of tools that exercise Allsorts font parsing, shaping, and subsetting.
//!
//! ## Unimplemented Features / Known Issues
//!
//! We don't currently support:
//!
//! * Shaping Mongolian, and Tibetan.
//! * Apple's [morx table](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html).
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
//! definition language project](https://github.com/yeslogic/fathom).
//!
//! ## Platform Support
//!
//! Allsorts CI runs tests on Linux, macOS, and Windows. Via Prince it is also
//! built for FreeBSD.
//!
//! ## Building and Testing
//!
//! **Minimum Supported Rust Version:** 1.51.0
//!
//! To build the crate ensure you have [Rust 1.51.0 or newer installed](https://www.rust-lang.org/tools/install).
//!
//! Build with `cargo build` and run the tests with `cargo test`.
//!
//! ### Cargo Features
//!
//! | Feature       | Description                              | Default Enabled | Extra Dependencies    |
//! |---------------|------------------------------------------|:---------------:|-----------------------|
//! | `outline`     | Enable code for accessing glyph outlines |        ✅       | `pathfinder_geometry` |
//! | `flate2_zlib` | Use the zlib backend to flate2           |        ✅       | `zlib`                |
//! | `flate2_rust` | Use the Rust backend to flate2           |        ❌       | `miniz_oxide`         |
//! | `prince`      | Enable Prince specific tests and code    |        ❌       |                       |
//!
//! **Note:** In our testing the `zlib` `flate2` backend was faster but you may
//! prefer the Rust backend for a pure Rust solution when compiling to WASM or
//! similar.
//!
//! ## Contributing
//!
//! Contributions are welcome, please refer to the
//! [contributing document](https://github.com/yeslogic/allsorts/blob/master/CONTRIBUTING.md)
//! for more details.
//!
//! ## Code of Conduct
//!
//! We aim to uphold the Rust community standards:
//!
//! > We are committed to providing a friendly, safe and welcoming environment for
//! > all, regardless of gender, sexual orientation, disability, ethnicity,
//! > religion, or similar personal characteristic.
//!
//! We follow the [Rust code of conduct](https://www.rust-lang.org/policies/code-of-conduct).
//!
//! ## Acknowledgements
//!
//! * [OpenType shaping documents](https://github.com/n8willis/opentype-shaping-documents/)
//!   forms the specification from which the shaping engine is implemented.
//! * [Harfbuzz](https://github.com/harfbuzz/harfbuzz) the widely used open source
//!   font shaping engine was used as reference for test output.
//! * The [Adobe Annotated OpenType Specification](https://github.com/adobe-type-tools/aots)
//!   test suite is used as part of the Allsorts test suite.
//! * [ttf-parser](https://github.com/RazrFalcon/ttf-parser) for CFF CharString parsing code.
//!
//! ## License
//!
//! Allsorts is distributed under the terms of the Apache License (Version 2.0).
//!
//! See [LICENSE](https://github.com/yeslogic/allsorts/blob/master/LICENSE) for details.

pub mod big5;
pub mod binary;
pub mod bitmap;
pub mod cff;
pub mod checksum;
pub mod context;
pub mod error;
pub mod font;
pub mod font_data;
pub mod gdef;
pub mod get_name;
pub mod glyph_info;
pub mod glyph_position;
pub mod gpos;
pub mod gsub;
pub mod layout;
pub mod macroman;
#[cfg(feature = "outline")]
pub mod outline;
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

pub use font::Font;
#[cfg(feature = "outline")]
pub use pathfinder_geometry;
pub use tinyvec;

pub const DOTTED_CIRCLE: char = '◌';
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

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

#[cfg(not(any(feature = "flate2_zlib", feature = "flate2_rust")))]
compile_error!("Allsorts is being built without one of `flate2_zlib` or `flate2_rust` Cargo features enabled. One of these must be enabled");
