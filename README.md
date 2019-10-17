<h1 align="center">
  <img src="https://files.wezm.net/Allsorts.svg" alt=""><br>
  Allsorts
</h1>

<div align="center">
  <strong>Font parser, shaping engine, and subsetter for OpenType, WOFF, and WOFF2 implemented in Rust</strong>
</div>

<br>

<div align="center">
  <a href="https://travis-ci.com/yeslogic/allsorts">
    <img src="https://travis-ci.com/yeslogic/allsorts.svg?token=4GA6ydxNNeb6XeELrMmg&amp;branch=master" alt="Build Status"></a>
  <a href="https://docs.rs/allsorts">
    <img src="https://docs.rs/allsorts/badge.svg" alt="Documentation">
  </a>
  <a href="https://crates.io/crates/allsorts">
    <img src="https://img.shields.io/crates/v/allsorts.svg" alt="Version">
  </a>
  <a href="https://github.com/yeslogic/allsorts/blob/master/LICENSE">
    <img src="https://img.shields.io/crates/l/allsorts.svg" alt="License">
  </a>
</div>

<br>

Allsorts is a font parser, shaping engine, and subsetter for OpenType, WOFF, and WOFF2
written entirely in Rust. It was extracted from
[Prince](https://www.princexml.com/), a tool that typesets and lays out HTML
and CSS documents into PDF.

The Allsorts shaping engine was developed in conjunction with [a specification
for OpenType shaping](https://github.com/n8willis/opentype-shaping-documents/),
which aims to specify OpenType font shaping behaviour.

## Features

* **Parse** TrueType (ttf), OpenType (otf), WOFF, and WOFF2 files.
* **Shape** Latin, [Indic scripts](https://en.wikipedia.org/wiki/Languages_of_India)
  (Bengali, Devanagari, Gujarati, Gurmukhi, Kannada, Malayalam, Oriya, Tamil, Telugu)
* **Subset** from TrueType (ttf), OpenType (otf), WOFF, and WOFF2 files into OpenType.

## What is font shaping?

Font shaping is the process of taking text in the form of Unicode codepoints
and a font, and laying out glyphs from the font according to the text. This
involves honouring kerning, ligatures, and substitutions specified by the font.
For some languages this is relatively straightforward. For others, such as
Indic scripts it is quite complex. After shaping another library such as
[Pathfinder](https://github.com/servo/pathfinder) or
[FreeType](https://www.freetype.org/) is responsible for rendering the glyphs.
To learn more about text rendering, Andrea Cognolato has a good [overview of
modern font rending on
Linux](https://mrandri19.github.io/2019/07/24/modern-text-rendering-linux-overview.html).
The concepts remain the similar on other platforms.

## Development Status

Allsorts is still under active development but has reached its first release
milestone with its inclusion in Prince 13. In Prince it is responsible for
all font loading, and font shaping with the exception of Arabic scripts.

Currently the font parsing code is hand written. It is planned for this to
eventually be replaced by machine generated code via our [declarative data
definition language project](https://github.com/yeslogic/ddl2/).

## Platform Support

Allsorts CI runs tests on Linux, macOS, and Windows. Via Prince it is also
built for FreeBSD.

## Building and Testing

**Minimum Supported Rust Version:** 1.38.0

To build the crate ensure you have [Rust 1.38.0 or newer installed](https://www.rust-lang.org/tools/install).

Build with `cargo build` and run the tests with `cargo test`.

## Contributing

Contributions are welcome, please refer to [CONTRIBUTING]()
for more details.

## Code of Conduct

We aim to uphold the Rust community standards:

> We are committed to providing a friendly, safe and welcoming environment for
> all, regardless of gender, sexual orientation, disability, ethnicity,
> religion, or similar personal characteristic.

We follow the [Rust code of conduct](https://www.rust-lang.org/policies/code-of-conduct).

## Acknowledgements

* [OpenType shaping documents](https://github.com/n8willis/opentype-shaping-documents/)
  formed the specification from which the shaping engine was implemented.
* [Harfbuzz](https://github.com/harfbuzz/harfbuzz) the widely used open source
  font shaping engine was used as reference for test output.
* The [Adobe Annotated OpenType Specification](https://github.com/adobe-type-tools/aots)
  test suite is used as part of the Allsorts test suite.

## License

Allsorts is distributed under the terms of the Apache License (Version 2.0).

See [LICENSE](LICENSE) for details.
