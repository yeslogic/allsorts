# Changelog

All notable changes to this project will be documented in this file. The
format is based on [Keep a Changelog], and this project aims to follow
[Semantic Versioning].

## [Unreleased]

### Fixed

- Recover from incorrect `loca` offset beyond end of `glyf` when glyph is valid.
- Ensure instruction length is written for composite glyph with empty
  instructions but the WE_HAVE_INSTRUCTIONS flag bit set.

## [0.3.0] - 2020-02-11

### Added

- Support for GSUB Lookup Type 8 (Reverse Chaining Contextual Single
  Substitution).
- Lookup function to retrieve metrics and bitmap data from `EBDT`/`CBDT`
  tables.
  - This constitutes initial, low-level support for bitmap fonts and emoji.

## [0.2.0] - 2020-01-30

### Added

- `CBLC` colour bitmap locator table parsing.
- Tests for black & white emoji.

### Changed

- `GlyfRecord::parse` takes `&mut self` instead of `self`.

### Fixed

- Rejection of simple glyphs with zero contours.
- Incorrect reading of glyphs from `glyf` table when `loca` offsets didn't
  start at zero.
- Issue where a prebase matra would "connect" to a preceding punctuation mark,
  due to the punctuation mark being considered part of the word.

## [0.1.0] - 2019-11-20

- Initial release

[Unreleased]: https://github.com/yeslogic/allsorts/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/yeslogic/allsorts/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/yeslogic/allsorts/releases/tag/v0.1.0

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html
