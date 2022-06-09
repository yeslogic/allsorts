# Changelog

All notable changes to this project will be documented in this file. The
format is based on [Keep a Changelog], and this project aims to follow
[Semantic Versioning].

## [Unreleased]

## [0.9.1] - 2022-06-09

### Changed

- Bump regex dev dependency

## [0.9.0] - 2022-03-25

### Fixed

- Subsetting now produces a valid standalone font for use outside PDF.
  [#27](https://github.com/yeslogic/allsorts/issues/27)
- Workaround fonts that contain invalid final `cmap` format 4 segment.
  [b6a9fd6](https://github.com/yeslogic/allsorts/commit/b6a9fd6dacfa2f7f6b9e3896fc0de0dc6be1500a)

## [0.8.0] - 2022-03-01

### Added

- Upgrade tables and dependencies to use Unicode 14.0 data.
- Allow the tables to consult for images to be specified.
  [#57](https://github.com/yeslogic/allsorts/pull/57)

### Changed

- Reorganise subsetting code in preparation for future changes.

## [0.7.1] - 2022-01-18

### Fixed

- Emit error if neither of the features for selecting a flate2 backend are
  provided.

## [0.7.0] - 2022-01-14

### Added

- Cargo features to allow selecting different backends of the flate2 crate.
  [#50](https://github.com/yeslogic/allsorts/issues/50)

## [0.6.0] - 2021-07-20

### Added

- Support for shaping Khmer, Lao, Sinhala, and Thai text.
- Support for laying out glyphs and obtaining their positions.
- Support for accessing glyph contours.
- Allow extra OpenType features to be supplied to `Font::shape`.
- Provide `offset_table` accessor on `OpenTypeFont`.

### Changed

- Combine `gpos::Info` and `gpos::Attachment` into `gpos::Placement`.
- `GPOS` performance improvements.
- Optimised handling of CFF fonts with custom characters sets.
- Always apply default shaping for complex scripts.
- Apply Unicode mark reordering to more scripts, not just Arabic and Indic
  scripts.
- Bump MSRV to 1.51.0.

### Fixed

- Handle `post` tables that map more than one glyph to the same name index.
- Handle non-adjacent cursive connections in `GPOS`.
- Support version 1.1 `GPOS`/`GSUB` tables.
- Fixed a mistake that resulted in `vmtx` table being repeatedly cloned in
  `Font`.
- Avoid some allocations when working with glyphs.

## [0.5.1] - 2020-12-18

### Fixed

- Updated README to reflect current feature set.

## [0.5.0] - 2020-12-18

### Added

- Simplified shaping API on `Font`.
- Unified API for accessing images in `CBLC`/`sbix`/`SVG` tables.
- Language specific shaping rules via `locl`
- Support for shaping Syriac.
- Export `tag!` macro.
- GSUB caching in Arabic, Syriac, and Indic.
- Support fonts with Big5 encoded cmap subtables.

### Changed

- Rename some types and methods to better reflect their function.

## [0.4.0] - 2020-06-17

### Added

- Support for deriving glyph names from post and cmap.
- Support for more OpenType features in gsub: common_ligatures,
  discretionary_ligatures, historical_ligatures, contextual_ligatures,
  small_caps, all_small_caps, lining_nums, oldstyle_nums, proportional_nums,
  tabular_nums, diagonal_fractions, stacked_fractions, ordinal, and
  slashed_zero.
- Consideration of variation selector when looking up glyph index.
- Support for shaping Arabic text.

### Changed

- Depend on just `num-traits` instead of `num`.
- Store RawVec unicodes in a TinyVec

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

[Unreleased]: https://github.com/yeslogic/allsorts/compare/v0.9.1...HEAD
[0.9.1]: https://github.com/yeslogic/allsorts/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/yeslogic/allsorts/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/yeslogic/allsorts/compare/v0.7.1...v0.8.0
[0.7.1]: https://github.com/yeslogic/allsorts/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/yeslogic/allsorts/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/yeslogic/allsorts/compare/v0.5.1...v0.6.0
[0.5.1]: https://github.com/yeslogic/allsorts/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/yeslogic/allsorts/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/yeslogic/allsorts/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/yeslogic/allsorts/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/yeslogic/allsorts/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/yeslogic/allsorts/releases/tag/v0.1.0

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html
