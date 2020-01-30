# Changelog

All notable changes to this project will be documented in this file. The
format is based on [Keep a Changelog], and this project aims to follow
[Semantic Versioning].

## [Unreleased]

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

[unreleased]: https://github.com/yeslogic/allsorts/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/yeslogic/allsorts/releases/tag/v0.1.0
[Keep a Changelog]: https://keepachangelog.com/en/1.0.0
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html
