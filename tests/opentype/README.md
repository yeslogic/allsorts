# OpenType Test Fixtures

This directory contains OpenType fixtures.

## test-font.ttx

This an XML description of a font file. `test-font.ttf` is generated from it. This test font
is deliberately tiny and only contains one non-empty glyph to make asserting details about
its properties easy.

`test-font.ttf` is generated with [fonttools] as follows. `fonttools` is
packaged as `fonttools` in Homebrew and Debian based systems and
`python-fonttools` on Arch Linux:

    ttx  -o test-font.ttf test-font.ttx

## head.bin

`head.bin` contains the `head` table from `test-font.ttf`. It was generated with the dump tool:

    cargo run --bin dump -- -t head tests/opentype/test-font.ttf > tests/opentype/head.bin

## hmtx.bin

`hmtx.bin` contains the `hmtx` table from `Ubuntu-R.ttf`. It was generated with the dump tool:

    cargo run --bin dump -- -t hmtx ../../../data/fonts/Ubuntu-R.ttf > tests/opentype/hmtx.bin

## name.bin

`name.bin` contains the `name` table from `test-font.ttf`. It was generated with the dump tool:

    cargo run --bin dump -- -t name ../../../data/fonts/test-font.ttf > tests/opentype/name.bin

## SFNT-TTF-Composite.ttf

`SFNT-TTF-Composite.ttf` was sourced from the [W3C woff2-tests][W3C woff2-tests-file].

## HardGothicNormal.ttf

This font uses the somewhat uncommon cmap subtable format 2.

`HardGothicNormal.ttf` was sourced from the [Tucows Archive][tucows-hardgothic].

## NotoSansJP-Regular.otf

This is a CID keyed CFF font, part of the Noto font family used under the terms of the
Open Font License 1.1. See `NOTO-LICENSE` for the full text.

[fonttools]: https://github.com/fonttools/fonttools
[W3C woff2-tests-file]: https://github.com/w3c/woff2-tests/blob/7efc18fb4d4c488ef7ebe04e6cb80ee0ef36741f/generators/resources/SFNT-TTF-Composite.ttf
[tucows-hardgothic]: https://archive.org/details/tucows_305977_Hard_Gothic_Normal
