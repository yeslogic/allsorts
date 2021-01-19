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

`head.bin` contains the `head` table from `test-font.ttf`. It was generated with allsorts-tools:

    allsorts dump -t head tests/opentype/test-font.ttf > tests/opentype/head.bin

## hmtx.bin

`hmtx.bin` contains the `hmtx` table from `Ubuntu-R.ttf`. It was generated with allsorts-tools:

    allsorts dump -t hmtx ../../../data/fonts/Ubuntu-R.ttf > tests/opentype/hmtx.bin

## name.bin

`name.bin` contains the `name` table from `test-font.ttf`. It was generated with allsorts-tools:

    allsorts dump -t name ../../../data/fonts/test-font.ttf > tests/opentype/name.bin

## post.bin

`post.bin` contains the `post` table from `esDQ311QOP6BJUrIyg.ttf` from
[Caudex Regular](https://fonts.gstatic.com/s/caudex/v10/esDQ311QOP6BJUrIyg.ttf).
This font is distributed under the terms of the
[SIL Open Font License 1.1](../licenses/Caudex.txt). It was generated with
allsorts-tools:

    allsorts dump -t post Caudex-Regular.ttf > tests/opentype/post.bin

## CBLC.bin and CBDT.bin

Extracted from [NotoColorEmoji.ttf](https://github.com/googlefonts/noto-emoji/blob/018aa149d622a4fea11f01c61a7207079da301bc/fonts/NotoColorEmoji.ttf),
released under 
[the SIL Open Font License, Version 1.1](https://github.com/googlefonts/noto-emoji/blob/018aa149d622a4fea11f01c61a7207079da301bc/fonts/LICENSE).

     allsorts dump -t CBLC NotoColorEmoji.ttf > allsorts/tests/fonts/opentype/CBLC.bin
     allsorts dump -t CBDT NotoColorEmoji.ttf > allsorts/tests/fonts/opentype/CBDT.bin

## SFNT-TTF-Composite.ttf

`SFNT-TTF-Composite.ttf` was sourced from the [W3C woff2-tests][W3C woff2-tests-file].

## HardGothicNormal.ttf

This font uses the somewhat uncommon cmap subtable format 2.

`HardGothicNormal.ttf` was sourced from the [Tucows Archive][tucows-hardgothic].

## NotoSansJP-Regular.otf

This is a CID keyed CFF font, part of the Noto font family used under the terms of the
Open Font License 1.1. See `NOTO-LICENSE` for the full text.

## Klei.otf

Source: <https://git.io/Je4f7>

This is a Type 1 CFF font used under the terms of the Open Font License 1.1.
See `KLEI-LICENSE` for the full text.

## Ubuntu Mono with Numderline.ttf

The font uses reverse chaining contextual single substitution to add an
underline under every digit triplets in long numbers.

Source: <https://thume.ca/numderline/>, <https://blog.janestreet.com/commas-in-big-numbers-everywhere/>

[fonttools]: https://github.com/fonttools/fonttools
[W3C woff2-tests-file]: https://github.com/w3c/woff2-tests/blob/7efc18fb4d4c488ef7ebe04e6cb80ee0ef36741f/generators/resources/SFNT-TTF-Composite.ttf
[tucows-hardgothic]: https://archive.org/details/tucows_305977_Hard_Gothic_Normal
