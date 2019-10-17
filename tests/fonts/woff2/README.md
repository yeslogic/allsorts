# WOFF2 Test Fixtures

This directory contains WOFF2 test fixtures:

* test-font.woff2
* SFNT-TTF-Composite.woff2
* roundtrip-hmtx-lsb-001.woff2
* roundtrip-offset-tables-001.woff2

## test-font.woff2

`test-font.woff2` is generated with [fonttools]. You will need the following installed:

* `fonttools` -- packaged as `fonttools` in Homebrew and Debian based systems
  and `python-fonttools` on Arch Linux.
* The python brotli library -- `python3-brotli` on Debian based systems,
  `python-brotli` on Arch Linux. Manual installation required on macOS
  (Probably something like: `pip install --user brotli`).

Generate the file as follows:

    ttx --flavor woff2 -o test-font.woff2 ../opentype/test-font.ttx

## SFNT-TTF-Composite.woff2

`SFNT-TTF-Composite.woff2` is built from `SFNT-TTF-Composite.ttf`, which was
sourced from the [W3C woff2-tests][W3C woff2-tests-file]. It is build as
follows:

    ttx --flavor woff2 -o SFNT-TTF-Composite.woff2 SFNT-TTF-Composite.ttx

## roundtrip-hmtx-lsb-001.woff2

`roundtrip-hmtx-lsb-001.woff2` is from the [woff2 test suite](https://github.com/w3c/woff2-compiled-tests/blob/506177099f0bf9aad2c72c1fbcac3a25e57e00cc/Decoder/Tests/xhtml1/roundtrip-hmtx-lsb-001.woff2).

## roundtrip-offset-tables-001.woff2

`roundtrip-offset-tables-001.woff2` is from the [woff2 test suite](https://github.com/w3c/woff2-compiled-tests/blob/506177099f0bf9aad2c72c1fbcac3a25e57e00cc/Decoder/Tests/xhtml1/roundtrip-offset-tables-001.woff2).

[fonttools]: https://github.com/fonttools/fonttools
[W3C woff2-tests-file]: https://github.com/w3c/woff2-tests/blob/7efc18fb4d4c488ef7ebe04e6cb80ee0ef36741f/generators/resources/SFNT-TTF-Composite.ttf
