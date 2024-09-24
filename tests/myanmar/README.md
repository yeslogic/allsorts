This directory contains:

* Good and bad inputs copied from [corpus](https://github.com/yeslogic/corpus).
* Expected indices for good inputs generated with invocations lke:

```
xargs -d '\n' -n1 hb-shape --shapers ot,fallback --no-glyph-names --no-clusters --no-positions --remove-default-ignorables ../fonts/myanmar/Padauk-Regular.ttf < good > harfbuzz/good-padauk
```

`--preserve-default-ignorables` prevents Harfbuzz from replacing things like ZWNJ with space in the output.

Harfbuzz version used:

```
$ hb-shape --version
hb-shape (HarfBuzz) 9.0.0
```
