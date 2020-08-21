#!/usr/bin/env python

import sys
from fontTools.ttx import makeOutputFileName
from fontTools.ttLib import TTFont

# Compiles a TTX file with gzipped SVG documents

def main(args=None):
    if args is None:
        args = sys.argv[1:]

    if len(args) < 1:
        print("usage: gzip.py "
              "INPUT.ttx [OUTPUT.ttf]", file=sys.stderr)
        return 1

    infile = args[0]
    if len(args) > 1:
        outfile = args[1]
    else:
        outfile = makeOutputFileName(infile, None, ".ttf")

    font = TTFont()
    font.importXML(infile)

    svg = font["SVG "]
    svg.compressed = True
    font.save(outfile)

if __name__ == "__main__":
    sys.exit(main())
