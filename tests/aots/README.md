# Adobe Annotated OpenType Specification Tests

This test data is generated from the aots.

## Regenerating the Tests

### Prerequisites

* Java Compiler

### Building

Clone the repo:

    git clone https://github.com/adobe-type-tools/aots.git
    cs aots

Build the aots library and test data:

    make

Copy:

    rsync -az tests/ /path/to/prince/src/fonts/fontcode-rust/tests/aots/

Generate the Rust test cases (updating the path your Prince checkout accordingly):

    java -jar jars/saxon9he.jar -s:src/opentype.xml -xsl:../../Work/prince/src/fonts/fontcode-rust/tests/aots/aots2testrust.xsl -o:../../Work/prince/src/fonts/fontcode-rust/tests/aots/testcases.rs
