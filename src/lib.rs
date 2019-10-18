#![warn(rust_2018_idioms)]

/// Reading and writing of binary data.
pub mod binary;
pub mod cff;
/// Checksum calculation routines.
pub mod checksum;
pub mod context;
pub mod error;
pub mod font_data_impl;
pub mod fontfile;
pub mod gdef;
pub mod get_name;
pub mod glyph_width;
pub mod gpos;
pub mod gsub;
pub mod indic;
pub mod layout;
/// Utilities for handling the Mac OS Roman character set.
pub mod macroman;
pub mod opentype;
pub mod post;
pub mod size;
/// Font subsetting.
pub mod subset;
pub mod tables;
pub mod tag;
/// Shared test code.
#[cfg(test)]
pub mod tests;
/// Reading of the WOFF format.
pub mod woff;
pub mod woff2;

#[macro_export]
macro_rules! read_table {
    ($source:expr, $tag:path, $t:ty, $index:expr) => {
        $source
            .read_table($tag, $index)?
            .ok_or(ParseError::MissingValue)?
            .scope()
            .read::<$t>()
    };
}
