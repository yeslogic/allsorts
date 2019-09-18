/// Read binary data
pub mod read;

/// Write binary data
pub mod write;

/// Calculate the length required to 32-bit (long) align data of length `len`
///
/// Example:
///
/// ```
/// use allsorts::binary::long_align;
///
/// let length = 123;
/// let padded_length = long_align(length);
/// assert_eq!(padded_length, 124);
/// ```
pub const fn long_align(len: usize) -> usize {
    (len + 3) / 4 * 4
}

/// Calculate the length required to 16-bit (word) align data of length `len`
///
/// Example:
///
/// ```
/// use allsorts::binary::word_align;
///
/// let length = 123;
/// let padded_length = word_align(length);
/// assert_eq!(padded_length, 124);
/// ```
pub const fn word_align(len: usize) -> usize {
    (len + 1) / 2 * 2
}

#[derive(Copy, Clone)]
pub enum U8 {}

#[derive(Copy, Clone)]
pub enum I8 {}

#[derive(Copy, Clone)]
pub enum U16Be {}

#[derive(Copy, Clone)]
pub enum I16Be {}

#[derive(Copy, Clone)]
pub enum U24Be {}

#[derive(Copy, Clone)]
pub enum U32Be {}

#[derive(Copy, Clone)]
pub enum I32Be {}

#[derive(Copy, Clone)]
pub enum I64Be {}
