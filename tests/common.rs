use std::path::{Path, PathBuf};

use crate::tables::{F2Dot14, Fixed};

pub fn fixture_path<P: AsRef<Path>>(path: P) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

/// Read a test fixture from a path relative to CARGO_MANIFEST_DIR
pub fn read_fixture<P: AsRef<Path>>(path: P) -> Vec<u8> {
    std::fs::read(&fixture_path(path)).expect("error reading file contents")
}

pub fn assert_fixed_close(actual: Fixed, expected: f32) {
    let expected = Fixed::from(expected);
    assert!(
        (actual.raw_value().wrapping_sub(expected.raw_value())).abs() <= 3,
        "{} ({:?}) != {} ({:?}) ± {}",
        f32::from(actual),
        actual,
        f32::from(expected),
        expected,
        3. / 65535.
    );
}

pub fn assert_f2dot14_close(actual: F2Dot14, expected: f32) {
    let expected = F2Dot14::from(expected);
    assert!(
        (actual.raw_value().wrapping_sub(expected.raw_value())).abs() <= 3,
        "{} ({:?}) != {} ({:?}) ± {}",
        f32::from(actual),
        actual,
        f32::from(expected),
        expected,
        3. / 16384.
    );
}
