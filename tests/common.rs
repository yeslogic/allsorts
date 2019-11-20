use std::path::{Path, PathBuf};

pub fn fixture_path<P: AsRef<Path>>(path: P) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

/// Read a test fixture from a path relative to CARGO_MANIFEST_DIR
pub fn read_fixture<P: AsRef<Path>>(path: P) -> Vec<u8> {
    std::fs::read(&fixture_path(path)).expect("error reading file contents")
}
