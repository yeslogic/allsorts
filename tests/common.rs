use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

pub fn fixture_path<P: AsRef<Path>>(path: P) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

/// Read a test fixture from a path relative to CARGO_MANIFEST_DIR
pub fn read_fixture<P: AsRef<Path>>(path: P) -> Vec<u8> {
    read_file(&fixture_path(path))
}

fn read_file<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = File::open(path).expect("unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .expect("error reading file contents");
    buffer
}
