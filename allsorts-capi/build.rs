extern crate cbindgen;

use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let header_out_dir =
        env::var("ALLSORTS_HEADER_OUT_DIR").unwrap_or_else(|_| "include".to_owned());

    println!("cargo:rerun-if-changed=src/lib.rs");
    cbindgen::generate(&crate_dir).unwrap().write_to_file(
        &(header_out_dir +
              "/allsorts.h"),
    );
}
