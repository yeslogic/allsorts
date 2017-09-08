extern crate cbindgen;

use cbindgen::Config;
use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let header_out_dir =
        env::var("ALLSORTS_CBINDGEN_OUT_DIR").unwrap_or_else(|_| "include".to_owned());

    println!("cargo:rerun-if-changed=src/lib.rs");

    let bindings = match env::var("ALLSORTS_CBINDGEN_CONFIG_FILE") {
        Err(_) => cbindgen::generate(&crate_dir).unwrap(),
        Ok(path) => {
            let config = Config::from_file(&path).unwrap();
            cbindgen::generate_with_config(&crate_dir, config).unwrap()
        }
    };

    bindings.write_to_file(&(header_out_dir + "/allsorts.h"));
}
