extern crate ddl;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::str::FromStr;

use ddl::syntax::ast::Program;

fn main() {
    let src = {
        let mut src_file = File::open("src/cmap.ddl").unwrap();
        let mut src = String::new();
        src_file.read_to_string(&mut src).unwrap();
        src
    };

    let mut program = Program::from_str(&src).unwrap();
    program.substitute(&ddl::syntax::ast::base_defs());
    ddl::syntax::check::check_program(&program).unwrap();
    let ir = ddl::ir::ast::Program::from(&program);

    let out_dir = env::var("OUT_DIR").unwrap();
    let mut file = File::create(out_dir + "/cmap.rs").unwrap();
    write!(file, "{}", ddl::codegen::LowerProgram(&ir)).unwrap();
}
