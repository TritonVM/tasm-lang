use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

use tasm_lang::ast_types::ListType;

fn main() {
    let mut args = env::args();
    let _ = args.next(); // executable name

    let filename = match (args.next(), args.next()) {
        (Some(filename), None) => filename,
        _ => {
            eprintln!("Usage: dump-syntax path/to/filename.rs");
            process::exit(1);
        }
    };

    let mut file = File::open(&filename).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    // TODO: Allow this to be set by CLI args
    let list_type = ListType::Unsafe;

    let output = tasm_lang::compile_to_string(&filename, list_type);

    println!("{output}");
}
