#![feature(rustc_private)]

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_hir_analysis;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use std::fs;
use std::fs::File;
use std::io::Write;
use std::process;

use clap::Arg;
use clap::ArgAction;
use clap::Command;
use itertools::Itertools;
pub use tasm_lib;
pub use tasm_lib::triton_vm;
use tasm_lib::triton_vm::prelude::LabelledInstruction;
pub use tasm_lib::twenty_first;

pub(crate) mod ast;
pub(crate) mod ast_types;
mod composite_types;
pub(crate) mod libraries;
pub(crate) mod rustc_frontend;
pub(crate) mod ssa;
mod subroutine;
pub(crate) mod tasm_code_generator;
#[cfg(test)]
pub(crate) mod tests_and_benchmarks;
pub(crate) mod type_checker;

const DEFAULT_ENTRYPOINT_NAME: &str = "main";

pub fn main() {
    let matches = Command::new("tasm-lang")
        .version("0.0")
        .about("A limited Rust -> TASM compiler")
        .arg(
            Arg::new("input")
                .help("The input file to process")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("output")
                .help("The output file to write to")
                .required(true)
                .index(2),
        )
        .arg(
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .help("Verbose: Print generated assembler to standard-out")
                .action(ArgAction::SetTrue),
        )
        .get_matches();

    let input_file_name = matches.get_one::<String>("input").unwrap();
    let output_file_name = matches.get_one::<String>("output").unwrap();
    let verbose = matches.get_flag("verbose");

    // Check if the input file has a .rs extension
    if !input_file_name.ends_with(".rs") {
        eprintln!("Error: The input file must have a .rs extension.");
        process::exit(1);
    }

    // Ensure the output file has a .tasm extension
    let output_file_name = if output_file_name.ends_with(".tasm") {
        output_file_name.clone()
    } else {
        format!("{output_file_name}.tasm")
    };

    let assembler = compile_to_string(input_file_name);

    // Write result to file
    let mut output_file =
        File::create(output_file_name.clone()).expect("Unable to create file {output_file}");
    output_file
        .write_all(assembler.as_bytes())
        .expect("Failed to write to output file {output_file}");

    println!("Successfully compiled {input_file_name} to {output_file_name}.",);

    if verbose {
        println!("{assembler}");
    }
}

/// Compile the program in the file to Triton assembly. The program's
/// entrypoint must be a function called `main`.
pub(crate) fn compile_to_instructions(file_path: &str) -> Vec<LabelledInstruction> {
    let content = fs::read_to_string(file_path).expect("Unable to read file {path}");
    let directory = std::path::Path::new(file_path)
        .parent()
        .map(|dir| dir.to_string_lossy().into_owned())
        .unwrap_or_default();
    let load_dependency = rustc_frontend::dependency_loader_for_directory(&directory);
    let program = rustc_frontend::parse_file(&content);

    rustc_frontend::compile_program(program, DEFAULT_ENTRYPOINT_NAME, &load_dependency).compose()
}

pub(crate) fn compile_to_string(file_path: &str) -> String {
    format!(
        "{}\n",
        compile_to_instructions(file_path).into_iter().join("\n")
    )
}
