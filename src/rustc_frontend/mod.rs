//! The front-end of the compiler: uses `rustc` to parse, resolve and
//! type-check a program, and lowers `rustc`'s typed intermediate
//! representation (THIR) into this compiler's own abstract syntax tree, which
//! the code generator consumes.

pub(crate) mod driver;
pub(crate) mod lower;
pub(crate) mod prelude;
pub(crate) mod preprocess;
pub(crate) mod types;

use std::fs;

use crate::libraries::all_libraries;
use crate::rustc_frontend::preprocess::DependencyLoader;
use crate::tasm_code_generator::compile_function;
use crate::tasm_code_generator::outer_function_tasm_code::OuterFunctionTasmCode;

/// Compile a program: type-check it with `rustc`, lower it to this compiler's
/// AST, and generate Triton assembly for it.
pub(crate) fn compile_program(
    file: syn::File,
    entrypoint_path: &str,
    load_dependency: DependencyLoader,
) -> OuterFunctionTasmCode {
    let preprocessed = preprocess::preprocess(file, entrypoint_path, load_dependency);
    let entrypoint = preprocessed.entrypoint;
    let libraries = all_libraries();

    let lowered = driver::with_type_context(preprocessed.crate_source, |tcx| {
        lower::lower_program(tcx, &libraries, &entrypoint)
    })
    .unwrap_or_else(|| panic!("Program does not compile; see rustc's diagnostics above"));

    compile_function(&lowered.entrypoint, &libraries, &lowered.composite_types)
}

/// Parse a Rust file that contains a program.
pub(crate) fn parse_file(source: &str) -> syn::File {
    syn::parse_file(source).expect("Unable to parse Rust code")
}

/// Load a program's dependencies from files in the same directory as the
/// program.
pub(crate) fn dependency_loader_for_directory(directory: &str) -> impl Fn(&str) -> syn::File {
    let directory = directory.to_owned();
    move |module_name: &str| {
        let path = format!("{directory}/{module_name}.rs");
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("unable to read dependency \"{path}\" from disk"));
        parse_file(&source)
    }
}

/// Type-check a program with `rustc`. Returns `false` if it does not compile.
#[cfg(test)]
pub(crate) fn type_checks(
    file: syn::File,
    entrypoint_path: &str,
    load_dependency: DependencyLoader,
) -> bool {
    let preprocessed = preprocess::preprocess(file, entrypoint_path, load_dependency);
    driver::with_type_context(preprocessed.crate_source, |_tcx| ()).is_some()
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    /// Every program in the test-suite must type-check under `rustc` against
    /// the prelude.
    #[test]
    fn all_ozk_programs_type_check() {
        let programs_dir = format!(
            "{}/src/tests_and_benchmarks/ozk/programs",
            env!("CARGO_MANIFEST_DIR")
        );
        let mut failures = vec![];
        let mut count = 0;
        for category in fs::read_dir(&programs_dir).unwrap() {
            let category = category.unwrap().path();
            if !category.is_dir() {
                continue;
            }
            for program in fs::read_dir(&category).unwrap() {
                let program = program.unwrap().path();
                if program.extension().is_none_or(|ext| ext != "rs") {
                    continue;
                }
                let source = fs::read_to_string(&program).unwrap();
                if !source.contains("fn main()") {
                    continue;
                }
                let loader = dependency_loader_for_directory(category.to_str().unwrap());
                let file = parse_file(&source);
                count += 1;
                eprintln!("### type-checking {}", program.display());
                if !type_checks(file, "main", &loader) {
                    failures.push(
                        program
                            .strip_prefix(Path::new(&programs_dir))
                            .unwrap()
                            .display()
                            .to_string(),
                    );
                }
            }
        }
        assert!(
            failures.is_empty(),
            "{}/{count} programs failed to type-check:\n{}",
            failures.len(),
            failures.join("\n")
        );
    }
}
