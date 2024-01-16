use std::fs;

use triton_vm::instruction::LabelledInstruction;

use crate::ast_types;
use crate::custom_type_resolver::resolve_custom_types;
use crate::extract_types_and_function;
use crate::tasm_code_generator::compile_function;
use crate::type_checker::annotate_fn_outer;
use crate::StructsAndMethodsRustAst;

const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");
const PROGRAMS_DIR: &str = "src/tests_and_benchmarks/ozk/programs";

/// The URL of the Triton VM GitHub repository. Corresponds to version 0.36.1.
const GITHUB_TVM_URL: &str =
    "https://raw.githubusercontent.com/TritonVM/triton-vm/4d4475e1/triton-vm/src";

#[derive(Debug, Clone)]
pub(crate) struct EntrypointLocation {
    pub entrypoint: String,
    pub source_file_location: SourceFileLocation,
}

#[derive(Debug, Clone)]
pub(crate) struct SourceFileLocation {
    pub directory: String,
    pub module_name: String,
    pub provider: SourceCodeProvider,
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) enum SourceCodeProvider {
    #[default]
    Disk,
    GitHub,
}

impl EntrypointLocation {
    pub fn disk(directory: &str, module_name: &str, entrypoint: &str) -> Self {
        let source_file_location = SourceFileLocation {
            directory: directory.to_owned(),
            module_name: module_name.to_owned(),
            provider: SourceCodeProvider::Disk,
        };

        Self {
            entrypoint: entrypoint.to_owned(),
            source_file_location,
        }
    }

    /// Like `new`, but uses the GitHub repository as the source of the Rust code.
    pub fn github(directory: &str, module_name: &str, entrypoint: &str) -> Self {
        let mut location = Self::disk(directory, module_name, entrypoint);
        location.source_file_location.provider = SourceCodeProvider::GitHub;
        location
    }

    pub fn extract_entrypoint(&self) -> syn::ItemFn {
        let parsed_file = self.source_file_location.parse_file();
        for item in &parsed_file.items {
            if let syn::Item::Fn(func) = item {
                if func.sig.ident == self.entrypoint {
                    return func.to_owned();
                }
            }
        }

        panic!("Failed to locate entrypoint {}", self.entrypoint);
    }
}

impl SourceFileLocation {
    pub fn parse_file(&self) -> syn::File {
        let file = match self.provider {
            SourceCodeProvider::Disk => self.read_file_from_disk(),
            SourceCodeProvider::GitHub => self.read_file_from_github(),
        };

        syn::parse_file(&file).expect("Unable to parse rust code")
    }

    fn read_file_from_disk(&self) -> String {
        let end_of_path = self.end_of_file_path();
        let path = format!("{MANIFEST_DIR}/{PROGRAMS_DIR}/{end_of_path}");

        fs::read_to_string(&path).unwrap_or_else(|_| panic!("unable to read \"{path}\" from disk"))
    }

    fn read_file_from_github(&self) -> String {
        let path = self.end_of_file_path();
        let url = format!("{GITHUB_TVM_URL}/{path}");

        reqwest::blocking::get(url)
            .unwrap_or_else(|_| panic!("failed to download file \"{path}\" from GitHub"))
            .text()
            .unwrap_or_else(|_| panic!("failed to read file \"{path}\" from GitHub"))
    }

    fn end_of_file_path(&self) -> String {
        let directory = &self.directory;
        let module_name = &self.module_name;
        format!("{directory}/{module_name}.rs")
    }
}

/// Return the Rust-AST for the given `entrypoint` function and all custom types defined in the outermost module.
pub(super) fn parse_functions_and_types(
    location: &EntrypointLocation,
) -> (syn::ItemFn, StructsAndMethodsRustAst) {
    let entrypoint = location.extract_entrypoint();
    let custom_types = parse_functions_and_types_inner(&location.source_file_location);

    (entrypoint, custom_types)
}

fn parse_functions_and_types_inner(location: &SourceFileLocation) -> StructsAndMethodsRustAst {
    let file = location.parse_file();
    let (mut custom_types, dependencies) = extract_types_and_function(&file);

    for dependency in dependencies {
        let new_location = SourceFileLocation {
            module_name: dependency,
            ..location.to_owned()
        };
        let imported_custom_types = parse_functions_and_types_inner(&new_location);
        custom_types.extend(imported_custom_types)
    }

    custom_types
}

pub(crate) fn compile_for_test(
    location: &EntrypointLocation,
    list_type: ast_types::ListType,
) -> Vec<LabelledInstruction> {
    get_standard_setup!(list_type, graft_config, libraries);

    let (entrypoint_fn, rust_struct_asts) = parse_functions_and_types(location);
    let mut oil_ast = graft_config.graft_fn_decl(&entrypoint_fn);
    let mut composite_types =
        graft_config.graft_custom_types_methods_and_associated_functions(rust_struct_asts);
    composite_types.checked_merge(graft_config.imported_custom_types);

    resolve_custom_types(&mut oil_ast, &mut composite_types);

    // type-check and annotate
    annotate_fn_outer(&mut oil_ast, &mut composite_types, &libraries);

    let tasm = compile_function(&oil_ast, &libraries, &composite_types);

    // compose
    tasm.compose()
}

pub(crate) fn compile_to_basic_snippet(
    rust_ast: syn::ItemFn,
    structs_and_methods: StructsAndMethodsRustAst,
    list_type: ast_types::ListType,
) -> String {
    get_standard_setup!(list_type, graft_config, libraries);
    let mut oil_ast = graft_config.graft_fn_decl(&rust_ast);
    let mut composite_types =
        graft_config.graft_custom_types_methods_and_associated_functions(structs_and_methods);

    resolve_custom_types(&mut oil_ast, &mut composite_types);

    // type-check and annotate
    annotate_fn_outer(&mut oil_ast, &mut composite_types, &libraries);

    let tasm = compile_function(&oil_ast, &libraries, &composite_types);

    tasm.generate_basic_snippet_implementation()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_method_prove_from_github() {
        let entrypoint_location = EntrypointLocation::github("", "lib", "prove");
        let _ = entrypoint_location.extract_entrypoint();
    }
}
