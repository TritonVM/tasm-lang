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
    pub(crate) entrypoint: String,
    pub(crate) source_file_location: SourceFileLocation,
}

#[derive(Debug, Clone)]
pub(crate) struct SourceFileLocation {
    pub(crate) directory: String,
    pub(crate) module_name: String,
    pub(crate) provider: SourceCodeProvider,
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) enum SourceCodeProvider {
    #[default]
    Disk,
    GitHub,
}

impl EntrypointLocation {
    pub(crate) fn disk(directory: &str, module_name: &str, entrypoint: &str) -> Self {
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

    /// Like [`Self::disk`], but uses the GitHub repository as the source of the Rust code.
    pub(crate) fn github(directory: &str, module_name: &str, entrypoint: &str) -> Self {
        let mut location = Self::disk(directory, module_name, entrypoint);
        location.source_file_location.provider = SourceCodeProvider::GitHub;
        location
    }

    pub(crate) fn extract_entrypoint(&self) -> syn::ItemFn {
        let parsed_file = self.source_file_location.parse_file();
        let items = self.fetch_module_items_containing_entrypoint(&parsed_file.items);
        self.extract_entrypoint_from_items(items)
    }

    fn fetch_module_items_containing_entrypoint<'a>(
        &self,
        mut items: &'a [syn::Item],
    ) -> &'a [syn::Item] {
        'mod_descend: for module_name in self.entrypoint_module_names() {
            'item_iter: for item in items {
                let syn::Item::Mod(module) = item else {
                    continue 'item_iter;
                };
                if module.ident != module_name {
                    continue 'item_iter;
                }
                let Some((_, ref module_items)) = module.content else {
                    panic!("module \"{module_name}\" is empty");
                };
                items = module_items;
                continue 'mod_descend;
            }
            panic!("Failed to locate module \"{module_name}\"");
        }

        items
    }

    fn extract_entrypoint_from_items(&self, items: &[syn::Item]) -> syn::ItemFn {
        for item in items {
            let syn::Item::Fn(func) = item else {
                continue;
            };
            if func.sig.ident == self.entrypoint_signature_name() {
                return func.to_owned();
            }
        }

        panic!("Failed to locate entrypoint {}", self.entrypoint);
    }

    fn entrypoint_module_names(&self) -> Vec<&str> {
        let Some((modules, _)) = self.entrypoint.rsplit_once("::") else {
            return vec![];
        };

        modules.split("::").collect()
    }

    fn entrypoint_signature_name(&self) -> &str {
        let Some((_, entrypoint)) = self.entrypoint.rsplit_once("::") else {
            return &self.entrypoint;
        };

        entrypoint
    }
}

impl SourceFileLocation {
    pub(crate) fn parse_file(&self) -> syn::File {
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

    let entrypoint_fn = location.extract_entrypoint();
    let rust_struct_asts = parse_functions_and_types_inner(&location.source_file_location);
    let mut oil_ast = graft_config.graft_fn_decl(&entrypoint_fn);
    let mut composite_types =
        graft_config.graft_custom_types_methods_and_associated_functions(rust_struct_asts);
    composite_types.checked_merge(graft_config.imported_custom_types);

    resolve_custom_types(&mut oil_ast, &mut composite_types);

    // type-check and annotate
    annotate_fn_outer(&mut oil_ast, &mut composite_types, &libraries);

    let tasm = compile_function(&oil_ast, &libraries, &composite_types);

    tasm.compose()
}

/// Produce a [`BasicSnippet`][basic_snippet] through compilation and string interpolation.
///
/// [basic_snippet]: tasm_lib::traits::basic_snippet::BasicSnippet
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
