use std::{collections::HashMap, fs};
use triton_vm::instruction::LabelledInstruction;

use crate::{
    ast_types, custom_type_resolver::resolve_custom_types, graft::CustomTypeRust,
    tasm_code_generator::compile_function, type_checker::annotate_fn_outer,
};

/// Mapping from name of a custom type to its type declaration and associated function
/// and methods.
pub type StructsAndMethodsRustAst = HashMap<String, (CustomTypeRust, Vec<syn::ImplItemMethod>)>;

fn extract_types_and_function(
    parsed_file: syn::File,
    function_name: Option<&str>,
) -> (StructsAndMethodsRustAst, Option<syn::ItemFn>, Vec<String>) {
    get_standard_setup!(ast_types::ListType::Unsafe, graft_config, _lib);
    let mut outer_function: Option<syn::ItemFn> = None;
    let mut custom_types: HashMap<String, (Option<CustomTypeRust>, Vec<syn::ImplItemMethod>)> =
        HashMap::default();
    let mut dependencies = vec![];
    for item in parsed_file.items {
        match item {
            // Top-level function declaration
            syn::Item::Fn(func) => {
                if let Some(function_name) = function_name {
                    if func.sig.ident == function_name {
                        outer_function = Some(func.to_owned());
                    }
                }
            }

            // `impl` code block
            syn::Item::Impl(item_impl) => {
                let type_name = graft_config
                    .syn_type_to_ast_type(&item_impl.self_ty)
                    .to_string();
                for impl_item in item_impl.items.iter() {
                    if let syn::ImplItem::Method(struct_method) = impl_item {
                        let custom_type_entry = custom_types.get_mut(&type_name);
                        match custom_type_entry {
                            Some(value) => {
                                value.1.push(struct_method.to_owned());
                            }
                            None => {
                                custom_types.insert(
                                    type_name.clone(),
                                    (None, vec![struct_method.to_owned()]),
                                );
                            }
                        };
                    }
                }
            }

            // Custom-type struct declaration
            syn::Item::Struct(item_struct) => {
                let key = item_struct.ident.to_string();
                let entry_mut = custom_types.get_mut(&key);
                match entry_mut {
                    Some(value) => {
                        value.0 = Some(CustomTypeRust::Struct(item_struct.to_owned()));
                    }
                    None => {
                        custom_types.insert(
                            key,
                            (Some(CustomTypeRust::Struct(item_struct.to_owned())), vec![]),
                        );
                    }
                };
            }

            // Custom-type enum declaration
            syn::Item::Enum(item_enum) => {
                let key = item_enum.ident.to_string();
                let entry_mut = custom_types.get_mut(&key);
                match entry_mut {
                    Some(value) => {
                        value.0 = Some(CustomTypeRust::Enum(item_enum.to_owned()));
                    }
                    None => {
                        custom_types.insert(
                            key,
                            (Some(CustomTypeRust::Enum(item_enum.to_owned())), vec![]),
                        );
                    }
                };
            }

            // Imports on the form `use super::<module>::*;`
            syn::Item::Use(syn::ItemUse {
                attrs: _,
                vis: _,
                use_token: _,
                leading_colon: _,
                tree,
                semi_token: _,
            }) => {
                fn get_module_name(tree: &syn::UseTree) -> Option<String> {
                    match tree {
                        syn::UseTree::Path(use_path) => {
                            if use_path.ident == "super" {
                                match use_path.tree.as_ref() {
                                    syn::UseTree::Path(use_path) => {
                                        if let syn::UseTree::Glob(_) = *use_path.tree {
                                            Some(use_path.ident.to_string())
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }

                if let Some(module_name) = get_module_name(&tree) {
                    dependencies.push(module_name);
                }
            }
            _ => (),
        }
    }

    // Each method must have a struct after parsing all code. So we can unwrap the Option type.
    let structs: StructsAndMethodsRustAst = custom_types
        .into_iter()
        .map(|(struct_name, (option_struct, methods))| (struct_name.clone(), (option_struct.unwrap_or_else(|| panic!("Couldn't find struct definition for {struct_name} for which methods was defined")), methods)))
        .collect();

    (structs, outer_function, dependencies)
}

/// Return the Rust-AST for the `main` function and all custom types defined in the
/// outermost module.
pub(super) fn parse_function_and_structs(
    directory: &str,
    module_name: &str,
    function_name: &str,
) -> (syn::ItemFn, StructsAndMethodsRustAst, String) {
    fn parse_function_and_structs_inner(
        directory: &str,
        module_name: &str,
        function_name: Option<&str>,
    ) -> (Option<syn::ItemFn>, StructsAndMethodsRustAst, String) {
        let path = format!(
            "{}/src/tests_and_benchmarks/ozk/programs/{directory}/{module_name}.rs",
            env!("CARGO_MANIFEST_DIR"),
        );
        let content = fs::read_to_string(path).expect("Unable to read file {path}");
        let parsed_file: syn::File = syn::parse_str(&content).expect("Unable to parse rust code");
        let (mut custom_types, main_parsed, dependencies) =
            extract_types_and_function(parsed_file, function_name);

        for dependency in dependencies {
            let (_, imported_custom_types, _) =
                parse_function_and_structs_inner(directory, &dependency, None);
            custom_types.extend(imported_custom_types.into_iter())
        }

        (main_parsed, custom_types, module_name.to_owned())
    }

    let (main_parsed, custom_types, module_name) =
        parse_function_and_structs_inner(directory, module_name, Some(function_name));
    match main_parsed {
        Some(main) => (main, custom_types, module_name.to_owned()),
        None => panic!("Failed to parse module {module_name}"),
    }
}

pub(crate) fn compile_for_test(
    directory: &str,
    module_name: &str,
    function_name: &str,
    list_type: ast_types::ListType,
) -> Vec<LabelledInstruction> {
    get_standard_setup!(list_type, graft_config, libraries);

    let (rust_main_ast, rust_struct_asts, _) =
        parse_function_and_structs(directory, module_name, function_name);
    let mut oil_ast = graft_config.graft_fn_decl(&rust_main_ast);
    // let (mut custom_types, mut methods, mut associated_functions) =
    //     graft_config.graft_custom_types_methods_and_associated_functions(rust_struct_asts);
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

#[allow(dead_code)]
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
