use std::{collections::HashMap, fs};
use triton_vm::instruction::LabelledInstruction;

use crate::{
    ast_types, custom_type_resolver::resolve_custom_types, graft::CustomTypeRust,
    tasm_code_generator::compile_function, type_checker::annotate_fn_outer,
};

/// Mapping from name of a custom type to its type declaration and associated function
/// and methods.
pub type StructsAndMethods = HashMap<String, (CustomTypeRust, Vec<syn::ImplItemMethod>)>;

fn extract_types_and_function(
    parsed_file: syn::File,
    function_name: &str,
) -> (StructsAndMethods, Option<syn::ItemFn>) {
    get_standard_setup!(ast_types::ListType::Unsafe, graft_config, _lib);
    let mut outer_function: Option<syn::ItemFn> = None;
    let mut custom_types: HashMap<String, (Option<CustomTypeRust>, Vec<syn::ImplItemMethod>)> =
        HashMap::default();
    for item in parsed_file.items {
        match item {
            // Top-level function declaration
            syn::Item::Fn(func) => {
                if func.sig.ident == function_name {
                    outer_function = Some(func.to_owned());
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
            _ => {}
        }
    }

    // Each method must have a struct after parsing all code. So we can unwrap the Option type.
    let structs: StructsAndMethods = custom_types
        .into_iter()
        .map(|(struct_name, (option_struct, methods))| (struct_name.clone(), (option_struct.unwrap_or_else(|| panic!("Couldn't find struct definition for {struct_name} for which methods was defined")), methods)))
        .collect();

    (structs, outer_function)
}

/// Return the Rust-AST for the `main` function and all custom types defined in the
/// outermost module.
pub(super) fn parse_function_and_structs(
    directory: &str,
    module_name: &str,
    function_name: &str,
) -> (syn::ItemFn, StructsAndMethods, String) {
    let path = format!(
        "{}/src/tests_and_benchmarks/ozk/programs/{directory}/{module_name}.rs",
        env!("CARGO_MANIFEST_DIR"),
    );
    let content = fs::read_to_string(&path).expect("Unable to read file {path}");
    let parsed_file: syn::File = syn::parse_str(&content).expect("Unable to parse rust code");
    let (custom_types, main_parsed) = extract_types_and_function(parsed_file, function_name);

    match main_parsed {
        Some(main) => (main, custom_types, module_name.to_owned()),
        None => panic!("Failed to parse file {path}"),
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
    let (mut custom_types, mut methods, mut associated_functions) =
        graft_config.graft_custom_types_methods_and_associated_functions(rust_struct_asts);

    resolve_custom_types(
        &mut oil_ast,
        &mut custom_types,
        &mut methods,
        &mut associated_functions,
    );

    // type-check and annotate
    annotate_fn_outer(
        &mut oil_ast,
        &custom_types,
        &mut methods,
        &mut associated_functions,
        &libraries,
    );

    let tasm = compile_function(
        &oil_ast,
        &libraries,
        methods,
        &associated_functions,
        &custom_types,
    );

    // compose
    tasm.compose()
}

#[allow(dead_code)]
pub(crate) fn compile_to_basic_snippet(
    rust_ast: syn::ItemFn,
    structs_and_methods: StructsAndMethods,
    list_type: ast_types::ListType,
) -> String {
    get_standard_setup!(list_type, graft_config, libraries);
    let mut oil_ast = graft_config.graft_fn_decl(&rust_ast);
    let (mut custom_types, mut methods, mut associated_functions) =
        graft_config.graft_custom_types_methods_and_associated_functions(structs_and_methods);

    resolve_custom_types(
        &mut oil_ast,
        &mut custom_types,
        &mut methods,
        &mut associated_functions,
    );

    // type-check and annotate
    annotate_fn_outer(
        &mut oil_ast,
        &custom_types,
        &mut methods,
        &mut associated_functions,
        &libraries,
    );

    let tasm = compile_function(
        &oil_ast,
        &libraries,
        methods,
        &associated_functions,
        &custom_types,
    );

    tasm.generate_basic_snippet_implementation()
}
