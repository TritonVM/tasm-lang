use itertools::Itertools;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::process;
use syn::ImplItemMethod;
use syn::Item;
use syn::ItemEnum;
use syn::ItemImpl;
use syn::ItemStruct;
use syn::UseTree;
use triton_vm::instruction::LabelledInstruction;

// TODO: The following import is necessary until `tasm-lib` branch `prelude` is incorporated there.
#[allow(clippy::single_component_path_imports)]
#[allow(unused_imports)]
use twenty_first;

use ast_types::ListType;
use graft::CustomTypeRust;

use crate::custom_type_resolver::resolve_custom_types;
use crate::graft::Graft;
use crate::tasm_code_generator::compile_function;
use crate::type_checker::annotate_fn_outer;

pub(crate) mod ast;
pub(crate) mod ast_types;
#[macro_use]
pub(crate) mod graft;
mod composite_types;
pub(crate) mod custom_type_resolver;
pub(crate) mod libraries;
pub(crate) mod ssa;
mod subroutine;
pub(crate) mod tasm_code_generator;
#[cfg(test)]
pub(crate) mod tests_and_benchmarks;
pub(crate) mod type_checker;

pub fn main() {
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

    let output = compile_to_string(&filename, list_type);

    println!("{output}");
}

/// Mapping from name of a custom type to its type declaration and associated function and methods.
pub(crate) type StructsAndMethodsRustAst = HashMap<String, (CustomTypeRust, Vec<ImplItemMethod>)>;

/// Like [`StructsAndMethodsRustAst`] but with an Option type for the type declaration.
type MaybeStructsAndMethodsRustAst = HashMap<String, (Option<CustomTypeRust>, Vec<ImplItemMethod>)>;

pub(crate) fn extract_types_and_function(
    parsed_file: &syn::File,
) -> (StructsAndMethodsRustAst, Vec<String>) {
    get_standard_setup!(ListType::Unsafe, graft_config, _lib);
    let mut types: MaybeStructsAndMethodsRustAst = HashMap::default();
    let mut dependencies = vec![];

    for item in &parsed_file.items {
        handle_item(&mut graft_config, &mut types, &mut dependencies, item);
    }

    let types = unwrap_custom_rust_type(types);
    (types, dependencies)
}

fn handle_item(
    graft_config: &mut Graft,
    types: &mut MaybeStructsAndMethodsRustAst,
    dependencies: &mut Vec<String>,
    item: &Item,
) {
    match item {
        Item::Impl(item_impl) => extract_methods_from_impl_block(graft_config, types, item_impl),
        Item::Struct(item_struct) => add_struct_declaration(types, item_struct),
        Item::Enum(item_enum) => add_custom_enum_declaration(types, item_enum),
        Item::Use(syn::ItemUse { tree, .. }) => {
            maybe_add_dependency_super_module_from_use_tree(dependencies, tree)
        }

        _ => (),
    }
}

fn extract_methods_from_impl_block(
    graft_config: &mut Graft,
    types: &mut MaybeStructsAndMethodsRustAst,
    item_impl: &ItemImpl,
) {
    let data_type = graft_config.syn_type_to_ast_type(&item_impl.self_ty);
    let type_name = data_type.to_string();

    for impl_item in &item_impl.items {
        let syn::ImplItem::Method(struct_method) = impl_item else {
            continue;
        };

        let custom_type_entry = types.get_mut(&type_name);
        match custom_type_entry {
            Some(value) => value.1.push(struct_method.to_owned()),
            None => _ = types.insert(type_name.clone(), (None, vec![struct_method.to_owned()])),
        };
    }
}

fn add_struct_declaration(
    custom_types: &mut MaybeStructsAndMethodsRustAst,
    item_struct: &ItemStruct,
) {
    let rust_struct = Some(CustomTypeRust::Struct(item_struct.to_owned()));
    let key = item_struct.ident.to_string();
    match custom_types.get_mut(&key) {
        Some(value) => value.0 = rust_struct,
        None => _ = custom_types.insert(key, (rust_struct, vec![])),
    };
}

fn add_custom_enum_declaration(
    custom_types: &mut MaybeStructsAndMethodsRustAst,
    item_enum: &ItemEnum,
) {
    let rust_enum = Some(CustomTypeRust::Enum(item_enum.to_owned()));
    let key = item_enum.to_owned().ident.to_string();
    match custom_types.get_mut(&key) {
        Some(value) => value.0 = rust_enum,
        None => _ = custom_types.insert(key, (rust_enum, vec![])),
    };
}

/// Handle imports of the form `use super::<module>::*;`
fn maybe_add_dependency_super_module_from_use_tree(dependencies: &mut Vec<String>, tree: &UseTree) {
    let UseTree::Path(use_path) = tree else {
        return;
    };
    if use_path.ident != "super" {
        return;
    };
    let UseTree::Path(use_path) = use_path.tree.as_ref() else {
        return;
    };
    let UseTree::Glob(_) = *use_path.tree else {
        return;
    };

    let module_name = use_path.ident.to_string();
    dependencies.push(module_name);
}

/// After parsing all code, each method must have a struct. Then, we can unwrap the Option type.
fn unwrap_custom_rust_type(
    custom_types: MaybeStructsAndMethodsRustAst,
) -> StructsAndMethodsRustAst {
    custom_types
        .into_iter()
        .map(|(name, (maybe_struct, methods))| {
            let the_struct = maybe_struct.unwrap_or_else(|| {
                panic!("Couldn't find struct definition for {name} for which methods was defined")
            });
            (name, (the_struct, methods))
        })
        .collect()
}

/// Limitation: this function cannot handle imports yet.
fn parse_function_and_types(file_path: &str) -> (syn::ItemFn, StructsAndMethodsRustAst) {
    let content = fs::read_to_string(file_path).expect("Unable to read file {path}");
    let parsed_file: syn::File = syn::parse_str(&content).expect("Unable to parse rust code");
    let entrypoint = extract_entrypoint(&parsed_file, "main");
    let (custom_types, dependencies) = extract_types_and_function(&parsed_file);

    assert!(
        dependencies.is_empty(),
        "Cannot handle dependencies here yet. See the OZK testing framework for a solution."
    );

    (entrypoint, custom_types)
}

pub(crate) fn compile_to_instructions(
    file_path: &str,
    list_type: ListType,
) -> Vec<LabelledInstruction> {
    get_standard_setup!(list_type, graft_config, libraries);

    let (rust_main_ast, rust_struct_asts) = parse_function_and_types(file_path);

    let mut oil_ast = graft_config.graft_fn_decl(&rust_main_ast);
    let mut composite_types =
        graft_config.graft_custom_types_methods_and_associated_functions(rust_struct_asts);
    composite_types.checked_merge(graft_config.imported_custom_types);

    resolve_custom_types(&mut oil_ast, &mut composite_types);
    annotate_fn_outer(&mut oil_ast, &mut composite_types, &libraries);

    let tasm = compile_function(&oil_ast, &libraries, &composite_types);
    // TODO: Add optimizations step here

    tasm.compose()
}

pub(crate) fn compile_to_string(file_path: &str, list_type: ListType) -> String {
    compile_to_instructions(file_path, list_type)
        .into_iter()
        .join("\n")
}

fn extract_entrypoint(parsed_file: &syn::File, entrypoint: &str) -> syn::ItemFn {
    for item in &parsed_file.items {
        let Item::Fn(func) = item else {
            continue;
        };
        if func.sig.ident == entrypoint {
            return func.to_owned();
        }
    }

    panic!("Failed to locate entrypoint {entrypoint}");
}
