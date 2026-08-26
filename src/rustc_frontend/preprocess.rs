//! Turns the source of a program into a self-contained crate that `rustc`
//! can type-check against the [prelude](super::prelude).
//!
//! Programs in this repository are dual-compiled: `rustc` compiles them as
//! part of the test-suite, where they are executed natively, and this compiler
//! compiles them to Triton assembly. In their native form they import types
//! from `twenty-first`, Rust shadows of `tasm-lib` snippets, and test helpers.
//! None of that is available when the program is compiled on its own, so the
//! preprocessing here
//!
//! - drops all `use` items (the prelude provides everything by name),
//! - drops test modules, `#[cfg(test)]` items and `#[test]` functions,
//! - inlines `use super::<module>::*;` dependencies,
//! - hoists an entrypoint from a nested module to the crate root,
//! - filters attributes down to what `rustc` understands without extra crates,
//! - adds empty `BFieldCodec` implementations for all declared types.

use std::collections::HashSet;

use itertools::Itertools;
use quote::quote;
use regex::Regex;
use syn::parse_quote;

use super::prelude;

/// Loads the source of a dependency module (`use super::<name>::*;`) by name.
pub(crate) type DependencyLoader<'a> = &'a dyn Fn(&str) -> syn::File;

/// Derives that can be kept, since they need no external crate.
const KNOWN_DERIVES: [&str; 8] = [
    "Clone",
    "Copy",
    "Debug",
    "Default",
    "Eq",
    "Hash",
    "PartialEq",
    "PartialOrd",
];

/// Attributes (other than `derive`) that are kept.
const KNOWN_ATTRIBUTES: [&str; 5] = ["allow", "doc", "inline", "must_use", "expect"];

/// Attribute marking a struct field that must not be part of the encoding.
/// Such fields are simply removed.
const IGNORED_FIELD_ATTRIBUTE: (&str, &str) = ("tasm_object", "(ignore)");

pub(crate) struct PreprocessedProgram {
    /// Source code of a crate containing the prelude and the program.
    pub(crate) crate_source: String,

    /// The name of the entrypoint function, which lives at the crate root.
    pub(crate) entrypoint: String,
}

/// Preprocess a program given as a parsed file. The entrypoint is given as a
/// `::`-separated path, e.g. `main` or `test::verify_stark_proof`; a module
/// path is only used to *locate* the entrypoint, which is then hoisted to
/// the crate root.
pub(crate) fn preprocess(
    file: syn::File,
    entrypoint_path: &str,
    load_dependency: DependencyLoader,
) -> PreprocessedProgram {
    let (module_path, entrypoint) = match entrypoint_path.rsplit_once("::") {
        Some((modules, name)) => (modules.split("::").collect_vec(), name.to_owned()),
        None => (vec![], entrypoint_path.to_owned()),
    };

    let mut items = vec![];
    if !module_path.is_empty() {
        let hoisted = extract_entrypoint_from_module(&file.items, &module_path, &entrypoint);
        items.push(syn::Item::Fn(hoisted));
    }

    let mut visited_dependencies = HashSet::new();
    items.extend(program_items(
        file.items,
        load_dependency,
        &mut visited_dependencies,
    ));

    let inner_attributes = file
        .attrs
        .into_iter()
        .filter(is_known_attribute)
        .map(|attr| quote!(#attr).to_string())
        .join("\n");
    let program = syn::File {
        shebang: None,
        attrs: vec![],
        items,
    };
    let program_source = normalize_visibility(quote!(#program).to_string());

    let prelude_module = prelude::PRELUDE_MODULE_NAME;
    let tasm_module = prelude::TASM_MODULE_NAME;
    let crate_source = format!(
        "{inner_attributes}\n\
         #![allow(unused, dead_code, unreachable_code, clippy::all)]\n\
         mod {prelude_module} {{\n{}\n}}\n\
         use {prelude_module}::*;\n\
         use {prelude_module}::{tasm_module} as rust_shadows;\n\
         {program_source}\n",
        prelude::prelude_source(&program_source),
    );

    if std::env::var("TASM_LANG_DUMP_CRATE").is_ok() {
        eprintln!("{crate_source}");
    }

    PreprocessedProgram {
        crate_source,
        entrypoint,
    }
}

/// Find the entrypoint function inside a nested module and return a copy.
fn extract_entrypoint_from_module(
    mut items: &[syn::Item],
    module_path: &[&str],
    entrypoint: &str,
) -> syn::ItemFn {
    for module_name in module_path {
        let module = items
            .iter()
            .find_map(|item| match item {
                syn::Item::Mod(module) if module.ident == module_name => Some(module),
                _ => None,
            })
            .unwrap_or_else(|| panic!("Failed to locate module \"{module_name}\""));
        let Some((_, module_items)) = &module.content else {
            panic!("module \"{module_name}\" is empty");
        };
        items = module_items;
    }

    items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == entrypoint => Some(function.clone()),
            _ => None,
        })
        .unwrap_or_else(|| {
            panic!(
                "Failed to locate entrypoint {entrypoint} in module {}",
                module_path.join("::")
            )
        })
}

/// The items of a program that survive preprocessing, including those of all
/// (transitive) dependencies.
fn program_items(
    items: Vec<syn::Item>,
    load_dependency: DependencyLoader,
    visited_dependencies: &mut HashSet<String>,
) -> Vec<syn::Item> {
    let mut kept = vec![];
    for item in items {
        match item {
            syn::Item::Use(syn::ItemUse { tree, .. }) => {
                let Some(dependency) = dependency_from_use_tree(&tree) else {
                    continue;
                };
                if !visited_dependencies.insert(dependency.clone()) {
                    continue;
                }
                let dependency_file = load_dependency(&dependency);
                kept.extend(program_items(
                    dependency_file.items,
                    load_dependency,
                    visited_dependencies,
                ));
            }
            syn::Item::Struct(mut item_struct) => {
                if is_test_only(&item_struct.attrs) {
                    continue;
                }
                item_struct.attrs = filter_attributes(item_struct.attrs);
                remove_ignored_fields(&mut item_struct.fields);
                let name = &item_struct.ident;
                let generics = &item_struct.generics;
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                kept.push(syn::Item::Struct(item_struct.clone()));
                kept.push(parse_quote! {
                    impl #impl_generics BFieldCodec for #name #ty_generics #where_clause {}
                });
                kept.push(parse_quote! {
                    impl #impl_generics TasmObject for #name #ty_generics #where_clause {}
                });
            }
            syn::Item::Enum(mut item_enum) => {
                if is_test_only(&item_enum.attrs) {
                    continue;
                }
                item_enum.attrs = filter_attributes(item_enum.attrs);
                for variant in item_enum.variants.iter_mut() {
                    variant.attrs = filter_attributes(std::mem::take(&mut variant.attrs));
                    remove_ignored_fields(&mut variant.fields);
                }
                let name = &item_enum.ident;
                let generics = &item_enum.generics;
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                kept.push(syn::Item::Enum(item_enum.clone()));
                kept.push(parse_quote! {
                    impl #impl_generics BFieldCodec for #name #ty_generics #where_clause {}
                });
            }
            syn::Item::Impl(mut item_impl) => {
                if is_test_only(&item_impl.attrs) {
                    continue;
                }
                item_impl.attrs = filter_attributes(item_impl.attrs);
                item_impl.items.retain(|impl_item| match impl_item {
                    syn::ImplItem::Method(method) => !is_test_only(&method.attrs),
                    _ => true,
                });
                for impl_item in item_impl.items.iter_mut() {
                    if let syn::ImplItem::Method(method) = impl_item {
                        method.attrs = filter_attributes(std::mem::take(&mut method.attrs));
                    }
                }
                kept.push(syn::Item::Impl(item_impl));
            }
            syn::Item::Fn(mut item_fn) => {
                if is_test_only(&item_fn.attrs) || is_test_function(&item_fn.attrs) {
                    continue;
                }
                item_fn.attrs = filter_attributes(item_fn.attrs);
                kept.push(syn::Item::Fn(item_fn));
            }
            syn::Item::Const(mut item_const) => {
                if is_test_only(&item_const.attrs) {
                    continue;
                }
                item_const.attrs = filter_attributes(item_const.attrs);
                kept.push(syn::Item::Const(item_const));
            }
            syn::Item::Type(mut item_type) => {
                if is_test_only(&item_type.attrs) {
                    continue;
                }
                item_type.attrs = filter_attributes(item_type.attrs);
                kept.push(syn::Item::Type(item_type));
            }

            // Modules are only used for tests and benchmarks. Everything else
            // is not something this compiler knows how to handle anyway.
            _ => (),
        }
    }

    kept
}

/// The program lives at the crate root, where restricted visibilities like
/// `pub(super)` are meaningless (and `pub(super)` is even an error).
fn normalize_visibility(source: String) -> String {
    let restricted_visibility = Regex::new(r"pub\s*\((super|crate|self|in [^)]*)\)").unwrap();
    restricted_visibility
        .replace_all(&source, "pub")
        .into_owned()
}

/// `use super::<module>::*;` declares a dependency on a sibling module.
fn dependency_from_use_tree(tree: &syn::UseTree) -> Option<String> {
    let syn::UseTree::Path(use_path) = tree else {
        return None;
    };
    if use_path.ident != "super" {
        return None;
    }
    let syn::UseTree::Path(use_path) = use_path.tree.as_ref() else {
        return None;
    };
    let syn::UseTree::Glob(_) = *use_path.tree else {
        return None;
    };

    Some(use_path.ident.to_string())
}

fn attribute_name(attr: &syn::Attribute) -> String {
    attr.path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .join("::")
}

fn is_test_only(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| {
        attribute_name(attr) == "cfg" && attr.tokens.to_string().replace(' ', "") == "(test)"
    })
}

fn is_test_function(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| {
        let name = attribute_name(attr);
        name == "test" || name.ends_with("::proptest") || name == "proptest"
    })
}

fn is_known_attribute(attr: &syn::Attribute) -> bool {
    KNOWN_ATTRIBUTES.contains(&attribute_name(attr).as_str())
}

/// Keep known attributes, and known derives.
fn filter_attributes(attrs: Vec<syn::Attribute>) -> Vec<syn::Attribute> {
    let mut kept = vec![];
    for attr in attrs {
        if attribute_name(&attr) == "derive" {
            let syn::Meta::List(derive_list) = attr.parse_meta().expect("derive must parse") else {
                continue;
            };
            let known_derives = derive_list
                .nested
                .iter()
                .filter_map(|nested| match nested {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => path.get_ident(),
                    _ => None,
                })
                .filter(|ident| KNOWN_DERIVES.contains(&ident.to_string().as_str()))
                .cloned()
                .collect_vec();
            if !known_derives.is_empty() {
                kept.push(parse_quote!(#[derive(#(#known_derives),*)]));
            }
        } else if is_known_attribute(&attr) {
            kept.push(attr);
        }
    }

    kept
}

/// Fields tagged `#[tasm_object(ignore)]` are not part of the data structure
/// as far as this compiler is concerned.
fn remove_ignored_fields(fields: &mut syn::Fields) {
    let is_ignored = |field: &syn::Field| {
        field.attrs.iter().any(|attr| {
            attribute_name(attr) == IGNORED_FIELD_ATTRIBUTE.0
                && attr.tokens.to_string().replace(' ', "") == IGNORED_FIELD_ATTRIBUTE.1
        })
    };
    let filter_field_attributes = |field: &mut syn::Field| {
        field.attrs = filter_attributes(std::mem::take(&mut field.attrs));
    };

    match fields {
        syn::Fields::Named(named) => {
            let filtered = named
                .named
                .iter()
                .filter(|field| !is_ignored(field))
                .cloned()
                .map(|mut field| {
                    filter_field_attributes(&mut field);
                    field
                })
                .collect();
            named.named = filtered;
        }
        syn::Fields::Unnamed(unnamed) => {
            let filtered = unnamed
                .unnamed
                .iter()
                .filter(|field| !is_ignored(field))
                .cloned()
                .map(|mut field| {
                    filter_field_attributes(&mut field);
                    field
                })
                .collect();
            unnamed.unnamed = filtered;
        }
        syn::Fields::Unit => (),
    }
}
