use std::fs;

use triton_vm::instruction::LabelledInstruction;

use crate::ast_types;
use crate::custom_type_resolver::resolve_custom_types;
use crate::extract_types_and_function;
use crate::tasm_code_generator::compile_function;
use crate::type_checker::annotate_fn_outer;
use crate::StructsAndMethodsRustAst;

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
