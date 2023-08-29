use std::fs;

use triton_vm::instruction::LabelledInstruction;

use crate::{
    graft::{graft_fn_decl, graft_structs},
    tasm_code_generator::compile_function,
    type_checker::annotate_fn,
};

fn extract_types_and_main(parsed_file: syn::File) -> (Vec<syn::ItemStruct>, Option<syn::ItemFn>) {
    let mut main_func: Option<syn::ItemFn> = None;
    let mut structs: Vec<syn::ItemStruct> = vec![];
    for item in parsed_file.items {
        if let syn::Item::Struct(struct_item) = &item {
            structs.push(struct_item.to_owned());
        }
        if let syn::Item::Fn(func) = &item {
            if func.sig.ident == "main" {
                main_func = Some(func.to_owned());
            }
        }
    }

    (structs, main_func)
}

pub(super) fn parse_main_and_structs(
    module_name: &str,
) -> (syn::ItemFn, Vec<syn::ItemStruct>, String) {
    let path = format!(
        "{}/src/tests_and_benchmarks/ozk/programs/{module_name}.rs",
        env!("CARGO_MANIFEST_DIR"),
    );
    let content = fs::read_to_string(&path).expect("Unable to read file {path}");
    let parsed_file: syn::File = syn::parse_str(&content).expect("Unable to parse rust code");
    let (structs, main_parsed) = extract_types_and_main(parsed_file);
    match main_parsed {
        Some(main) => (main, structs, module_name.to_owned()),
        None => panic!("Failed to parse file {path}"),
    }
}

pub(crate) fn compile_for_test(module_name: &str) -> Vec<LabelledInstruction> {
    let (parsed_main, parsed_structs, _) = parse_main_and_structs(module_name);

    // parse test
    let mut function = graft_fn_decl(&parsed_main);
    let structs = graft_structs(parsed_structs);

    // type-check and annotate
    annotate_fn(&mut function, structs);

    // compile
    let tasm = compile_function(&function);

    // compose
    tasm.compose()
}
