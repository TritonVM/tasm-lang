use itertools::Itertools;
use std::collections::HashMap;
use tasm_lib::get_init_tvm_stack;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{ast, graft::graft, tasm::compile, types::annotate_fn};

pub fn graft_check_compile_prop(item_fn: &syn::ItemFn) -> String {
    // parse test
    let mut function = graft(item_fn);

    // type-check and annotate
    annotate_fn(&mut function);

    // println!("{:#?}", function);

    // compile
    let tasm = compile(&function);
    let tasm_string: String = tasm.iter().map(|instr| instr.to_string()).join("\n");
    println!("{tasm_string}");
    tasm_string
}

pub fn compile_execute_and_compare_prop(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit>,
    expected_outputs: Vec<ast::ExprLit>,
) {
    let code = graft_check_compile_prop(item_fn);
    let function_name = item_fn.sig.ident.to_string();
    let code = format!(
        "
        call {function_name}
        halt

        {code}"
    );

    println!("{code}");

    let mut stack = get_init_tvm_stack();
    for input_arg in input_args {
        // TODO: Rewrite this using `to_sequence()` from the Hashable trait
        let mut bfe_sequence: Vec<BFieldElement> = match input_arg {
            ast::ExprLit::Bool(b) => vec![BFieldElement::new(b as u64)],
            ast::ExprLit::U32(v) => vec![BFieldElement::new(v as u64)],
            ast::ExprLit::U64(v) => vec![
                BFieldElement::new(v >> 32),
                BFieldElement::new(v & u32::MAX as u64),
            ],
            ast::ExprLit::BFE(bfe) => vec![bfe],
            ast::ExprLit::XFE(xfe) => xfe.coefficients.to_vec(),
            ast::ExprLit::Digest(digest) => digest.values().to_vec(),
        };
        stack.append(&mut bfe_sequence);
    }

    // println!(
    //     "init_stack: {}",
    //     stack.iter().map(|x| x.to_string()).join(",")
    // );
    let mut expected_final_stack = get_init_tvm_stack();
    for output in expected_outputs {
        // TODO: Rewrite this using `to_sequence()` from the Hashable trait
        let mut bfe_sequence: Vec<BFieldElement> = match output {
            ast::ExprLit::Bool(b) => vec![BFieldElement::new(b as u64)],
            ast::ExprLit::U32(v) => vec![BFieldElement::new(v as u64)],
            ast::ExprLit::U64(v) => vec![
                BFieldElement::new(v >> 32),
                BFieldElement::new(v & u32::MAX as u64),
            ],
            ast::ExprLit::BFE(bfe) => vec![bfe],
            ast::ExprLit::XFE(xfe) => xfe.coefficients.to_vec(),
            ast::ExprLit::Digest(digest) => digest.values().to_vec(),
        };
        expected_final_stack.append(&mut bfe_sequence);
    }

    let init_stack_length = stack.len();
    let exec_result = tasm_lib::execute(
        &code,
        &mut stack,
        expected_final_stack.len() as isize - init_stack_length as isize,
        vec![],
        vec![],
        &mut HashMap::default(),
    );

    assert_eq!(
        expected_final_stack,
        exec_result.final_stack,
        "Code execution must produce expected stack `{}`. \n\nTVM:\n{}\nExpected:\n{}",
        function_name,
        exec_result
            .final_stack
            .iter()
            .map(|x| x.to_string())
            .collect_vec()
            .join(","),
        expected_final_stack
            .iter()
            .map(|x| x.to_string())
            .collect_vec()
            .join(","),
    );
}
