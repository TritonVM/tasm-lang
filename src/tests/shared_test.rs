use itertools::Itertools;
use std::collections::HashMap;
use tasm_lib::get_init_tvm_stack;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::ast;
use crate::graft::graft;
use crate::tasm_code_generator::compile;
use crate::types::{self, annotate_fn, Typing};

#[allow(dead_code)]
pub fn graft_check_compile_prop(item_fn: &syn::ItemFn) -> String {
    // parse test
    let mut function = graft(item_fn);

    // type-check and annotate
    annotate_fn(&mut function);

    println!("{function:#?}");

    // compile
    let tasm = compile(&function);
    let tasm_string: String = tasm.iter().map(|instr| instr.to_string()).join("\n");
    // println!("{tasm_string}");
    tasm_string
}

#[allow(dead_code)]
pub fn compare_prop_with_stack_and_memory(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: HashMap<BFieldElement, BFieldElement>,
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
        let input_arg_seq = input_arg.to_sequence();
        stack.append(&mut input_arg_seq.into_iter().rev().collect());
    }

    let mut expected_final_stack = get_init_tvm_stack();
    for output in expected_outputs {
        let output_seq = output.to_sequence();
        expected_final_stack.append(&mut output_seq.into_iter().rev().collect());
    }

    let init_stack_length = stack.len();
    let mut actual_memory = init_memory;
    let exec_result = tasm_lib::execute(
        &code,
        &mut stack,
        expected_final_stack.len() as isize - init_stack_length as isize,
        vec![],
        vec![],
        &mut actual_memory,
    );

    // Assert stack matches expected stack
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

    // Verify that memory behaves as expected
    if actual_memory != expected_final_memory {
        let mut expected_final_memory = expected_final_memory.iter().collect_vec();
        expected_final_memory
            .sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
        let expected_final_memory_str = expected_final_memory
            .iter()
            .map(|x| format!("({} => {})", x.0, x.1))
            .collect_vec()
            .join(",");

        let mut actual_memory = actual_memory.iter().collect_vec();
        actual_memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
        let actual_memory_str = actual_memory
            .iter()
            .map(|x| format!("({} => {})", x.0, x.1))
            .collect_vec()
            .join(",");
        panic!("Memory must match expected value after execution.\n\nGot: {actual_memory_str}\n\nExpected: {expected_final_memory_str}",)
    }
}

#[allow(dead_code)]
pub fn compare_prop_with_stack(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
) {
    compare_prop_with_stack_and_memory(
        item_fn,
        input_args,
        expected_outputs,
        HashMap::default(),
        HashMap::default(),
    )
}

#[allow(dead_code)]
pub fn bool_lit(value: bool) -> ast::ExprLit<types::Typing> {
    ast::ExprLit::Bool(value)
}

#[allow(dead_code)]
pub fn u32_lit(value: u32) -> ast::ExprLit<types::Typing> {
    ast::ExprLit::U32(value)
}

#[allow(dead_code)]
pub fn u64_lit(value: u64) -> ast::ExprLit<types::Typing> {
    ast::ExprLit::U64(value)
}

#[allow(dead_code)]
pub fn bfe_lit(value: BFieldElement) -> ast::ExprLit<types::Typing> {
    ast::ExprLit::BFE(value)
}
