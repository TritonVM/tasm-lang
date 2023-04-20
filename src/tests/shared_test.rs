use itertools::Itertools;
use std::collections::HashMap;
use tasm_lib::dyn_malloc::DYN_MALLOC_ADDRESS;
use tasm_lib::{get_init_tvm_stack, rust_shadowing_helper_functions};
use twenty_first::shared_math::b_field_element::{BFieldElement, BFIELD_ONE, BFIELD_ZERO};
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::ast;
use crate::graft::graft_fn_decl;
use crate::tasm_code_generator::{compile_function, GlobalCompilerState};
use crate::types::{self, annotate_fn, GetType, Typing};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct InputOutputTestCase {
    pub input_args: Vec<ast::ExprLit<Typing>>,
    pub expected_outputs: Vec<ast::ExprLit<Typing>>,
}

impl InputOutputTestCase {
    #[allow(dead_code)]
    pub fn new(
        input_args: Vec<ast::ExprLit<Typing>>,
        expected_outputs: Vec<ast::ExprLit<Typing>>,
    ) -> Self {
        Self {
            input_args,
            expected_outputs,
        }
    }
}

/// Get the execution code and the name of the compiled function
fn compile_for_run_test(item_fn: &syn::ItemFn) -> (String, String) {
    let function_name = item_fn.sig.ident.to_string();
    let code = graft_check_compile_prop(item_fn);
    let code = format!(
        "
        call {function_name}
        halt

        {code}"
    );

    (code, function_name)
}

#[allow(dead_code)]
pub fn graft_check_compile_prop(item_fn: &syn::ItemFn) -> String {
    // parse test
    let mut function = graft_fn_decl(item_fn);

    // type-check and annotate
    annotate_fn(&mut function);

    // println!("{function:#?}");

    // compile
    let tasm = compile_function(&function, &mut GlobalCompilerState::default());
    let tasm_string: String = tasm.iter().map(|instr| instr.to_string()).join("\n");
    // println!("{tasm_string}");
    tasm_string
}

#[allow(dead_code)]
fn execute_compiled_with_stack_memory_and_ins(
    code: &str,
    input_args: Vec<ast::ExprLit<Typing>>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
    expected_stack_diff: isize,
) -> anyhow::Result<tasm_lib::ExecutionResult> {
    let mut stack = get_init_tvm_stack();
    for input_arg in input_args {
        let input_arg_seq = input_arg.to_sequence();
        stack.append(&mut input_arg_seq.into_iter().rev().collect());
    }

    // Run the tasm-lib's execute function without requesting initialization of the dynamic
    // memory allocator, as this is the compiler's responsibility.
    println!("executing code:\n {code}");
    tasm_lib::execute(
        code,
        &mut stack,
        expected_stack_diff,
        std_in,
        secret_in,
        memory,
        None,
    )
}

#[allow(dead_code)]
/// Execute a function with provided input and initial memory
pub fn execute_with_stack_memory_and_ins(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
    expected_stack_diff: isize,
) -> anyhow::Result<tasm_lib::ExecutionResult> {
    // Compile
    let (code, _fn_name) = compile_for_run_test(item_fn);

    // Run and compare
    execute_compiled_with_stack_memory_and_ins(
        &code,
        input_args,
        memory,
        std_in,
        secret_in,
        expected_stack_diff,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn compare_compiled_prop_with_stack_and_memory_and_ins(
    code: &str,
    function_name: &str,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
) {
    let mut expected_final_stack = get_init_tvm_stack();
    for output in expected_outputs {
        let output_seq = output.to_sequence();
        expected_final_stack.append(&mut output_seq.into_iter().rev().collect());
    }

    let init_stack_length: usize = get_init_tvm_stack().len()
        + input_args
            .iter()
            .map(|arg| arg.get_type().size_of())
            .sum::<usize>();
    let mut actual_memory = init_memory;
    let exec_result = execute_compiled_with_stack_memory_and_ins(
        code,
        input_args,
        &mut actual_memory,
        std_in,
        secret_in,
        expected_final_stack.len() as isize - init_stack_length as isize,
    )
    .unwrap();

    // Assert stack matches expected stack
    assert_eq!(
        expected_final_stack,
        exec_result.final_stack,
        "Code execution must produce expected stack `{}`. \n\nTVM:\n{}\n\nExpected:\n{}\n",
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

    // Verify that memory behaves as expected, if expected value is set. Don't bother verifying the value
    // of the dyn malloc address though, as this is considered an implementation detail and is really hard
    // to keep track of.
    if expected_final_memory.as_ref().is_some()
        && !expected_final_memory
            .as_ref()
            .unwrap()
            .contains_key(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64))
    {
        actual_memory.remove(&BFieldElement::new(DYN_MALLOC_ADDRESS as u64));
    }

    if let Some(efm) = expected_final_memory {
        if actual_memory != efm {
            let mut expected_final_memory = efm.iter().collect_vec();
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
            panic!("Memory must match expected value after execution.\n\nTVM: {actual_memory_str}\n\nExpected: {expected_final_memory_str}",)
        }
    }
}

#[allow(dead_code)]
pub fn compare_prop_with_stack_and_memory_and_ins(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
) {
    let (code, function_name) = compile_for_run_test(item_fn);
    compare_compiled_prop_with_stack_and_memory_and_ins(
        &code,
        &function_name,
        input_args,
        expected_outputs,
        init_memory,
        expected_final_memory,
        std_in,
        secret_in,
    )
}

#[allow(dead_code)]
pub fn compare_prop_with_stack_and_memory(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
) {
    compare_prop_with_stack_and_memory_and_ins(
        item_fn,
        input_args,
        expected_outputs,
        init_memory,
        expected_final_memory,
        vec![],
        vec![],
    )
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
        None,
    )
}

#[allow(dead_code)]
pub fn multiple_compare_prop_with_stack(
    item_fn: &syn::ItemFn,
    test_cases: Vec<InputOutputTestCase>,
) {
    // Compile
    let (code, fn_name) = compile_for_run_test(item_fn);

    for test_case in test_cases {
        compare_compiled_prop_with_stack_and_memory_and_ins(
            &code,
            &fn_name,
            test_case.input_args,
            test_case.expected_outputs,
            HashMap::default(),
            None,
            vec![],
            vec![],
        )
    }
}

#[allow(dead_code)]
pub fn show_memory(memory: &HashMap<BFieldElement, BFieldElement>) {
    let mut memory = memory.iter().collect_vec();
    memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());

    for (k, v) in memory {
        println!("{} => {}", k, v);
    }
}

/// Panic if expected list does not match list on specific memory address
/// Assumes that the "safe list" implementation is used.
#[allow(dead_code)]
pub fn assert_list_equal(
    expected_list: Vec<ast::ExprLit<types::Typing>>,
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) {
    let element_type: Option<ast::DataType> = if !expected_list.is_empty() {
        Some(expected_list[0].get_type())
    } else {
        None
    };

    // assert elements agree on type
    if let Some(element_type) = element_type.clone() {
        for elem in expected_list.iter() {
            types::assert_type_equals(
                &elem.get_type(),
                &element_type,
                "assert_list_equal test helper function",
            );
        }
    }

    let actual_length =
        rust_shadowing_helper_functions::safe_list::safe_list_get_length(list_pointer, memory);
    let expected_length = expected_list.len();
    if expected_length != actual_length {
        let mut actual_memory = memory.iter().collect_vec();
        actual_memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
        let actual_mem_as_str = actual_memory
            .iter()
            .map(|x| format!("({} => {})", x.0, x.1))
            .collect_vec()
            .join(",");

        panic!(
            "Length of list must match. \nActual: {actual_length}\n expected: {expected_length}\n\n\n Memory was: {}",
            actual_mem_as_str
        );
    }

    #[allow(clippy::needless_range_loop)]
    for i in 0..expected_list.len() {
        if expected_list[i].to_sequence()
            != rust_shadowing_helper_functions::safe_list::safe_list_read(
                list_pointer,
                i,
                memory,
                element_type.as_ref().unwrap().size_of(),
            )
        {
            let mut actual_memory = memory.iter().collect_vec();
            actual_memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
            let actual_mem_as_str = actual_memory
                .iter()
                .map(|x| format!("({} => {})", x.0, x.1))
                .collect_vec()
                .join(",");

            panic!(
                "Element number {i} did not match expected value of [{}]. \n Memory was: {}",
                expected_list[i]
                    .to_sequence()
                    .iter()
                    .map(|x| x.to_string())
                    .join(", "),
                actual_mem_as_str
            );
        }
    }
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

#[allow(dead_code)]
pub fn xfe_lit(value: XFieldElement) -> ast::ExprLit<types::Typing> {
    ast::ExprLit::XFE(value)
}

#[allow(dead_code)]
pub fn digest_lit(value: Digest) -> ast::ExprLit<types::Typing> {
    ast::ExprLit::Digest(value)
}

#[allow(dead_code)]
pub fn bool_to_bfe(b: bool) -> BFieldElement {
    if b {
        BFIELD_ONE
    } else {
        BFIELD_ZERO
    }
}

#[allow(dead_code)]
pub fn split(value: u64) -> Vec<BFieldElement> {
    vec![((value >> 32) as u32).into(), (value as u32).into()]
}
