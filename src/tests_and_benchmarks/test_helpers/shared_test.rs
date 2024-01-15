use std::collections::HashMap;

use anyhow::bail;
use anyhow::Ok;
use anyhow::Result;
use itertools::Itertools;
use tasm_lib::empty_stack;
use tasm_lib::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
use tasm_lib::rust_shadowing_helper_functions;
use tasm_lib::VmOutputState;
use tasm_lib::DIGEST_LENGTH;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::twenty_first::shared_math::b_field_element::BFIELD_ONE;
use triton_vm::twenty_first::shared_math::b_field_element::BFIELD_ZERO;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;
use triton_vm::vm::VMState;
use triton_vm::BFieldElement;
use triton_vm::Digest;
use triton_vm::NonDeterminism;
use triton_vm::Program;
use triton_vm::PublicInput;

use crate::ast;
use crate::ast_types;
use crate::composite_types::CompositeTypes;
use crate::tasm_code_generator::compile_function;
use crate::type_checker;
use crate::type_checker::annotate_fn_outer;
use crate::type_checker::GetType;
use crate::type_checker::Typing;

#[derive(Debug, Clone)]
pub struct InputOutputTestCase {
    pub input_args: Vec<ast::ExprLit<Typing>>,
    pub expected_outputs: Vec<ast::ExprLit<Typing>>,
}

impl InputOutputTestCase {
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

pub(crate) fn init_memory_from<T: BFieldCodec>(
    data_struct: &T,
    memory_address: BFieldElement,
) -> NonDeterminism<BFieldElement> {
    let data_struct_encoded = data_struct.encode();
    let init_ram: HashMap<BFieldElement, BFieldElement> = data_struct_encoded
        .into_iter()
        .zip(memory_address.value()..)
        .map(|(v, k)| (k.into(), v))
        .collect();
    NonDeterminism::default().with_ram(init_ram)
}

/// Get the execution code and the name of the compiled function
pub fn compile_for_run_test(
    item_fn: &syn::ItemFn,
    list_type: ast_types::ListType,
) -> (Vec<LabelledInstruction>, String) {
    let function_name = item_fn.sig.ident.to_string();
    let code = graft_check_compile_prop(item_fn, list_type);

    (code, function_name)
}

pub fn graft_check_compile_prop(
    item_fn: &syn::ItemFn,
    list_type: ast_types::ListType,
) -> Vec<LabelledInstruction> {
    get_standard_setup!(list_type, graft_config, libraries);
    let mut intermediate_language_ast = graft_config.graft_fn_decl(item_fn);

    // type-check and annotate. Doesn't handle structs and methods yet.
    let mut composite_types = CompositeTypes::default();
    annotate_fn_outer(
        &mut intermediate_language_ast,
        &mut composite_types,
        &libraries,
    );

    // compile
    let tasm = compile_function(&intermediate_language_ast, &libraries, &composite_types);
    tasm.compose()
}

pub fn execute_compiled_with_stack_memory_and_ins_for_bench(
    code: &[LabelledInstruction],
    input_args: Vec<ast::ExprLit<Typing>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
    expected_stack_diff: isize,
) -> Result<tasm_lib::ExecutionResult> {
    let mut stack = empty_stack();
    for input_arg in input_args {
        let input_arg_seq = input_arg.encode();
        stack.append(&mut input_arg_seq.into_iter().rev().collect());
    }

    // Run the tasm-lib's execute function without requesting initialization of the dynamic
    // memory allocator, as this is the compiler's responsibility.
    tasm_lib::execute_bench_deprecated(
        code,
        &mut stack,
        expected_stack_diff,
        std_in,
        non_determinism,
    )
}

pub fn execute_compiled_with_stack_memory_and_ins_for_test(
    code: &[LabelledInstruction],
    input_args: Vec<ast::ExprLit<Typing>>,
    memory: &HashMap<BFieldElement, BFieldElement>,
    std_in: Vec<BFieldElement>,
    mut non_determinism: NonDeterminism<BFieldElement>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let mut initial_stack = empty_stack();
    for input_arg in input_args {
        let input_arg_seq = input_arg.encode();
        initial_stack.append(&mut input_arg_seq.into_iter().rev().collect());
    }
    let initial_stack_len = initial_stack.len() as isize;

    if !non_determinism.ram.is_empty() && !memory.is_empty() {
        bail!("Cannot specify initial memory with both `memory` and `non_determinism`. Please pick only one.");
    }
    non_determinism.ram.extend(memory.clone());

    let program = Program::new(code);
    let mut vm_state = VMState::new(&program, PublicInput::new(std_in), non_determinism);
    vm_state.op_stack.stack = initial_stack;
    vm_state.run()?;

    let terminal_stack_len = vm_state.op_stack.len() as isize;
    let expected_terminal_stack_len = initial_stack_len + expected_stack_diff;
    if terminal_stack_len != expected_terminal_stack_len {
        bail!("Expected stack length to be {expected_terminal_stack_len} but was {terminal_stack_len}");
    }

    let final_sponge_state = vm_state
        .sponge_state
        .map(|state| tasm_lib::VmHasherState { state });
    let output_state = VmOutputState {
        output: vm_state.public_output,
        final_stack: vm_state.op_stack.stack,
        final_ram: vm_state.ram,
        final_sponge_state,
    };

    Ok(output_state)
}

pub fn execute_with_stack_unsafe_lists(
    rust_ast: &syn::ItemFn,
    stack_start: Vec<ast::ExprLit<Typing>>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let (code, _fn_name) = compile_for_run_test(rust_ast, ast_types::ListType::Unsafe);

    // Run and return final VM state
    execute_compiled_with_stack_memory_and_ins_for_test(
        &code,
        stack_start,
        &HashMap::default(),
        vec![],
        NonDeterminism::new(vec![]),
        expected_stack_diff,
    )
}

pub fn execute_with_stack_safe_lists(
    rust_ast: &syn::ItemFn,
    stack_start: Vec<ast::ExprLit<Typing>>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let (code, _fn_name) = compile_for_run_test(rust_ast, ast_types::ListType::Safe);

    // Run and return final VM state
    execute_compiled_with_stack_memory_and_ins_for_test(
        &code,
        stack_start,
        &HashMap::default(),
        vec![],
        NonDeterminism::default(),
        expected_stack_diff,
    )
}

/// Execute a function with provided input and initial memory
pub fn execute_with_stack_and_memory_safe_lists(
    rust_ast: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    memory: &HashMap<BFieldElement, BFieldElement>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    // Compile
    let (code, _fn_name) = compile_for_run_test(rust_ast, ast_types::ListType::Safe);

    // Run and return final VM state
    execute_compiled_with_stack_memory_and_ins_for_test(
        &code,
        input_args,
        memory,
        vec![],
        NonDeterminism::default(),
        expected_stack_diff,
    )
}

/// Execute a function with provided input and initial memory
pub fn execute_with_stack_memory_and_ins_safe_lists(
    rust_ast: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    memory: &HashMap<BFieldElement, BFieldElement>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let (code, _fn_name) = compile_for_run_test(rust_ast, ast_types::ListType::Safe);

    execute_compiled_with_stack_memory_and_ins_for_test(
        &code,
        input_args,
        memory,
        std_in,
        non_determinism,
        expected_stack_diff,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn compare_compiled_prop_with_stack_and_memory_and_ins(
    code: &[LabelledInstruction],
    function_name: &str,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
) {
    let mut expected_final_stack = empty_stack();
    for output in expected_outputs {
        let output_seq = output.encode();
        expected_final_stack.append(&mut output_seq.into_iter().rev().collect());
    }

    let init_stack_length: usize = empty_stack().len()
        + input_args
            .iter()
            .map(|arg| arg.get_type().stack_size())
            .sum::<usize>();
    let exec_result = execute_compiled_with_stack_memory_and_ins_for_test(
        code,
        input_args,
        &init_memory,
        std_in,
        non_determinism,
        expected_final_stack.len() as isize - init_stack_length as isize,
    )
    .unwrap();

    // Assert stack matches expected stack
    assert_eq!(
        expected_final_stack
            .iter()
            .skip(DIGEST_LENGTH)
            .cloned()
            .collect_vec(),
        exec_result
            .final_stack
            .iter()
            .skip(DIGEST_LENGTH)
            .cloned()
            .collect_vec(),
        "Code execution must produce expected stack `{}`. \n\nTVM:\n{}\n\nExpected:\n{}\n\n\
        Code was:\n{}",
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
        code.iter().join("\n")
    );

    // Verify that memory behaves as expected, if expected value is set. Don't bother verifying the
    // value of the dyn malloc address though, as this is considered an implementation detail and is
    // really hard to keep track of.
    let mut final_ram = exec_result.final_ram.clone();
    if expected_final_memory.as_ref().is_some()
        && !expected_final_memory
            .as_ref()
            .unwrap()
            .contains_key(&DYN_MALLOC_ADDRESS)
    {
        final_ram.remove(&DYN_MALLOC_ADDRESS);
    }

    if let Some(efm) = expected_final_memory {
        if final_ram != efm {
            let mut expected_final_memory = efm.iter().collect_vec();
            expected_final_memory
                .sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
            let expected_final_memory_str = expected_final_memory
                .iter()
                .map(|x| format!("({} => {})", x.0, x.1))
                .collect_vec()
                .join(",");

            let mut actual_memory = final_ram.iter().collect_vec();
            actual_memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());
            let actual_memory_str = actual_memory
                .iter()
                .map(|x| format!("({} => {})", x.0, x.1))
                .collect_vec()
                .join(",");
            panic!(
                "Memory must match expected value after execution.\n\nTVM: {actual_memory_str}\n\n\
                Expected: {expected_final_memory_str}",
            )
        }
    }
}

pub fn compare_prop_with_stack_and_memory_and_ins_unsafe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
) {
    let (code, function_name) = compile_for_run_test(item_fn, ast_types::ListType::Unsafe);
    compare_compiled_prop_with_stack_and_memory_and_ins(
        &code,
        &function_name,
        input_args,
        expected_outputs,
        init_memory,
        expected_final_memory,
        std_in,
        non_determinism,
    )
}

pub fn compare_prop_with_stack_and_memory_and_ins_safe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
) {
    let (code, function_name) = compile_for_run_test(item_fn, ast_types::ListType::Safe);
    compare_compiled_prop_with_stack_and_memory_and_ins(
        &code,
        &function_name,
        input_args,
        expected_outputs,
        init_memory,
        expected_final_memory,
        std_in,
        non_determinism,
    )
}

pub fn compare_prop_with_stack_and_memory_safe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    init_memory: HashMap<BFieldElement, BFieldElement>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
) {
    compare_prop_with_stack_and_memory_and_ins_safe_lists(
        item_fn,
        input_args,
        expected_outputs,
        init_memory,
        expected_final_memory,
        vec![],
        NonDeterminism::new(vec![]),
    )
}

pub fn compare_prop_with_stack_unsafe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
) {
    let (code, function_name) = compile_for_run_test(item_fn, ast_types::ListType::Unsafe);
    compare_compiled_prop_with_stack_and_memory_and_ins(
        &code,
        &function_name,
        input_args,
        expected_outputs,
        HashMap::default(),
        None,
        vec![],
        NonDeterminism::new(vec![]),
    )
}

pub fn compare_prop_with_stack_safe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
) {
    compare_prop_with_stack_and_memory_safe_lists(
        item_fn,
        input_args,
        expected_outputs,
        HashMap::default(),
        None,
    )
}

pub fn multiple_compare_prop_with_stack_safe_lists(
    item_fn: &syn::ItemFn,
    test_cases: Vec<InputOutputTestCase>,
) {
    // Compile
    let (code, fn_name) = compile_for_run_test(item_fn, ast_types::ListType::Safe);

    for test_case in test_cases {
        compare_compiled_prop_with_stack_and_memory_and_ins(
            &code,
            &fn_name,
            test_case.input_args,
            test_case.expected_outputs,
            HashMap::default(),
            None,
            vec![],
            NonDeterminism::new(vec![]),
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
pub fn assert_list_equal(
    expected_list: Vec<ast::ExprLit<type_checker::Typing>>,
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) {
    let element_type: Option<ast_types::DataType> = if !expected_list.is_empty() {
        Some(expected_list[0].get_type())
    } else {
        None
    };

    // assert elements agree on type
    if let Some(element_type) = element_type.clone() {
        for elem in expected_list.iter() {
            type_checker::assert_type_equals(
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
        if expected_list[i].encode()
            != rust_shadowing_helper_functions::safe_list::safe_list_get(
                list_pointer,
                i,
                memory,
                element_type.as_ref().unwrap().stack_size(),
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
                    .encode()
                    .iter()
                    .map(|x| x.to_string())
                    .join(", "),
                actual_mem_as_str
            );
        }
    }
}

pub fn bool_lit(value: bool) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Bool(value)
}

pub fn u32_lit(value: u32) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::U32(value)
}

pub fn u64_lit(value: u64) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::U64(value)
}

pub fn u128_lit(value: u128) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::U128(value)
}

pub fn bfe_lit(value: BFieldElement) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Bfe(value)
}

pub fn xfe_lit(value: XFieldElement) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Xfe(value)
}

pub fn digest_lit(value: Digest) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Digest(value)
}

pub fn bool_to_bfe(b: bool) -> BFieldElement {
    if b {
        BFIELD_ONE
    } else {
        BFIELD_ZERO
    }
}

pub fn split(value: u64) -> Vec<BFieldElement> {
    vec![((value >> 32) as u32).into(), (value as u32).into()]
}
