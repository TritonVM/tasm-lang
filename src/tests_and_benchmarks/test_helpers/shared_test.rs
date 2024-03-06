use std::collections::HashMap;

use anyhow::bail;
use anyhow::Ok;
use anyhow::Result;
use itertools::Itertools;
use tasm_lib::empty_stack;
use tasm_lib::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
use tasm_lib::rust_shadowing_helper_functions;
use tasm_lib::triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::shared_math::b_field_element::BFIELD_ONE;
use tasm_lib::twenty_first::shared_math::b_field_element::BFIELD_ZERO;
use tasm_lib::VmOutputState;
use tasm_lib::DIGEST_LENGTH;

use crate::ast;
use crate::ast_types;
use crate::composite_types::CompositeTypes;
use crate::graft::Graft;
use crate::libraries::all_libraries;
use crate::tasm_code_generator::compile_function;
use crate::tests_and_benchmarks::ozk::ozk_parsing::compile_for_test;
use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::type_checker;
use crate::type_checker::annotate_fn_outer;
use crate::type_checker::GetType;
use crate::type_checker::Typing;

#[derive(Debug, Clone)]
pub(crate) struct InputOutputTestCase {
    pub(crate) input_args: Vec<ast::ExprLit<Typing>>,
    pub(crate) expected_outputs: Vec<ast::ExprLit<Typing>>,
}

impl InputOutputTestCase {
    pub(crate) fn new(
        input_args: Vec<ast::ExprLit<Typing>>,
        expected_outputs: Vec<ast::ExprLit<Typing>>,
    ) -> Self {
        Self {
            input_args,
            expected_outputs,
        }
    }
}

pub(crate) fn item_fn(item: syn::Item) -> syn::ItemFn {
    match item {
        syn::Item::Fn(item_fn) => item_fn,
        other => panic!("item_fn: expected fn, found: {other:#?}"),
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
pub(crate) fn compile_for_run_test(item_fn: &syn::ItemFn) -> (Vec<LabelledInstruction>, String) {
    let function_name = item_fn.sig.ident.to_string();
    let code = graft_check_compile_prop(item_fn);

    (code, function_name)
}

pub(crate) fn graft_check_compile_prop(item_fn: &syn::ItemFn) -> Vec<LabelledInstruction> {
    let libraries = all_libraries();
    let mut graft_config = Graft::new(&libraries);
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

pub(crate) fn execute_compiled_with_stack_and_ins_for_bench(
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

#[derive(Debug, Clone)]
pub(crate) struct TritonVMTestCase {
    entrypoint: EntrypointLocation,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
}

impl TritonVMTestCase {
    pub(crate) fn new(entrypoint: EntrypointLocation) -> Self {
        Self {
            entrypoint,
            std_in: vec![],
            non_determinism: NonDeterminism::default(),
        }
    }

    pub(crate) fn with_std_in(mut self, std_in: Vec<BFieldElement>) -> Self {
        self.std_in = std_in;
        self
    }

    pub(crate) fn with_non_determinism(
        mut self,
        non_determinism: NonDeterminism<BFieldElement>,
    ) -> Self {
        self.non_determinism = non_determinism;
        self
    }

    pub(crate) fn execute(self) -> Result<VmOutputState> {
        let mut vm_state = self.initial_vm_state();
        vm_state.run()?;

        Self::verify_stack_len_unchanged(&vm_state)?;

        Ok(Self::convert_vm_state_to_output_state(vm_state))
    }

    fn initial_vm_state(self) -> VMState {
        let program = Program::new(&self.compile());
        let public_input = PublicInput::new(self.std_in);
        let vm_state = VMState::new(&program, public_input, self.non_determinism);

        tasm_lib::maybe_write_debuggable_program_to_disk(&program, &vm_state);

        vm_state
    }

    pub(crate) fn compile(&self) -> Vec<LabelledInstruction> {
        compile_for_test(&self.entrypoint)
    }

    fn verify_stack_len_unchanged(vm_state: &VMState) -> Result<()> {
        let terminal_stack_len = vm_state.op_stack.len();
        match terminal_stack_len {
            NUM_OP_STACK_REGISTERS => Ok(()),
            _ => bail!(
                "Expected stack length to be {NUM_OP_STACK_REGISTERS} but was {terminal_stack_len}"
            ),
        }
    }

    fn convert_vm_state_to_output_state(vm_state: VMState) -> VmOutputState {
        VmOutputState {
            output: vm_state.public_output,
            final_stack: vm_state.op_stack.stack,
            final_ram: vm_state.ram,
            final_sponge: vm_state.sponge,
        }
    }
}

pub(crate) fn execute_compiled_with_stack_and_ins_for_test(
    code: &[LabelledInstruction],
    input_args: Vec<ast::ExprLit<Typing>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let mut initial_stack = empty_stack();
    for input_arg in input_args {
        let input_arg_seq = input_arg.encode();
        initial_stack.append(&mut input_arg_seq.into_iter().rev().collect());
    }
    let initial_stack_len = initial_stack.len() as isize;

    let program = Program::new(code);
    let mut vm_state = VMState::new(&program, PublicInput::new(std_in), non_determinism);
    vm_state.op_stack.stack = initial_stack;
    vm_state.run()?;

    let terminal_stack_len = vm_state.op_stack.len() as isize;
    let expected_terminal_stack_len = initial_stack_len + expected_stack_diff;
    if terminal_stack_len != expected_terminal_stack_len {
        bail!("Expected stack length to be {expected_terminal_stack_len} but was {terminal_stack_len}");
    }

    let output_state = VmOutputState {
        output: vm_state.public_output,
        final_stack: vm_state.op_stack.stack,
        final_ram: vm_state.ram,
        final_sponge: vm_state.sponge,
    };

    Ok(output_state)
}

pub(crate) fn execute_with_stack_safe_lists(
    rust_ast: &syn::ItemFn,
    stack_start: Vec<ast::ExprLit<Typing>>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let (code, _fn_name) = compile_for_run_test(rust_ast);
    execute_compiled_with_stack_and_ins_for_test(
        &code,
        stack_start,
        vec![],
        NonDeterminism::default(),
        expected_stack_diff,
    )
}

/// Execute a function with provided input and initial memory
pub(crate) fn execute_with_stack_and_ins_safe_lists(
    rust_ast: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
    expected_stack_diff: isize,
) -> Result<VmOutputState> {
    let (code, _fn_name) = compile_for_run_test(rust_ast);

    execute_compiled_with_stack_and_ins_for_test(
        &code,
        input_args,
        std_in,
        non_determinism,
        expected_stack_diff,
    )
}

pub(crate) fn compare_compiled_prop_with_stack_and_ins(
    code: &[LabelledInstruction],
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
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
    let exec_result = execute_compiled_with_stack_and_ins_for_test(
        code,
        input_args,
        std_in,
        non_determinism,
        expected_final_stack.len() as isize - init_stack_length as isize,
    )
    .unwrap();

    assert_stack_equivalence(expected_final_stack, exec_result.final_stack);
    maybe_assert_ram_equivalence(expected_final_memory, exec_result.final_ram);
}

fn assert_stack_equivalence(
    expected_final_stack: Vec<BFieldElement>,
    final_stack: Vec<BFieldElement>,
) {
    let skip_program_digest =
        |stack: Vec<BFieldElement>| stack.into_iter().skip(DIGEST_LENGTH).collect_vec();
    let stack_to_string = |stack: &[BFieldElement]| stack.iter().join(",");

    let final_stack_str = stack_to_string(&final_stack);
    let expected_stack_str = stack_to_string(&expected_final_stack);
    assert_eq!(
        skip_program_digest(expected_final_stack),
        skip_program_digest(final_stack),
        "TVM:\n{final_stack_str}\n\nExpected:\n{expected_stack_str}\n\n",
    );
}

/// Verify that memory behaves as expected, if expected value is set. Don't bother verifying the value of the dyn
/// malloc address though, as this is considered an implementation detail and is really hard to keep track of.
fn maybe_assert_ram_equivalence(
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    mut final_ram: HashMap<BFieldElement, BFieldElement>,
) {
    let Some(expected_final_memory) = expected_final_memory else {
        return;
    };

    if !expected_final_memory.contains_key(&DYN_MALLOC_ADDRESS) {
        final_ram.remove(&DYN_MALLOC_ADDRESS);
    }

    let ram_to_string = |ram: &HashMap<BFieldElement, BFieldElement>| {
        ram.iter()
            .sorted_by_key(|&(key, _)| key.value())
            .map(|(k, v)| format!("({k} => {v})"))
            .join(",")
    };

    assert_eq!(
        final_ram,
        expected_final_memory,
        "Memory must match expected value after execution.\
        \n\nTVM:      {}\
        \n\nExpected: {}",
        ram_to_string(&final_ram),
        ram_to_string(&expected_final_memory),
    );
}

pub(crate) fn compare_prop_with_stack_and_ins_unsafe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
) {
    let (code, _) = compile_for_run_test(item_fn);
    compare_compiled_prop_with_stack_and_ins(
        &code,
        input_args,
        expected_outputs,
        expected_final_memory,
        std_in,
        non_determinism,
    )
}

pub(crate) fn compare_prop_with_stack_and_ins_safe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
    expected_final_memory: Option<HashMap<BFieldElement, BFieldElement>>,
    std_in: Vec<BFieldElement>,
    non_determinism: NonDeterminism<BFieldElement>,
) {
    let (code, _) = compile_for_run_test(item_fn);
    compare_compiled_prop_with_stack_and_ins(
        &code,
        input_args,
        expected_outputs,
        expected_final_memory,
        std_in,
        non_determinism,
    )
}

pub(crate) fn compare_prop_with_stack_safe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
) {
    compare_prop_with_stack_and_ins_safe_lists(
        item_fn,
        input_args,
        expected_outputs,
        None,
        vec![],
        NonDeterminism::new(vec![]),
    )
}

pub(crate) fn compare_prop_with_stack_unsafe_lists(
    item_fn: &syn::ItemFn,
    input_args: Vec<ast::ExprLit<Typing>>,
    expected_outputs: Vec<ast::ExprLit<Typing>>,
) {
    let (code, _) = compile_for_run_test(item_fn);
    compare_compiled_prop_with_stack_and_ins(
        &code,
        input_args,
        expected_outputs,
        None,
        vec![],
        NonDeterminism::new(vec![]),
    )
}

pub(crate) fn multiple_compare_prop_with_stack_safe_lists(
    item_fn: &syn::ItemFn,
    test_cases: Vec<InputOutputTestCase>,
) {
    let (code, _) = compile_for_run_test(item_fn);
    for test_case in test_cases {
        compare_compiled_prop_with_stack_and_ins(
            &code,
            test_case.input_args,
            test_case.expected_outputs,
            None,
            vec![],
            NonDeterminism::new(vec![]),
        )
    }
}

#[allow(dead_code)]
pub(crate) fn show_memory(memory: &HashMap<BFieldElement, BFieldElement>) {
    let mut memory = memory.iter().collect_vec();
    memory.sort_unstable_by(|&a, &b| a.0.value().partial_cmp(&b.0.value()).unwrap());

    for (k, v) in memory {
        println!("{} => {}", k, v);
    }
}

/// Panic if expected list does not match list on specific memory address
/// Assumes that the "safe list" implementation is used.
pub(crate) fn assert_list_equal(
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
        rust_shadowing_helper_functions::list::list_get_length(list_pointer, memory);
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
            != rust_shadowing_helper_functions::list::list_get(
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

pub(crate) fn bool_lit(value: bool) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Bool(value)
}

pub(crate) fn u32_lit(value: u32) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::U32(value)
}

pub(crate) fn u64_lit(value: u64) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::U64(value)
}

pub(crate) fn u128_lit(value: u128) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::U128(value)
}

pub(crate) fn bfe_lit(value: BFieldElement) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Bfe(value)
}

pub(crate) fn xfe_lit(value: XFieldElement) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Xfe(value)
}

pub(crate) fn digest_lit(value: Digest) -> ast::ExprLit<type_checker::Typing> {
    ast::ExprLit::Digest(value)
}

pub(crate) fn bool_to_bfe(b: bool) -> BFieldElement {
    if b {
        BFIELD_ONE
    } else {
        BFIELD_ZERO
    }
}

pub(crate) fn split(value: u64) -> Vec<BFieldElement> {
    vec![((value >> 32) as u32).into(), (value as u32).into()]
}
