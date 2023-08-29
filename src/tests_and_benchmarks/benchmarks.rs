use std::collections::HashMap;
use tasm_lib::snippet_bencher::{write_benchmarks, BenchmarkCase, BenchmarkResult};
use triton_vm::{instruction::LabelledInstruction, BFieldElement, NonDeterminism};

use super::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_bench;
use crate::{ast, type_checker::Typing};

pub mod mmr;

#[derive(Debug, Clone)]
pub struct BenchmarkInput {
    pub input_args: Vec<ast::ExprLit<Typing>>,
    pub memory: HashMap<BFieldElement, BFieldElement>,
    pub std_in: Vec<BFieldElement>,
    pub non_determinism: NonDeterminism<BFieldElement>,
}

fn benchmark_code(
    function_name: String,
    code: Vec<LabelledInstruction>,
    benchmark_input: BenchmarkInput,
    expected_stack_diff: isize,
    case: BenchmarkCase,
) -> BenchmarkResult {
    let mut memory = benchmark_input.memory;
    let execution_result = execute_compiled_with_stack_memory_and_ins_for_bench(
        &code,
        benchmark_input.input_args,
        &mut memory,
        benchmark_input.std_in,
        benchmark_input.non_determinism,
        expected_stack_diff,
    )
    .expect("Execution for benchmarking must succeed");

    BenchmarkResult {
        name: function_name,
        clock_cycle_count: execution_result.cycle_count,
        hash_table_height: execution_result.hash_table_height,
        u32_table_height: execution_result.u32_table_height,
        case,
    }
}

pub fn execute_and_write_benchmark(
    function_name: String,
    code: Vec<LabelledInstruction>,
    common_case: BenchmarkInput,
    worst_case: BenchmarkInput,
    expected_stack_diff: isize,
) {
    let benchmark_result_common = benchmark_code(
        function_name.clone(),
        code.clone(),
        common_case,
        expected_stack_diff,
        BenchmarkCase::CommonCase,
    );
    let benchmark_result_worst = benchmark_code(
        function_name,
        code,
        worst_case,
        expected_stack_diff,
        BenchmarkCase::WorstCase,
    );

    write_benchmarks(vec![benchmark_result_common, benchmark_result_worst]);
}
