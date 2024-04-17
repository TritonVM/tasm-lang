use std::collections::HashMap;
use std::fs::create_dir_all;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use tasm_lib::snippet_bencher::write_benchmarks;
use tasm_lib::snippet_bencher::BenchmarkCase;
use tasm_lib::snippet_bencher::NamedBenchmarkResult;
use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::type_checker::Typing;

use super::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_bench;

pub(crate) mod mmr;

#[derive(Debug, Default, Clone)]
pub(crate) struct BenchmarkInput {
    pub(crate) input_args: Vec<ast::ExprLit<Typing>>,
    pub(crate) memory: HashMap<BFieldElement, BFieldElement>,
    pub(crate) std_in: Vec<BFieldElement>,
    pub(crate) non_determinism: NonDeterminism,
}

fn benchmark_code(
    function_name: String,
    code: Vec<LabelledInstruction>,
    benchmark_input: BenchmarkInput,
    expected_stack_diff: isize,
    case: BenchmarkCase,
) -> NamedBenchmarkResult {
    let benchmark_result = execute_compiled_with_stack_and_ins_for_bench(
        &code,
        benchmark_input.input_args,
        benchmark_input.std_in,
        benchmark_input.non_determinism,
        expected_stack_diff,
    )
    .expect("Execution for benchmarking must succeed");

    NamedBenchmarkResult {
        name: function_name,
        benchmark_result,
        case,
    }
}

pub(crate) fn profile(function_name: String, code: Vec<LabelledInstruction>, case: BenchmarkInput) {
    // Write profile for common-case input
    let nondeterminism = case.non_determinism;
    let public_input = PublicInput::from(case.std_in);
    assert!(
        case.input_args.is_empty(),
        "Can only profile on empty input for now"
    );
    assert!(
        case.memory.is_empty(),
        "Can only profile on empty init memory for now"
    );
    let program: Program = Program::new(&code);
    let profile =
        tasm_lib::generate_full_profile(&function_name, program, &public_input, &nondeterminism);

    let mut path = PathBuf::new();
    path.push("profiles");
    create_dir_all(&path).expect("profiles directory should exist");

    path.push(Path::new(&function_name).with_extension("profile"));
    let mut file = std::fs::File::create(&path).expect("open file for writing");
    write!(file, "{profile}").unwrap();
}

pub(crate) fn execute_and_write_benchmark(
    function_name: String,
    code: Vec<LabelledInstruction>,
    common_case: BenchmarkInput,
    worst_case: BenchmarkInput,
    expected_stack_diff: isize,
) {
    let benchmark_result_common = benchmark_code(
        function_name.clone(),
        code.clone(),
        common_case.clone(),
        expected_stack_diff,
        BenchmarkCase::CommonCase,
    );
    let benchmark_result_worst = benchmark_code(
        function_name.clone(),
        code.clone(),
        worst_case,
        expected_stack_diff,
        BenchmarkCase::WorstCase,
    );

    write_benchmarks(vec![benchmark_result_common, benchmark_result_worst]);
}
