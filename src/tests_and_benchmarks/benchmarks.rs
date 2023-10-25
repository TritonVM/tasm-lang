use std::{
    collections::HashMap,
    fs::create_dir_all,
    io::Write,
    path::{Path, PathBuf},
};
use tasm_lib::snippet_bencher::{write_benchmarks, BenchmarkCase, BenchmarkResult};
use triton_vm::{
    instruction::LabelledInstruction, program::ProfileLine, BFieldElement, NonDeterminism, Program,
};

use super::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_bench;
use crate::{ast, type_checker::Typing};

pub mod mmr;

#[derive(Debug, Default, Clone)]
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

pub fn profile(function_name: String, code: Vec<LabelledInstruction>, case: BenchmarkInput) {
    // Write profile for common-case input
    let nondeterminism = case.non_determinism;
    let public_input = case.std_in;
    assert!(
        case.input_args.is_empty(),
        "Can only profile on empty input for now"
    );
    assert!(
        case.memory.is_empty(),
        "Can only profile on empty init memory for now"
    );
    let program: Program = Program::new(&code);
    let (_output, profile) = program
        .profile(public_input.clone().into(), nondeterminism.clone())
        .unwrap();

    let mut str = format!("{function_name}:\n");
    str = format!("{str}\n# call graph\n");
    for line in profile.iter() {
        let indentation = vec!["  "; line.call_stack_depth].join("");
        let label = &line.label;
        let cycle_count = line.cycle_count;
        str = format!("{str}{indentation} {label}: {cycle_count}\n");
    }
    str = format!("{str}\n# aggregated\n");
    let mut aggregated: Vec<ProfileLine> = vec![];
    for line in profile {
        if let Some(agg) = aggregated.iter_mut().find(|a| a.label == line.label) {
            agg.cycle_count += line.cycle_count;
            agg.call_stack_depth = std::cmp::min(agg.call_stack_depth, line.call_stack_depth);
        } else {
            aggregated.push(line);
        }
    }
    for line in aggregated {
        let indentation = vec!["  "; line.call_stack_depth].join("");
        let label = line.label;
        let cycle_count = line.cycle_count;
        str = format!("{str}{indentation} {label}: {cycle_count}\n");
    }

    // write profile to standard output in case someone is watching
    println!("{str}");

    // write profile to profile file
    let mut path = PathBuf::new();
    path.push("profiles");
    create_dir_all(&path).expect("profiles directory should exist");

    path.push(Path::new(&function_name).with_extension("profile"));
    let mut file = std::fs::File::create(&path).expect("open file for writing");
    write!(file, "{str}").unwrap();
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
