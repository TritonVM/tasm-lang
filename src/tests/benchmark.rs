use serde_json::to_writer_pretty;
use std::{
    collections::HashMap,
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};
use tasm_lib::snippet_bencher::{BenchmarkCase, BenchmarkResult};
use triton_vm::BFieldElement;

use super::shared_test::execute_compiled_with_stack_memory_and_ins;
use crate::{ast, types::Typing};

pub fn benchmark_code(
    function_name: String,
    code: &str,
    input_args: Vec<ast::ExprLit<Typing>>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    std_in: Vec<BFieldElement>,
    secret_in: Vec<BFieldElement>,
    expected_stack_diff: isize,
    case: BenchmarkCase,
) -> BenchmarkResult {
    let execution_result = execute_compiled_with_stack_memory_and_ins(
        code,
        input_args,
        memory,
        std_in,
        secret_in,
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
