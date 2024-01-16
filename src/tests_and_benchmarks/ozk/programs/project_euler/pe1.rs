use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // https://projecteuler.net/problem=1
    let mut i: u32 = 1;
    let mut acc: u32 = 0;

    while i < 1000 {
        if i % 3 == 0 || i % 5 == 0 {
            acc += i;
        }

        i += 1;
    }

    tasm::tasm_io_write_to_stdout___u32(acc);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn pe1_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Test function in Triton VM
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe1", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =
            execute_with_stack_safe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(native_output, vm_output.output);

        println!("vm_output.output: {}", vm_output.output.iter().join(","));
    }
}

mod benches {
    use crate::tests_and_benchmarks::benchmarks::execute_and_write_benchmark;
    use crate::tests_and_benchmarks::benchmarks::profile;
    use crate::tests_and_benchmarks::benchmarks::BenchmarkInput;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn pe1_bench() {
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe1", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let (code, _) = compile_for_run_test(&parsed, crate::ast_types::ListType::Safe);

        let common_case = BenchmarkInput::default();
        let worst_case = BenchmarkInput::default();
        let name = "project_euler_1".to_owned();
        execute_and_write_benchmark(
            name.clone(),
            code.clone(),
            common_case.clone(),
            worst_case,
            0,
        );
        profile(name, code, common_case);
    }
}
