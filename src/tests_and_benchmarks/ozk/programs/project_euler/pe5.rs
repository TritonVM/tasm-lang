use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::needless_else)]
fn main() {
    // https://projecteuler.net/problem=5
    // 2 = 2, 3 = 3, 5 = 5, 6 = 2*3,
    // 7 = 7, 8 = 2^3, 9 = 3^2, 10 = 2*5, 11 = 11, 12 = 3*2^2, 13 = 13, 14 = 2*7, 15 = 3 * 5,
    // 16 = 2^4, 17 = 17, 18 = 2*3^2, 19 = 19, 20 = 2^2*5
    let ret: u32 = 2u32.pow(4) * 3u32.pow(2) * 5 * 7 * 11 * 13 * 17 * 19;
    tasm::tasm_io_write_to_stdout___u32(ret);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::triton_vm::prelude::*;
    use itertools::Itertools;

    #[test]
    fn pe5_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);

        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe5", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_safe_lists()
            .expect_stack_difference(0)
            .execute()
            .unwrap();

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
    fn pe5_bench() {
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe5", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let (code, _) = compile_for_run_test(&parsed, crate::ast_types::ListType::Safe);

        let common_case = BenchmarkInput::default();
        let worst_case = BenchmarkInput::default();
        let name = "project_euler_5".to_owned();
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
