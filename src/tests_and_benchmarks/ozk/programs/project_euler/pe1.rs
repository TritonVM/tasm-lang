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
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::triton_vm::prelude::*;
    use itertools::Itertools;

    use super::*;

    #[test]
    fn pe1_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        let entrypoint = EntrypointLocation::disk("project_euler", "pe1", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_safe_lists()
            .with_non_determinism(non_determinism)
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
