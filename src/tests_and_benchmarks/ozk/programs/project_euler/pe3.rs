use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // https://projecteuler.net/problem=3

    // Notice that the number used on the Project Euler website is `600851475143`
    // but we use a smaller number here, since we don't want this test too take
    // too long.
    // let mut composite_number: u64 = 600851475143;
    let mut composite_number: u64 = 600851;
    let mut candidate_divisor: u64 = 2;
    while candidate_divisor * candidate_divisor <= composite_number {
        // Notice that current div mod implementation could be sped up by using
        // non-determinism and proving the result instead actually calculating
        // the result of the `div_mod` operation as we do here.
        if composite_number % candidate_divisor == 0 {
            composite_number /= candidate_divisor;
        } else {
            candidate_divisor += 1;
        }
    }

    tasm::tasm_io_write_to_stdout___u64(composite_number);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn pe3_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Test function in Triton VM
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe3", "main");
        let (parsed, _) = ozk_parsing::parse_functions_and_types(&entrypoint_location);
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
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn pe3_bench() {
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe3", "main");
        let (parsed, _) = ozk_parsing::parse_functions_and_types(&entrypoint_location);
        let (code, _) = compile_for_run_test(&parsed, crate::ast_types::ListType::Safe);

        let common_case = BenchmarkInput::default();
        let worst_case = BenchmarkInput::default();
        let name = "project_euler_3_i600851".to_owned();
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
