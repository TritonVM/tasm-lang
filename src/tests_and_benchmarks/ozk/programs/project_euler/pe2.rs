use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // https://projecteuler.net/problem=2
    let mut previous: u32 = 1;
    let mut current: u32 = 1;
    let mut acc: u32 = 0;
    while current <= 4_000_000 {
        if current % 2 == 0 {
            acc += current;
        }

        let tmp: u32 = current;
        current += previous;
        previous = tmp;
    }

    tasm::tasmlib_io_write_to_stdout___u32(acc);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use itertools::Itertools;
    use tasm_lib::triton_vm::prelude::*;

    #[test]
    fn pe2_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);

        let entrypoint = EntrypointLocation::disk("project_euler", "pe2", "main");
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();

        assert_eq!(native_output, vm_output.public_output);
        assert_eq!(4613732, native_output[0].value());

        println!(
            "vm_output.public_output: {}",
            vm_output.public_output.iter().join(",")
        );
    }
}

mod benches {
    use crate::tests_and_benchmarks::benchmarks::execute_and_write_benchmark;
    use crate::tests_and_benchmarks::benchmarks::profile;
    use crate::tests_and_benchmarks::benchmarks::BenchmarkInput;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn pe2_bench() {
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe2", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let (code, _) = compile_for_run_test(&parsed);

        let common_case = BenchmarkInput::default();
        let worst_case = BenchmarkInput::default();
        let name = "project_euler_2".to_owned();
        execute_and_write_benchmark(name.clone(), code.clone(), common_case.clone(), worst_case);
        profile(name, code, common_case);
    }
}
