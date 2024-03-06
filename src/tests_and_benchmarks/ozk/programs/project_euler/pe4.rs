use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(clippy::needless_else)]
fn main() {
    // https://projecteuler.net/problem=4
    // Warning! Takes four minutes to run on my fast machine with the original
    // problem size, which is `min_value = 100`, `max_value = 1000`
    // So we use a smaller problem size here.

    /// Convert a u32 number into a list of its decimal digits. Overwrites the
    /// list provided as input. Least significant digit occupies the lowest
    /// indices of the `digits` list.
    fn find_decimal_digits(value: u32, digits: &mut Vec<u32>) {
        let mut rem: u32 = value;
        digits.clear();
        while rem != 0 {
            digits.push(rem % 10);
            rem /= 10;
        }

        return;
    }

    #[allow(clippy::ptr_arg)]
    /// Return true iff list is the same read backwards as forwards
    fn list_is_palindrome(list: &Vec<u32>) -> bool {
        let list_length: usize = list.len();
        let mut i: usize = 0;
        let mut result: bool = true;
        while i != list_length / 2 {
            if list[i] != list[list_length - 1 - i] {
                result = false;
            }

            i += 1;
        }

        return result;
    }

    let mut decimal_digits: Vec<u32> = Vec::<u32>::default();
    let min_value: u32 = 10;
    let max_value: u32 = 50;
    let mut lhs: u32 = min_value;
    let mut max_palindrome: u32 = 0;
    while lhs != max_value {
        let mut rhs: u32 = min_value;
        while rhs != lhs + 1 {
            let prod: u32 = lhs * rhs;
            find_decimal_digits(prod, &mut decimal_digits);
            // short-circuiting and better control-flow (e.g. `break`) would
            // give this a *massive* speedup.
            if list_is_palindrome(&decimal_digits) && prod > max_palindrome {
                max_palindrome = prod;
            }
            rhs += 1;
        }

        lhs += 1;
    }

    tasm::tasm_io_write_to_stdout___u32(max_palindrome);

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
    fn pe4_test() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);

        let entrypoint = EntrypointLocation::disk("project_euler", "pe4", "main");
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();

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
    fn pe4_bench() {
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe4", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let (code, _) = compile_for_run_test(&parsed);

        let common_case = BenchmarkInput::default();
        let worst_case = BenchmarkInput::default();
        let name = "project_euler_4_w_unsafe_lists_i10_to_50".to_owned();
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
