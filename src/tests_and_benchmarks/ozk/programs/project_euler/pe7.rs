use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // https://projecteuler.net/problem=7
    // Find the 10_001st prime number.

    // We reduce the problem size to 101 as this algorithm otherwise takes too
    // long to run.
    let index_of_prime_to_find: u32 = 101;
    let log_of_desired_index: u32 = u32::BITS - index_of_prime_to_find.leading_zeros() - 1;
    let sieve_size: u32 = index_of_prime_to_find * log_of_desired_index;
    let mut primes: Vec<bool> = Vec::<bool>::with_capacity(sieve_size as usize);
    primes.push(false);
    primes.push(false);
    let mut tmp_vec_initializer: u32 = 2;
    while tmp_vec_initializer < sieve_size {
        primes.push(true);
        tmp_vec_initializer += 1;
    }

    let mut num_primes_found: u32 = 1;
    let mut prime_candidate: u32 = 3;
    let mut last_prime_found: u32 = prime_candidate;
    while num_primes_found < index_of_prime_to_find {
        if primes[prime_candidate as usize] {
            num_primes_found += 1;
            last_prime_found = prime_candidate;
            let mut multiples_of_found_prime: u32 = 2 * prime_candidate;
            while multiples_of_found_prime < sieve_size {
                primes[multiples_of_found_prime as usize] = false;
                multiples_of_found_prime += prime_candidate;
            }
        }
        prime_candidate += 2;
    }

    tasm::tasm_io_write_to_stdout___u32(index_of_prime_to_find);
    tasm::tasm_io_write_to_stdout___u32(last_prime_found);

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    #[test]
    fn pe7_test() {
        // Test function on host machine
        let timer = std::time::Instant::now();
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);
        let prime_number_count: u32 = native_output[0].try_into().unwrap();
        let computed_element = native_output[1];
        let time_passed = timer.elapsed();
        println!("native_output for prime number {prime_number_count} (took {time_passed:?}): {computed_element}");

        // Test function in Triton VM
        let (parsed, _, _) =
            ozk_parsing::parse_function_and_structs("project_euler", "pe7", "main");
        let expected_stack_diff = 0;
        let stack_start = vec![];
        let vm_output =
            execute_with_stack_unsafe_lists(&parsed, stack_start, expected_stack_diff).unwrap();
        assert_eq!(native_output, vm_output.output);

        println!(
            "vm_output.output for prime number {prime_number_count}: {}",
            vm_output.output.iter().skip(1).join("\n")
        );
    }
}

mod benches {
    use crate::tests_and_benchmarks::{
        benchmarks::{execute_and_write_benchmark, profile, BenchmarkInput},
        ozk::ozk_parsing,
        test_helpers::shared_test::*,
    };

    #[test]
    fn pe7_bench() {
        let (parsed, _, _) =
            ozk_parsing::parse_function_and_structs("project_euler", "pe7", "main");
        let (code, _) = compile_for_run_test(&parsed, crate::ast_types::ListType::Safe);

        let common_case = BenchmarkInput::default();
        let worst_case = BenchmarkInput::default();
        let name = "project_euler_7_i101".to_owned();
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
