use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let index_of_prime_to_find: u32 = tasm::tasmlib_io_read_stdin___u32();
    let log2_of_desired_index: u32 = index_of_prime_to_find.ilog2();
    let sieve_size: u32 = index_of_prime_to_find * log2_of_desired_index;
    let mut primes: Vec<bool> = Vec::<bool>::default();

    // 0 and 1 are not primes
    primes.push(false);
    primes.push(false);

    // Initialize all cells to `true`
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

    tasm::tasmlib_io_write_to_stdout___u32(last_prime_found);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use tasm_lib::triton_vm::prelude::*;

    #[test]
    fn pe7_test() {
        let timer = std::time::Instant::now();
        let index_of_prime = 101;
        let stdin = vec![bfe!(index_of_prime)];
        let non_determinism = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism);
        let computed_element = native_output[0];
        let time_passed = timer.elapsed();
        println!("native_output for prime number {index_of_prime} (took {time_passed:?}): {computed_element}");

        let entrypoint = EntrypointLocation::disk("project_euler", "pe7", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.public_output);

        println!(
            "vm_output.public_output for prime number {index_of_prime}: {}",
            vm_output.public_output.iter().skip(1).join("\n")
        );
    }
}

mod benches {
    use crate::tests_and_benchmarks::benchmarks::execute_and_write_benchmark;
    use crate::tests_and_benchmarks::benchmarks::profile;
    use crate::tests_and_benchmarks::benchmarks::BenchmarkInput;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::twenty_first::prelude::*;

    #[test]
    fn pe7_bench() {
        let entrypoint_location = EntrypointLocation::disk("project_euler", "pe7", "main");
        let parsed = entrypoint_location.extract_entrypoint();
        let (code, _) = compile_for_run_test(&parsed);

        let common_case = BenchmarkInput {
            std_in: vec![bfe!(101)],
            ..Default::default()
        };
        let worst_case = BenchmarkInput {
            std_in: vec![bfe!(1001)],
            ..Default::default()
        };

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
