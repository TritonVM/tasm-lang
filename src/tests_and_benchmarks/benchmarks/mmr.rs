#[cfg(test)]
mod benchmark {
    use std::collections::HashMap;

    use rand::random;
    use tasm_lib::rust_shadowing_helper_functions;
    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use tasm_lib::twenty_first::util_types::mmr::mmr_trait::Mmr;

    use crate::tests_and_benchmarks::benchmarks::execute_and_write_benchmark;
    use crate::tests_and_benchmarks::benchmarks::BenchmarkInput;
    use crate::tests_and_benchmarks::programs;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn verify_mmr_ap_benchmark() {
        fn prepare_benchmark_case(log2_size: u32) -> BenchmarkInput {
            let leaf_count_after_add = 1u64 << log2_size;
            let peaks: Vec<Digest> = random_elements(log2_size as usize);
            let mut mmra = MmrAccumulator::init(peaks, leaf_count_after_add - 1);

            let own_leaf: Digest = random();
            let mp = mmra.append(own_leaf);
            let auth_path = mp.authentication_path;

            let mut memory = HashMap::default();
            let peaks_pointer: BFieldElement = 10000u64.into();
            let peaks = mmra.peaks();
            rust_shadowing_helper_functions::list::list_insert(peaks_pointer, peaks, &mut memory);

            let leaf_index = leaf_count_after_add - 1;
            let ap_pointer: BFieldElement = 20000u64.into();
            rust_shadowing_helper_functions::list::list_insert(ap_pointer, auth_path, &mut memory);

            let good_inputs = vec![
                bfe_lit(peaks_pointer),
                u64_lit(mmra.num_leafs()),
                bfe_lit(ap_pointer),
                u64_lit(leaf_index),
                digest_lit(own_leaf),
            ];

            BenchmarkInput {
                input_args: good_inputs,
                memory,
                std_in: vec![],
                non_determinism: NonDeterminism::new(vec![]),
            }
        }

        let rast = programs::mmr::verify_authentication_path_with_local_function();
        let (code, fn_name) = compile_for_run_test(&rast);

        let common_case = prepare_benchmark_case(31);
        let worst_case = prepare_benchmark_case(50);

        execute_and_write_benchmark(fn_name, code, common_case, worst_case)
    }
}
