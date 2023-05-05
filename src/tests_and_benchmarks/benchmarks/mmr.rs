#[cfg(test)]
mod benchmark {
    use std::collections::HashMap;

    use rand::random;
    use tasm_lib::{rust_shadowing_helper_functions, Digest};
    use triton_vm::BFieldElement;
    use twenty_first::{
        shared_math::{other::random_elements, tip5::Tip5},
        test_shared::mmr::get_rustyleveldb_ammr_from_digests,
        util_types::mmr::{mmr_membership_proof::MmrMembershipProof, mmr_trait::Mmr},
    };

    use crate::tests_and_benchmarks::{
        benchmarks::{execute_and_write_benchmark, BenchmarkInput},
        programs,
        shared_test::*,
    };

    #[test]
    fn verify_benchmark() {
        type H = Tip5;
        let rast = programs::mmr::verify_authentication_path_with_local_function();
        let (code, fn_name) = compile_for_run_test(&rast);
        let digests: Vec<Digest> = random_elements(10);
        let mut ammr = get_rustyleveldb_ammr_from_digests(digests.clone());
        let leaf_index = random::<u64>() % 10;

        let mut memory = HashMap::default();
        let peaks_pointer: BFieldElement = 10000u64.into();
        let peaks = ammr.get_peaks();
        let capacity = 2000;
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            peaks_pointer,
            capacity,
            peaks,
            &mut memory,
        );
        let ap_pointer: BFieldElement = 20000u64.into();
        let mp: MmrMembershipProof<H> = ammr.prove_membership(leaf_index).0;
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            ap_pointer,
            capacity,
            mp.authentication_path,
            &mut memory,
        );

        let own_leaf = digests[leaf_index as usize];
        let good_inputs = vec![
            bfe_lit(peaks_pointer),
            u64_lit(ammr.count_leaves()),
            bfe_lit(ap_pointer),
            u64_lit(leaf_index),
            digest_lit(own_leaf),
        ];
        let common_case = BenchmarkInput {
            input_args: good_inputs.clone(),
            memory: memory.clone(),
            std_in: vec![],
            secret_in: vec![],
        };
        let worst_case = BenchmarkInput {
            input_args: good_inputs,
            memory,
            std_in: vec![],
            secret_in: vec![],
        };
        execute_and_write_benchmark(fn_name, &code, common_case, worst_case, -10)
    }
}
