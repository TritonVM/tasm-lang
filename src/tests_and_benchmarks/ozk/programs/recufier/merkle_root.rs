#![allow(clippy::explicit_auto_deref)]
// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::{BFieldElement, Digest};
use twenty_first::{
    shared_math::bfield_codec::BFieldCodec, util_types::algebraic_hasher::AlgebraicHasher,
};
type H = twenty_first::shared_math::tip5::Tip5;

fn main() {
    fn merkle_root(leafs: &Vec<Digest>, start: usize, stop: usize) -> Digest {
        let result: Digest = if stop == start + 1usize {
            leafs[start]
        } else {
            let half: usize = (stop - start) / 2;
            let left: Digest = merkle_root(leafs, start, stop - half);
            let right: Digest = merkle_root(leafs, start + half, stop);
            H::hash_pair(left, right)
        };

        return result;
    }

    let elements: Box<Vec<Digest>> =
        Vec::<Digest>::decode(&tasm::load_from_memory(BFieldElement::new(2000))).unwrap();
    let length: usize = elements.len();

    let root: Digest = merkle_root(&(*elements), 0usize, length);
    tasm::tasm_io_write_to_stdout___digest(root);

    return;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ast_types;
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use twenty_first::shared_math::other::random_elements;
    use twenty_first::util_types::merkle_tree::{CpuParallel, MerkleTree};
    use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    #[test]
    fn merkle_root_test() {
        // Test function on host machine
        for input_length in [1, 2, 4, 8, 16, 64] {
            let stdin = vec![];
            let digest_list = random_elements(input_length);
            let non_determinism = init_memory_from(&digest_list, 2000u64.into());
            let merkle_tree: MerkleTree<H> = CpuParallel::from_digests(&digest_list);
            let expected_output = merkle_tree.get_root().values().to_vec();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
            assert_eq!(native_output, expected_output);

            // Test function in Triton VM
            let (rust_ast, _, _) =
                ozk_parsing::parse_function_and_structs("recufier", "merkle_root", "main");
            let expected_stack_diff = 0;
            let (code, _fn_name) = compile_for_run_test(&rust_ast, ast_types::ListType::Unsafe);
            let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
                &code,
                vec![],
                &mut HashMap::default(),
                stdin,
                non_determinism,
                expected_stack_diff,
            )
            .unwrap();
            assert_eq!(expected_output, vm_output.output);
        }
    }
}

mod benches {
    use super::*;
    use crate::tests_and_benchmarks::{
        benchmarks::{execute_and_write_benchmark, profile, BenchmarkInput},
        ozk::ozk_parsing,
        test_helpers::shared_test::*,
    };
    use twenty_first::shared_math::other::random_elements;

    #[test]
    fn merkle_root_bench() {
        fn get_input(length: usize) -> BenchmarkInput {
            let digest_list: Vec<Digest> = random_elements(length);
            let non_determinism = init_memory_from(&digest_list, 2000u64.into());

            BenchmarkInput {
                non_determinism,
                ..Default::default()
            }
        }

        let code = ozk_parsing::compile_for_test(
            "recufier",
            "merkle_root",
            "main",
            crate::ast_types::ListType::Unsafe,
        );

        let common_case_input = get_input(16);
        let worst_case_input = get_input(256);

        let name = "recufier_merkle_root".to_owned();
        execute_and_write_benchmark(
            name.clone(),
            code.clone(),
            common_case_input.clone(),
            worst_case_input,
            0,
        );
        profile(name, code, common_case_input);
    }
}