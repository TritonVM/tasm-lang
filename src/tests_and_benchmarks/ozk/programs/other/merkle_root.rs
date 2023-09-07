// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::{BFieldElement, Digest};
use twenty_first::{
    shared_math::bfield_codec::BFieldCodec, util_types::algebraic_hasher::AlgebraicHasher,
};
type H = twenty_first::shared_math::tip5::Tip5;

fn main() {
    fn merkle_root(leafs: &Vec<Digest>, start: usize, stop: usize) -> Digest {
        #[allow(unused_assignments)]
        let mut result: Digest = Digest::default();
        if stop == start + 1usize {
            result = leafs[start];
        } else {
            let half: usize = (stop - start) / 2;
            let left: Digest = merkle_root(leafs, start, stop - half);
            let right: Digest = merkle_root(leafs, start + half, stop);
            result = H::hash_pair(&left, &right);
        }

        return result;
    }

    let elements: Box<Vec<Digest>> =
        Vec::<Digest>::decode(&tasm::load_from_memory(BFieldElement::new(2000))).unwrap();

    let root: Digest = merkle_root(&elements, 0usize, 64usize);
    tasm::tasm_io_write_to_stdout_digest(root);

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
    use itertools::Itertools;
    use twenty_first::shared_math::other::random_elements;
    use twenty_first::util_types::merkle_tree::{CpuParallel, MerkleTree};
    use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    #[test]
    fn merkle_root_test() {
        // Test function on host machine
        let stdin = vec![];
        let digest_list = random_elements(64);
        let non_determinism = init_memory_from(&digest_list, 2000u64.into());
        let merkle_tree: MerkleTree<H> = CpuParallel::from_digests(&digest_list);
        let expected_output = merkle_tree.get_root().values().to_vec();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let (rust_ast, _, _) = ozk_parsing::parse_main_and_structs("other", "merkle_root");
        let expected_stack_diff = 0;
        let (code, _fn_name) = compile_for_run_test(&rust_ast, ast_types::ListType::Unsafe);
        println!("code:\n{}", code.iter().join("\n"));
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
