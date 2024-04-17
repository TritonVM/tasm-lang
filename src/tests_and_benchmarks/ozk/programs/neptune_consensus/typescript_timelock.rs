use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let unlock_date: u64 = 1707475556087u64;

    let tx_kernel_digest: Digest = tasm::tasmlib_io_read_stdin___digest();

    let timestamp: BFieldElement = tasm::tasmlib_io_read_secin___bfe();

    let leaf_index: u32 = 5;
    let leaf: Digest = Tip5::hash_varlen(&timestamp.encode());
    let tree_height: u32 = 3;
    tasm::tasmlib_hashing_merkle_verify(tx_kernel_digest, leaf_index, leaf, tree_height);
    assert!(unlock_date < timestamp.value());

    return;
}

#[cfg(test)]
mod test {
    use std::time::SystemTime;
    use std::time::UNIX_EPOCH;

    use itertools::Itertools;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::util_types::merkle_tree::CpuParallel;
    use tasm_lib::twenty_first::util_types::merkle_tree::MerkleTree;
    use tasm_lib::twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    fn current_time() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64
    }

    fn tx_kernel_merkle_tree_with_specific_leaf(
        leaf: Digest,
        leaf_index: usize,
    ) -> (MerkleTree<Tip5>, Vec<Digest>) {
        let mut eight_digests = random_elements(8);
        eight_digests[leaf_index] = leaf;
        let mt = CpuParallel::from_digests(&eight_digests).unwrap();
        let ap = mt.authentication_structure(&[leaf_index]).unwrap();

        (mt, ap)
    }

    #[test]
    fn typescript_timelock_test() {
        let timestamp_encoded = BFieldElement::new(current_time()).encode();
        let leaf = Tip5::hash_varlen(&timestamp_encoded);
        let (mt, ap) = tx_kernel_merkle_tree_with_specific_leaf(leaf, 5);

        let std_in = mt.root().reversed().values().to_vec();
        let non_determinism = NonDeterminism::new(timestamp_encoded).with_digests(ap);

        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), non_determinism.clone());

        let entrypoint =
            EntrypointLocation::disk("neptune_consensus", "typescript_timelock", "main");
        let vm_output = TritonVMTestCase::new(entrypoint.clone())
            .with_non_determinism(non_determinism)
            .with_std_in(std_in)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.public_output);

        let code = TritonVMTestCase::new(entrypoint).compile();
        println!("{}", code.iter().join("\n"));
    }
}
