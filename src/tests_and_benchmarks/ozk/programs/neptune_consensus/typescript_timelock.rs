use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::tests_and_benchmarks::ozk::rust_shadows::load_from_memory;
use crate::tests_and_benchmarks::test_helpers::from_neptune_core::{
    get_swbf_indices, Coin, MsMembershipProof, TransactionKernel, Utxo, NUM_TRIALS,
};

fn main() {
    // divine in the current program's hash digest and assert that it is correct
    let self_digest: Digest = tasm::tasm_io_read_secin___digest();
    tasm::assert_own_program_digest(self_digest);

    // read standard input: the transaction kernel mast hash
    let tx_kernel_digest: Digest = tasm::tasm_io_read_stdin___digest();

    // divine the timestamp and authenticate it against the kernel mast hash
    let leaf_index: u32 = 5;
    let timestamp: BFieldElement = tasm::tasm_io_read_secin___bfe();
    let leaf: Digest = Tip5::hash_varlen(&timestamp.encode());
    let tree_height: u32 = 3;
    tasm::tasm_hashing_merkle_verify(tx_kernel_digest, leaf_index, leaf, tree_height);

    // get pointers to objects living in nondeterministic memory:
    //  - list of input UTXOs
    //  - list of input UTXOs' membership proofs in the mutator set
    //  - transaction kernel
    let input_utxos: Vec<Utxo> =
        *Vec::<Utxo>::decode(&load_from_memory(tasm::tasm_io_read_secin___bfe())).unwrap();
    let input_mps: Vec<MsMembershipProof> =
        *Vec::<MsMembershipProof>::decode(&load_from_memory(tasm::tasm_io_read_secin___bfe()))
            .unwrap();
    let transaction_kernel: TransactionKernel =
        *TransactionKernel::decode(&load_from_memory(tasm::tasm_io_read_secin___bfe())).unwrap();

    // authenticate kernel
    let transaction_kernel_hash = Tip5::hash(&transaction_kernel);
    assert_eq!(transaction_kernel_hash, tx_kernel_digest);

    // compute the inputs (removal records' absolute index sets)
    let mut inputs_derived: Vec<Digest> = Vec::with_capacity(input_utxos.len());
    let mut i: usize = 0;
    while i < input_utxos.len() {
        let aocl_leaf_index: u64 = input_mps[i].auth_path_aocl.leaf_index;
        let receiver_preimage: Digest = input_mps[i].receiver_preimage;
        let sender_randomness: Digest = input_mps[i].sender_randomness;
        let item: Digest = Tip5::hash(&input_utxos[i]);
        let index_set: [u128; NUM_TRIALS as usize] =
            get_swbf_indices(item, sender_randomness, receiver_preimage, aocl_leaf_index);
        inputs_derived.push(Tip5::hash(&index_set));
        i += 1;
    }

    // read inputs (absolute index sets) from kernel
    let mut inputs_kernel: Vec<Digest> = Vec::with_capacity(transaction_kernel.inputs.len());
    let mut i = 0;
    while i < transaction_kernel.inputs.len() {
        let index_set = transaction_kernel.inputs[i].absolute_indices.to_vec();
        inputs_kernel.push(Tip5::hash(&index_set));
        i += 1;
    }

    // authenticate inputs
    tasm::tasm_list_unsafeimplu32_multiset_equality(inputs_derived, inputs_kernel);

    // iterate over inputs
    let mut i: usize = 0;
    while i < input_utxos.len() {
        // get coins
        let coins: &Vec<Coin> = &input_utxos[i].coins;

        // if this typescript is present
        let mut j: usize = 0;
        while j < coins.len() {
            let coin: &Coin = &coins[j];
            if coin.type_script_hash == self_digest {
                // extract state
                let state: &Vec<BFieldElement> = &coin.state;

                // assert format
                assert!(state.len() == 1);

                // extract timestamp
                let release_date: BFieldElement = state[0];

                // test time lock
                assert!(release_date.value() < timestamp.value());
            }
            j += 1;
        }
        i += 1;
    }

    return;
}

#[cfg(test)]
mod test {
    use std::time::SystemTime;
    use std::time::UNIX_EPOCH;

    use itertools::Itertools;
    use rand::thread_rng;
    use rand::Rng;
    use tasm_lib::twenty_first::shared_math::other::random_elements;
    use tasm_lib::twenty_first::util_types::merkle_tree::CpuParallel;
    use tasm_lib::twenty_first::util_types::merkle_tree::MerkleTree;
    use tasm_lib::twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::from_neptune_core::pseudorandom_transaction_kernel;
    use crate::tests_and_benchmarks::test_helpers::from_neptune_core::pseudorandom_utxo;
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
    fn output_code() {
        let entrypoint =
            EntrypointLocation::disk("neptune_consensus", "typescript_timelock", "main");

        let code = TritonVMTestCase::new(entrypoint).compile();
        println!("{}", code.iter().join("\n"));
    }
}
