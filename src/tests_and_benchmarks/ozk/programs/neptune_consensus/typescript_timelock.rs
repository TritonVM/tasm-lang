use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
struct Coin {
    pub type_script_hash: Digest,
    pub state: Vec<BFieldElement>,
}

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
struct Utxo {
    pub lock_script_hash: Digest,
    pub coins: Vec<Coin>,
}

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
struct SaltedUtxos {
    pub utxos: Vec<Utxo>,
    pub salt: [BFieldElement; 3],
}

fn main() {
    // get in the current program's hash digest
    let self_digest: Digest = tasm::tasmlib_verifier_own_program_digest();

    // read standard input:
    //  - transaction kernel mast hash
    //  - input salted utxos digest
    //  - output salted utxos digest
    // (All type scripts take this triple as input.)
    let tx_kernel_digest: Digest = tasm::tasmlib_io_read_stdin___digest();
    let input_utxos_digest: Digest = tasm::tasmlib_io_read_stdin___digest();
    let _output_utxos_digest: Digest = tasm::tasmlib_io_read_stdin___digest();

    // divine the timestamp and authenticate it against the kernel mast hash
    let leaf_index: u32 = 5;
    let timestamp: BFieldElement = tasm::tasmlib_io_read_secin___bfe();
    let leaf: Digest = Tip5::hash(&timestamp);
    let tree_height: u32 = 3;
    tasm::tasmlib_hashing_merkle_verify(tx_kernel_digest, tree_height, leaf_index, leaf);

    // get pointers to objects living in nondeterministic memory:
    //  - input Salted UTXOs
    let input_utxos_pointer: BFieldElement = tasm::tasmlib_io_read_secin___bfe();

    // it's important to read the outputs digest too, but we actually don't care about
    // the output UTXOs (in this type script)
    let _output_utxos_pointer: u64 = tasm::tasmlib_io_read_secin___bfe().value();

    // authenticate salted input UTXOs against the digest that was read from stdin
    let input_salted_utxos: Box<SaltedUtxos> =
        SaltedUtxos::decode(&tasm::load_from_memory(input_utxos_pointer)).unwrap();
    let input_salted_utxos_digest: Digest = Tip5::hash(&input_salted_utxos);
    assert!(input_salted_utxos_digest == input_utxos_digest);

    // iterate over inputs
    let input_utxos: &Vec<Utxo> = &input_salted_utxos.utxos;
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
    use std::collections::HashMap;
    use std::time::SystemTime;
    use std::time::UNIX_EPOCH;

    use itertools::Itertools;
    use tasm_lib::memory::encode_to_memory;
    use tasm_lib::twenty_first::math::other::random_elements;
    use tasm_lib::twenty_first::util_types::merkle_tree::CpuParallel;
    use tasm_lib::twenty_first::util_types::merkle_tree::MerkleTree;
    use twenty_first::prelude::MerkleTreeMaker;

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
    ) -> (MerkleTree, Vec<Digest>) {
        let mut eight_digests = random_elements(8);
        eight_digests[leaf_index] = leaf;
        let mt = CpuParallel::from_digests(&eight_digests).unwrap();
        let ap = mt.authentication_structure(&[leaf_index]).unwrap();

        (mt, ap)
    }

    fn simple_salted_utxos(release_date: BFieldElement, program: &Program) -> SaltedUtxos {
        let coin = Coin {
            type_script_hash: program.hash(),
            state: vec![release_date],
        };
        let utxos = vec![Utxo {
            lock_script_hash: Digest::default(),
            coins: vec![coin],
        }];

        SaltedUtxos {
            utxos,
            salt: random_elements(3).try_into().unwrap(),
        }
    }

    fn prepare_nd_and_stdin(
        tx_timestamp: BFieldElement,
        salted_utxos: &SaltedUtxos,
    ) -> (NonDeterminism, Vec<BFieldElement>) {
        let input_utxos_pointer = bfe!(52);
        let output_utxos_pointer = bfe!(100052);

        let timestamp_encoded = tx_timestamp.encode();
        let input_utxos_pointer_encoded = input_utxos_pointer.encode();
        let output_utxos_pointer_encoded = output_utxos_pointer.encode();
        let individual_tokens = [
            timestamp_encoded.clone(),
            input_utxos_pointer_encoded,
            output_utxos_pointer_encoded,
        ]
        .concat();

        let leaf = Tip5::hash_varlen(&timestamp_encoded);
        let (mt, ap) = tx_kernel_merkle_tree_with_specific_leaf(leaf, 5);

        let mut ram = HashMap::default();
        encode_to_memory(&mut ram, input_utxos_pointer, salted_utxos);

        let stdin = [
            mt.root().reversed().values().to_vec(),
            Tip5::hash(salted_utxos).reversed().values().to_vec(),
            Digest::default().values().to_vec(),
        ]
        .concat();

        let nd = NonDeterminism::new(individual_tokens)
            .with_digests(ap)
            .with_ram(ram);

        (nd, stdin)
    }

    #[test]
    fn typescript_timelock_test_pass() {
        let entrypoint =
            EntrypointLocation::disk("neptune_consensus", "typescript_timelock", "main");
        let code = TritonVMTestCase::new(entrypoint.clone()).compile();
        let program = Program::new(&code);
        let salted_utxos = simple_salted_utxos(bfe!(1), &program);
        let current_time = BFieldElement::new(current_time());
        let (nd, stdin) = prepare_nd_and_stdin(current_time, &salted_utxos);

        let native_output = rust_shadows::wrap_main_with_io_and_program_digest(&main)(
            stdin.clone(),
            nd.clone(),
            program,
        );

        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_non_determinism(nd)
            .with_std_in(stdin)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.public_output);

        println!("{}", code.iter().join("\n"));
    }

    #[test]
    fn typescript_timelock_test_future_release_date_fail() {
        let entrypoint =
            EntrypointLocation::disk("neptune_consensus", "typescript_timelock", "main");
        let code = TritonVMTestCase::new(entrypoint.clone()).compile();
        let program = Program::new(&code);
        let release_time = bfe!(current_time()) + bfe!(100);
        let salted_utxos = simple_salted_utxos(release_time, &program);
        let current_time = BFieldElement::new(current_time());
        let (nd, stdin) = prepare_nd_and_stdin(current_time, &salted_utxos);

        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_non_determinism(nd)
            .with_std_in(stdin)
            .execute();
        assert!(vm_output.is_err());
    }
}
