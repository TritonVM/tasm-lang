use std::collections::HashMap;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use anyhow::bail;
use itertools::Itertools;
use tasm_lib::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use tasm_lib::{structure::tasm_object::TasmObject, triton_vm::prelude::*};

pub(crate) const NUM_TRIALS: u32 = 45;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec)]
pub(crate) struct AbsoluteIndexSet([u128; NUM_TRIALS as usize]);

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
pub(crate) struct RemovalRecord {
    pub(crate) absolute_indices: AbsoluteIndexSet,
    pub(crate) target_chunks: ChunkDictionary,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, BFieldCodec)]
pub(crate) struct AdditionRecord {
    pub(crate) canonical_commitment: Digest,
}

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
struct TransactionKernel {
    pub(crate) inputs: Vec<RemovalRecord>,

    // `outputs` contains the commitments (addition records) that go into the AOCL
    pub(crate) outputs: Vec<AdditionRecord>,

    pub(crate) pubscript_hashes_and_inputs: Vec<PubScriptHashAndInput>,
    pub(crate) fee: Amount,
    pub(crate) coinbase: Option<Amount>,

    // number of milliseconds since unix epoch
    pub(crate) timestamp: BFieldElement,

    pub(crate) mutator_set_hash: Digest,
}

impl TransactionKernel {
    pub(crate) fn mast_sequences(&self) -> Vec<Vec<BFieldElement>> {
        let input_utxos_sequence = self.inputs.encode();

        let output_utxos_sequence = self.outputs.encode();

        let pubscript_sequence = self.pubscript_hashes_and_inputs.encode();

        let fee_sequence = self.fee.encode();

        let coinbase_sequence = self.coinbase.encode();

        let timestamp_sequence = self.timestamp.encode();

        let mutator_set_hash_sequence = self.mutator_set_hash.encode();

        vec![
            input_utxos_sequence,
            output_utxos_sequence,
            pubscript_sequence,
            fee_sequence,
            coinbase_sequence,
            timestamp_sequence,
            mutator_set_hash_sequence,
        ]
    }

    pub(crate) fn mast_hash(&self) -> Digest {
        // get a sequence of BFieldElements for each field
        let sequences = self.mast_sequences();

        let mut mt_leafs = sequences
            .iter()
            .map(|seq| VmHasher::hash_varlen(seq))
            .collect_vec();

        // pad until power of two
        while mt_leafs.len() & (mt_leafs.len() - 1) != 0 {
            mt_leafs.push(Digest::default());
        }

        // compute Merkle tree and return hash
        <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&mt_leafs)
            .unwrap()
            .root()
    }
}

#[derive(Debug, Clone, BFieldCodec)]
struct MmrMembershipProof {
    pub leaf_index: u64,
    pub authentication_path: Vec<Digest>,
}

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec)]
pub(crate) struct Chunk {
    pub(crate) relative_indices: Vec<u32>,
}

#[derive(Clone, Debug, Default)]
struct ChunkDictionary {
    // {chunk index => (MMR membership proof for the whole chunk to which index belongs, chunk value)}
    pub(crate) dictionary: HashMap<u64, (MmrMembershipProof, Chunk)>,
}

impl BFieldCodec for ChunkDictionary {
    type Error = anyhow::Error;

    fn decode(sequence: &[BFieldElement]) -> anyhow::Result<Box<Self>> {
        if sequence.is_empty() {
            bail!("Cannot decode empty sequence of BFieldElements as ChunkDictionary");
        }
        let num_entries = sequence[0].value() as usize;
        let mut read_index = 1;
        let mut dictionary = HashMap::new();
        for _ in 0..num_entries {
            // read key
            let key_length = 2;
            if sequence.len() < read_index + key_length {
                bail!("Cannot decode sequence of BFieldElements as ChunkDictionary: missing key");
            }
            let key = *u64::decode(&sequence[read_index..read_index + key_length])?;
            read_index += key_length;

            // read membership proof
            if sequence.len() <= read_index {
                bail!("Cannot decode sequence of BFieldElements as ChunkDictionary: missing membership proof");
            }
            let memproof_length = sequence[read_index].value() as usize;
            read_index += 1;
            let membership_proof =
                *MmrMembershipProof::decode(&sequence[read_index..read_index + memproof_length])?;
            read_index += memproof_length;

            // read chunk
            if sequence.len() <= read_index {
                bail!("Cannot decode sequence of BFieldElements as ChunkDictionary: missing chunk");
            }
            let chunk_length = sequence[read_index].value() as usize;
            read_index += 1;
            let chunk = *Chunk::decode(&sequence[read_index..read_index + chunk_length])?;
            read_index += chunk_length;

            dictionary.insert(key, (membership_proof, chunk));
        }

        Ok(Box::new(ChunkDictionary { dictionary }))
    }

    fn encode(&self) -> Vec<BFieldElement> {
        let mut string = vec![BFieldElement::new(self.dictionary.keys().len() as u64)];
        for key in self.dictionary.keys().sorted() {
            string.append(&mut key.encode());
            let mut membership_proof_encoded = self.dictionary[key].0.encode();
            string.push(BFieldElement::new(membership_proof_encoded.len() as u64));
            string.append(&mut membership_proof_encoded);
            let mut chunk_encoded = self.dictionary[key].1.encode();
            string.push(BFieldElement::new(chunk_encoded.len() as u64));
            string.append(&mut chunk_encoded);
        }
        string
    }

    fn static_length() -> Option<usize> {
        None
    }
}

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec, TasmObject)]
struct Utxo {
    pub(crate) lock_script_hash: Digest,
    pub(crate) coins: Vec<Coin>,
}

#[derive(Clone, Debug, PartialEq, Eq, BFieldCodec)]

struct Coin {
    pub(crate) type_script_hash: Digest,
    pub(crate) state: Vec<BFieldElement>,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
struct MsMembershipProof {
    pub(crate) sender_randomness: Digest,
    pub(crate) receiver_preimage: Digest,
    pub(crate) auth_path_aocl: MmrMembershipProof,
    pub(crate) target_chunks: ChunkDictionary,
}

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
    let input_utxos_pointer: u64 = tasm::tasm_io_read_secin___bfe().value();
    let maybe_input_utxos: Vec<Utxo> = *Vec::<Utxo>::decode(&tasm::load_from_memory(
        BFieldElement::new(input_utxos_pointer),
    ))
    .unwrap();
    let input_mps_pointer: BFieldElement = tasm::tasm_io_read_secin___bfe();
    let input_mps: Vec<MsMembershipProof> =
        *Vec::<MsMembershipProof>::decode(&tasm::load_from_memory(input_mps_pointer)).unwrap();
    let transaction_kernel_pointer: BFieldElement = tasm::tasm_io_read_secin___bfe();
    let transaction_kernel: TransactionKernel =
        *TransactionKernel::decode(&tasm::load_from_memory(transaction_kernel_pointer)).unwrap();

    // // authenticate kernel
    // let transaction_kernel_hash = Tip5::hash(&transaction_kernel);
    // assert_eq!(transaction_kernel_hash, tx_kernel_digest);

    // // compute the inputs (removal records' absolute index sets)
    // let mut inputs_derived: Vec<Digest> = Vec::with_capacity(input_utxos.len());
    // let mut i: usize = 0;
    // while i < input_utxos.len() {
    //     let aocl_leaf_index: u64 = input_mps[i].auth_path_aocl.leaf_index;
    //     let receiver_preimage: Digest = input_mps[i].receiver_preimage;
    //     let sender_randomness: Digest = input_mps[i].sender_randomness;
    //     let item: Digest = Tip5::hash(&input_utxos[i]);
    //     let index_set: [u128; NUM_TRIALS as usize] =
    //         get_swbf_indices(item, sender_randomness, receiver_preimage, aocl_leaf_index);
    //     inputs_derived.push(Tip5::hash(&index_set));
    //     i += 1;
    // }

    // // read inputs (absolute index sets) from kernel
    // let mut inputs_kernel: Vec<Digest> = Vec::with_capacity(transaction_kernel.inputs.len());
    // let mut i = 0;
    // while i < transaction_kernel.inputs.len() {
    //     let index_set = transaction_kernel.inputs[i].absolute_indices.to_vec();
    //     inputs_kernel.push(Tip5::hash(&index_set));
    //     i += 1;
    // }

    // // authenticate inputs
    // tasm::tasm_list_unsafeimplu32_multiset_equality(inputs_derived, inputs_kernel);

    // // iterate over inputs
    // let mut i: usize = 0;
    // while i < input_utxos.len() {
    //     // get coins
    //     let coins: &Vec<Coin> = &input_utxos[i].coins;

    //     // if this typescript is present
    //     let mut j: usize = 0;
    //     while j < coins.len() {
    //         let coin: &Coin = &coins[j];
    //         if coin.type_script_hash == self_digest {
    //             // extract state
    //             let state: &Vec<BFieldElement> = &coin.state;

    //             // assert format
    //             assert!(state.len() == 1);

    //             // extract timestamp
    //             let release_date: BFieldElement = state[0];

    //             // test time lock
    //             assert!(release_date.value() < timestamp.value());
    //         }
    //         j += 1;
    //     }
    //     i += 1;
    // }

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
