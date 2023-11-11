#![allow(clippy::explicit_auto_deref)]
// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use arbitrary::Arbitrary;
use itertools::Itertools;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::{tip5::Tip5State, x_field_element::XFieldElement};
type _H = twenty_first::shared_math::tip5::Tip5;
type VmHasherState = Tip5State;

#[derive(Debug, Clone, PartialEq, Eq, Hash, BFieldCodec, Arbitrary)]
pub struct FriResponse {
    /// The authentication structure of the Merkle tree.
    pub auth_structure: Vec<Digest>,
    /// The values of the opened leaves of the Merkle tree.
    pub revealed_leaves: Vec<XFieldElement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Arbitrary, BFieldCodec)]
pub enum ProofItem {
    AuthenticationStructure(Vec<Digest>),
    MasterBaseTableRows(Vec<Vec<BFieldElement>>),
    MasterExtTableRows(Vec<Vec<XFieldElement>>),
    OutOfDomainBaseRow(Vec<XFieldElement>),
    OutOfDomainExtRow(Vec<XFieldElement>),
    OutOfDomainQuotientSegments([XFieldElement; 4]),
    MerkleRoot(Digest),
    Log2PaddedHeight(u32),
    QuotientSegmentsElements(Vec<[XFieldElement; 4]>),
    FriCodeword(Vec<XFieldElement>),
    FriResponse(FriResponse),
}

impl ProofItem {
    fn _include_in_fiat_shamir_heuristic(&self) -> bool {
        match self {
            ProofItem::MerkleRoot(_) => {
                return true;
            }
            ProofItem::OutOfDomainBaseRow(_) => {
                return true;
            }
            ProofItem::OutOfDomainExtRow(_) => {
                return true;
            }
            ProofItem::OutOfDomainQuotientSegments(_) => {
                return true;
            }
            // all of the following are implied by a corresponding Merkle root
            _ => {
                return false;
            }
        };
    }

    fn _as_merkle_root(&self) -> Digest {
        let mut ret: Digest = Digest::default();
        match self {
            Self::MerkleRoot(bs) => {
                ret = *bs;
            }
            _ => {
                assert!(false);
            }
        };

        return ret;
    }
}

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
pub struct VmProofStreamCompiled {
    pub word_index: u32,
    pub data: Vec<BFieldElement>,
    #[bfield_codec(ignore)]
    #[tasm_object(ignore)]
    pub sponge_state: VmHasherState,
}

impl PartialEq for VmProofStreamCompiled {
    fn eq(&self, other: &Self) -> bool {
        self.word_index == other.word_index && self.data == other.data
    }
}

impl VmProofStreamCompiled {
    pub fn _dequeue(&mut self) -> Box<ProofItem> {
        // TODO: Problem might be that the word index is set incorrectly here?
        // maybe some pointer should point to the Merkle root but is pointing
        // at the proof stream instead?
        assert!((self.word_index as usize) < self.data.len());
        let size = self.data[self.word_index as usize].value() as usize;
        let sequence =
            &self.data[(self.word_index as usize + 1)..(self.word_index as usize + 1 + size)];
        let item = ProofItem::decode(sequence).unwrap();
        // let item: Box<ProofItem> = ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(
        //     self.word_index as u64,
        // )))
        // .unwrap();
        self.word_index += size as u32 + 1;

        if item._include_in_fiat_shamir_heuristic() {
            // TODO: Perform Fiat-Shamir here!
            // self.fiat_shamir(&item);
        }

        item
    }
}

fn _main() {
    // Assume that the VmProofStream object is found in memory
    let mut proof_stream: Box<VmProofStreamCompiled> =
        VmProofStreamCompiled::decode(&tasm::load_from_memory(BFieldElement::new(1u64))).unwrap();

    // Goal: To be able to write
    let first_item: Box<ProofItem> = proof_stream._dequeue();
    let _first_root: Digest = first_item._as_merkle_root();

    return;
}

mod tests {

    use super::*;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use itertools::Itertools;
    use rand::random;

    impl VmProofStreamCompiled {
        // Is only used for test, does not need to be handled by TVM
        pub fn new(items: &[ProofItem]) -> Self {
            Self {
                word_index: 1,
                data: items.to_vec().encode(),
                sponge_state: VmHasherState::new(
                    twenty_first::util_types::algebraic_hasher::Domain::VariableLength,
                ),
            }
        }
    }

    #[ignore = "Doesn't work yet"]
    #[test]
    fn vm_proof_stream_struct_test() {
        let _stdin: Vec<BFieldElement> = vec![];
        // let digest_list = random_elements(16);
        let first_root: Digest = random();
        let items: Vec<ProofItem> = vec![ProofItem::MerkleRoot(first_root)];
        let vm_proof_stream = VmProofStreamCompiled::new(&items);
        println!(
            "vm_proof_stream.encoded(): {}",
            vm_proof_stream.encode().iter().join(",")
        );
        let decoded = VmProofStreamCompiled::decode(&vm_proof_stream.encode()).unwrap();
        assert_eq!(vm_proof_stream, *decoded);
        let non_determinism = init_memory_from(&vm_proof_stream, 1u64.into());
        println!("non_determinism: {{{}}}", {
            let mut values = non_determinism
                .ram
                .iter()
                .map(|(&x, &y)| (x, y))
                .collect_vec();
            values.sort_unstable_by_key(|x| x.0.value());
            values.iter().map(|(a, v)| format!("{a} => {v}")).join(",")
        });
        // let merkle_tree: MerkleTree<H> = CpuParallel::from_digests(&digest_list);
        // let expected_output = merkle_tree.get_root().values().to_vec();

        // TODO: Add below code when this works
        // let expected_output = vec![];
        // let native_output = rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism);
        // assert_eq!(native_output, expected_output);

        // // TODO: Include below code once `ProofItem` support has been added
        // // Test function in Triton VM
        // let test_program = ozk_parsing::compile_for_test(
        //     "recufier",
        //     "vm_proof_stream",
        //     "main",
        //     crate::ast_types::ListType::Unsafe,
        // );
        // let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
        //     &test_program,
        //     vec![],
        //     &mut HashMap::default(),
        //     Default::default(),
        //     Default::default(),
        //     0,
        // )
        // .unwrap();
    }
}
