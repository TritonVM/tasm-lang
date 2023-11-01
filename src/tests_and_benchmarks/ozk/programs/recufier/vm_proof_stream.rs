#![allow(clippy::explicit_auto_deref)]
// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::{proof_item::ProofItem, BFieldElement, Digest};
use twenty_first::shared_math::{bfield_codec::BFieldCodec, tip5::Tip5State};
type _H = twenty_first::shared_math::tip5::Tip5;
type VmHasherState = Tip5State;

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
pub struct VmProofStreamCompiled {
    pub word_index: u32,
    pub data: Vec<BFieldElement>,
    #[bfield_codec(ignore)]
    #[tasm_object(ignore)]
    pub sponge_state: VmHasherState,
}

impl VmProofStreamCompiled {
    fn dequeue(&mut self) -> Box<ProofItem> {
        return tasm::tasm_recufier_proof_stream_dequeue(self);
        // todo!()
    }
    // fn dequeue(&mut self) -> (BFieldElement, BFieldElement) {
    //     // tasm_lib::recufier::proof_stream::dequeue::
    //     tasm::tasm_recufier_proof_stream_dequeue(self);
    //     // if self.word_index as usize >= self.data.len() {
    //     //     bail!("No more words left in stream.")
    //     // }
    //     // let size = self.data[self.word_index as usize].value() as usize;
    //     // let sequence =
    //     //     &self.data[(self.word_index as usize + 1)..(self.word_index as usize + 1 + size)];
    //     // self.word_index += size as u32 + 1;
    //     // let item = *ProofItem::decode(sequence)?;

    //     // if item.include_in_fiat_shamir_heuristic() {
    //     //     self.fiat_shamir(&item);
    //     // }

    //     // Ok(Box::new(item))
    //     todo!()
    // }
}

fn main() {
    // Assume that the VmProofStream object is found in memory
    let mut proof_stream: Box<VmProofStreamCompiled> =
        VmProofStreamCompiled::decode(&tasm::load_from_memory(BFieldElement::new(84u64))).unwrap();

    // Goal: To be able to write
    let _first_item: Box<ProofItem> = proof_stream.dequeue();
    // let first_root: Digest = first_item.as_merkle_root().unwrap();

    return;
}

mod tests {

    use super::*;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use itertools::Itertools;
    use rand::random;

    impl VmProofStreamCompiled {
        pub fn new(items: &[triton_vm::proof_item::ProofItem]) -> Self {
            Self {
                word_index: 1,
                data: items.to_vec().encode(),
                sponge_state: VmHasherState::new(
                    twenty_first::util_types::algebraic_hasher::Domain::VariableLength,
                ),
            }
        }
    }

    #[test]
    fn vm_proof_stream_struct_test() {
        let stdin = vec![];
        // let digest_list = random_elements(16);
        let first_root: Digest = random();
        let items: Vec<ProofItem> = vec![ProofItem::MerkleRoot(first_root)];
        let vm_proof_stream = VmProofStreamCompiled::new(&items);
        let non_determinism = init_memory_from(&vm_proof_stream, 84u64.into());
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
        let expected_output = vec![];
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism);
        assert_eq!(native_output, expected_output);

        // TODO: Include below code once `ProofItem` support has been added
        // Test function in Triton VM
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
