#![allow(clippy::explicit_auto_deref)]

use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::twenty_first::shared_math::tip5::Tip5State;
use triton_vm::BFieldElement;
use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

use super::proof_item::*;

type VmHasherState = Tip5State;

#[derive(Debug, Clone, BFieldCodec, TasmObject)]
pub struct VmProofStream {
    pub word_index: u32,
    pub(crate) data: Vec<BFieldElement>,
    #[bfield_codec(ignore)]
    #[tasm_object(ignore)]
    pub sponge_state: VmHasherState,
}

fn main() {
    // Assume that the VmProofStream object is found in memory
    let _proof_stream: Box<VmProofStream> =
        VmProofStream::decode(&tasm::load_from_memory(BFieldElement::new(0u64))).unwrap();

    // let proof_item: Box<ProofItem> = tasm::tasm_recufier_proof_stream_dequeue(&mut proof_stream);

    // Goal: To be able to write
    // let first_item: Box<ProofItem> = proof_stream._dequeue();
    // let _first_root: Digest = first_item.as_merkle_root();

    return;
}

#[cfg(test)]
mod test {

    use num::Zero;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::compile_for_test;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    impl VmProofStream {
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

    #[test]
    fn vm_proof_stream_struct_test() {
        let stdin: Vec<BFieldElement> = vec![];
        let first_root: Digest = random();
        let items: Vec<ProofItem> = vec![ProofItem::MerkleRoot(first_root)];
        let vm_proof_stream = VmProofStream::new(&items);
        let non_determinism = init_memory_from(&vm_proof_stream, BFieldElement::zero());
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        // let merkle_tree: MerkleTree<H> = CpuParallel::from_digests(&digest_list);
        // let expected_output = merkle_tree.get_root().values().to_vec();

        // Test function in Triton VM
        let test_program = compile_for_test(
            "recufier",
            "vm_proof_stream",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            Default::default(),
            Default::default(),
            0,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
