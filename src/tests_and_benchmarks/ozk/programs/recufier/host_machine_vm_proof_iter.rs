use crate::libraries::vm_proof_iter;
use crate::tests_and_benchmarks::ozk::bfield_codec;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::triton_vm::proof_stream::ProofStream;
use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;
use tasm_lib::DIGEST_LENGTH;

// This struct should only be seen be `rustc`, not
// by `tasm-lang`
pub(super) struct VmProofIter {
    pub current_item_pointer: BFieldElement,
}

impl VmProofIter {
    pub(super) fn next_as_merkleroot(&self, sponge_state: &mut Tip5State) -> Box<Digest> {
        let expected_discriminant =
            BFieldElement::new(ProofItemVariant::MerkleRoot.bfield_codec_discriminant() as u64);
        let discriminant_pointer = self.current_item_pointer + BFieldElement::one();
        let actual_discriminant =
            bfield_codec::decode_from_memory_using_size::<BFieldElement>(discriminant_pointer, 1);
        assert_eq!(expected_discriminant, *actual_discriminant);
        let merkle_root = bfield_codec::decode_from_memory_using_size::<Digest>(
            discriminant_pointer + BFieldElement::one(),
            DIGEST_LENGTH,
        );

        merkle_root
    }
}
