use num::One;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::proof_item::ProofItemVariant;
use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;

use crate::tests_and_benchmarks::ozk::bfield_codec::decode_from_memory_using_size;
use crate::triton_vm::fri::AuthenticationStructure;
use crate::triton_vm::proof_item::FriResponse;

// This struct should only be seen be `rustc`, not by `tasm-lang`
pub(super) struct VmProofIter {
    pub current_item_pointer: BFieldElement,
}

macro_rules! vm_proof_iter_impl {
    ($($variant:ident($payload:ty) defines $next_as_fn:ident uses $try_into_fn:ident,)+) => {
        impl VmProofIter {
            $(
            pub(super) fn $next_as_fn(&mut self, sponge_state: &mut Tip5State) -> Box<$payload> {
                dbg!("entering", self.current_item_pointer.value());
                let read_size_from_ram = || {
                    let bfe = decode_from_memory_using_size::<BFieldElement>(
                        self.current_item_pointer,
                        1,
                    );
                    bfe.value() as usize
                };
                let item_size = ProofItemVariant::$variant
                    .payload_static_length()
                    .unwrap_or_else(read_size_from_ram);

                dbg!("before discriminant", self.current_item_pointer.value());
                let discriminant_pointer = self.current_item_pointer + BFieldElement::one();
                let proof_item =
                    decode_from_memory_using_size::<ProofItem>(discriminant_pointer, item_size);

                self.current_item_pointer += BFieldElement::new(item_size as u64 + 1);

                // todo debug block VVVVVV
                dbg!("entering debug block");
                let next_discriminant_pointer = self.current_item_pointer + BFieldElement::one();
                let next_discriminant = decode_from_memory_using_size::<BFieldElement>(next_discriminant_pointer, 1);
                dbg!(next_discriminant.value());
                // todo debug block ^^^^^^

                if ProofItemVariant::$variant.include_in_fiat_shamir_heuristic() {
                    Tip5::pad_and_absorb_all(sponge_state, &proof_item.encode());
                }

                let payload = proof_item.$try_into_fn().unwrap();
                Box::new(payload)
            }
        )+
        }
    };
}

vm_proof_iter_impl!(
    MerkleRoot(Digest) defines next_as_merkleroot
        uses try_into_merkle_root,
    OutOfDomainBaseRow(Vec<XFieldElement>) defines next_as_outofdomainbaserow
        uses try_into_out_of_domain_base_row,
    OutOfDomainExtRow(Vec<XFieldElement>) defines next_as_outofdomainextrow
        uses try_into_out_of_domain_ext_row,
    OutOfDomainQuotientSegments([XFieldElement; 4]) defines next_as_outofdomainquotientsegments
        uses try_into_out_of_domain_quot_segments,
    AuthenticationStructure(AuthenticationStructure) defines next_as_authenticationstructure
        uses try_into_authentication_structure,
    MasterBaseTableRows(Vec<Vec<BFieldElement>>) defines next_as_masterbasetablerows
        uses try_into_master_base_table_rows,
    MasterExtTableRows(Vec<Vec<XFieldElement>>) defines next_as_masterexttablerows
        uses try_into_master_ext_table_rows,
    Log2PaddedHeight(u32) defines next_as_log2paddedheight
        uses try_into_log2_padded_height,
    QuotientSegmentsElements(Vec<[XFieldElement; 4]>) defines next_as_quotientsegmentselements
        uses try_into_quot_segments_elements,
    FriCodeword(Vec<XFieldElement>) defines next_as_fricodeword
        uses try_into_fri_codeword,
    FriResponse(FriResponse) defines next_as_friresponse
        uses try_into_fri_response,
);
