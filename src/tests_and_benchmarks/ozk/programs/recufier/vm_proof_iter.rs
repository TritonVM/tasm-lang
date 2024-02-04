use crate::tests_and_benchmarks::ozk::programs::recufier::host_machine_vm_proof_iter::VmProofIter;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::proof_stream::ProofStream;
use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;

fn main() {
    let mut sponge_state: Tip5State = Tip5::init();
    let vm_proof_iter_stack: VmProofIter = VmProofIter {
        current_item_pointer: BFieldElement::new(2),
    };
    let vm_proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(vm_proof_iter_stack);
    let a_merkle_root: Box<Digest> = vm_proof_iter.next_as_merkleroot(&mut sponge_state);

    tasm::tasm_io_write_to_stdout___digest(*a_merkle_root);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use itertools::Itertools;
    use tasm_lib::twenty_first::shared_math::b_field_element::BFieldElement;

    #[test]
    fn test_next_as_merkle_root() {
        let entrypoint_location = EntrypointLocation::disk("recufier", "vm_proof_iter", "main");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let non_determinism = non_determinism();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(Vec::default(), non_determinism.clone());
        let vm_output = test_case
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.output);
    }

    fn proof() -> Proof {
        let digest: Digest = Digest::new(
            (100u64..=104)
                .map(BFieldElement::new)
                .collect_vec()
                .try_into()
                .unwrap(),
        );

        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(ProofItem::MerkleRoot(digest));
        proof_stream.into()
    }

    fn non_determinism() -> NonDeterminism<BFieldElement> {
        let Proof(raw_proof) = proof();
        let ram = raw_proof
            .into_iter()
            .enumerate()
            .map(|(k, v)| (BFieldElement::new(k as u64), v))
            .collect();

        NonDeterminism::default().with_ram(ram)
    }
}
