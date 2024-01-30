use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

use super::vm_proof_stream::*;

pub fn recufy() {
    tasm::tasm_recufier_assert_stdin_starts_with_own_program_digest();
    let inner_proof_iter: VmProofIter = VmProofIter::new();
    let proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(inner_proof_iter);
    let log_2_padded_height: u32 = proof_iter.next_as_log_2_padded_height();
    tasm::tasm_io_write_to_stdout___u32(log_2_padded_height);
    return;
}

#[cfg(test)]
mod tests {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use crate::triton_vm::prelude::*;
    use crate::triton_vm::proof_item::ProofItem;
    use crate::triton_vm::proof_stream::ProofStream;

    use super::*;

    fn recufier_std_in(test_case: &TritonVMTestCase) -> Vec<BFieldElement> {
        let code = test_case.compile();
        let program = Program::new(&code);
        let program_digest = program.hash::<Tip5>();

        program_digest.reversed().values().to_vec()
    }

    fn proof() -> Proof {
        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(ProofItem::Log2PaddedHeight(42));
        proof_stream.into()
    }

    fn recufier_non_determinism() -> NonDeterminism<BFieldElement> {
        let ram = proof()
            .0
            .into_iter()
            .enumerate()
            .map(|(k, v)| (BFieldElement::new(k as u64), v))
            .collect();

        NonDeterminism::default().with_ram(ram)
    }

    #[test]
    fn recursive_verification() {
        let entrypoint_location = EntrypointLocation::disk("recufier", "verify", "recufy");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let std_in = recufier_std_in(&test_case);
        let non_determinism = recufier_non_determinism();

        let native_output =
            rust_shadows::wrap_main_with_io(&recufy)(std_in.clone(), non_determinism.clone());

        let vm_output = test_case
            .with_std_in(std_in)
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        assert_eq!(native_output, vm_output.output);
    }
}
