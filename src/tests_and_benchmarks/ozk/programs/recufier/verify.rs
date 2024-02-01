use num::One;
use num::Zero;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::triton_vm::prelude::tip5::Tip5State;
use crate::twenty_first::prelude::*;

use super::vm_proof_stream::*;

struct Recufier;

impl Recufier {
    /// Manual encoding of a [`Claim`][claim] containing only a program digest.
    ///
    /// [claim]: crate::triton_vm::prelude::Claim
    pub fn encode_claim(program_digest: Digest) -> Vec<BFieldElement> {
        let mut encoding: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(9);
        encoding.push(BFieldElement::one());
        encoding.push(BFieldElement::zero());
        encoding.push(BFieldElement::one());
        encoding.push(BFieldElement::zero());

        let Digest([elt_0, elt_1, elt_2, elt_3, elt_4]) = program_digest;

        encoding.push(elt_0);
        encoding.push(elt_1);
        encoding.push(elt_2);
        encoding.push(elt_3);
        encoding.push(elt_4);

        return encoding;
    }
}

pub fn recufy() {
    let own_digest: Digest = tasm::tasm_recufier_read_and_verify_own_program_digest_from_std_in();

    let mut sponge_state: Tip5State = Tip5::init();
    let encoded_claim: Vec<BFieldElement> = Recufier::encode_claim(own_digest);
    Tip5::pad_and_absorb_all(&mut sponge_state, &encoded_claim);

    let inner_proof_iter: VmProofIter = VmProofIter::new();
    let mut proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(inner_proof_iter);
    let log_2_padded_height: u32 = proof_iter.next_as_log_2_padded_height();
    tasm::tasm_io_write_to_stdout___u32(log_2_padded_height);

    let out_of_domain_base_row: Vec<XFieldElement> = proof_iter.next_as_out_of_domain_base_row();
    let first_element: XFieldElement = out_of_domain_base_row[0];
    tasm::tasm_io_write_to_stdout___xfe(first_element);

    let produce: [BFieldElement; 10] = Tip5::squeeze(&mut sponge_state);
    {
        let mut i: usize = 0;
        while i < 10 {
            tasm::tasm_io_write_to_stdout___bfe(produce[i]);
            i += 1;
        }
    }
    return;
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

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
        let dummy_ood_base_row = [[1337, 1338, 1339], [1001, 1002, 1004], [7001, 7002, 7004]]
            .map(XFieldElement::new_u64)
            .to_vec();

        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(ProofItem::Log2PaddedHeight(42));
        proof_stream.enqueue(ProofItem::OutOfDomainBaseRow(dummy_ood_base_row));
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

    #[proptest]
    fn manual_claim_encoding_corresponds_to_actual_encoded_claim_without_input_and_output(
        #[strategy(arb())] program_digest: Digest,
    ) {
        let actual_claim = Claim {
            program_digest,
            input: vec![],
            output: vec![],
        };
        let manual_encoding = Recufier::encode_claim(program_digest);
        prop_assert_eq!(actual_claim.encode(), manual_encoding);
    }
}
