use tasm_lib::triton_vm::prelude::*;
use tasm_lib::triton_vm::proof_item::ProofItem;
use tasm_lib::triton_vm::proof_stream::ProofStream;
use tasm_lib::twenty_first::shared_math::tip5::Tip5State;
use tasm_lib::twenty_first::util_types::algebraic_hasher::SpongeHasher;

use crate::tests_and_benchmarks::ozk::programs::recufier::host_machine_vm_proof_iter::VmProofIter;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn call_all_next_methods() {
    let mut sponge_state: Tip5State = Tip5::init();
    let vm_proof_iter_stack: VmProofIter = VmProofIter {
        current_item_pointer: BFieldElement::new(2),
    };
    let mut vm_proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(vm_proof_iter_stack);
    let a_merkle_root: Box<Digest> = vm_proof_iter.next_as_merkleroot(&mut sponge_state);
    tasm::tasm_io_write_to_stdout___digest(*a_merkle_root);

    let out_of_domain_base_row: Box<Vec<XFieldElement>> =
        vm_proof_iter.next_as_outofdomainbaserow(&mut sponge_state);
    tasm::tasm_io_write_to_stdout___xfe(out_of_domain_base_row[0]);
    tasm::tasm_io_write_to_stdout___xfe(out_of_domain_base_row[1]);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use tasm_lib::triton_vm::proof_item::FriResponse;
    use tasm_lib::twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn test_all_next_as_methods() {
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "vm_proof_iter_next_as", "call_all_next_methods");
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let non_determinism = non_determinism();
        {
            let mut ram: Vec<(BFieldElement, BFieldElement)> =
                non_determinism.ram.clone().into_iter().collect();
            ram.sort_unstable_by_key(|k| k.0.value());
            println!(
                "Init RAM: {}",
                ram.iter().map(|(k, v)| format!("({k} => {v})")).join(", ")
            );
        }
        let native_output = rust_shadows::wrap_main_with_io(&call_all_next_methods)(
            Vec::default(),
            non_determinism.clone(),
        );
        let vm_output = test_case
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        // assert_eq!(native_output, vm_output.output);
        if native_output != vm_output.output {
            println!("Expected:\n{}", native_output.iter().join(","));
            println!("Got:\n{}", vm_output.output.iter().join(","));
            panic!()
        }
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

    fn arbitrary_digest() -> Digest {
        Digest::new(
            (100u64..=104)
                .map(BFieldElement::new)
                .collect_vec()
                .try_into()
                .unwrap(),
        )
    }

    fn arbitrary_out_of_domain_base_row() -> Vec<XFieldElement> {
        (400u64..402)
            .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
            .collect_vec()
    }

    fn arbitrary_out_of_domain_ext_row() -> Vec<XFieldElement> {
        (600u64..642)
            .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
            .collect_vec()
    }

    fn arbitrary_out_of_domain_quotient_segments() -> [XFieldElement; 4] {
        (14u64..=17)
            .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
            .collect_vec()
            .try_into()
            .unwrap()
    }

    fn arbitrary_auth_structure() -> Vec<Digest> {
        (400u64..420)
            .map(|i| {
                Digest::new([
                    i.into(),
                    (i + 2).into(),
                    (i + 5).into(),
                    (i - 5).into(),
                    (i + 15).into(),
                ])
            })
            .collect_vec()
    }

    fn arbitrary_master_base_table_rows() -> Vec<Vec<BFieldElement>> {
        vec![
            vec![
                BFieldElement::new(420),
                BFieldElement::new(422),
                BFieldElement::new(419),
            ],
            vec![
                BFieldElement::new(1420),
                BFieldElement::new(1422),
                BFieldElement::new(1419),
            ],
            vec![
                BFieldElement::new(2420),
                BFieldElement::new(2422),
                BFieldElement::new(2419),
            ],
        ]
    }

    fn arbitrary_ext_base_table_rows() -> Vec<Vec<XFieldElement>> {
        vec![
            (14u64..=17)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec(),
            (1014u64..=1017)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec(),
            (2014u64..=2017)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec(),
        ]
    }

    fn arbitrary_log2_padded_height() -> u32 {
        22
    }

    fn arbitrary_quotient_segments_elements() -> Vec<[XFieldElement; 4]> {
        vec![
            (14u64..=17)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec()
                .try_into()
                .unwrap(),
            (2214u64..=2217)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec()
                .try_into()
                .unwrap(),
            (3314u64..=3317)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec()
                .try_into()
                .unwrap(),
            (4414u64..=4417)
                .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
                .collect_vec()
                .try_into()
                .unwrap(),
        ]
    }

    fn arbitrary_fri_codeword() -> Vec<XFieldElement> {
        (14u64..=170)
            .map(|i| XFieldElement::new([i.into(), (i + 2).into(), (i + 5).into()]))
            .collect_vec()
    }

    fn arbitrary_fri_response() -> FriResponse {
        FriResponse {
            auth_structure: arbitrary_auth_structure(),
            revealed_leaves: arbitrary_fri_codeword(),
        }
    }

    fn proof() -> Proof {
        let mut proof_stream = ProofStream::<Tip5>::new();
        proof_stream.enqueue(ProofItem::MerkleRoot(arbitrary_digest()));
        proof_stream.enqueue(ProofItem::OutOfDomainBaseRow(
            arbitrary_out_of_domain_base_row(),
        ));
        // proof_stream.enqueue(ProofItem::OutOfDomainExtRow(
        //     arbitrary_out_of_domain_ext_row(),
        // ));
        // proof_stream.enqueue(ProofItem::OutOfDomainQuotientSegments(
        //     arbitrary_out_of_domain_quotient_segments(),
        // ));
        // proof_stream.enqueue(ProofItem::AuthenticationStructure(
        //     arbitrary_auth_structure(),
        // ));
        // proof_stream.enqueue(ProofItem::MasterBaseTableRows(
        //     arbitrary_master_base_table_rows(),
        // ));
        // proof_stream.enqueue(ProofItem::MasterExtTableRows(
        //     arbitrary_ext_base_table_rows(),
        // ));
        // proof_stream.enqueue(ProofItem::Log2PaddedHeight(arbitrary_log2_padded_height()));
        // proof_stream.enqueue(ProofItem::QuotientSegmentsElements(
        //     arbitrary_quotient_segments_elements(),
        // ));
        // proof_stream.enqueue(ProofItem::FriCodeword(arbitrary_fri_codeword()));
        // proof_stream.enqueue(ProofItem::FriResponse(arbitrary_fri_response()));
        proof_stream.into()
    }
}
