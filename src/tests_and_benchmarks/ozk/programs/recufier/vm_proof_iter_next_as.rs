#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use itertools::Itertools;
    use tasm_lib::triton_vm::proof_item::FriResponse;
    use tasm_lib::twenty_first::math::b_field_element::BFieldElement;
    use tasm_lib::twenty_first::math::polynomial::Polynomial;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::ozk::rust_shadows::Tip5WithState;
    use crate::tests_and_benchmarks::ozk::rust_shadows::VmProofIter;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use crate::triton_vm::stark::NUM_QUOTIENT_SEGMENTS;
    use crate::triton_vm::table::master_table::MasterMainTable;
    use crate::triton_vm::table::master_table::MasterTable;
    use crate::triton_vm::table::AuxiliaryRow;
    use crate::triton_vm::table::MainRow;
    use crate::triton_vm::table::QuotientSegments;
    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::triton_vm::proof_item::ProofItem;
    use tasm_lib::triton_vm::proof_stream::ProofStream;
    use tasm_lib::triton_vm::table::master_table::MasterAuxTable;

    /// The function being tested here. Dual-compiled by `rustc` and `tasm-lang`.
    fn call_all_next_methods() {
        Tip5WithState::init();
        let vm_proof_iter_stack: VmProofIter = VmProofIter {
            current_item_pointer: BFieldElement::new(2),
        };
        let mut vm_proof_iter: Box<VmProofIter> = Box::<VmProofIter>::new(vm_proof_iter_stack);
        let a_merkle_root: Box<Digest> = vm_proof_iter.next_as_merkleroot();
        tasm::tasmlib_io_write_to_stdout___digest(*a_merkle_root);

        let out_of_domain_main_row: Box<Box<[XFieldElement; 379]>> =
            vm_proof_iter.next_as_outofdomainmainrow();
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_main_row[0]);
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_main_row[1]);

        let out_of_domain_aux_row: Box<Box<[XFieldElement; 88]>> =
            vm_proof_iter.next_as_outofdomainauxrow();
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_aux_row[0]);
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_aux_row[1]);

        let out_of_domain_quotient_segments: Box<[XFieldElement; 4]> =
            vm_proof_iter.next_as_outofdomainquotientsegments();
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_quotient_segments[0]);
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_quotient_segments[1]);
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_quotient_segments[2]);
        tasm::tasmlib_io_write_to_stdout___xfe(out_of_domain_quotient_segments[3]);

        let authentication_structure: Box<Vec<Digest>> =
            vm_proof_iter.next_as_authenticationstructure();
        tasm::tasmlib_io_write_to_stdout___digest(authentication_structure[0]);
        tasm::tasmlib_io_write_to_stdout___digest(authentication_structure[1]);
        tasm::tasmlib_io_write_to_stdout___digest(authentication_structure[2]);

        let mbtw: Box<Vec<[BFieldElement; 379]>> = vm_proof_iter.next_as_mastermaintablerows();
        {
            let mut j: usize = 0;
            while j < mbtw.len() {
                let mut i: usize = 0;
                while i < mbtw[j].len() {
                    tasm::tasmlib_io_write_to_stdout___bfe(mbtw[j][i]);
                    i += 1;
                }
                j += 1;
            }
        }

        let metr: Box<Vec<[XFieldElement; 88]>> = vm_proof_iter.next_as_masterauxtablerows();
        {
            let mut j: usize = 0;
            while j < metr.len() {
                let mut i: usize = 0;
                while i < metr[j].len() {
                    tasm::tasmlib_io_write_to_stdout___xfe(metr[j][i]);
                    i += 1;
                }
                j += 1;
            }
        }

        let log2paddedheight: Box<u32> = vm_proof_iter.next_as_log2paddedheight();
        tasm::tasmlib_io_write_to_stdout___u32(*log2paddedheight);

        let quotient_segments_elements: Box<Vec<[XFieldElement; 4]>> =
            vm_proof_iter.next_as_quotientsegmentselements();
        {
            let mut j: usize = 0;
            while j < quotient_segments_elements.len() {
                tasm::tasmlib_io_write_to_stdout___xfe(quotient_segments_elements[j][0]);
                tasm::tasmlib_io_write_to_stdout___xfe(quotient_segments_elements[j][1]);
                tasm::tasmlib_io_write_to_stdout___xfe(quotient_segments_elements[j][2]);
                tasm::tasmlib_io_write_to_stdout___xfe(quotient_segments_elements[j][3]);
                j += 1;
            }
        }

        let fri_codeword: Box<Vec<XFieldElement>> = vm_proof_iter.next_as_fricodeword();
        {
            let mut j: usize = 0;
            while j < fri_codeword.len() {
                tasm::tasmlib_io_write_to_stdout___xfe(fri_codeword[j]);
                j += 1;
            }
        }

        let fri_polynomial: Box<Polynomial<XFieldElement>> = vm_proof_iter.next_as_fripolynomial();
        {
            let mut j: usize = 0;
            while j < fri_polynomial.coefficients().len() {
                tasm::tasmlib_io_write_to_stdout___xfe(fri_polynomial.coefficients()[j]);
                j += 1;
            }
        }

        let fri_response: Box<FriResponse> = vm_proof_iter.next_as_friresponse();
        {
            let mut j: usize = 0;
            while j < fri_response.auth_structure.len() {
                tasm::tasmlib_io_write_to_stdout___digest(fri_response.auth_structure[j]);
                j += 1;
            }
        }
        {
            let mut j: usize = 0;
            while j < fri_response.revealed_leaves.len() {
                tasm::tasmlib_io_write_to_stdout___xfe(fri_response.revealed_leaves[j]);
                j += 1;
            }
        }

        return;
    }

    #[test]
    fn test_all_next_as_methods() {
        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "vm_proof_iter_next_as",
            "test::call_all_next_methods",
        );
        let test_case = TritonVMTestCase::new(entrypoint_location);
        let non_determinism = non_determinism();
        {
            let mut ram: Vec<(BFieldElement, BFieldElement)> =
                non_determinism.ram.clone().into_iter().collect();
            ram.sort_unstable_by_key(|k| k.0.value());
        }
        let native_output = rust_shadows::wrap_main_with_io(&call_all_next_methods)(
            Vec::default(),
            non_determinism.clone(),
        );
        let vm_output = test_case
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        // assert_eq!(native_output, vm_output.public_output);
        if native_output != vm_output.public_output {
            println!("Expected:\n{}", native_output.iter().join(","));
            println!("Got:\n{}", vm_output.public_output.iter().join(","));
            panic!()
        }
    }

    fn non_determinism() -> NonDeterminism {
        let Proof(raw_proof) = proof();
        let ram: HashMap<BFieldElement, BFieldElement> = raw_proof
            .into_iter()
            .enumerate()
            .map(|(k, v)| (BFieldElement::new(k as u64), v))
            .collect();

        NonDeterminism::default().with_ram(ram)
    }

    fn into_xfe(seed: u64) -> XFieldElement {
        xfe!([seed, seed + 2, seed + 5])
    }

    fn arbitrary_digest(seed: u64) -> Digest {
        let pseudo_digest = |i| [i, i + 2, i + 5, i - 5, i + 15].map(BFieldElement::new);
        Digest::new(pseudo_digest(seed))
    }

    fn arbitrary_out_of_domain_main_row(from: u64) -> MainRow<XFieldElement> {
        let to = from + MasterMainTable::NUM_COLUMNS as u64;
        let row = (from..to).map(into_xfe).collect_vec();
        row.try_into().unwrap()
    }

    fn arbitrary_aux_row(from: u64) -> AuxiliaryRow {
        let to = from + MasterAuxTable::NUM_COLUMNS as u64;
        let row = (from..to).map(into_xfe).collect_vec();
        row.try_into().unwrap()
    }

    fn arbitrary_quotient_segments(from: u64) -> QuotientSegments {
        let to = from + NUM_QUOTIENT_SEGMENTS as u64;
        (from..to).map(into_xfe).collect_vec().try_into().unwrap()
    }

    fn arbitrary_auth_structure() -> Vec<Digest> {
        (400..403).map(arbitrary_digest).collect_vec()
    }

    fn arbitrary_main_row(from: u64) -> MainRow<BFieldElement> {
        let to = from + MasterMainTable::NUM_COLUMNS as u64;
        let row = (from..to).map(BFieldElement::new).collect_vec();
        row.try_into().unwrap()
    }

    fn arbitrary_master_main_table_rows() -> Vec<MainRow<BFieldElement>> {
        [420, 1420, 2420].map(arbitrary_main_row).to_vec()
    }

    fn arbitrary_aux_main_table_rows() -> Vec<AuxiliaryRow> {
        [14u64, 1014u64, 2014u64].map(arbitrary_aux_row).to_vec()
    }

    fn arbitrary_log2_padded_height() -> u32 {
        22
    }

    fn arbitrary_quotient_segments_elements() -> Vec<QuotientSegments> {
        [14, 2214, 3314, 4414]
            .map(arbitrary_quotient_segments)
            .to_vec()
    }

    fn arbitrary_fri_codeword() -> Vec<XFieldElement> {
        (14u64..=170).map(into_xfe).collect_vec()
    }

    fn arbitrary_fri_polynomial() -> Polynomial<'static, XFieldElement> {
        Polynomial::new(arbitrary_fri_codeword())
    }

    fn arbitrary_fri_response() -> FriResponse {
        FriResponse {
            auth_structure: arbitrary_auth_structure(),
            revealed_leaves: arbitrary_fri_codeword(),
        }
    }

    fn proof() -> Proof {
        let mut proof_stream = ProofStream::new();
        proof_stream.enqueue(ProofItem::MerkleRoot(arbitrary_digest(42)));

        let ood_main_row = Box::new(arbitrary_out_of_domain_main_row(1337));
        proof_stream.enqueue(ProofItem::OutOfDomainMainRow(ood_main_row));

        let ood_aux_row = Box::new(arbitrary_aux_row(1001));
        proof_stream.enqueue(ProofItem::OutOfDomainAuxRow(ood_aux_row));

        let quot_segments = arbitrary_quotient_segments(800);
        proof_stream.enqueue(ProofItem::OutOfDomainQuotientSegments(quot_segments));

        let auth_structure = arbitrary_auth_structure();
        proof_stream.enqueue(ProofItem::AuthenticationStructure(auth_structure));

        let base_rows = arbitrary_master_main_table_rows();
        proof_stream.enqueue(ProofItem::MasterMainTableRows(base_rows));

        let ext_rows = arbitrary_aux_main_table_rows();
        proof_stream.enqueue(ProofItem::MasterAuxTableRows(ext_rows));

        proof_stream.enqueue(ProofItem::Log2PaddedHeight(arbitrary_log2_padded_height()));

        let quot_segments_elements = arbitrary_quotient_segments_elements();
        proof_stream.enqueue(ProofItem::QuotientSegmentsElements(quot_segments_elements));
        proof_stream.enqueue(ProofItem::FriCodeword(arbitrary_fri_codeword()));
        proof_stream.enqueue(ProofItem::FriPolynomial(arbitrary_fri_polynomial()));
        proof_stream.enqueue(ProofItem::FriResponse(arbitrary_fri_response()));

        proof_stream.into()
    }
}
