use arbitrary::Arbitrary;
use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;
use triton_vm::BFieldElement;
use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

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
    fn include_in_fiat_shamir_heuristic(&self) -> bool {
        return match self {
            ProofItem::MerkleRoot(_) => {
                //
                true
            }
            ProofItem::OutOfDomainBaseRow(_) => {
                //
                true
            }
            ProofItem::OutOfDomainExtRow(_) => {
                //
                true
            }
            ProofItem::OutOfDomainQuotientSegments(_) => {
                //
                true
            }
            // all other possibilities are implied by a corresponding Merkle root
            _ => {
                //
                false
            }
        };
    }

    pub fn as_authentication_structure(&self) -> Vec<Digest> {
        return match self {
            ProofItem::AuthenticationStructure(caps) => {
                //
                caps.to_owned()
            }
            _ => {
                //
                panic!()
            }
        };
    }

    pub fn as_master_base_table_rows(&self) -> Vec<Vec<BFieldElement>> {
        return match self {
            ProofItem::MasterBaseTableRows(bss) => {
                //
                bss.to_owned()
            }
            _ => {
                //
                panic!()
            }
        };
    }

    pub fn as_master_ext_table_rows(&self) -> Vec<Vec<XFieldElement>> {
        return match self {
            ProofItem::MasterExtTableRows(xss) => {
                //
                xss.to_owned()
            }
            _ => {
                //
                panic!()
            }
        };
    }

    pub fn as_out_of_domain_base_row(&self) -> Vec<XFieldElement> {
        return match self {
            ProofItem::OutOfDomainBaseRow(xs) => {
                //
                xs.to_owned()
            }
            _ => {
                //
                panic!()
            }
        };
    }

    pub fn as_out_of_domain_ext_row(&self) -> Vec<XFieldElement> {
        return match self {
            ProofItem::OutOfDomainExtRow(xs) => {
                //
                xs.to_owned()
            }
            _ => {
                //
                panic!()
            }
        };
    }

    pub fn as_out_of_domain_quotient_segments(&self) -> [XFieldElement; 4] {
        return match self {
            ProofItem::OutOfDomainQuotientSegments(xs) => {
                //
                *xs
            }
            _ => {
                panic!()
            }
        };
    }

    fn as_merkle_root(&self) -> Digest {
        return match self {
            ProofItem::MerkleRoot(bs) => {
                //
                *bs
            }
            _ => {
                panic!()
            }
        };
    }

    pub fn as_log2_padded_height(&self) -> u32 {
        return match self {
            ProofItem::Log2PaddedHeight(height) => {
                //
                *height
            }
            _ => {
                //
                panic!()
            }
        };
    }

    // todo: enable this once arrays work in lib
    /*
    pub fn as_quotient_segments_elements(&self) -> Vec<[XFieldElement; 4]> {
        #[allow(unused_assignments)]
        let mut quotient_segments_elements: Vec<[XFieldElement; 4]> =
            Vec::<[XFieldElement; 4]>::with_capacity(0);
        match self {
            ProofItem::QuotientSegmentsElements(xss) => {
                quotient_segments_elements = xss.to_owned();
            }
            _ => {
                panic!();
            }
        };

        return quotient_segments_elements;
    }*/

    pub fn as_fri_codeword(&self) -> Vec<XFieldElement> {
        return match self {
            ProofItem::FriCodeword(xs) => {
                //
                xs.to_owned()
            }
            _ => {
                panic!()
            }
        };
    }

    // todo: enable this, figure out `to_owned()` for `FriResponse`
    /*
    pub fn as_fri_response(&self) -> FriResponse {
        let auth_structure: Vec<Digest> = Vec::<Digest>::with_capacity(0);
        let revealed_leaves: Vec<XFieldElement> = Vec::<XFieldElement>::with_capacity(0);
        #[allow(unused_assignments)]
        let mut fri_response: FriResponse = FriResponse {
            auth_structure,
            revealed_leaves,
        };

        match self {
            ProofItem::FriResponse(response) => {
                fri_response = response.to_owned();
            }
            _ => {
                panic!();
            }
        };

        return fri_response;
    }*/
}

#[cfg(test)]
mod test {
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use super::*;

    fn proof_item_load_auth_structure_from_memory() {
        let auth_struct: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(!auth_struct.include_in_fiat_shamir_heuristic());

        let auth_structure: Vec<Digest> = auth_struct.as_authentication_structure();

        {
            let mut i: usize = 0;
            while i < auth_structure.len() {
                tasm::tasm_io_write_to_stdout___digest(auth_structure[i]);
                i += 1;
            }
        }

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_auth_structure_from_memory_test(
        #[strategy(arb())] auth_structure: Vec<Digest>,
    ) {
        let ap_start_address: BFieldElement = BFieldElement::new(0);
        let proof_item = ProofItem::AuthenticationStructure(auth_structure);
        let non_determinism = init_memory_from(&proof_item, ap_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_auth_structure_from_memory)(
            vec![],
            non_determinism.clone(),
        );

        let entrypoint = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_auth_structure_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_master_base_table_rows_from_memory() {
        let mbt_rows_item: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(!mbt_rows_item.include_in_fiat_shamir_heuristic());
        let _rows: Vec<Vec<BFieldElement>> = mbt_rows_item.as_master_base_table_rows();

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_master_base_table_rows_from_memory_test(
        #[strategy(0_usize..100)] _line_length: usize,
        #[strategy(vec(vec(arb(), #_line_length), 0..100))] mbt_rows: Vec<Vec<BFieldElement>>,
    ) {
        let proof_item = ProofItem::MasterBaseTableRows(mbt_rows);
        let mbt_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, mbt_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_master_base_table_rows_from_memory)(
            vec![],
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_master_base_table_rows_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_master_ext_table_rows_from_memory() {
        let met_rows_item: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(!met_rows_item.include_in_fiat_shamir_heuristic());
        let _rows: Vec<Vec<XFieldElement>> = met_rows_item.as_master_ext_table_rows();

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_master_ext_table_rows_from_memory_test(
        #[strategy(0_usize..100)] _line_length: usize,
        #[strategy(vec(vec(arb(), #_line_length), 0..100))] met_rows: Vec<Vec<XFieldElement>>,
    ) {
        let proof_item = ProofItem::MasterExtTableRows(met_rows);
        let met_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, met_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_master_ext_table_rows_from_memory)(
            vec![],
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_master_ext_table_rows_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_out_of_domain_base_row_from_memory() {
        let ood_base_row_item: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(ood_base_row_item.include_in_fiat_shamir_heuristic());

        let row: Vec<XFieldElement> = ood_base_row_item.as_out_of_domain_base_row();
        {
            let mut i: usize = 0;
            while i < row.len() {
                tasm::tasm_io_write_to_stdout___xfe(row[i]);
                i += 1;
            }
        }

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_out_of_domain_base_row_from_memory_test(
        #[strategy(arb())] ood_base_row: Vec<XFieldElement>,
    ) {
        let proof_item = ProofItem::OutOfDomainBaseRow(ood_base_row);
        let ood_base_row_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, ood_base_row_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_out_of_domain_base_row_from_memory)(
            vec![],
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_out_of_domain_base_row_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_out_of_domain_ext_row_from_memory() {
        let ood_ext_row_item: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(ood_ext_row_item.include_in_fiat_shamir_heuristic());

        let row: Vec<XFieldElement> = ood_ext_row_item.as_out_of_domain_ext_row();
        {
            let mut i: usize = 0;
            while i < row.len() {
                tasm::tasm_io_write_to_stdout___xfe(row[i]);
                i += 1;
            }
        }

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_out_of_domain_ext_row_from_memory_test(
        #[strategy(arb())] ood_ext_row: Vec<XFieldElement>,
    ) {
        let proof_item = ProofItem::OutOfDomainExtRow(ood_ext_row);
        let ood_ext_row_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, ood_ext_row_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_out_of_domain_ext_row_from_memory)(
            vec![],
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_out_of_domain_ext_row_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_out_of_domain_quotient_segments_from_memory() {
        let ood_quotient_segments_item: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(ood_quotient_segments_item.include_in_fiat_shamir_heuristic());

        let segments: [XFieldElement; 4] =
            ood_quotient_segments_item.as_out_of_domain_quotient_segments();
        {
            tasm::tasm_io_write_to_stdout___xfe(segments[0]);
            tasm::tasm_io_write_to_stdout___xfe(segments[1]);
            tasm::tasm_io_write_to_stdout___xfe(segments[2]);
            tasm::tasm_io_write_to_stdout___xfe(segments[3]);
            // let mut i: usize = 0;
            // while i < segments.len() {
            //     tasm::tasm_io_write_to_stdout___xfe(segments[i]);
            //     i += 1;
            // }
        }

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_out_of_domain_quotient_segments_from_memory_test(
        #[strategy(arb())] ood_quotient_segments: [XFieldElement; 4],
    ) {
        let proof_item = ProofItem::OutOfDomainQuotientSegments(ood_quotient_segments);
        let ood_quotient_segments_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, ood_quotient_segments_start_address);

        let native_output = wrap_main_with_io(
            &proof_item_load_out_of_domain_quotient_segments_from_memory,
        )(vec![], non_determinism.clone());

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_out_of_domain_quotient_segments_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_merkle_root_from_memory() {
        let merkle_root_pi: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(merkle_root_pi.include_in_fiat_shamir_heuristic());
        let merkle_root: Digest = merkle_root_pi.as_merkle_root();
        tasm::tasm_io_write_to_stdout___digest(merkle_root);

        return;
    }

    #[proptest(cases = 10)]
    fn proof_item_load_merkle_root_from_memory_test(#[strategy(arb())] root: Digest) {
        let stdin = vec![];
        let proof_item = ProofItem::MerkleRoot(root);
        let mr_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, mr_start_address);

        let host_machine_output = wrap_main_with_io(&proof_item_load_merkle_root_from_memory)(
            stdin,
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_merkle_root_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(host_machine_output, vm_output.output);
    }

    fn proof_item_load_log2_padded_height_from_memory() {
        let log2_padded_height_pi: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(!log2_padded_height_pi.include_in_fiat_shamir_heuristic());
        let log2_padded_height: u32 = log2_padded_height_pi.as_log2_padded_height();
        tasm::tasm_io_write_to_stdout___u32(log2_padded_height);

        return;
    }

    #[proptest(cases = 10)]
    fn proof_item_load_log2_padded_height_from_memory_test(
        #[strategy(0_u32..100)] log2_padded_height: u32,
    ) {
        let stdin = vec![];
        let proof_item = ProofItem::Log2PaddedHeight(log2_padded_height);
        let lph_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, lph_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_log2_padded_height_from_memory)(
            stdin,
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_log2_padded_height_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    fn proof_item_load_fri_codeword_from_memory() {
        let fri_codeword_pi: Box<ProofItem> =
            ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
        assert!(!fri_codeword_pi.include_in_fiat_shamir_heuristic());
        let fri_codeword: Vec<XFieldElement> = fri_codeword_pi.as_fri_codeword();

        {
            let mut i: usize = 0;
            while i < fri_codeword.len() {
                tasm::tasm_io_write_to_stdout___xfe(fri_codeword[i]);
                i += 1;
            }
        }

        return;
    }

    #[proptest(cases = 20)]
    fn proof_item_load_fri_codeword_from_memory_test(
        #[strategy(arb())] fri_codeword: Vec<XFieldElement>,
    ) {
        let proof_item = ProofItem::FriCodeword(fri_codeword);
        let fri_codeword_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, fri_codeword_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_fri_codeword_from_memory)(
            vec![],
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "test::proof_item_load_fri_codeword_from_memory",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_non_determinism(non_determinism)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }
}
