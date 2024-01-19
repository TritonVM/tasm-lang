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
        let mut is_included: bool = false;
        match self {
            ProofItem::MerkleRoot(_) => {
                is_included = true;
            }
            ProofItem::OutOfDomainBaseRow(_) => {
                is_included = true;
            }
            ProofItem::OutOfDomainExtRow(_) => {
                is_included = true;
            }
            ProofItem::OutOfDomainQuotientSegments(_) => {
                is_included = true;
            }
            // all other possibilities are implied by a corresponding Merkle root
            _ => {}
        };

        return is_included;
    }

    pub fn as_authentication_structure(&self) -> Vec<Digest> {
        #[allow(unused_assignments)]
        let mut auth_structure: Vec<Digest> = Vec::<Digest>::with_capacity(0);
        match self {
            ProofItem::AuthenticationStructure(caps) => {
                auth_structure = caps.to_owned();
            }
            _ => {
                panic!();
            }
        };

        return auth_structure;
    }

    pub fn as_master_base_table_rows(&self) -> Vec<Vec<BFieldElement>> {
        #[allow(unused_assignments)]
        let mut mbt_rows: Vec<Vec<BFieldElement>> = Vec::<Vec<BFieldElement>>::with_capacity(0);
        match self {
            ProofItem::MasterBaseTableRows(bss) => {
                mbt_rows = bss.to_owned();
            }
            _ => {
                panic!();
            }
        };

        return mbt_rows;
    }

    fn as_merkle_root(&self) -> Digest {
        #[allow(unused_assignments)]
        let mut root: Digest = Digest::default();
        match self {
            ProofItem::MerkleRoot(bs) => {
                root = *bs;
            }
            _ => {
                panic!();
            }
        };

        return root;
    }
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

        let mbt_rows: Vec<Vec<BFieldElement>> = mbt_rows_item.as_master_base_table_rows();
        if mbt_rows.len() > 0 {
            let line_length: usize = mbt_rows[0].len();
            let mut i: usize = 1;
            while i < mbt_rows.len() {
                assert!(line_length == mbt_rows[i].len());
                i += 1;
            }
        }

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

        let host_machine_output = wrap_main_with_io(
            &proof_item_load_master_base_table_rows_from_memory,
        )(vec![], non_determinism.clone());

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

        prop_assert_eq!(host_machine_output, vm_output.output);
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
}
