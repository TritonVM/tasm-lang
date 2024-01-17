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

    fn as_merkle_root(&self) -> Digest {
        #[allow(unused_assignments)]
        let mut root: Digest = Digest::default();
        match self {
            ProofItem::MerkleRoot(bs) => {
                root = *bs;
            }
            _ => {
                panic!("wrong assumptions, buddy");
            }
        };

        return root;
    }
}

fn proof_item_stored_to_memory() {
    let auth_path: Vec<Digest> = Vec::<Digest>::with_capacity(0);
    let ap: Box<ProofItem> = Box::<ProofItem>::new(ProofItem::AuthenticationStructure(auth_path));
    assert!(!ap.include_in_fiat_shamir_heuristic());

    let merkle_root_pi: ProofItem = ProofItem::MerkleRoot(Digest::default());
    let merkle_root: Digest = merkle_root_pi.as_merkle_root();
    tasm::tasm_io_write_to_stdout___digest(merkle_root);

    return;
}

fn proof_item_load_auth_path_from_memory() {
    let ap: Box<ProofItem> =
        ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
    assert!(!ap.include_in_fiat_shamir_heuristic());

    return;
}

fn proof_item_load_merkle_root_from_memory() {
    let merkle_root_pi: Box<ProofItem> =
        ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
    assert!(merkle_root_pi.include_in_fiat_shamir_heuristic());
    let merkle_root: Digest = merkle_root_pi.as_merkle_root();
    tasm::tasm_io_write_to_stdout___digest(merkle_root);

    return;
}

#[cfg(test)]
mod test {
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;
    use triton_vm::NonDeterminism;

    use crate::ast_types::ListType;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::compile_for_test;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows::wrap_main_with_io;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    #[proptest]
    fn proof_item_stored_to_memory_test(#[strategy(vec(arb(), 5))] std_in: Vec<BFieldElement>) {
        // Rust program on host machine
        let non_determinism = NonDeterminism::default();
        let native_output = wrap_main_with_io(&proof_item_stored_to_memory)(
            std_in.clone(),
            non_determinism.clone(),
        );

        // Run test on Triton-VM
        let entrypoint =
            EntrypointLocation::disk("recufier", "proof_item", "proof_item_stored_to_memory");
        let test_program = compile_for_test(&entrypoint, ListType::Unsafe);

        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            std_in,
            non_determinism,
            0,
        )
        .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    #[proptest]
    fn proof_item_load_auth_path_from_memory_test(#[strategy(arb())] auth_structure: Vec<Digest>) {
        // Rust program on host machine
        let stdin = vec![];

        let ap_start_address: BFieldElement = BFieldElement::new(0);
        let proof_item = ProofItem::AuthenticationStructure(auth_structure);
        let non_determinism = init_memory_from(&proof_item, ap_start_address);

        let native_output = wrap_main_with_io(&proof_item_load_auth_path_from_memory)(
            stdin.clone(),
            non_determinism.clone(),
        );

        // Run test on Triton-VM
        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "proof_item_load_auth_path_from_memory",
        );
        let test_program = compile_for_test(&entrypoint_location, ListType::Unsafe);

        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        prop_assert_eq!(native_output, vm_output.output);
    }

    #[proptest]
    fn proof_item_load_merkle_root_from_memory_test(#[strategy(arb())] root: Digest) {
        let stdin = vec![];
        let proof_item = ProofItem::MerkleRoot(root);
        let mr_start_address = BFieldElement::new(0);
        let non_determinism = init_memory_from(&proof_item, mr_start_address);

        let host_machine_output = wrap_main_with_io(&proof_item_load_merkle_root_from_memory)(
            stdin.clone(),
            non_determinism.clone(),
        );

        let entrypoint_location = EntrypointLocation::disk(
            "recufier",
            "proof_item",
            "proof_item_load_merkle_root_from_memory",
        );
        let test_program = compile_for_test(&entrypoint_location, ListType::Unsafe);
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        prop_assert_eq!(host_machine_output, vm_output.output);
    }
}
