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
        #[allow(unused_assignments)]
        let mut ret: bool = false;
        match self {
            ProofItem::MerkleRoot(_) => {
                ret = true;
            }
            ProofItem::OutOfDomainBaseRow(_) => {
                ret = true;
            }
            ProofItem::OutOfDomainExtRow(_) => {
                ret = true;
            }
            ProofItem::OutOfDomainQuotientSegments(_) => {
                ret = true;
            }
            // all of the following are implied by a corresponding Merkle root
            _ => {
                ret = false;
            }
        };

        return ret;
    }

    #[allow(clippy::assertions_on_constants)]
    fn as_merkle_root(&self) -> Digest {
        let mut ret: Digest = Digest::default();
        match self {
            ProofItem::MerkleRoot(bs) => {
                ret = *bs;
            }
            _ => {
                assert!(false);
            }
        };

        return ret;
    }
}

fn proof_item_stored_to_memory() {
    let ap: Box<ProofItem> = Box::<ProofItem>::new(ProofItem::AuthenticationStructure(
        Vec::<Digest>::with_capacity(0),
    ));
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
    use std::collections::HashMap;

    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    #[test]
    fn proof_item_stored_to_memory_test() {
        // Rust program on host machine
        let stdin = vec![random(), random(), random(), random(), random()];
        let non_determinism = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&proof_item_stored_to_memory)(
            stdin.clone(),
            non_determinism.clone(),
        );

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "recufier",
            "proof_item",
            "proof_item_stored_to_memory",
            crate::ast_types::ListType::Unsafe,
        );

        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn proof_item_load_auth_path_from_memory_test() {
        // Rust program on host machine
        let stdin = vec![];

        const AP_START_ADDRESS: BFieldElement = BFieldElement::new(0);
        let auth_path = ProofItem::AuthenticationStructure(vec![]);
        let non_determinism = init_memory_from(&auth_path, AP_START_ADDRESS);

        let native_output = rust_shadows::wrap_main_with_io(&proof_item_load_auth_path_from_memory)(
            stdin.clone(),
            non_determinism.clone(),
        );

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "recufier",
            "proof_item",
            "proof_item_load_auth_path_from_memory",
            crate::ast_types::ListType::Unsafe,
        );

        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        assert_eq!(native_output, vm_output.output);
    }

    #[test]
    fn proof_item_load_merkle_root_from_memory_test() {
        let stdin = vec![];
        let auth_path = ProofItem::MerkleRoot(random());
        const MR_START_ADDRESS: BFieldElement = BFieldElement::new(0);
        let non_determinism = init_memory_from(&auth_path, MR_START_ADDRESS);

        let host_machine_output = rust_shadows::wrap_main_with_io(
            &proof_item_load_merkle_root_from_memory,
        )(stdin.clone(), non_determinism.clone());

        let test_program = ozk_parsing::compile_for_test(
            "recufier",
            "proof_item",
            "proof_item_load_merkle_root_from_memory",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        assert_eq!(host_machine_output, vm_output.output);
    }
}
