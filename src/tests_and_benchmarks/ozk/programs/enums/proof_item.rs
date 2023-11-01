// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use itertools::Itertools;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

#[derive(BFieldCodec)]
pub struct FriResponse {
    /// The authentication structure of the Merkle tree.
    pub auth_structure: Vec<Digest>,
    /// The values of the opened leaves of the Merkle tree.
    pub revealed_leaves: Vec<XFieldElement>,
}

#[derive(BFieldCodec)]
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

fn main() {
    let digest_from_standard_in: Digest = tasm::tasm_io_read_stdin___digest();
    let _a: ProofItem = ProofItem::MerkleRoot(digest_from_standard_in);
    // tasm::tasm_io_write_to_stdout___digest();
    return;
}

mod tests {
    use super::*;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;

    #[test]
    fn proof_item_enum_test() {
        // let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let non_determinism = NonDeterminism::default();
        let expected_output = vec![];
        let a_0: Digest = random();
        let mut a0_encoded_reverse = a_0.encode();
        a0_encoded_reverse.reverse();
        let stdin = [a0_encoded_reverse].concat();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "enums",
            "proof_item",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.output);
        if expected_output != vm_output.output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
