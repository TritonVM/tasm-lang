use num::Zero;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(BFieldCodec)]
pub(crate) struct FriResponse {
    /// The authentication structure of the Merkle tree.
    pub(crate) auth_structure: Vec<Digest>,
    /// The values of the opened leaves of the Merkle tree.
    pub(crate) revealed_leaves: Vec<XFieldElement>,
}

#[derive(BFieldCodec)]
pub(crate) enum ProofItem {
    AuthenticationStructure(Vec<Digest>),
    MasterMainTableRows(Vec<Vec<BFieldElement>>),
    MasterAuxTableRows(Vec<Vec<XFieldElement>>),
    OutOfDomainMainRow(Vec<XFieldElement>),
    OutOfDomainAuxRow(Vec<XFieldElement>),
    OutOfDomainQuotientSegments([XFieldElement; 4]),
    MerkleRoot(Digest),
    Log2PaddedHeight(u32),
    QuotientSegmentsElements(Vec<[XFieldElement; 4]>),
    FriCodeword(Vec<XFieldElement>),
    FriResponse(FriResponse),
}

impl ProofItem {
    fn discriminant(&self) -> BFieldElement {
        #[allow(unused_assignments)]
        let mut discriminant: BFieldElement = BFieldElement::zero();
        match self {
            ProofItem::AuthenticationStructure(_) => {
                discriminant = BFieldElement::new(0);
            }
            ProofItem::MasterMainTableRows(_) => {
                discriminant = BFieldElement::new(1);
            }
            ProofItem::MasterAuxTableRows(_) => {
                discriminant = BFieldElement::new(2);
            }
            ProofItem::OutOfDomainMainRow(_) => {
                discriminant = BFieldElement::new(3);
            }
            ProofItem::OutOfDomainAuxRow(_) => {
                discriminant = BFieldElement::new(4);
            }
            ProofItem::OutOfDomainQuotientSegments(_) => {
                discriminant = BFieldElement::new(1);
            }
            ProofItem::MerkleRoot(_) => {
                discriminant = BFieldElement::new(5);
            }
            ProofItem::Log2PaddedHeight(_) => {
                discriminant = BFieldElement::new(6);
            }
            ProofItem::QuotientSegmentsElements(_) => {
                discriminant = BFieldElement::new(7);
            }
            ProofItem::FriCodeword(_) => {
                discriminant = BFieldElement::new(8);
            }
            ProofItem::FriResponse(_) => {
                discriminant = BFieldElement::new(9);
            }
        };

        return discriminant;
    }

    fn get_merkle_root(&self) -> Digest {
        #[allow(unused_assignments)]
        let mut root: Digest = Digest::default();
        match self {
            ProofItem::MerkleRoot(digest) => {
                root = *digest;
            }
            _ => {
                panic!();
            }
        };

        return root;
    }
}

fn main() {
    let boxed_proof_item: Box<ProofItem> =
        ProofItem::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    tasm::tasmlib_io_write_to_stdout___bfe(boxed_proof_item.discriminant());
    tasm::tasmlib_io_write_to_stdout___digest(boxed_proof_item.get_merkle_root());

    // Crash if not `MerkleRoot`. Explicitly don't use a wildcard.
    match *boxed_proof_item {
        ProofItem::AuthenticationStructure(_) => {
            panic!();
        }
        ProofItem::MasterMainTableRows(_) => {
            panic!();
        }
        ProofItem::MasterAuxTableRows(_) => {
            panic!();
        }
        ProofItem::OutOfDomainMainRow(_) => {
            panic!();
        }
        ProofItem::OutOfDomainAuxRow(_) => {
            panic!();
        }
        ProofItem::OutOfDomainQuotientSegments(_) => {
            panic!();
        }
        ProofItem::MerkleRoot(a) => {
            let b: BFieldElement = BFieldElement::new(10009 << 32);
            tasm::tasmlib_io_write_to_stdout___digest(a);
            tasm::tasmlib_io_write_to_stdout___bfe(b);
        }
        ProofItem::Log2PaddedHeight(_) => {
            panic!();
        }
        ProofItem::QuotientSegmentsElements(_) => {
            panic!();
        }
        ProofItem::FriCodeword(_) => {
            panic!();
        }
        ProofItem::FriResponse(_) => {
            panic!();
        }
    };

    // With wildcard
    #[allow(unused_assignments)]
    let mut d: BFieldElement = BFieldElement::new(100u64);
    match *boxed_proof_item {
        ProofItem::AuthenticationStructure(_) => {
            panic!();
        }
        _ => {
            let c: BFieldElement = BFieldElement::new(555u64 << 32);
            tasm::tasmlib_io_write_to_stdout___bfe(c);
            d = BFieldElement::new(200u64);
        }
    };

    tasm::tasmlib_io_write_to_stdout___bfe(d);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::random;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    #[test]
    fn boxed_proof_item_enum_test() {
        let a_0: Digest = random();
        let proof_item = ProofItem::MerkleRoot(a_0);
        let non_determinism = init_memory_from(&proof_item, BFieldElement::new(84));
        let expected_output = [
            vec![BFieldElement::new(5)],
            a_0.encode(),
            a_0.encode(),
            vec![BFieldElement::new(10009 << 32)],
            vec![BFieldElement::new(555 << 32)],
            vec![BFieldElement::new(200)],
        ]
        .concat();
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("enums", "boxed_proof_item_simple", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        if expected_output != vm_output.public_output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.public_output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
