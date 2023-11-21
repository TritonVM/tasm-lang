#![allow(clippy::assertions_on_constants)]

// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use itertools::Itertools;
use triton_vm::{BFieldElement, Digest};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(BFieldCodec)]
pub enum EnumType {
    A(Vec<BFieldElement>),
    B(Vec<Vec<BFieldElement>>),
}

impl EnumType {
    // fn discriminant(&self) -> BFieldElement {
    //     #[allow(unused_assignments)]
    //     let mut discriminant: BFieldElement = BFieldElement::new(u32::MAX as u64);
    //     match self {
    //         EnumType::A(_) => {
    //             discriminant = BFieldElement::new(0);
    //         }
    //         EnumType::B(_) => {
    //             discriminant = BFieldElement::new(1);
    //         }
    //     };

    //     return discriminant;
    // }
}

fn main() {
    let b: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    // tasm::tasm_io_write_to_stdout___bfe(boxed_proof_item.discriminant());

    match *b {
        EnumType::A(_) => {
            assert!(true);
        }
        EnumType::B(_) => {
            assert!(false);
        }
    };

    match *b {
        EnumType::A(_) => {
            assert!(true);
        }
        EnumType::B(_) => {
            assert!(false);
        }
    };

    return;
}

mod tests {
    use super::*;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    #[test]
    fn match_with_wildcard_bindings_test() {
        let a_0 = random();
        let proof_item = EnumType::A(vec![a_0]);
        let non_determinism = init_memory_from(&proof_item, BFieldElement::new(84));
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "enums",
            "match_with_wildcard_bindings",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        println!("executing:\n{}", test_program.iter().join("\n"));
        {
            let mut ram: Vec<(BFieldElement, BFieldElement)> =
                non_determinism.ram.clone().into_iter().collect();
            ram.sort_unstable_by_key(|(p, _v)| p.value());
            println!(
                "{}",
                ram.iter().map(|(p, v)| format!("{p} => {v}")).join(", ")
            );
        }
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism.clone(),
            0,
        )
        .unwrap();
        if native_output != vm_output.output {
            panic!(
                "native_output:\n {}, got:\n{}. Code was:\n{}",
                native_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
