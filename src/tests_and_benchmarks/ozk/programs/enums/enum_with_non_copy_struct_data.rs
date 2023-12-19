use arbitrary::Arbitrary;
use triton_vm::Digest;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
struct NotCopyStruct {
    a_list_of_digests: Vec<Digest>,
}

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
enum EnumType {
    A(NotCopyStruct),
}

fn main() {
    let boxed_enum_type: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let on_stack: EnumType = *boxed_enum_type;

    match on_stack {
        EnumType::A(non_copy_struct) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(8));
            tasm::tasm_io_write_to_stdout___u32(non_copy_struct.a_list_of_digests.len() as u32);
        }
    };

    return;
}

mod tests {
    use super::*;
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    #[test]
    fn enum_with_non_copy_struct_data_test() {
        for _ in 0..30 {
            let rand: [u8; 100] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&enum_value, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let test_program = ozk_parsing::compile_for_test(
                "enums",
                "enum_with_non_copy_struct_data",
                "main",
                crate::ast_types::ListType::Unsafe,
            );
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
                {
                    let mut ram: Vec<(BFieldElement, BFieldElement)> =
                        non_determinism.ram.clone().into_iter().collect();
                    ram.sort_unstable_by_key(|(p, _v)| p.value());
                    println!(
                        "{}",
                        ram.iter().map(|(p, v)| format!("{p} => {v}")).join(", ")
                    );
                }
                panic!(
                    "native_output:\n{}\nVM output:\n{}. Code was:\n{}\nrand was {}\ninput was: {enum_value:#?}",
                    native_output.iter().join(", "),
                    vm_output.output.iter().join(", "),
                    test_program.iter().join("\n"),
                    rand.iter().join(",")
                );
            }
        }
    }
}
