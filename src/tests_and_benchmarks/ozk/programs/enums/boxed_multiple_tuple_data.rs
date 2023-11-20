use arbitrary::Arbitrary;
use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
enum EnumType {
    A(BFieldElement, BFieldElement),
}

fn main() {
    let boxed_enum_type: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    match boxed_enum_type.as_ref() {
        EnumType::A(elem0, elem1) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(1));
            tasm::tasm_io_write_to_stdout___bfe(*elem0);
            tasm::tasm_io_write_to_stdout___bfe(*elem1);
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
    fn boxed_enum_with_multi_tuple_test() {
        for _ in 0..2 {
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
                "boxed_multiple_tuple_data",
                "main",
                crate::ast_types::ListType::Unsafe,
            );
            let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
                &test_program,
                vec![],
                &mut HashMap::default(),
                stdin,
                non_determinism,
                0,
            )
            .unwrap();
            if native_output != vm_output.output {
                panic!(
                    "native_output:\n{}\nVM output:\n{}. Code was:\n{}\nrand was {}\n",
                    native_output.iter().join(", "),
                    vm_output.output.iter().join(", "),
                    test_program.iter().join("\n"),
                    rand.iter().join(",")
                );
            }
        }
    }
}
