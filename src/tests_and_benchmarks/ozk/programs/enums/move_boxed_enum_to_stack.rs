use arbitrary::Arbitrary;
use itertools::Itertools;
use triton_vm::Digest;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
enum EnumType {
    A(u32),
    B(u64),
    C(Digest),
    D(Vec<XFieldElement>),
    E,
    F([Digest; 4]),
}

fn main() {
    let boxed_enum_type: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let on_stack: EnumType = *boxed_enum_type;

    match on_stack {
        EnumType::A(num) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(1));
            tasm::tasm_io_write_to_stdout___u32(num);
        }
        EnumType::B(num) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(2));
            tasm::tasm_io_write_to_stdout___u64(num);
        }
        EnumType::C(digest) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(3));
            tasm::tasm_io_write_to_stdout___digest(digest);
        }
        EnumType::D(list) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(4));
            tasm::tasm_io_write_to_stdout___u32(list.len() as u32);
        }
        EnumType::E => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(5));
        }
        EnumType::F(digests) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(6));
            tasm::tasm_io_write_to_stdout___digest(digests[0]);
            tasm::tasm_io_write_to_stdout___digest(digests[1]);
            tasm::tasm_io_write_to_stdout___digest(digests[3]);
            tasm::tasm_io_write_to_stdout___digest(digests[2]);
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
    fn move_boxed_enum_to_stack_test() {
        for _ in 0..20 {
            let rand: [u8; 100] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&enum_value, BFieldElement::new(84));
            {
                let mut ram_sorted: Vec<(BFieldElement, BFieldElement)> =
                    non_determinism.ram.clone().into_iter().collect_vec();
                ram_sorted.sort_unstable_by_key(|x| x.0.value());
            }

            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let test_program = ozk_parsing::compile_for_test(
                "enums",
                "move_boxed_enum_to_stack",
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
                    "native_output:\n{}, got:\n{}. Code was:\n{}\nrand was {}\n",
                    native_output.iter().join(", "),
                    vm_output.output.iter().join(", "),
                    test_program.iter().join("\n"),
                    rand.iter().join(",")
                );
            }
        }
    }
}
