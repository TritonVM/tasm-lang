use tasm_lib::triton_vm::prelude::*;

use arbitrary::Arbitrary;

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

#[cfg(test)]
mod test {

    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    #[test]
    fn move_boxed_enum_to_stack_test() {
        for _ in 0..30 {
            let rand: [u8; 200] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&enum_value, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let entrypoint_location =
                EntrypointLocation::disk("enums", "move_boxed_enum_to_stack", "main");
            let test_program = ozk_parsing::compile_for_test(
                &entrypoint_location,
                crate::ast_types::ListType::Unsafe,
            );
            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &test_program,
                vec![],
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
