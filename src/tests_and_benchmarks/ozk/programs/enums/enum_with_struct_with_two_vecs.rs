use arbitrary::Arbitrary;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
struct NotCopyStruct {
    // digests: Vec<Digest>,
    // bfe: BFieldElement,
    xfes: Vec<XFieldElement>,
    xfe: XFieldElement,
}

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
enum EnumType {
    B(NotCopyStruct),
}

fn main() {
    let boxed_enum_type: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    match boxed_enum_type.as_ref() {
        EnumType::B(non_copy_struct) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(8));
            tasm::tasmlib_io_write_to_stdout___u32(non_copy_struct.xfes.len() as u32);
            // tasm::tasmlib_io_write_to_stdout___u32(non_copy_struct.digests.len() as u32);
        }
    };

    let on_stack: EnumType = *boxed_enum_type;

    match on_stack {
        EnumType::B(non_copy_struct) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(8));
            // tasm::tasmlib_io_write_to_stdout___u32(non_copy_struct.digests.len() as u32);
            tasm::tasmlib_io_write_to_stdout___u32(non_copy_struct.xfes.len() as u32);
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    #[test]
    fn enum_with_struct_with_two_vecs_test() {
        for _ in 0..30 {
            let rand: [u8; 300] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&enum_value, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let entrypoint_location =
                EntrypointLocation::disk("enums", "enum_with_struct_with_two_vecs", "main");
            let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &test_program,
                vec![],
                stdin,
                non_determinism,
                0,
            )
            .unwrap();
            if native_output != vm_output.public_output {
                panic!(
                    "native_output:\n{}\nVM output:\n{}. Code was:\n{}\nrand was {}\ninput was: {enum_value:#?}",
                    native_output.iter().join(", "),
                    vm_output.public_output.iter().join(", "),
                    test_program.iter().join("\n"),
                    rand.iter().join(",")
                );
            }
        }
    }
}
