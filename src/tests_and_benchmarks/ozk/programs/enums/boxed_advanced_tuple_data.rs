#![allow(clippy::len_zero)]

use arbitrary::Arbitrary;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
enum EnumType {
    A,
    B(Digest),
    C(Vec<XFieldElement>),
    D(BFieldElement, BFieldElement),
    E(Vec<BFieldElement>, XFieldElement),
    F(XFieldElement, Vec<XFieldElement>),
    G(
        XFieldElement,
        Vec<XFieldElement>,
        Vec<Digest>,
        u32,
        Vec<Digest>,
    ),
}

fn main() {
    let boxed_enum_type: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    match boxed_enum_type.as_ref() {
        EnumType::A => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(1));
        }
        EnumType::B(digest) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(2));
            tasm::tasmlib_io_write_to_stdout___digest(*digest);
        }
        EnumType::C(xfes) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(3));
            tasm::tasmlib_io_write_to_stdout___u32(xfes.len() as u32);
            if xfes.len() > 0 {
                tasm::tasmlib_io_write_to_stdout___xfe(xfes[0]);
            }
            if xfes.len() > 1 {
                tasm::tasmlib_io_write_to_stdout___xfe(xfes[1]);
            }
        }
        EnumType::D(bfe0, bfe1) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(4));
            tasm::tasmlib_io_write_to_stdout___bfe(*bfe0);
            tasm::tasmlib_io_write_to_stdout___bfe(*bfe1);
        }
        EnumType::E(bfes, xfe) => {
            //
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(5));
            tasm::tasmlib_io_write_to_stdout___u32(bfes.len() as u32);
            if bfes.len() > 0 {
                tasm::tasmlib_io_write_to_stdout___bfe(bfes[0]);
            }
            if bfes.len() > 1 {
                tasm::tasmlib_io_write_to_stdout___bfe(bfes[1]);
            }
            tasm::tasmlib_io_write_to_stdout___xfe(*xfe);
        }
        EnumType::F(xfe, xfes) => {
            //
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(6));
            tasm::tasmlib_io_write_to_stdout___u32(xfes.len() as u32);
            if xfes.len() > 0 {
                tasm::tasmlib_io_write_to_stdout___xfe(xfes[0]);
            }
            tasm::tasmlib_io_write_to_stdout___xfe(*xfe);
            if xfes.len() > 1 {
                tasm::tasmlib_io_write_to_stdout___xfe(xfes[1]);
            }
        }
        EnumType::G(xfe, xfes, digests0, unsi32, digests1) => {
            //
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(7));
            tasm::tasmlib_io_write_to_stdout___u32(digests1.len() as u32);
            tasm::tasmlib_io_write_to_stdout___u32(digests0.len() as u32);
            tasm::tasmlib_io_write_to_stdout___u32(xfes.len() as u32);
            tasm::tasmlib_io_write_to_stdout___u32(*unsi32);
            if digests1.len() > 0 {
                tasm::tasmlib_io_write_to_stdout___digest(digests1[0]);
            }
            tasm::tasmlib_io_write_to_stdout___xfe(*xfe);
            if digests1.len() > 1 {
                tasm::tasmlib_io_write_to_stdout___digest(digests1[1]);
            }
            if digests0.len() > 0 {
                tasm::tasmlib_io_write_to_stdout___digest(digests0[0]);
            }
            tasm::tasmlib_io_write_to_stdout___xfe(*xfe);
            if digests0.len() > 1 {
                tasm::tasmlib_io_write_to_stdout___digest(digests0[1]);
            }
            if xfes.len() > 0 {
                tasm::tasmlib_io_write_to_stdout___xfe(xfes[0]);
            }
            tasm::tasmlib_io_write_to_stdout___xfe(*xfe);
            if xfes.len() > 1 {
                tasm::tasmlib_io_write_to_stdout___xfe(xfes[1]);
            }
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    #[test]
    fn boxed_enum_with_advanced_tuple_test() {
        let mut test_cases = vec![];
        for _ in 0..10 {
            let rand: [u8; 2000] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            test_cases.push(enum_value);
        }

        test_cases.push(
            // Check with elements in all vecs
            EnumType::G(
                random(),
                random_elements(4),
                random_elements(5),
                random(),
                random_elements(6),
            ),
        );

        for test_case in test_cases {
            let non_determinism = init_memory_from(&test_case, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let entrypoint_location =
                EntrypointLocation::disk("enums", "boxed_advanced_tuple_data", "main");
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
                    "native_output:\n{}\nVM output:\n{}. Code was:\n{}\n",
                    native_output.iter().join(", "),
                    vm_output.public_output.iter().join(", "),
                    test_program.iter().join("\n"),
                );
            }
        }
    }
}
