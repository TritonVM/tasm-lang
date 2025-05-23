use arbitrary::Arbitrary;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(BFieldCodec, Arbitrary)]
pub(crate) enum EnumType {
    A(Vec<BFieldElement>),
    B(Vec<Vec<BFieldElement>>),
}

fn main() {
    let b: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    match *b {
        EnumType::A(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(0));
        }
        EnumType::B(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(1));
        }
    };

    match *b {
        EnumType::A(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(2));
        }
        EnumType::B(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(3));
        }
    };

    match b.as_ref() {
        EnumType::A(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(4));
        }
        EnumType::B(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(5));
        }
    };

    match b.as_ref() {
        EnumType::A(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(6));
        }
        EnumType::B(_) => {
            tasm::tasmlib_io_write_to_stdout___bfe(BFieldElement::new(7));
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
    fn match_with_wildcard_bindings_test() {
        for _ in 0..4 {
            let rand: [u8; 200] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&enum_value, BFieldElement::new(84));
            let stdin = vec![];

            // Run test on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let entrypoint_location =
                EntrypointLocation::disk("enums", "match_with_wildcard_bindings", "main");
            let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
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
            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &test_program,
                vec![],
                stdin,
                non_determinism.clone(),
                0,
            )
            .unwrap();
            if native_output != vm_output.public_output {
                panic!(
                    "native_output:\n {}, got:\n{}. Code was:\n{}",
                    native_output.iter().join(", "),
                    vm_output.public_output.iter().join(", "),
                    test_program.iter().join("\n")
                );
            }
        }
    }
}
