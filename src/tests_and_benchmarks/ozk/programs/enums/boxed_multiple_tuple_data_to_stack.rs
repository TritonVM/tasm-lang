use arbitrary::Arbitrary;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
enum EnumType {
    A(BFieldElement, BFieldElement),
}

fn main() {
    let b: Box<EnumType> =
        EnumType::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();

    let on_stack: EnumType = *b;

    match on_stack {
        EnumType::A(elem0, elem1) => {
            tasm::tasm_io_write_to_stdout___bfe(BFieldElement::new(1));
            tasm::tasm_io_write_to_stdout___bfe(elem0);
            tasm::tasm_io_write_to_stdout___bfe(elem1);
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
    fn boxed_multiple_tuple_data_to_stack_test() {
        for _ in 0..2 {
            let rand: [u8; 100] = random();
            let enum_value = EnumType::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            let non_determinism = init_memory_from(&enum_value, BFieldElement::new(84));
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let entrypoint_location =
                EntrypointLocation::disk("enums", "boxed_multiple_tuple_data_to_stack", "main");
            let test_program = ozk_parsing::compile_for_test(
                &entrypoint_location,
                crate::ast_types::ListType::Unsafe,
            );

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
