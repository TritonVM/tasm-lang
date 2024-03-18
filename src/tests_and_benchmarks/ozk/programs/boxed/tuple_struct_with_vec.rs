use arbitrary::Arbitrary;
use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Arbitrary, BFieldCodec, Clone, Debug)]
struct TupleStruct2(Vec<XFieldElement>, u32); // TODO: Use (u128, XFieldElement) here instead

fn main() {
    let boxed_struct: Box<TupleStruct2> =
        TupleStruct2::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let on_stack: TupleStruct2 = *boxed_struct;

    tasm::tasm_io_write_to_stdout___u32(on_stack.1);
    tasm::tasm_io_write_to_stdout___u32(on_stack.0.len() as u32);
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
    fn move_boxed_tuple_struct_with_vec_test() {
        for _ in 0..2 {
            let rand: [u8; 100] = random();
            let struct_value = TupleStruct2::arbitrary(&mut Unstructured::new(&rand)).unwrap();
            println!("struct_value: {struct_value:#?}");
            println!(
                "struct_value.encoded: {}",
                struct_value.encode().iter().join(", ")
            );
            let non_determinism = init_memory_from(&struct_value, BFieldElement::new(84));
            {
                let mut ram: Vec<(BFieldElement, BFieldElement)> =
                    non_determinism.ram.clone().into_iter().collect();
                ram.sort_unstable_by_key(|(p, _v)| p.value());
                println!(
                    "{}",
                    ram.iter().map(|(p, v)| format!("{p} => {v}")).join(", ")
                );
            }
            let stdin = vec![];

            // Run program on host machine
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

            // Run test on Triton-VM
            let entrypoint_location =
                EntrypointLocation::disk("boxed", "tuple_struct_with_vec", "main");
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
                    "native_output:\n{}\nVM output:\n{}. Code was:\n{}\nrand was {}\n",
                    native_output.iter().join(", "),
                    vm_output.public_output.iter().join(", "),
                    test_program.iter().join("\n"),
                    rand.iter().join(",")
                );
            }
        }
    }
}
