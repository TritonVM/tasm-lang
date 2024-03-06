use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::BFieldCodec;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(TasmObject, BFieldCodec)]
struct InnerStruct {
    b: u32,
}

#[derive(TasmObject, BFieldCodec)]
struct NestedStruct {
    a: InnerStruct,
}

fn main() {
    let test_struct: Box<NestedStruct> =
        NestedStruct::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();
    tasm::tasm_io_write_to_stdout___u32(test_struct.a.b);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    impl NestedStruct {
        fn new(inner_val: u32) -> NestedStruct {
            NestedStruct {
                a: InnerStruct { b: inner_val },
            }
        }
    }

    #[test]
    fn simple_nested_structs_test() {
        let ts = NestedStruct::new(2023);
        let non_determinism = init_memory_from(&ts, BFieldElement::new(300));

        let expected_output = vec![BFieldElement::new(2023)];
        let stdin = vec![];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(expected_output, native_output);

        let entrypoint_location =
            EntrypointLocation::disk("structs", "simple_nested_struct", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        assert_eq!(expected_output, vm_output.output);
    }
}
