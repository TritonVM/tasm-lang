use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

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
    tasm::tasm_io_write_to_stdout_u32(test_struct.a.b);

    return;
}

mod tests {
    use std::collections::HashMap;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::{
        execute_compiled_with_stack_memory_and_ins_for_test, init_memory_from,
    };

    use super::*;
    use itertools::Itertools;

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

        let test_program = ozk_parsing::compile_for_test("structs", "simple_nested_struct");
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();

        assert_eq!(expected_output, vm_output.output);
    }
}
