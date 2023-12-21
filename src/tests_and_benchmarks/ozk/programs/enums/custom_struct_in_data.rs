use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

struct CustomStruct(u32);

enum SimpleEnum {
    A(CustomStruct),
}

fn main() {
    let a: SimpleEnum = SimpleEnum::A(CustomStruct(100));
    #[allow(unused_assignments)]
    let mut output: u32 = 0;
    match a {
        SimpleEnum::A(value) => {
            output = value.0;
        }
    };
    tasm::tasm_io_write_to_stdout___u32(output);

    return;
}

mod tests {
    use super::*;
    use itertools::Itertools;
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;

    #[test]
    fn custom_struct_in_enum_data_test() {
        let non_determinism = NonDeterminism::default();
        let expected_output = vec![BFieldElement::new(100)];
        let std_in = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "enums",
            "custom_struct_in_data",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            std_in,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.output);
        if expected_output != vm_output.output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
