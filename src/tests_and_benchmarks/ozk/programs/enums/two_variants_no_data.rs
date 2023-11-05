pub enum SimpleEnum {
    A,
    B,
}

fn main() {
    let a: SimpleEnum = SimpleEnum::A;
    let b: SimpleEnum = SimpleEnum::B;

    match a {
        SimpleEnum::A => {
            assert!(true);
        }
        SimpleEnum::B => {
            assert!(false);
        }
    };

    match b {
        SimpleEnum::A => {
            assert!(false);
        }
        SimpleEnum::B => {
            assert!(true);
        }
    };

    match a {
        _ => {
            assert!(true);
        }
    };

    match a {
        SimpleEnum::A => {
            assert!(true);
        }
        _ => {
            assert!(false);
        }
    };

    return;
}

mod tests {
    use super::*;
    use itertools::Itertools;
    use std::collections::HashMap;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;

    #[test]
    fn two_variants_no_data_test() {
        // let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let non_determinism = NonDeterminism::default();
        let expected_output = vec![];
        let std_in = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "enums",
            "two_variants_no_data",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
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
