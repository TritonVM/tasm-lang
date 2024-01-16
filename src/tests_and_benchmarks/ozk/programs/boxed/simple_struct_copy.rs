use tasm_lib::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
struct SimpleStruct {
    a: u32,
    b: Digest,
    c: XFieldElement,
}

fn main() {
    let struct_0: SimpleStruct = SimpleStruct {
        a: 403,
        b: tasm::tasm_io_read_stdin___digest(),
        c: tasm::tasm_io_read_stdin___xfe(),
    };
    let boxed_struct_0: Box<SimpleStruct> = Box::<SimpleStruct>::new(struct_0);
    tasm::tasm_io_write_to_stdout___u32(boxed_struct_0.a);
    tasm::tasm_io_write_to_stdout___digest(boxed_struct_0.b);
    tasm::tasm_io_write_to_stdout___xfe(boxed_struct_0.c);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

    #[test]
    fn boxed_simple_copy_struct() {
        let non_determinism = NonDeterminism::default();
        let stdin = random_elements(8);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "simple_struct_copy",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
