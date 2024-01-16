#![allow(clippy::needless_borrow)]
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let mut list: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    list.push(BFieldElement::new(102));
    let boxed_list: Box<Vec<BFieldElement>> = Box::<Vec<BFieldElement>>::new(list);
    tasm::tasm_io_write_to_stdout___bfe(boxed_list[0]);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_a_list_simple() {
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "box_a_list_simple",
            "main",
            crate::ast_types::ListType::Unsafe,
        );

        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_stack_diff = 0;
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap()
        .output;
        assert_eq!(native_output, vm_output);
    }
}
