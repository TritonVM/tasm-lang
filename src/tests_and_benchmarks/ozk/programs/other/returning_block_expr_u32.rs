use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let i: u32 = tasm::tasm_io_read_stdin___bfe().value() as u32;
    let a: u32 = {
        let b: u32 = 100;
        let c: u32 = 200;
        let j: u32 = tasm::tasm_io_read_stdin___bfe().value() as u32;
        let k: u32 = tasm::tasm_io_read_stdin___bfe().value() as u32;
        let d: u32 = 400u32;

        b + c + d + i + j * k
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[cfg(test)]
mod test {

    use triton_vm::BFieldElement;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn returning_block_expr_u32_test() {
        // Test function on host machine
        let i: u32 = 15;
        let j: u32 = 16;
        let k: u32 = 32;
        let stdin = vec![
            BFieldElement::new(i as u64),
            BFieldElement::new(j as u64),
            BFieldElement::new(k as u64),
        ];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![BFieldElement::new(100 + 200 + 400 + 15 + 16 * 32)];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let (parsed, _, _) =
            ozk_parsing::parse_function_and_structs("other", "returning_block_expr_u32", "main");
        let expected_stack_diff = 0;
        let vm_output = execute_with_stack_and_ins_safe_lists(
            &parsed,
            vec![],
            stdin,
            NonDeterminism::new(vec![]),
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
