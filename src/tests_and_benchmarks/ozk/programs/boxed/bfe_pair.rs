use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // Store two BFieldElements in memory. Then read them out again.
    let a: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let b: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let boxed_ab: Box<(BFieldElement, BFieldElement)> =
        Box::<(BFieldElement, BFieldElement)>::new((a, b));
    let e: (BFieldElement, BFieldElement) = *boxed_ab;
    assert!(a == e.0);
    assert!(b == e.1);

    tasm::tasm_io_write_to_stdout___bfe(e.1);
    tasm::tasm_io_write_to_stdout___bfe(e.0);
    tasm::tasm_io_write_to_stdout___bfe(e.0);

    return;
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn boxed_bfe_pair_test() {
        // Test function on host machine
        let a: BFieldElement = random();
        let b: BFieldElement = random();
        let stdin = vec![a, b];
        let non_determinism = NonDeterminism::default();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        let expected_output = vec![b, a, a];
        assert_eq!(native_output, expected_output);

        let entrypoint = EntrypointLocation::disk("boxed", "bfe_pair", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        if expected_output != vm_output.output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.output.iter().join(",")
            );
        }
    }
}
