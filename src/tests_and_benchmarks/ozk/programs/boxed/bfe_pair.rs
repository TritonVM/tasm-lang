use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // Store two BFieldElements in memory. Then read them out again.
    let a: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    let b: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    let boxed_ab: Box<(BFieldElement, BFieldElement)> =
        Box::<(BFieldElement, BFieldElement)>::new((a, b));
    let e: (BFieldElement, BFieldElement) = *boxed_ab;
    assert!(a == e.0);
    assert!(b == e.1);

    tasm::tasmlib_io_write_to_stdout___bfe(e.1);
    tasm::tasmlib_io_write_to_stdout___bfe(e.0);
    tasm::tasmlib_io_write_to_stdout___bfe(e.0);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use rand::random;

    #[test]
    fn boxed_bfe_pair_test() {
        let a: BFieldElement = random();
        let b: BFieldElement = random();
        let stdin = vec![a, b];
        let expected_output = vec![b, a, a];

        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), NonDeterminism::default());
        assert_eq!(native_output, expected_output);

        let entrypoint = EntrypointLocation::disk("boxed", "bfe_pair", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .execute()
            .unwrap();
        assert_eq!(expected_output, vm_output.public_output);
    }
}
