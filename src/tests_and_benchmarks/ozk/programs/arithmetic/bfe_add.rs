use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let a: BFieldElement = BFieldElement::new(14);
    let b: BFieldElement = BFieldElement::new(15);
    let c: BFieldElement = a + b;
    tasm::tasmlib_io_write_to_stdout___bfe(c);

    let d: u64 = 1001 + (1u64 << 32);
    let e: BFieldElement = BFieldElement::new(d);
    tasm::tasmlib_io_write_to_stdout___bfe(e);

    let f: u64 = 1001000 + (1u64 << 32);
    let g: BFieldElement = BFieldElement::new(f);
    tasm::tasmlib_io_write_to_stdout___bfe(g);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn add_bfe_ozk_test_same_file() {
        // Test function on host machine
        let stdin = vec![];
        let non_determinism = NonDeterminism::new(vec![]);
        let expected_output = vec![
            BFieldElement::new(29),
            BFieldElement::new(1001 + (1u64 << 32)),
            BFieldElement::new(1001000 + (1u64 << 32)),
        ];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        let entrypoint = EntrypointLocation::disk("arithmetic", "bfe_add", "main");
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();

        assert_eq!(expected_output, vm_output.public_output);
    }
}
