use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let a: BFieldElement = BFieldElement::new(17u64);
    let a_u64: u64 = a.value();
    let b: BFieldElement = BFieldElement::new(a_u64);
    tasm::tasm_io_write_to_stdout___bfe(b);

    let c: BFieldElement = BFieldElement::new((1 << 32) + 17u64);
    let c_u64: u64 = c.value();
    let d: BFieldElement = BFieldElement::new(c_u64);
    tasm::tasm_io_write_to_stdout___bfe(d);
    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn value_test() {
        // Test function on host machine
        let stdin: Vec<BFieldElement> = vec![];
        let non_determinism: NonDeterminism<BFieldElement> = NonDeterminism::default();
        let native_output = rust_shadows::wrap_main_with_io(&main)(stdin, non_determinism);

        let expected_output = [17, (1 << 32) + 17].map(BFieldElement::new).to_vec();
        assert_eq!(native_output, expected_output);

        let entrypoint = EntrypointLocation::disk("other", "value", "main");
        let vm_output = TritonVMTestCase::new(entrypoint).execute().unwrap();

        assert_eq!(expected_output, *vm_output.public_output);
    }
}
