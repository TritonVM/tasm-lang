use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

/// Field element arithmetic with inferred types, including `XFieldElement * BFieldElement`.
fn main() {
    let a = tasm::tasmlib_io_read_stdin___bfe();
    let b = tasm::tasmlib_io_read_stdin___bfe();
    let x = tasm::tasmlib_io_read_stdin___xfe();

    let sum = a + b;
    let product = a * b;
    let difference = a - b;
    let scaled = x * a;
    let shifted = x + b;
    let squared = x * x;
    let negated = -x;

    let lifted = a.lift();
    let combined = lifted * scaled + shifted - squared;

    tasm::tasmlib_io_write_to_stdout___bfe(sum);
    tasm::tasmlib_io_write_to_stdout___bfe(product);
    tasm::tasmlib_io_write_to_stdout___bfe(difference);
    tasm::tasmlib_io_write_to_stdout___xfe(scaled);
    tasm::tasmlib_io_write_to_stdout___xfe(negated);
    tasm::tasmlib_io_write_to_stdout___xfe(combined);
    tasm::tasmlib_io_write_to_stdout___bool(sum == a + b);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn inferred_field_elements_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..5)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint =
                EntrypointLocation::disk("type_inference", "inferred_field_elements", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
