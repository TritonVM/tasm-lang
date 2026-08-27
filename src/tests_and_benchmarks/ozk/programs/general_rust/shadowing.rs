use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

/// Variables can be shadowed, also with a different type.
fn main() {
    let value = tasm::tasmlib_io_read_stdin___u32();
    let value = value as u64 * 3;
    let value = value + 7;
    let value = BFieldElement::new(value);
    let value = value * value;

    let other = 5;
    {
        let other = other + 1;
        tasm::tasmlib_io_write_to_stdout___u32(other);
    }
    tasm::tasmlib_io_write_to_stdout___u32(other);
    tasm::tasmlib_io_write_to_stdout___bfe(value);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn shadowing_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..1)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint = EntrypointLocation::disk("general_rust", "shadowing", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
