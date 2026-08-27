use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

/// Lists whose element types are inferred from their use.
#[allow(clippy::len_zero)] // `is_empty` is not supported by the compiler
fn main() {
    let count = tasm::tasmlib_io_read_stdin___u32() % 20;

    let mut bfes = Vec::new();
    let mut squares = Vec::default();
    let mut i = 0;
    while i < count {
        let element = tasm::tasmlib_io_read_stdin___bfe();
        bfes.push(element);
        squares.push(element * element);
        i += 1;
    }

    let mut total = BFieldElement::new(0);
    let mut j = 0;
    while j < bfes.len() {
        total += bfes[j] + squares[j];
        j += 1;
    }

    tasm::tasmlib_io_write_to_stdout___u32(bfes.len() as u32);
    tasm::tasmlib_io_write_to_stdout___bfe(total);
    if bfes.len() > 0 {
        tasm::tasmlib_io_write_to_stdout___bfe(bfes[bfes.len() - 1]);
        let last = squares.pop().unwrap();
        tasm::tasmlib_io_write_to_stdout___bfe(last);
    }

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn inferred_vectors_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..21)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint = EntrypointLocation::disk("type_inference", "inferred_vectors", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
