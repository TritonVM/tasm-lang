use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

/// A free helper function, called from `main` and from another helper.
fn square(value: u64) -> u64 {
    value * value
}

fn sum_of_squares(a: u64, b: u64) -> u64 {
    square(a) + square(b)
}

fn factorial(n: u32) -> u64 {
    if n == 0 {
        1
    } else {
        n as u64 * factorial(n - 1)
    }
}

fn main() {
    let a = (tasm::tasmlib_io_read_stdin___u32() % 1000) as u64;
    let b = (tasm::tasmlib_io_read_stdin___u32() % 1000) as u64;

    // A nested helper function, which sees no local variables
    fn cube(value: u64) -> u64 {
        value * square(value)
    }

    tasm::tasmlib_io_write_to_stdout___u64(sum_of_squares(a, b));
    tasm::tasmlib_io_write_to_stdout___u64(cube(a % 1000));
    tasm::tasmlib_io_write_to_stdout___u64(factorial((b % 15) as u32));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn helper_functions_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..2)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint = EntrypointLocation::disk("general_rust", "helper_functions", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
