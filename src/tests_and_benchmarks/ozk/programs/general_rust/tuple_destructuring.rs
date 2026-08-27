use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn div_rem(dividend: u32, divisor: u32) -> (u32, u32) {
    return (dividend / divisor, dividend % divisor);
}

fn main() {
    let dividend = tasm::tasmlib_io_read_stdin___u32() % 1_000_000;
    let divisor = tasm::tasmlib_io_read_stdin___u32() % 100 + 1;

    let (quotient, remainder) = div_rem(dividend, divisor);
    let (a, (b, c)) = (quotient + 1, (remainder * 2, quotient == remainder));
    let Digest([d0, _d1, _d2, _d3, d4]) = tasm::tasmlib_io_read_stdin___digest();

    tasm::tasmlib_io_write_to_stdout___u32(quotient);
    tasm::tasmlib_io_write_to_stdout___u32(remainder);
    tasm::tasmlib_io_write_to_stdout___u32(a);
    tasm::tasmlib_io_write_to_stdout___u32(b);
    tasm::tasmlib_io_write_to_stdout___bool(c);
    tasm::tasmlib_io_write_to_stdout___bfe(d0 + d4);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn tuple_destructuring_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..7)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint =
                EntrypointLocation::disk("general_rust", "tuple_destructuring", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
