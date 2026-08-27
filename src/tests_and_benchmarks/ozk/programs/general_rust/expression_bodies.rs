use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
enum Op {
    Add,
    Mul,
    Max,
}

/// Match arms without braces, implicit returns, and blocks as expressions.
fn apply(op: Op, lhs: u32, rhs: u32) -> u32 {
    match op {
        Op::Add => lhs + rhs,
        Op::Mul => lhs * rhs,
        Op::Max => {
            if lhs > rhs {
                lhs
            } else {
                rhs
            }
        }
    }
}

#[allow(clippy::let_and_return)] // the block-with-binding is the point of this test
fn main() {
    let lhs = tasm::tasmlib_io_read_stdin___u32() % 1000;
    let rhs = tasm::tasmlib_io_read_stdin___u32() % 1000;
    let selector = tasm::tasmlib_io_read_stdin___u32() % 3;

    let op = if selector == 0 {
        Op::Add
    } else if selector == 1 {
        Op::Mul
    } else {
        Op::Max
    };

    let result = apply(op, lhs, rhs);
    let doubled = {
        let tmp = result + result;
        tmp
    };

    tasm::tasmlib_io_write_to_stdout___u32(result);
    tasm::tasmlib_io_write_to_stdout___u32(doubled);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn expression_bodies_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..3)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint = EntrypointLocation::disk("general_rust", "expression_bodies", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
