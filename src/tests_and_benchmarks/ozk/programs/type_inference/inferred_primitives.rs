use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

/// No type annotations on any local binding; all types are inferred by `rustc`.
fn main() {
    let a = tasm::tasmlib_io_read_stdin___u32() % 1_000_000;
    let b = tasm::tasmlib_io_read_stdin___u64();
    let flag = tasm::tasmlib_io_read_stdin___u32() % 2 == 1;

    let mut acc = 0;
    let mut i = 0;
    while i < 10 {
        acc += a % 7 + i;
        i += 1;
    }

    let wide = b / 3 + 1;
    let narrow = a >> 3;
    let mixed = (wide % 1_000_000) as u32 + narrow;

    let chosen = if flag { acc } else { mixed };

    tasm::tasmlib_io_write_to_stdout___u32(acc);
    tasm::tasmlib_io_write_to_stdout___u64(wide);
    tasm::tasmlib_io_write_to_stdout___u32(mixed);
    tasm::tasmlib_io_write_to_stdout___u32(chosen);
    tasm::tasmlib_io_write_to_stdout___bool(chosen == acc);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn inferred_primitives_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..4)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint =
                EntrypointLocation::disk("type_inference", "inferred_primitives", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
