use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn classify(value: u32) -> u32 {
    let class = if value < 10 {
        0
    } else if value < 100 {
        1
    } else if value < 1000 {
        2
    } else {
        3
    };

    return class;
}

fn main() {
    let value = tasm::tasmlib_io_read_stdin___u32() % 5000;

    let class = classify(value);
    let description = if class == 0 {
        100
    } else if class == 1 {
        200
    } else if class == 2 {
        300
    } else {
        400
    };

    tasm::tasmlib_io_write_to_stdout___u32(class);
    tasm::tasmlib_io_write_to_stdout___u32(description);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn else_if_chains_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..1)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint = EntrypointLocation::disk("general_rust", "else_if_chains", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
