use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

struct Counter {
    count: u32,
    total: u64,
    weight: BFieldElement,
}

#[allow(clippy::vec_init_then_push)] // `vec![]` is not supported by the compiler
fn main() {
    let steps = tasm::tasmlib_io_read_stdin___u32() % 50;
    let weight = tasm::tasmlib_io_read_stdin___bfe();

    let mut counter = Counter {
        count: 0,
        total: 0,
        weight: BFieldElement::new(1),
    };
    let mut values = Vec::new();
    values.push(0u64);
    values.push(0u64);

    let mut i = 0;
    while i < steps {
        counter.count += 1;
        counter.total += i as u64 * 3;
        counter.weight *= weight;
        values[(i % 2) as usize] += i as u64;
        i += 1;
    }
    counter.total -= 1;
    counter.total <<= 1;

    tasm::tasmlib_io_write_to_stdout___u32(counter.count);
    tasm::tasmlib_io_write_to_stdout___u64(counter.total);
    tasm::tasmlib_io_write_to_stdout___bfe(counter.weight);
    tasm::tasmlib_io_write_to_stdout___u64(values[0]);
    tasm::tasmlib_io_write_to_stdout___u64(values[1]);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn compound_assignment_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..2)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint =
                EntrypointLocation::disk("general_rust", "compound_assignment", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
