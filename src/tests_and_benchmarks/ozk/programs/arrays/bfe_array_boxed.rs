use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    // let a: BFieldElement = BFieldElement::new(100);
    let bfe_array_boxed: Box<[BFieldElement; 4]> = {
        let bfe_array: [BFieldElement; 4] = [
            BFieldElement::new(1000),
            BFieldElement::new(1001),
            BFieldElement::new(1002),
            BFieldElement::new(1003),
        ];

        Box::<[BFieldElement; 4]>::new(bfe_array)
    };

    // let b: BFieldElement = BFieldElement::new(200);
    // bfe_array_boxed[0] = tasm::tasmlib_io_read_stdin___bfe();
    // bfe_array_boxed[1] = tasm::tasmlib_io_read_stdin___bfe();

    // Don't set element 2 to verify that indexing into
    // agrees with ordering of a array declaration as made above
    // let _l: BFieldElement = tasm::tasmlib_io_read_stdin___bfe();
    // bfe_array_boxed[3] = tasm::tasmlib_io_read_stdin___bfe();
    // let c: BFieldElement = BFieldElement::new(400);

    tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[2]);
    // tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[0]);
    // tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[0]);
    // tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[2]);
    // tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[3]);
    // let d: BFieldElement = BFieldElement::new(1u64 << 50);
    // tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[3]);
    // tasm::tasmlib_io_write_to_stdout___bfe(bfe_array_boxed[0]);
    // tasm::tasmlib_io_write_to_stdout___bfe(a);
    // tasm::tasmlib_io_write_to_stdout___bfe(b);
    // tasm::tasmlib_io_write_to_stdout___bfe(c);
    // tasm::tasmlib_io_write_to_stdout___bfe(d);

    return;
}

#[cfg(test)]
mod test {

    use itertools::Itertools;
    use rand::random;
    use tasm_lib::triton_vm::prelude::*;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[ignore = "Doesn't work yet, as we can't move pointer types to memory"]
    #[test]
    fn bfe_array_boxed_test() {
        let non_determinism = NonDeterminism::default();

        let stdin: [BFieldElement; 10] = random();
        let stdin = stdin.to_vec();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.to_vec(), non_determinism.clone());

        let expected_output = vec![BFieldElement::new(1002)];
        assert_eq!(native_output, expected_output);

        let entrypoint = ozk_parsing::EntrypointLocation::disk("arrays", "bfe_array_boxed", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .with_non_determinism(non_determinism)
            .execute()
            .unwrap();

        if expected_output != vm_output.public_output {
            panic!(
                "expected_output:\n{}, got:\n{}",
                expected_output.iter().join(", "),
                vm_output.public_output.iter().join(", "),
            );
        }
    }
}
