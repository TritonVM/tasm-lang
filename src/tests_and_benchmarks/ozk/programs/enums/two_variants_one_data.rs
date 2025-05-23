use tasm_lib::twenty_first::prelude::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Debug)]
pub(crate) enum SimpleEnum {
    A(XFieldElement),
    B,
}

fn main() {
    let a: SimpleEnum = SimpleEnum::A(tasm::tasmlib_io_read_stdin___xfe());
    let b: SimpleEnum = SimpleEnum::B;

    match b {
        SimpleEnum::A(_xfe) => {
            panic!();
        }
        SimpleEnum::B => {}
    };

    match a {
        SimpleEnum::A(xfe) => {
            let i: u32 = 14;
            tasm::tasmlib_io_write_to_stdout___xfe(xfe);
            tasm::tasmlib_io_write_to_stdout___u32(i);
            tasm::tasmlib_io_write_to_stdout___xfe(xfe);

            match a {
                SimpleEnum::A(xfe_1) => {
                    tasm::tasmlib_io_write_to_stdout___xfe(xfe_1);
                    tasm::tasmlib_io_write_to_stdout___u32(i);
                    tasm::tasmlib_io_write_to_stdout___xfe(xfe);
                    tasm::tasmlib_io_write_to_stdout___xfe(xfe_1);
                }
                SimpleEnum::B => {
                    panic!();
                }
            };
        }
        SimpleEnum::B => {
            panic!();
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::random;
    use tasm_lib::triton_vm::prelude::*;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    #[test]
    fn two_variants_one_data_test() {
        let non_determinism = NonDeterminism::default();
        let random_xfe: XFieldElement = random();
        let mut std_in = random_xfe.encode();
        std_in.reverse();
        let expected_output = [
            random_xfe.encode(),
            vec![BFieldElement::new(14)],
            random_xfe.encode(),
            random_xfe.encode(),
            vec![BFieldElement::new(14)],
            random_xfe.encode(),
            random_xfe.encode(),
        ]
        .concat();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("enums", "two_variants_one_data", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            std_in,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.public_output);
        if expected_output != vm_output.public_output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.public_output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
