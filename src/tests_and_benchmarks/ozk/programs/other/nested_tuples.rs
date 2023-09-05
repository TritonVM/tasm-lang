// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;
use twenty_first::shared_math::x_field_element::XFieldElement;

fn main() {
    let mut a: (BFieldElement, (XFieldElement, XFieldElement)) = (
        BFieldElement::new(300u64),
        (
            XFieldElement::new([
                BFieldElement::new(101),
                BFieldElement::new(102),
                BFieldElement::new(103),
            ]),
            XFieldElement::new([
                BFieldElement::new(401),
                BFieldElement::new(402),
                BFieldElement::new(403),
            ]),
        ),
    );

    tasm::tasm_io_write_to_stdout_xfe(a.1 .1);
    tasm::tasm_io_write_to_stdout_bfe(a.0);
    tasm::tasm_io_write_to_stdout_xfe(a.1 .0);
    tasm::tasm_io_write_to_stdout_xfe(a.1 .0);
    tasm::tasm_io_write_to_stdout_bfe(a.0);

    a.1 .0 = XFieldElement::new([
        BFieldElement::new(0xffff_0000_0001_0002),
        BFieldElement::new(0xffff_0000_0001_0003),
        BFieldElement::new(0xffff_0000_0001_0004),
    ]);
    a.0 = BFieldElement::new(900);
    tasm::tasm_io_write_to_stdout_bfe(a.0);
    tasm::tasm_io_write_to_stdout_xfe(a.1 .0);
    tasm::tasm_io_write_to_stdout_xfe(a.1 .0);

    return;
}

mod tests {
    use super::*;
    use itertools::Itertools;
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;

    #[test]
    fn nested_tuples_test() {
        let non_determinism = NonDeterminism::new(vec![]);
        let a_init = (
            BFieldElement::new(300),
            (
                XFieldElement::new(
                    (101u64..104)
                        .map(|x| x.into())
                        .collect_vec()
                        .try_into()
                        .unwrap(),
                ),
                XFieldElement::new(
                    (401u64..404)
                        .map(|x| x.into())
                        .collect_vec()
                        .try_into()
                        .unwrap(),
                ),
            ),
        );
        let a_final = (
            BFieldElement::new(900),
            (
                XFieldElement::new(
                    (0xffff_0000_0001_0002u64..0xffff_0000_0001_0005)
                        .map(|x| x.into())
                        .collect_vec()
                        .try_into()
                        .unwrap(),
                ),
                XFieldElement::new(
                    (401u64..404)
                        .map(|x| x.into())
                        .collect_vec()
                        .try_into()
                        .unwrap(),
                ),
            ),
        );

        let expected_output = [
            a_init.1 .1.encode(),
            a_init.0.encode(),
            a_init.1 .0.encode(),
            a_init.1 .0.encode(),
            a_init.0.encode(),
            a_final.0.encode(),
            a_final.1 .0.encode(),
            a_final.1 .0.encode(),
        ]
        .concat();
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test("other", "nested_tuples");
        println!("test_program is:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
