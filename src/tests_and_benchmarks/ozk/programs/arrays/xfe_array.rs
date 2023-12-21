use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;
use twenty_first::shared_math::x_field_element::XFieldElement;

fn main() {
    let k: BFieldElement = BFieldElement::new(1337);
    let a: Box<BFieldElement> = Box::<BFieldElement>::new(BFieldElement::new(100));
    let mut xfe_array: [XFieldElement; 4] = [
        XFieldElement::new([
            BFieldElement::new(45),
            BFieldElement::new(46),
            BFieldElement::new(47),
        ]),
        XFieldElement::new([
            BFieldElement::new(49),
            BFieldElement::new(50),
            BFieldElement::new(51),
        ]),
        XFieldElement::new([
            BFieldElement::new(52),
            BFieldElement::new(53),
            BFieldElement::new(54),
        ]),
        XFieldElement::new([
            BFieldElement::new(56),
            BFieldElement::new(57),
            BFieldElement::new(58),
        ]),
    ];

    let b: Box<BFieldElement> = Box::<BFieldElement>::new(BFieldElement::new(200));
    xfe_array[0] = tasm::tasm_io_read_stdin___xfe();
    xfe_array[1] = tasm::tasm_io_read_stdin___xfe();

    // Don't set element 2 to verify that indexing into
    // agrees with ordering of a array declaration as made above
    let l: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    xfe_array[3] = tasm::tasm_io_read_stdin___xfe();
    let c: BFieldElement = BFieldElement::new(400);

    tasm::tasm_io_write_to_stdout___xfe(xfe_array[3]);
    tasm::tasm_io_write_to_stdout___xfe(xfe_array[0]);
    tasm::tasm_io_write_to_stdout___xfe(xfe_array[2]);
    tasm::tasm_io_write_to_stdout___xfe(xfe_array[2]);
    tasm::tasm_io_write_to_stdout___xfe(xfe_array[1]);
    tasm::tasm_io_write_to_stdout___bfe(*a);
    tasm::tasm_io_write_to_stdout___bfe(*b);
    tasm::tasm_io_write_to_stdout___bfe(c);
    tasm::tasm_io_write_to_stdout___xfe(l);
    tasm::tasm_io_write_to_stdout___bfe(k);

    return;
}

mod tests {
    use super::*;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;
    use itertools::Itertools;
    use rand::random;
    use std::collections::HashMap;
    use triton_vm::{BFieldElement, NonDeterminism};
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    #[test]
    fn xfe_array_test() {
        let non_determinism = NonDeterminism::default();

        let xfes: [XFieldElement; 4] = random();
        let stdin = {
            let mut ret = vec![];
            for elem in xfes {
                let mut elem = elem.encode();
                elem.reverse();
                ret.append(&mut elem);
            }

            ret
        };

        let expected_output = vec![
            xfes[3].encode(),
            xfes[0].encode(),
            XFieldElement::new([
                BFieldElement::new(52),
                BFieldElement::new(53),
                BFieldElement::new(54),
            ])
            .encode(),
            XFieldElement::new([
                BFieldElement::new(52),
                BFieldElement::new(53),
                BFieldElement::new(54),
            ])
            .encode(),
            xfes[1].encode(),
            // a, b, c, l, k
            vec![BFieldElement::new(100)],
            vec![BFieldElement::new(200)],
            vec![BFieldElement::new(400)],
            xfes[2].encode(),
            vec![BFieldElement::new(1337)],
        ]
        .concat();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.to_vec(), non_determinism.clone());
        println!("native_output: {native_output:#?}");
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let test_program = ozk_parsing::compile_for_test(
            "arrays",
            "xfe_array",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
        println!("vm_output.output: {:#?}", vm_output.output);

        println!("Final stack is: {}", vm_output.final_stack.iter().join(","));
    }
}
