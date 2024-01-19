use triton_vm::twenty_first::shared_math::x_field_element::XFieldElement;
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

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

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
    use triton_vm::BFieldElement;
    use triton_vm::NonDeterminism;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use super::*;

    #[proptest(cases = 20)]
    fn xfe_array_test(#[strategy(arb())] xfes: [XFieldElement; 4]) {
        let std_in = xfes
            .map(|xfe| xfe.encode().into_iter().rev())
            .into_iter()
            .flatten()
            .collect_vec();
        let non_determinism = NonDeterminism::default();

        let native_output = rust_shadows::wrap_main_with_io(&main)(std_in.clone(), non_determinism);
        println!("native_output: {native_output:#?}");

        let expected_output = vec![
            xfes[3].encode(),
            xfes[0].encode(),
            XFieldElement::new([52, 53, 54].map(BFieldElement::new)).encode(),
            XFieldElement::new([52, 53, 54].map(BFieldElement::new)).encode(),
            xfes[1].encode(),
            // a, b, c, l, k
            [100, 200, 400].map(BFieldElement::new).to_vec(),
            xfes[2].encode(),
            vec![BFieldElement::new(1337)],
        ]
        .concat();
        assert_eq!(native_output, expected_output);

        let entrypoint_location = EntrypointLocation::disk("arrays", "xfe_array", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .with_std_in(std_in)
            .expect_stack_difference(0)
            .execute()
            .unwrap();

        assert_eq!(expected_output, vm_output.output);
        println!("vm_output.output: {:#?}", vm_output.output);

        println!("Final stack is: {}", vm_output.final_stack.iter().join(","));
    }
}
