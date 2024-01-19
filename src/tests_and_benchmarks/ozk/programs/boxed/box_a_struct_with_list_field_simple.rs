use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(BFieldCodec)]
struct WithListField {
    bfes: Vec<BFieldElement>,
}

fn main() {
    let mut list: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    list.push(BFieldElement::new(102));
    list.push(BFieldElement::new(2222));
    list.push(BFieldElement::new(3333333));
    let wlf: WithListField = WithListField { bfes: list };
    let boxed: Box<WithListField> = Box::<WithListField>::new(wlf);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[0]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[1]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[2]);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_a_struct_with_list_field_simple() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("boxed", "box_a_struct_with_list_field_simple", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
