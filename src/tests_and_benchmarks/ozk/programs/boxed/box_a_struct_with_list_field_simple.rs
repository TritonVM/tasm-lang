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
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_a_struct_with_list_field_simple() {
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "box_a_struct_with_list_field_simple", "main");
        let test_program =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);

        let stdin = vec![];
        let non_determinism = NonDeterminism::default();
        let expected_stack_diff = 0;
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap()
        .output;
        assert_eq!(native_output, vm_output);
    }
}
