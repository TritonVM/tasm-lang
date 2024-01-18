use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::BFieldElement;
use twenty_first::shared_math::{bfield_codec::BFieldCodec, x_field_element::XFieldElement};

#[derive(BFieldCodec)]
struct WithListFields {
    bfes: Vec<BFieldElement>,
    xfes: Vec<XFieldElement>,
}

#[allow(clippy::redundant_field_names)]
fn main() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(32);
    let mut xfes: Vec<XFieldElement> = Vec::<XFieldElement>::with_capacity(32);
    bfes.push(BFieldElement::new(102));
    bfes.push(BFieldElement::new(2222));
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    xfes.push(tasm::tasm_io_read_stdin___xfe());
    let wlf: WithListFields = WithListFields {
        bfes: bfes,
        xfes: xfes,
    };
    let boxed: Box<WithListFields> = Box::<WithListFields>::new(wlf);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[0]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[0]);
    tasm::tasm_io_write_to_stdout___xfe(boxed.xfes[1]);
    tasm::tasm_io_write_to_stdout___bfe(boxed.bfes[1]);

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::{ozk_parsing, rust_shadows};
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use itertools::Itertools;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::other::random_elements;

    #[test]
    fn box_a_struct_with_two_list_fields() {
        let entrypoint_location =
            EntrypointLocation::disk("boxed", "box_a_struct_with_two_list_fields", "main");
        let code =
            ozk_parsing::compile_for_test(&entrypoint_location, crate::ast_types::ListType::Unsafe);
        println!("code:\n{}", code.iter().join("\n"));
        let stdin = random_elements(6);
        let non_determinism = NonDeterminism::default();
        let expected_stack_diff = 0;
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &code,
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
