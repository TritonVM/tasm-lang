use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(BFieldCodec)]
struct WithListFields {
    bfes: Vec<BFieldElement>,
    xfes: Vec<XFieldElement>,
}

#[allow(clippy::redundant_field_names)]
fn main() {
    let mut bfes: Vec<BFieldElement> = Vec::<BFieldElement>::default();
    let mut xfes: Vec<XFieldElement> = Vec::<XFieldElement>::default();
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
    use tasm_lib::twenty_first::shared_math::other::random_elements;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn box_a_struct_with_two_list_fields() {
        let stdin = random_elements(6);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("boxed", "box_a_struct_with_two_list_fields", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
