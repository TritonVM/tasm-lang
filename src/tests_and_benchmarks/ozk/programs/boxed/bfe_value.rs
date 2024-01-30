use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let a: BFieldElement = tasm::tasm_io_read_stdin___bfe();
    let a_value: u64 = a.value();
    let boxed: Box<BFieldElement> = Box::<BFieldElement>::new(a);
    let boxed_value: u64 = boxed.value();

    assert!(a_value == boxed_value);

    return;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn method_on_boxed_bfe_gets_right_value() {
        let std_in = vec![BFieldElement::new(100)];
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("boxed", "bfe_value", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
