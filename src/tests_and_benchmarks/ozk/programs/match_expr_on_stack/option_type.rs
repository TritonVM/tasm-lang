#[cfg(test)]
mod test {
    use num::One;
    use num::Zero;
    use tasm_lib::triton_vm::prelude::*;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use tasm_lib::twenty_first::prelude::XFieldElement;

    fn most_basic() {
        let some_val: Option<u64> = Some(100);
        let a: BFieldElement = match some_val {
            Some(val) => {
                //
                BFieldElement::new(val)
            }
            None => {
                //
                BFieldElement::new(0)
            }
        };

        tasm::tasmlib_io_write_to_stdout___bfe(a);
        return;
    }

    #[test]
    fn match_expr_very_simple_most_basic_test() {
        rust_shadows::wrap_main_with_io(&most_basic)(vec![], NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("match_expr_on_stack", "option_type", "test::most_basic");
        TritonVMTestCase::new(entrypoint).execute().unwrap();
    }

    fn with_bindings_in_arm_body_0() {
        let some_val: Option<u64> = Some(100);
        let a: BFieldElement = match some_val {
            Some(val) => {
                //
                let new_val: u64 = val + val;
                BFieldElement::new(new_val)
            }
            None => {
                //
                let new_val: u64 = 1u64;
                BFieldElement::new(new_val)
            }
        };

        tasm::tasmlib_io_write_to_stdout___bfe(a);
        return;
    }

    #[test]
    fn match_expr_with_bindings_in_arm_body_0_test() {
        rust_shadows::wrap_main_with_io(&with_bindings_in_arm_body_0)(
            vec![],
            NonDeterminism::default(),
        );
        let entrypoint = EntrypointLocation::disk(
            "match_expr_on_stack",
            "option_type",
            "test::with_bindings_in_arm_body_0",
        );
        TritonVMTestCase::new(entrypoint).execute().unwrap();
    }

    fn with_bindings_in_arm_body_1() {
        let some_val: Option<XFieldElement> = Some(XFieldElement::one());
        let a: XFieldElement = match some_val {
            Some(val) => {
                let new_val: XFieldElement = val + val;
                let new_new_val: XFieldElement = new_val + val;
                let new_new_new_val: XFieldElement = new_val + val + new_new_val;
                new_new_new_val + val
            }
            None => {
                let _new_val: u64 = 1u64;
                XFieldElement::zero()
            }
        };

        tasm::tasmlib_io_write_to_stdout___xfe(a);
        return;
    }

    #[test]
    fn match_expr_with_bindings_in_arm_body_1_test() {
        rust_shadows::wrap_main_with_io(&with_bindings_in_arm_body_1)(
            vec![],
            NonDeterminism::default(),
        );
        let entrypoint = EntrypointLocation::disk(
            "match_expr_on_stack",
            "option_type",
            "test::with_bindings_in_arm_body_1",
        );
        TritonVMTestCase::new(entrypoint).execute().unwrap();
    }
}
