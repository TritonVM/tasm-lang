#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use triton_vm::{BFieldElement, NonDeterminism};

    fn main() {
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

        // let a: BFieldElement = match some_val {
        //     Some(val) => {
        //         //
        //         BFieldElement::new(val)
        //     }
        //     None => {
        //         //
        //         panic!()
        //     }
        // };

        tasm::tasm_io_write_to_stdout___bfe(a);

        return;
    }

    #[test]
    fn match_expr_very_simple_test() {
        rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("match_expr", "very_simple", "test::main");
        TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
    }
}
