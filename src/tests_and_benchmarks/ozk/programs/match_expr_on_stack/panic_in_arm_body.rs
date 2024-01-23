use super::three_variants_type::*;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use rand::random;
    use triton_vm::{BFieldElement, NonDeterminism};

    fn a() {
        let input: u32 = tasm::tasm_io_read_stdin___u32();
        let tv: ThreeVariants = ThreeVariants::A;
        let val: u32 = match tv {
            ThreeVariants::A => {
                //
                input
            }
            ThreeVariants::B(_) => {
                //
                panic!()
            }
            ThreeVariants::C(_) => {
                //
                panic!()
            }
        };

        tasm::tasm_io_write_to_stdout___u32(val);

        return;
    }

    #[test]
    fn panic_in_arm_body_a() {
        let input: Vec<u32> = vec![random()];
        let input: Vec<BFieldElement> = input
            .into_iter()
            .map(|x| BFieldElement::new(x as u64))
            .collect();
        rust_shadows::wrap_main_with_io(&a)(input.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("match_expr_on_stack", "panic_in_arm_body", "test::a");
        TritonVMTestCase::new(entrypoint)
            .with_std_in(input)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
    }
}
