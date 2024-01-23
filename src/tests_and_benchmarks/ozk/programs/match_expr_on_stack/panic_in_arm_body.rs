use super::three_variants_type::*;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use rand::random;
    use tasm_lib::DIGEST_LENGTH;
    use triton_vm::{BFieldElement, NonDeterminism};
    use twenty_first::shared_math::other::random_elements;

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

    fn b() {
        let tv: ThreeVariants = ThreeVariants::B(0x1000_0000_0000_0000u128);
        let val: u32 = match tv {
            ThreeVariants::A => {
                //
                panic!()
            }
            ThreeVariants::B(inner) => {
                //
                tasm::tasm_io_write_to_stdout___u128(inner);
                200
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
    fn panic_in_arm_body_b() {
        rust_shadows::wrap_main_with_io(&b)(vec![], NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("match_expr_on_stack", "panic_in_arm_body", "test::b");
        TritonVMTestCase::new(entrypoint)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
    }

    fn c() {
        let tv: ThreeVariants = ThreeVariants::C(tasm::tasm_io_read_stdin___digest());
        let val: u32 = match tv {
            ThreeVariants::A => {
                //
                panic!()
            }
            ThreeVariants::B(inner) => {
                //
                tasm::tasm_io_write_to_stdout___u128(inner);
                panic!()
            }
            ThreeVariants::C(digest) => {
                //
                tasm::tasm_io_write_to_stdout___digest(digest);
                tasm::tasm_io_write_to_stdout___digest(digest);
                600
            }
        };

        tasm::tasm_io_write_to_stdout___u32(val);

        return;
    }

    #[test]
    fn panic_in_arm_body_c() {
        let input: Vec<BFieldElement> = random_elements(DIGEST_LENGTH);
        rust_shadows::wrap_main_with_io(&c)(input.clone(), NonDeterminism::default());
        let entrypoint =
            EntrypointLocation::disk("match_expr_on_stack", "panic_in_arm_body", "test::b");
        TritonVMTestCase::new(entrypoint)
            .with_std_in(input)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
    }
}
