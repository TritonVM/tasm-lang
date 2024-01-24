use proptest::collection::vec;
use proptest::prelude::*;
use proptest_arbitrary_interop::arb;
use tasm_lib::DIGEST_LENGTH;
use test_strategy::proptest;
use triton_vm::BFieldElement;
use triton_vm::NonDeterminism;

use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows;
use crate::tests_and_benchmarks::test_helpers::shared_test::*;

use super::three_variants_type::*;

fn panic_on_variant_b_and_c() {
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

#[proptest(cases = 10)]
fn assert_no_panic_in_arm_body_a(input_element: u32) {
    let std_in = vec![input_element.into()];
    let rust_program = rust_shadows::wrap_main_with_io(&panic_on_variant_b_and_c);
    rust_program(std_in.clone(), NonDeterminism::default());

    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "panic_in_arm_body",
        "panic_on_variant_b_and_c",
    );
    TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
}

fn panic_on_variants_a_and_c() {
    let tv: ThreeVariants = ThreeVariants::B(0x2u128);
    let val: u32 = match tv {
        ThreeVariants::A => {
            //
            panic!()
        }
        ThreeVariants::B(inner) => {
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
fn assert_no_panic_in_arm_body_b() {
    let rust_program = rust_shadows::wrap_main_with_io(&panic_on_variants_a_and_c);
    let native_output = rust_program(vec![], NonDeterminism::default());

    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "panic_in_arm_body",
        "panic_on_variants_a_and_c",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    assert_eq!(native_output, vm_output.output);
}

fn panic_on_variants_a_and_b() {
    let tv: ThreeVariants = ThreeVariants::C(tasm::tasm_io_read_stdin___digest());
    let val: u32 = match tv {
        ThreeVariants::A => {
            //
            panic!()
        }
        ThreeVariants::B(inner) => {
            tasm::tasm_io_write_to_stdout___u128(inner);
            panic!()
        }
        ThreeVariants::C(digest) => {
            tasm::tasm_io_write_to_stdout___digest(digest);
            tasm::tasm_io_write_to_stdout___digest(digest);
            600
        }
    };

    tasm::tasm_io_write_to_stdout___u32(val);

    return;
}

#[proptest(cases = 10)]
fn assert_no_panic_in_arm_body_c(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
) {
    let rust_program = rust_shadows::wrap_main_with_io(&panic_on_variants_a_and_b);
    let native_output = rust_program(std_in.clone(), NonDeterminism::default());

    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "panic_in_arm_body",
        "panic_on_variants_a_and_b",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}
