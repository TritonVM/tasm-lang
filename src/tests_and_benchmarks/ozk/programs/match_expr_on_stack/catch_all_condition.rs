use super::three_variants_type::*;
use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows;
use crate::tests_and_benchmarks::test_helpers::shared_test::*;
use tasm_lib::triton_vm::prelude::*;

use proptest::collection::vec;
use proptest::prop_assert_eq;
use proptest_arbitrary_interop::arb;
use tasm_lib::DIGEST_LENGTH;
use test_strategy::proptest;

fn catch_all_covers_a() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();

    let a: u32 = match val {
        ThreeVariants::C(digest) => {
            tasm::tasm_io_write_to_stdout___u32(101);
            let b: u64 = 100;
            tasm::tasm_io_write_to_stdout___u32(101);
            tasm::tasm_io_write_to_stdout___digest(digest);

            2 * b as u32
        }
        ThreeVariants::B(inner) => {
            tasm::tasm_io_write_to_stdout___u32(100);
            let b: u32 = 50;
            tasm::tasm_io_write_to_stdout___u32(100);
            tasm::tasm_io_write_to_stdout___u128(inner);

            b * 2
        }
        _ => {
            tasm::tasm_io_write_to_stdout___u32(102);
            let b: u64 = 200;
            tasm::tasm_io_write_to_stdout___u32(102);

            2 * b as u32
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_a_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_a)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_a",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}

fn catch_all_covers_b() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();
    let a: u32 = match val {
        ThreeVariants::A => {
            tasm::tasm_io_write_to_stdout___u32(100);
            let b: u32 = 50;
            tasm::tasm_io_write_to_stdout___u32(100);

            b * 2
        }
        ThreeVariants::C(digest) => {
            tasm::tasm_io_write_to_stdout___u32(101);
            let b: u64 = 100;
            tasm::tasm_io_write_to_stdout___u32(101);
            tasm::tasm_io_write_to_stdout___digest(digest);

            2 * b as u32
        }
        _ => {
            tasm::tasm_io_write_to_stdout___u32(102);
            let b: u64 = 200;
            tasm::tasm_io_write_to_stdout___u32(102);

            2 * b as u32
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_b_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_b)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_b",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}

fn catch_all_covers_c() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();

    let a: u32 = match val {
        ThreeVariants::A => {
            tasm::tasm_io_write_to_stdout___u32(100);
            let b: u32 = 50;
            tasm::tasm_io_write_to_stdout___u32(100);

            b * 2
        }
        ThreeVariants::B(_) => {
            tasm::tasm_io_write_to_stdout___u32(101);
            let b: u64 = 100;
            tasm::tasm_io_write_to_stdout___u32(101);

            2 * b as u32
        }
        _ => {
            tasm::tasm_io_write_to_stdout___u32(102);
            let b: u64 = 200;
            tasm::tasm_io_write_to_stdout___u32(102);

            2 * b as u32
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_c_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_c)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_c",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}

fn catch_all_covers_a_and_b() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();
    let function_val: u32 = 1234;

    let a: u32 = match val {
        ThreeVariants::C(digest) => {
            tasm::tasm_io_write_to_stdout___u32(101);
            let b: u64 = 100;
            tasm::tasm_io_write_to_stdout___u32(101);
            tasm::tasm_io_write_to_stdout___digest(digest);

            2 * b as u32
        }
        _ => {
            let local_val_0: u32 = 1234;
            tasm::tasm_io_write_to_stdout___u32(102);
            tasm::tasm_io_write_to_stdout___u32(102);
            let local_val_1: u32 = 434343;

            2 * function_val + local_val_0 + local_val_1
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_a_and_b_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_a_and_b)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_a_and_b",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}

fn catch_all_covers_a_and_c() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();
    let function_val: u32 = 1234;

    let a: u32 = match val {
        ThreeVariants::B(_) => {
            tasm::tasm_io_write_to_stdout___u32(101111);
            let b: u64 = 999;
            tasm::tasm_io_write_to_stdout___u32(101111);
            tasm::tasm_io_write_to_stdout___u32(function_val);

            2 * b as u32
        }
        _ => {
            let local_val_0: u32 = 1234;
            tasm::tasm_io_write_to_stdout___u32(102);
            tasm::tasm_io_write_to_stdout___u32(102);
            tasm::tasm_io_write_to_stdout___u32(function_val + 3);
            let local_val_1: u32 = 434343;

            2 * function_val + local_val_0 + local_val_1
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);
    tasm::tasm_io_write_to_stdout___u32(function_val);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_a_and_c_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_a_and_c)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_a_and_c",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}

fn catch_all_covers_b_and_c() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();
    let a: u32 = match val {
        ThreeVariants::A => {
            tasm::tasm_io_write_to_stdout___u32(100);
            let b: u32 = 50;
            tasm::tasm_io_write_to_stdout___u32(100);

            b * 2
        }
        _ => {
            let local_val_0: u32 = 1234;
            tasm::tasm_io_write_to_stdout___u32(102);
            tasm::tasm_io_write_to_stdout___u32(102);
            let local_val_1: u32 = 434343;

            2 * local_val_0 + local_val_1
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_b_and_c_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_b_and_c)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_b_and_c",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}

#[allow(clippy::match_single_binding)]
fn catch_all_covers_a_b_c() {
    let val: ThreeVariants = ThreeVariants::random_from_std_in();
    let a: u32 = match val {
        _ => {
            let local_val_0: u32 = 1234;
            tasm::tasm_io_write_to_stdout___u32(102);
            tasm::tasm_io_write_to_stdout___u32(102);
            let local_val_1: u32 = 434343;

            2 * local_val_0 + local_val_1
        }
    };

    tasm::tasm_io_write_to_stdout___u32(a);

    return;
}

#[proptest(cases = 10)]
fn catch_all_covers_a_b_c_test(
    #[strategy(vec(arb(), DIGEST_LENGTH))] std_in: Vec<BFieldElement>,
    #[strategy(arb())] input: u32,
) {
    let std_in = [vec![BFieldElement::new(input as u64)], std_in].concat();
    let native_output = rust_shadows::wrap_main_with_io(&catch_all_covers_a_b_c)(
        std_in.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk(
        "match_expr_on_stack",
        "catch_all_condition",
        "catch_all_covers_a_b_c",
    );
    let vm_output = TritonVMTestCase::new(entrypoint)
        .with_std_in(std_in)
        .expect_stack_difference(0)
        .execute()
        .unwrap();
    prop_assert_eq!(native_output, vm_output.output);
}
