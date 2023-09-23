use triton_vm::{BFieldElement, Digest};

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
struct TupleStructDinner(u64, BFieldElement);

#[derive(Clone, Copy)]
struct TupleStructDoubter(TupleStructDinner, Digest, TupleStructDinner);

#[allow(clippy::type_complexity)]
fn main() {
    let a_dinner_0: u64 = tasm::tasm_io_read_stdin_u64();
    let a_dinner_1: BFieldElement = tasm::tasm_io_read_stdin_bfe();
    let a_dinner: TupleStructDinner = TupleStructDinner(a_dinner_0, a_dinner_1);

    let doubter_1: Digest = tasm::tasm_io_read_stdin_digest();

    let b_dinner_0: u64 = tasm::tasm_io_read_stdin_u64();
    let b_dinner_1: BFieldElement = tasm::tasm_io_read_stdin_bfe();
    let b_dinner: TupleStructDinner = TupleStructDinner(b_dinner_0, b_dinner_1);

    let s: TupleStructDoubter = TupleStructDoubter(a_dinner, doubter_1, b_dinner);
    let boxed_s: Box<TupleStructDoubter> = Box::<TupleStructDoubter>::new(s);
    let t: ((u64, BFieldElement), Digest, (u64, BFieldElement)) = (
        (a_dinner_0, a_dinner_1),
        doubter_1,
        (b_dinner_0, b_dinner_1),
    );
    let boxed_t: Box<((u64, BFieldElement), Digest, (u64, BFieldElement))> =
        Box::<((u64, BFieldElement), Digest, (u64, BFieldElement))>::new(t);

    // Test outer struct/tuples field 0
    assert!(a_dinner_0 == s.0 .0);
    assert!(a_dinner_0 == boxed_s.0 .0);
    assert!(a_dinner_0 == t.0 .0);
    assert!(a_dinner_0 == boxed_t.0 .0);
    assert!(a_dinner_1 == s.0 .1);
    assert!(a_dinner_1 == boxed_s.0 .1);
    assert!(a_dinner_1 == t.0 .1);
    assert!(a_dinner_1 == boxed_t.0 .1);

    // Test outer struct/tuples field 1
    assert!(doubter_1 == s.1);
    assert!(doubter_1 == boxed_s.1);
    assert!(doubter_1 == t.1);
    assert!(doubter_1 == boxed_t.1);

    // Test outer struct/tuples field 2
    assert!(b_dinner_0 == s.2 .0);
    assert!(b_dinner_0 == boxed_s.2 .0);
    assert!(b_dinner_0 == t.2 .0);
    assert!(b_dinner_0 == boxed_t.2 .0);
    assert!(b_dinner_1 == s.2 .1);
    assert!(b_dinner_1 == boxed_s.2 .1);
    assert!(b_dinner_1 == t.2 .1);
    assert!(b_dinner_1 == boxed_t.2 .1);

    // Pull down entire structs/nested tuple onto stack
    let unboxed_s: TupleStructDoubter = *boxed_s;
    let unboxed_t: ((u64, BFieldElement), Digest, (u64, BFieldElement)) = *boxed_t;

    assert!(a_dinner_0 == unboxed_s.0 .0);
    assert!(a_dinner_0 == unboxed_t.0 .0);
    assert!(a_dinner_1 == unboxed_s.0 .1);
    assert!(a_dinner_1 == unboxed_t.0 .1);

    assert!(doubter_1 == unboxed_s.1);
    assert!(doubter_1 == unboxed_t.1);

    assert!(b_dinner_0 == unboxed_s.2 .0);
    assert!(b_dinner_0 == unboxed_t.2 .0);
    assert!(b_dinner_1 == unboxed_s.2 .1);
    assert!(b_dinner_1 == unboxed_t.2 .1);

    return;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::tests_and_benchmarks::{
        ozk::{ozk_parsing, rust_shadows},
        test_helpers::shared_test::*,
    };
    use itertools::Itertools;
    use rand::random;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    #[test]
    fn nested_tuples_test() {
        // Test function on host machine
        let a_dinner_0: u64 = random();
        let mut a_dinner_0_encoded_reverse = a_dinner_0.encode();
        a_dinner_0_encoded_reverse.reverse();
        let a_dinner_1: BFieldElement = random();
        let mut a_dinner_1_encoded_reverse = a_dinner_1.encode();
        a_dinner_1_encoded_reverse.reverse();

        let doubter_1: Digest = random();
        let mut d_encoded_reverse = doubter_1.encode();
        d_encoded_reverse.reverse();

        let b_dinner_0: u64 = random();
        let mut b_dinner_0_encoded_reverse = b_dinner_0.encode();
        b_dinner_0_encoded_reverse.reverse();
        let b_dinner_1: BFieldElement = random();
        let mut b_dinner_1_encoded_reverse = b_dinner_1.encode();
        b_dinner_1_encoded_reverse.reverse();

        let expected_output = vec![];
        let stdin = [
            a_dinner_0_encoded_reverse,
            a_dinner_1_encoded_reverse,
            d_encoded_reverse,
            b_dinner_0_encoded_reverse,
            b_dinner_1_encoded_reverse,
        ]
        .concat();
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "nested_tuples",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &mut HashMap::default(),
            stdin,
            NonDeterminism::new(vec![]),
            expected_stack_diff,
        )
        .unwrap();
        if expected_output != vm_output.output {
            panic!(
                "expected:\n{}\n\ngot:\n{}",
                expected_output.iter().join(","),
                vm_output.output.iter().join(",")
            );
        }
    }
}
