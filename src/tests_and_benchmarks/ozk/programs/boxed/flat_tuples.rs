use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
struct TupleStructA(u128, u64, Digest);
#[derive(Clone, Copy)]
struct TupleStructB(Digest, Digest, Digest);

#[derive(Clone, Copy)]
struct TupleStructC(u32, u32);

fn main() {
    // Construct a values
    // TODO: Also test where we pull the entire tuple/struct down to the stack again
    let a_0: u128 = tasm::tasm_io_read_stdin___u128();
    let a_1: u64 = tasm::tasm_io_read_stdin___u64();
    let a_2: Digest = tasm::tasm_io_read_stdin___digest();
    let ts_a: TupleStructA = TupleStructA(a_0, a_1, a_2);
    let boxed_ts_a: Box<TupleStructA> = Box::<TupleStructA>::new(ts_a);
    let bare_tuple_a: (u128, u64, Digest) = (a_0, a_1, a_2);
    let boxed_bare_tuple_a: Box<(u128, u64, Digest)> =
        Box::<(u128, u64, Digest)>::new(bare_tuple_a);
    let unboxed_ts_a: TupleStructA = *boxed_ts_a;
    let unboxed_bare_tuple_a: (u128, u64, Digest) = *boxed_bare_tuple_a;
    let a_0_again_0: u128 = ts_a.0;
    let a_0_again_1: u128 = boxed_ts_a.0;
    let a_0_again_2: u128 = bare_tuple_a.0;
    let a_0_again_3: u128 = boxed_bare_tuple_a.0;
    let a_0_again_4: u128 = unboxed_ts_a.0;
    let a_0_again_5: u128 = unboxed_bare_tuple_a.0;
    let a_1_again_0: u64 = ts_a.1;
    let a_1_again_1: u64 = boxed_ts_a.1;
    let a_1_again_2: u64 = bare_tuple_a.1;
    let a_1_again_3: u64 = boxed_bare_tuple_a.1;
    let a_1_again_4: u64 = unboxed_ts_a.1;
    let a_1_again_5: u64 = unboxed_bare_tuple_a.1;
    let a_2_again_0: Digest = ts_a.2;
    let a_2_again_1: Digest = boxed_ts_a.2;
    let a_2_again_2: Digest = bare_tuple_a.2;
    let a_2_again_3: Digest = boxed_bare_tuple_a.2;
    let a_2_again_4: Digest = unboxed_ts_a.2;
    let a_2_again_5: Digest = unboxed_bare_tuple_a.2;

    // Construct b values
    let b_0: Digest = tasm::tasm_io_read_stdin___digest();
    let b_1: Digest = tasm::tasm_io_read_stdin___digest();
    let b_2: Digest = tasm::tasm_io_read_stdin___digest();
    let ts_b: TupleStructB = TupleStructB(b_0, b_1, b_2);
    let boxed_ts_b: Box<TupleStructB> = Box::<TupleStructB>::new(ts_b);
    let bare_tuple_b: (Digest, Digest, Digest) = (b_0, b_1, b_2);
    let boxed_bare_tuple_b: Box<(Digest, Digest, Digest)> =
        Box::<(Digest, Digest, Digest)>::new((b_0, b_1, b_2));
    let unboxed_ts_b: TupleStructB = *boxed_ts_b;
    let unboxed_bare_tuple_b: (Digest, Digest, Digest) = *boxed_bare_tuple_b;
    let b_0_again_0: Digest = ts_b.0;
    let b_0_again_1: Digest = boxed_ts_b.0;
    let b_0_again_2: Digest = bare_tuple_b.0;
    let b_0_again_3: Digest = boxed_bare_tuple_b.0;
    let b_0_again_4: Digest = unboxed_ts_b.0;
    let b_0_again_5: Digest = unboxed_bare_tuple_b.0;
    let b_1_again_0: Digest = ts_b.1;
    let b_1_again_1: Digest = boxed_ts_b.1;
    let b_1_again_2: Digest = bare_tuple_b.1;
    let b_1_again_3: Digest = boxed_bare_tuple_b.1;
    let b_1_again_4: Digest = unboxed_ts_b.1;
    let b_1_again_5: Digest = unboxed_bare_tuple_b.1;
    let b_2_again_0: Digest = ts_b.2;
    let b_2_again_1: Digest = boxed_ts_b.2;
    let b_2_again_2: Digest = bare_tuple_b.2;
    let b_2_again_3: Digest = boxed_bare_tuple_b.2;
    let b_2_again_4: Digest = unboxed_ts_b.2;
    let b_2_again_5: Digest = unboxed_bare_tuple_b.2;

    // Construct c values
    let c_0: u32 = tasm::tasm_io_read_stdin___bfe().value() as u32;
    let c_1: u32 = tasm::tasm_io_read_stdin___bfe().value() as u32;
    let ts_c: TupleStructC = TupleStructC(c_0, c_1);
    let boxed_ts_c: Box<TupleStructC> = Box::<TupleStructC>::new(ts_c);
    let bare_tuple_c: (u32, u32) = (c_0, c_1);
    let boxed_bare_tuple_c: Box<(u32, u32)> = Box::<(u32, u32)>::new(bare_tuple_c);
    let unboxed_ts_c: TupleStructC = *boxed_ts_c;
    let unboxed_bare_tuple_c: (u32, u32) = *boxed_bare_tuple_c;
    let c_0_again_0: u32 = ts_c.0;
    let c_0_again_1: u32 = boxed_ts_c.0;
    let c_0_again_2: u32 = bare_tuple_c.0;
    let c_0_again_3: u32 = boxed_bare_tuple_c.0;
    let c_0_again_4: u32 = unboxed_ts_c.0;
    let c_0_again_5: u32 = unboxed_bare_tuple_c.0;
    let c_1_again_0: u32 = ts_c.1;
    let c_1_again_1: u32 = boxed_ts_c.1;
    let c_1_again_2: u32 = bare_tuple_c.1;
    let c_1_again_3: u32 = boxed_bare_tuple_c.1;
    let c_1_again_4: u32 = unboxed_ts_c.1;
    let c_1_again_5: u32 = unboxed_bare_tuple_c.1;

    // Check a_0 access
    assert!(a_0 == a_0_again_0);
    assert!(a_0 == a_0_again_1);
    assert!(a_0 == a_0_again_2);
    assert!(a_0 == a_0_again_3);
    assert!(a_0 == a_0_again_4);
    assert!(a_0 == a_0_again_5);
    assert!(a_0 == ts_a.0);
    assert!(a_0 == boxed_ts_a.0);
    assert!(a_0 == bare_tuple_a.0);
    assert!(a_0 == boxed_bare_tuple_a.0);

    // Check a_1 access
    assert!(a_1 == a_1_again_0);
    assert!(a_1 == a_1_again_1);
    assert!(a_1 == a_1_again_2);
    assert!(a_1 == a_1_again_3);
    assert!(a_1 == a_1_again_4);
    assert!(a_1 == a_1_again_5);
    assert!(a_1 == ts_a.1);
    assert!(a_1 == boxed_ts_a.1);
    assert!(a_1 == bare_tuple_a.1);
    assert!(a_1 == boxed_bare_tuple_a.1);

    // Check a_2 access
    assert!(a_2 == a_2_again_0);
    assert!(a_2 == a_2_again_1);
    assert!(a_2 == a_2_again_2);
    assert!(a_2 == a_2_again_3);
    assert!(a_2 == a_2_again_4);
    assert!(a_2 == a_2_again_5);
    assert!(a_2 == ts_a.2);
    assert!(a_2 == boxed_ts_a.2);
    assert!(a_2 == bare_tuple_a.2);
    assert!(a_2 == boxed_bare_tuple_a.2);

    // Check b_0 access
    assert!(b_0 == b_0_again_0);
    assert!(b_0 == b_0_again_1);
    assert!(b_0 == b_0_again_2);
    assert!(b_0 == b_0_again_3);
    assert!(b_0 == b_0_again_4);
    assert!(b_0 == b_0_again_5);
    assert!(b_0 == ts_b.0);
    assert!(b_0 == boxed_ts_b.0);
    assert!(b_0 == bare_tuple_b.0);
    assert!(b_0 == boxed_bare_tuple_b.0);

    // Check b_1 access
    assert!(b_1 == b_1_again_0);
    assert!(b_1 == b_1_again_1);
    assert!(b_1 == b_1_again_2);
    assert!(b_1 == b_1_again_3);
    assert!(b_1 == b_1_again_4);
    assert!(b_1 == b_1_again_5);
    assert!(b_1 == ts_b.1);
    assert!(b_1 == boxed_ts_b.1);
    assert!(b_1 == bare_tuple_b.1);
    assert!(b_1 == boxed_bare_tuple_b.1);

    // Check b_2 access
    assert!(b_2 == b_2_again_0);
    assert!(b_2 == b_2_again_1);
    assert!(b_2 == b_2_again_2);
    assert!(b_2 == b_2_again_3);
    assert!(b_2 == b_2_again_4);
    assert!(b_2 == b_2_again_5);
    assert!(b_2 == ts_b.2);
    assert!(b_2 == boxed_ts_b.2);
    assert!(b_2 == bare_tuple_b.2);
    assert!(b_2 == boxed_bare_tuple_b.2);

    // Check c access
    assert!(c_0 == c_0_again_0);
    assert!(c_0 == c_0_again_1);
    assert!(c_0 == c_0_again_2);
    assert!(c_0 == c_0_again_3);
    assert!(c_0 == c_0_again_4);
    assert!(c_0 == c_0_again_5);
    assert!(c_1 == c_1_again_0);
    assert!(c_1 == c_1_again_1);
    assert!(c_1 == c_1_again_2);
    assert!(c_1 == c_1_again_3);
    assert!(c_1 == c_1_again_4);
    assert!(c_1 == c_1_again_5);

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
    fn flat_tuples_test() {
        // Test function on host machine
        let a_0: u128 = random();
        let a_1: u64 = random();
        let a_2: Digest = random();
        let b_0: Digest = random();
        let b_1: Digest = random();
        let b_2: Digest = random();
        let c_0: u32 = random();
        let c_1: u32 = random();
        let mut a0_encoded_reverse = a_0.encode();
        a0_encoded_reverse.reverse();
        let mut a1_encoded_reverse = a_1.encode();
        a1_encoded_reverse.reverse();
        let mut a2_encoded_reverse = a_2.encode();
        a2_encoded_reverse.reverse();
        let mut b0_encoded_reverse = b_0.encode();
        b0_encoded_reverse.reverse();
        let mut b1_encoded_reverse = b_1.encode();
        b1_encoded_reverse.reverse();
        let mut b2_encoded_reverse = b_2.encode();
        b2_encoded_reverse.reverse();
        let mut c_0_encoded_reverse = c_0.encode();
        c_0_encoded_reverse.reverse();
        let mut c_1_encoded_reverse = c_1.encode();
        c_1_encoded_reverse.reverse();

        // let expected_output = [a.encode(), a.encode(), b.encode(), b.encode()].concat();
        let expected_output = vec![];
        let stdin = [
            a0_encoded_reverse,
            a1_encoded_reverse,
            a2_encoded_reverse,
            b0_encoded_reverse,
            b1_encoded_reverse,
            b2_encoded_reverse,
            c_0_encoded_reverse,
            c_1_encoded_reverse,
        ]
        .concat();
        let non_determinism = NonDeterminism::new(vec![]);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let test_program = ozk_parsing::compile_for_test(
            "boxed",
            "flat_tuples",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
        let expected_stack_diff = 0;
        println!("test_program:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &test_program,
            vec![],
            &HashMap::default(),
            stdin,
            non_determinism,
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
