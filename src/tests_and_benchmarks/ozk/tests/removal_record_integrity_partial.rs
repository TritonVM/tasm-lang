use std::collections::HashMap;

use crate::tests_and_benchmarks::{
    ozk::{self, ozk_parsing},
    test_helpers::shared_test::*,
};
use rand::random;
use tasm_lib::Digest;
use triton_vm::NonDeterminism;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[test]
fn removal_record_integrity_partial_test() {
    let input: Digest = random();
    let input = input.encode();
    let non_determinism = NonDeterminism::new(vec![]);
    let expected_output = vec![];
    let native_output = ozk::rust_shadows::wrap_main_with_io(
        &ozk::programs::removal_record_integrity_partial::main,
    )(input.clone(), non_determinism.clone());
    assert_eq!(native_output, expected_output);

    // Test function in Triton VM
    let (parsed, _) = ozk_parsing::parse_main("removal_record_integrity_partial");
    let expected_stack_diff = 0;
    let vm_output = execute_with_stack_memory_and_ins(
        &parsed,
        vec![],
        &mut HashMap::default(),
        input,
        non_determinism,
        expected_stack_diff,
    )
    .unwrap();
    assert_eq!(expected_output, vm_output.output);
}
