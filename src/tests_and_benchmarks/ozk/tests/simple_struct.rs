use std::collections::HashMap;

use crate::tests_and_benchmarks::{
    ozk::{self, ozk_parsing, programs::simple_struct::TestStruct, rust_shadows},
    test_helpers::shared_test::*,
};
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[test]
fn simple_struct_ozk_test() {
    // Test function on host machine
    let input = vec![];
    let test_struct = TestStruct {
        a: BFieldElement::new(14),
        b: BFieldElement::new(15),
    };

    let non_determinism = NonDeterminism {
        individual_tokens: vec![],
        digests: vec![],
        ram: test_struct
            .encode()
            .into_iter()
            .zip(1u64..)
            .map(|(v, k)| (k.into(), v))
            .collect(),
    };
    let expected_output = vec![BFieldElement::new(14), BFieldElement::new(15)];
    let native_output = rust_shadows::wrap_main_with_io(&ozk::programs::simple_struct::main)(
        input.clone(),
        non_determinism.clone(),
    );
    assert_eq!(native_output, expected_output);

    // Test function in Triton VM
    let (parsed, _, _) = ozk_parsing::parse_main_and_structs("simple_struct");
    println!("parsed: {parsed:?}");
    let expected_stack_diff = 0;
    let _vm_output = execute_with_stack_memory_and_ins(
        &parsed,
        vec![],
        &mut HashMap::default(),
        input,
        non_determinism,
        expected_stack_diff,
    )
    .unwrap();
}
