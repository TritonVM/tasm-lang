use std::collections::HashMap;

use crate::graft::{graft_fn_decl, graft_structs};
use crate::tasm_code_generator::compile_function;
use crate::tests_and_benchmarks::ozk::{
    self, ozk_parsing, programs::simple_struct::TestStruct, rust_shadows,
};
use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_memory_and_ins_for_test;
use crate::types::annotate_fn;

use triton_vm::instruction::LabelledInstruction;
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
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
    // let expected_output = vec![BFieldElement::new(14)];
    let native_output = rust_shadows::wrap_main_with_io(&ozk::programs::simple_struct::main)(
        input.clone(),
        non_determinism.clone(),
    );
    assert_eq!(native_output, expected_output);

    // Test function in Triton VM
    let (parsed_main, parsed_structs, _) = ozk_parsing::parse_main_and_structs("simple_struct");

    // parse test
    let mut function = graft_fn_decl(&parsed_main);
    let structs = graft_structs(parsed_structs);

    // type-check and annotate
    annotate_fn(&mut function, structs);

    // compile
    let tasm = compile_function(&function);

    // compose
    let instructions = tasm.compose();

    let function_name = if let LabelledInstruction::Label(fn_name) = instructions.first().unwrap() {
        fn_name.to_owned()
    } else {
        panic!("First instruction of compiled instructions must be a label")
    };

    let wrapped_instructions = triton_asm!(
        call {function_name}
        halt

        {&instructions}
    );

    let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
        &wrapped_instructions,
        vec![],
        &mut HashMap::default(),
        input,
        non_determinism,
        0,
    )
    .unwrap();
    assert_eq!(expected_output, vm_output.output);
}
