use crate::graft::{graft_fn_decl, graft_structs};
use crate::tasm_code_generator::compile_function;
use crate::tests_and_benchmarks::ozk::{
    self, ozk_parsing, programs::simple_struct::TestStruct, rust_shadows,
};
use crate::types::annotate_fn;

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
    let (parsed_main, parsed_structs, _) = ozk_parsing::parse_main_and_structs("simple_struct");
    println!("parsed_main: {parsed_main:?}");

    // parse test
    let mut function = graft_fn_decl(&parsed_main);
    let structs = graft_structs(parsed_structs);

    // type-check and annotate
    annotate_fn(&mut function, structs);

    // compile
    let tasm = compile_function(&function);
}
