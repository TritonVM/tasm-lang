// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use triton_vm::{BFieldElement, Digest};
use twenty_first::{
    shared_math::bfield_codec::BFieldCodec, util_types::algebraic_hasher::AlgebraicHasher,
};
type H = twenty_first::shared_math::tip5::Tip5;

fn main() {
    let elements: Box<Vec<BFieldElement>> =
        Vec::<BFieldElement>::decode(&tasm::load_from_memory(BFieldElement::new(2000))).unwrap();

    let digest: Digest = H::hash_varlen(&elements);

    tasm::tasm_io_write_to_stdout_digest(digest);

    return;
}

mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        ast_types,
        tests_and_benchmarks::{
            ozk::{ozk_parsing, rust_shadows},
            test_helpers::shared_test::*,
        },
    };
    use twenty_first::shared_math::other::random_elements;

    #[test]
    fn hash_varlen_test() {
        // Test function on host machine
        let stdin = vec![];
        let bfe_list = random_elements(111);
        let non_determinism = init_memory_from(&bfe_list, 2000u64.into());
        let expected_output = H::hash_varlen(&bfe_list);
        let expected_output = expected_output.values().to_vec();
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Test function in Triton VM
        let (rust_ast, _, _) = ozk_parsing::parse_main_and_structs("other", "hash_varlen");
        let expected_stack_diff = 0;
        let (code, _fn_name) = compile_for_run_test(&rust_ast, ast_types::ListType::Unsafe);
        let vm_output = execute_compiled_with_stack_memory_and_ins_for_test(
            &code,
            vec![],
            &mut HashMap::default(),
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
