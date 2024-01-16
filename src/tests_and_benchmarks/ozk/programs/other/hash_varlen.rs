#![allow(clippy::explicit_auto_deref)]
#![allow(clippy::needless_borrow)]

use triton_vm::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use triton_vm::BFieldElement;
use triton_vm::Digest;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

type H = twenty_first::shared_math::tip5::Tip5;

fn main() {
    let elements: Box<Vec<BFieldElement>> =
        Vec::<BFieldElement>::decode(&tasm::load_from_memory(BFieldElement::new(2000))).unwrap();

    let digest: Digest = H::hash_varlen(&(*elements));

    tasm::tasm_io_write_to_stdout___digest(digest);

    return;
}

#[cfg(test)]
mod test {

    use triton_vm::twenty_first::shared_math::other::random_elements;

    use crate::ast_types;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

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
        let entrypoint_location = EntrypointLocation::disk("other", "hash_varlen", "main");
        let (rust_ast, _) = ozk_parsing::parse_functions_and_types(&entrypoint_location);
        let expected_stack_diff = 0;
        let (code, _fn_name) = compile_for_run_test(&rust_ast, ast_types::ListType::Unsafe);
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &code,
            vec![],
            stdin,
            non_determinism,
            expected_stack_diff,
        )
        .unwrap();
        assert_eq!(expected_output, vm_output.output);
    }
}
