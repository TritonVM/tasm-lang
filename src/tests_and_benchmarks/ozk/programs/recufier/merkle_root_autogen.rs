#![allow(clippy::manual_swap)]

use tasm_lib::Digest;
use triton_vm::twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

type H = twenty_first::shared_math::tip5::Tip5;

#[allow(clippy::ptr_arg)]
#[allow(clippy::vec_init_then_push)]
#[allow(dead_code)]
fn merkle_root(leafs: &Vec<Digest>, start: usize, stop: usize) -> Digest {
    // #[allow(unused_assignments)]
    // let mut result: Digest = Digest::default();
    let result: Digest = if stop == start + 1usize {
        leafs[start]
    } else {
        let half: usize = (stop - start) / 2;
        let left: Digest = merkle_root(leafs, start, stop - half);
        let right: Digest = merkle_root(leafs, start + half, stop);
        H::hash_pair(left, right)
    };

    return result;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::compile_to_basic_snippet;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::parse_functions_and_types;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;

    #[test]
    fn merkle_root_to_basic_snippet_test() {
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "merkle_root_autogen", "merkle_root");
        let (rust_ast, _) = parse_functions_and_types(&entrypoint_location);
        let as_bs = compile_to_basic_snippet(
            rust_ast,
            std::collections::HashMap::default(),
            crate::ast_types::ListType::Unsafe,
        );
        println!("{}", as_bs);
    }
}
