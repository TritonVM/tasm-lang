#![allow(clippy::manual_swap)]

// Allows the use of input/output on the native architecture
use tasm_lib::Digest;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
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

mod tests {
    #[test]
    fn merkle_root_to_basic_snippet_test() {
        let (rust_ast, _, _) =
            crate::tests_and_benchmarks::ozk::ozk_parsing::parse_function_and_structs(
                "recufier",
                "merkle_root_autogen",
                "merkle_root",
            );
        let as_bs = crate::tests_and_benchmarks::ozk::ozk_parsing::compile_to_basic_snippet(
            rust_ast,
            std::collections::HashMap::default(),
            crate::ast_types::ListType::Unsafe,
        );
        println!("{}", as_bs);
    }
}
