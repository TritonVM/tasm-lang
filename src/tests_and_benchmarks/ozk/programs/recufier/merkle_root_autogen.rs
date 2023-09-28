#![allow(clippy::manual_swap)]

// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use num::One;
use tasm_lib::Digest;
use triton_vm::BFieldElement;
use twenty_first::{
    shared_math::bfield_codec::BFieldCodec, util_types::algebraic_hasher::AlgebraicHasher,
};
type H = twenty_first::shared_math::tip5::Tip5;

#[allow(clippy::ptr_arg)]
#[allow(clippy::vec_init_then_push)]
fn main(leafs: &Vec<Digest>, start: usize, stop: usize) -> Digest {
    // #[allow(unused_assignments)]
    // let mut result: Digest = Digest::default();
    let result: Digest = if stop == start + 1usize {
        leafs[start]
    } else {
        let half: usize = (stop - start) / 2;
        let left: Digest = main(leafs, start, stop - half);
        let right: Digest = main(leafs, start + half, stop);
        H::hash_pair(&left, &right)
    };

    return result;
}

mod tests {
    #[test]
    fn merkle_root_to_basic_snippet_test() {
        let (rust_ast, _, _) =
            crate::tests_and_benchmarks::ozk::ozk_parsing::parse_main_and_structs(
                "recufier",
                "merkle_root_autogen",
            );
        let as_bs = crate::tests_and_benchmarks::ozk::ozk_parsing::compile_to_basic_snippet(
            rust_ast,
            std::collections::HashMap::default(),
            crate::ast_types::ListType::Unsafe,
        );
        println!("{}", as_bs);
    }
}
