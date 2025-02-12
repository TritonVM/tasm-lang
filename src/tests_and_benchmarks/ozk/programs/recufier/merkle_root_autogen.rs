#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing::compile_to_basic_snippet;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::twenty_first::prelude::*;

    /// Output the `Merkle root` implementation as a `BasicSnippet` implementation.
    #[test]
    fn merkle_root_to_basic_snippet() {
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "merkle_root_autogen", "test::merkle_root");
        let rust_ast = entrypoint_location.extract_entrypoint();
        let as_bs = compile_to_basic_snippet(rust_ast, std::collections::HashMap::default());
        println!("{}", as_bs);
    }

    #[allow(clippy::ptr_arg)]
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
            Tip5::hash_pair(left, right)
        };

        return result;
    }
}
