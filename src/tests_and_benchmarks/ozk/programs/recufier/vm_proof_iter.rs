use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::tests_and_benchmarks::ozk::rust_shadows;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::test_helpers::shared_test::*;
use tasm_lib::triton_vm::prelude::*;

#[cfg(test)]
mod tests {
    use tasm_lib::twenty_first::shared_math::b_field_element::BFieldElement;

    // This struct should only be seen be `rustc`, not
    // by `tasm-lang`
    struct VmProofIter {
        pub current_item_pointer: BFieldElement,
    }
}
