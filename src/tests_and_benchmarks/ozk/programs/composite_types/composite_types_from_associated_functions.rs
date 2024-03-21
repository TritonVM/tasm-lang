fn main() {
    Foo::bar();

    return;
}

struct Foo(u32);

impl Foo {
    #[allow(clippy::vec_init_then_push)]
    fn bar() {
        let mut vec: Vec<u32> = Vec::<u32>::default();
        vec.push(101);
        let _array: [u32; 1] = <[u32; 1]>::try_from(vec).unwrap();

        return;
    }
}

#[cfg(test)]
mod tests {
    use tasm_lib::triton_vm::program::NonDeterminism;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn composite_type_associated_function_test() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint_location = EntrypointLocation::disk(
            "composite_types",
            "composite_types_from_associated_functions",
            "main",
        );
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
