use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[allow(unused_allocation)]
fn main() {
    Box::<Foo>::new(Foo(42)).ref_self_method();
    Foo(43).owned_self_method();

    return;
}

struct Foo(u32);

impl Foo {
    #[allow(clippy::vec_init_then_push)]
    fn ref_self_method(&self) {
        let mut vec: Vec<u32> = Vec::<u32>::default();
        vec.push(101);
        let _array: [u32; 1] = <[u32; 1]>::try_from(vec).unwrap();

        // print field-0 value to suppress linter-warning
        tasm::tasmlib_io_write_to_stdout___u32(self.0);

        return;
    }

    #[allow(clippy::vec_init_then_push)]
    fn owned_self_method(self) {
        let mut vec: Vec<u32> = Vec::<u32>::default();
        vec.push(101);
        let maybe_array: Result<[u32; 1], _> = <[u32; 1]>::try_from(vec);
        let array: [u32; 1] = maybe_array.unwrap();
        tasm::tasmlib_io_write_to_stdout___u32(array[0]);

        // print field-0 value to suppress linter-warning
        tasm::tasmlib_io_write_to_stdout___u32(self.0);

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
    fn composite_type_methods_test() {
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(vec![], NonDeterminism::default());
        let entrypoint_location =
            EntrypointLocation::disk("composite_types", "composite_types_from_methods", "main");
        let vm_output = TritonVMTestCase::new(entrypoint_location)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
