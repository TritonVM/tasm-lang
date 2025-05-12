use tasm_lib::twenty_first::prelude::Digest;
use tasm_lib::twenty_first::prelude::XFieldElement;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
struct SimpleStruct {
    a: u32,
    b: Digest,
    c: XFieldElement,
}

fn main() {
    let struct_0: SimpleStruct = SimpleStruct {
        a: 403,
        b: tasm::tasmlib_io_read_stdin___digest(),
        c: tasm::tasmlib_io_read_stdin___xfe(),
    };
    let boxed_struct_0: Box<SimpleStruct> = Box::<SimpleStruct>::new(struct_0);
    tasm::tasmlib_io_write_to_stdout___u32(boxed_struct_0.a);
    tasm::tasmlib_io_write_to_stdout___digest(boxed_struct_0.b);
    tasm::tasmlib_io_write_to_stdout___xfe(boxed_struct_0.c);

    return;
}

#[cfg(test)]
mod test {
    use tasm_lib::triton_vm::prelude::*;
    use tasm_lib::twenty_first::math::other::random_elements;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

    #[test]
    fn boxed_simple_copy_struct() {
        let stdin = random_elements(8);
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), NonDeterminism::default());

        let entrypoint = EntrypointLocation::disk("boxed", "simple_struct_copy", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(stdin)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.public_output);
    }
}
