use tasm_lib::triton_vm::prelude::*;

use super::simple_struct::*;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

fn main() {
    let ts: Box<SimpleStruct> =
        SimpleStruct::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();

    tasm::tasmlib_io_write_to_stdout___u128(ts.a);
    tasm::tasmlib_io_write_to_stdout___bfe(ts.b);
    tasm::tasmlib_io_write_to_stdout___bool(ts.c);
    tasm::tasmlib_io_write_to_stdout___u32(ts.d.len() as u32);
    tasm::tasmlib_io_write_to_stdout___digest(ts.e);

    return;
}

#[cfg(test)]
mod test {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use itertools::Itertools;
    use rand::random;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    #[test]
    fn import_type_declaration_test() {
        let rand: [u8; 2000] = random();
        let test_struct = SimpleStruct::arbitrary(&mut Unstructured::new(&rand)).unwrap();
        let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let stdin = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());

        // Run test on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("other", "import_type_declaration", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        if native_output != vm_output.public_output {
            panic!(
                "native_output:\n {}, got:\n{}. Code was:\n{}",
                native_output.iter().join(", "),
                vm_output.public_output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
