use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::shared_math::other::random_elements;

use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::tests_and_benchmarks::ozk::rust_shadows;
use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;

fn call_constructor_once() {
    fn construct_xfe_4_array(xfe: XFieldElement) -> [XFieldElement; 4] {
        return [xfe; 4];
    }

    let element: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    let array: [XFieldElement; 4] = construct_xfe_4_array(element);
    tasm::tasm_io_write_to_stdout___xfe(array[0]);
    tasm::tasm_io_write_to_stdout___xfe(array[1]);
    tasm::tasm_io_write_to_stdout___xfe(array[2]);
    tasm::tasm_io_write_to_stdout___xfe(array[3]);

    // Print out earlier declaration to ensure stack-view is correct
    tasm::tasm_io_write_to_stdout___xfe(element);

    return;
}

#[test]
fn call_array_constructor_once() {
    let input = random_elements(3);
    let native_output = rust_shadows::wrap_main_with_io(&call_constructor_once)(
        input.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk("arrays", "repeat_syntax", "call_constructor_once");
    let vm_output = TritonVMTestCase::new(entrypoint.clone())
        .expect_stack_difference(0)
        .with_std_in(input)
        .execute()
        .unwrap();
    assert_eq!(native_output, vm_output.output);
}

fn call_constructor_twice() {
    fn construct_xfe_5_array(xfe: XFieldElement) -> [XFieldElement; 5] {
        return [xfe; 5];
    }

    let element0: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    let array0: [XFieldElement; 5] = construct_xfe_5_array(element0);
    let element1: XFieldElement = tasm::tasm_io_read_stdin___xfe();
    let array1: [XFieldElement; 5] = construct_xfe_5_array(element1);
    tasm::tasm_io_write_to_stdout___xfe(array0[0]);
    tasm::tasm_io_write_to_stdout___xfe(array0[1]);
    tasm::tasm_io_write_to_stdout___xfe(array0[2]);
    tasm::tasm_io_write_to_stdout___xfe(array0[3]);
    tasm::tasm_io_write_to_stdout___xfe(array0[4]);
    tasm::tasm_io_write_to_stdout___xfe(array1[0]);
    tasm::tasm_io_write_to_stdout___xfe(array1[1]);
    tasm::tasm_io_write_to_stdout___xfe(array1[2]);
    tasm::tasm_io_write_to_stdout___xfe(array1[3]);
    tasm::tasm_io_write_to_stdout___xfe(array1[4]);

    // Print out earlier declarations to ensure stack-view is correct
    tasm::tasm_io_write_to_stdout___xfe(element1);
    tasm::tasm_io_write_to_stdout___xfe(element0);

    return;
}

#[test]
fn call_array_constructor_twice() {
    let input = random_elements(6);
    let native_output = rust_shadows::wrap_main_with_io(&call_constructor_twice)(
        input.clone(),
        NonDeterminism::default(),
    );
    let entrypoint = EntrypointLocation::disk("arrays", "repeat_syntax", "call_constructor_twice");
    let vm_output = TritonVMTestCase::new(entrypoint.clone())
        .expect_stack_difference(0)
        .with_std_in(input)
        .execute()
        .unwrap();
    assert_eq!(native_output, vm_output.output);
}
