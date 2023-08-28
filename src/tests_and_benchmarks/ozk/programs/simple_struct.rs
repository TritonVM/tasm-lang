// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

pub(crate) const SIMPLE_STRUCTS_BFIELD_CODEC_START_ADDRESS: u64 = 84;

#[derive(TasmObject, BFieldCodec)]
pub(crate) struct TestStruct {
    pub a: BFieldElement,
    pub b: BFieldElement,
}

pub(crate) fn main() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(84))).unwrap();
    let a: &BFieldElement = &test_struct.a; // Use 1 `&`, ignore the 2nd `&`.
    tasm::tasm_io_write_to_stdout_bfe(*a); // Implement both `*` and method `to_owned` to mean put this onto the stack. We might need exceptions for list though.

    let b: &BFieldElement = &test_struct.b;
    tasm::tasm_io_write_to_stdout_bfe(*b);
    return;
}
