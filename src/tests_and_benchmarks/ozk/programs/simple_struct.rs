// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use tasm_lib::structure::tasm_object::TasmObject;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(TasmObject, BFieldCodec)]
pub(crate) struct TestStruct {
    pub a: BFieldElement,
    pub b: BFieldElement,
}

pub(crate) fn main() {
    let test_struct: TestStruct = *TestStruct::decode(&mut tasm::load_from_memory()).unwrap();
    let a = &test_struct.a;
    tasm::tasm_io_write_to_stdout_bfe(*a);
    tasm::tasm_io_write_to_stdout_bfe(test_struct.b);
    return;
}
