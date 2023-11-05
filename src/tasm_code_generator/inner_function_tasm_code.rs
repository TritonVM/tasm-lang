use crate::subroutine::SubRoutine;
use triton_vm::triton_asm;

#[derive(Clone, Debug)]
pub(crate) struct InnerFunctionTasmCode {
    pub name: String,
    pub call_depth_zero_code: SubRoutine,
    pub sub_routines: Vec<SubRoutine>,
}

impl InnerFunctionTasmCode {
    /// Return a dummy value, needed to allow recursive calls for methods.
    pub fn dummy_value(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            call_depth_zero_code: triton_asm!({name}: return).try_into().unwrap(),
            sub_routines: vec![],
        }
    }
}
