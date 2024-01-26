use crate::subroutine::SubRoutine;
use triton_vm::triton_asm;

#[derive(Clone, Debug)]
pub(crate) struct InnerFunctionTasmCode {
    pub(crate) name: String,
    pub(crate) call_depth_zero_code: SubRoutine,
    pub(crate) sub_routines: Vec<SubRoutine>,
}

impl InnerFunctionTasmCode {
    /// Return a dummy value, needed to allow recursive calls for methods.
    /// This dummy-value is guaranteed to crash if it is ever invoked.
    pub(crate) fn dummy_value(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            call_depth_zero_code: triton_asm!({name}: push 0 assert return)
                .try_into()
                .unwrap(),
            sub_routines: vec![],
        }
    }
}
