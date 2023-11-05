use super::ValueIdentifier;
use crate::tasm_code_generator::InnerFunctionTasmCode;
use crate::tasm_code_generator::SubRoutine;
use crate::tasm_code_generator::VStack;
use std::collections::HashMap;
use std::collections::HashSet;

pub type VarAddr = HashMap<String, ValueIdentifier>;

/// State for managing the compilation of a single function
#[derive(Clone, Debug, Default)]

pub(crate) struct FunctionState {
    pub vstack: VStack,
    pub var_addr: VarAddr,
    pub spill_required: HashSet<ValueIdentifier>,
    pub subroutines: Vec<SubRoutine>,
}

impl FunctionState {
    /// Add a compiled function and its subroutines to the list of subroutines, thus
    /// preserving the structure that would get lost if lists of instructions were
    /// simply concatenated.
    pub(crate) fn add_compiled_fn_to_subroutines(&mut self, mut function: InnerFunctionTasmCode) {
        self.subroutines.append(&mut function.sub_routines);
        self.subroutines.push(function.call_depth_zero_code);
    }
}
