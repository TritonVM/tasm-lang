use super::ValueIdentifier;
use crate::tasm_code_generator::InnerFunctionTasmCode;
use crate::tasm_code_generator::SubRoutine;
use crate::tasm_code_generator::VStack;
use std::collections::HashMap;
use std::collections::HashSet;

pub(crate) type VarAddr = HashMap<String, ValueIdentifier>;

/// State for managing the compilation of a single function
#[derive(Clone, Debug, Default)]

pub(crate) struct FunctionState {
    pub(crate) vstack: VStack,
    pub(crate) var_addr: VarAddr,
    pub(crate) spill_required: HashSet<ValueIdentifier>,
    pub(crate) subroutines: Vec<SubRoutine>,
}

impl FunctionState {
    /// Add a compiled function and its subroutines to the list of subroutines, thus
    /// preserving the structure that would get lost if lists of instructions were
    /// simply concatenated.
    pub(crate) fn add_compiled_fn_to_subroutines(&mut self, mut function: InnerFunctionTasmCode) {
        self.subroutines.append(&mut function.sub_routines);
        self.subroutines.push(function.call_depth_zero_code);
    }

    /// Force the compiler's view of the stack and bindings to specific values
    pub(crate) fn restore_stack_and_bindings(
        &mut self,
        previous_stack: &VStack,
        previous_var_addr: &VarAddr,
    ) {
        self.vstack = previous_stack.to_owned();
        self.var_addr = previous_var_addr.to_owned();
    }
}
