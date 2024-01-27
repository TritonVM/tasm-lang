use tasm_lib::triton_vm::prelude::LabelledInstruction;
use tasm_lib::triton_vm::triton_asm;

use crate::ast_types::Tuple;
use crate::tasm_code_generator::move_top_stack_value_to_memory;
use crate::tasm_code_generator::CompilerState;

impl Tuple {
    /// ```text
    /// BEFORE: _ [value] *value
    /// AFTER: _
    /// ```
    pub(crate) fn store_to_memory(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
        if self.is_copy() {
            return move_top_stack_value_to_memory(None, self.stack_size());
        }

        let label_for_subroutine = format!("store_{}_to_memory", self.label_friendly_name());

        let call_store_sr = triton_asm!(call {
            label_for_subroutine
        });
        if state.contains_subroutine(&label_for_subroutine) {
            return call_store_sr;
        }

        let mut subroutine = triton_asm!(
            {label_for_subroutine}:
        );

        let field_pointer_pointer = state.static_memory_allocation(1);
        for (_field_id, dtype) in self.field_ids_and_types_reversed() {
            // _ [[fields] [current_field]] *current_field_or_field_size

            let handle_field =
                dtype.encode_as_field_leave_next_free_address(state, field_pointer_pointer);
            subroutine.extend(handle_field);
            // _ [[fields]] *next_field_or_field_size
        }

        subroutine.extend(triton_asm!(
            // _ *next_free_word

            pop 1
            // _

            return
        ));
        state.add_library_function(subroutine.try_into().unwrap());

        call_store_sr
    }
}
