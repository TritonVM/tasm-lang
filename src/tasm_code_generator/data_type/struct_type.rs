use num::Zero;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::triton_instr;
use triton_vm::BFieldElement;

use crate::ast_types;
use crate::ast_types::StructVariant;
use crate::tasm_code_generator::CompilerState;

impl ast_types::StructType {
    /// BEFORE: _ *struct
    /// AFTER:  _ [[fields]]
    pub(crate) fn load_from_memory(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
        fn load_from_memory_struct_is_not_copy(
            struct_type: &ast_types::StructType,
            state: &mut CompilerState,
        ) -> Vec<LabelledInstruction> {
            // stack: _ *struct

            let (pointer_pointer, mut code) = struct_type.get_all_field_pointers(state);
            // stack: _

            for (field_count, (_field_id, dtype)) in struct_type.field_ids_and_types().enumerate() {
                let static_address_for_field_pointer = pointer_pointer.value() + field_count as u64;

                code.append(&mut triton_asm!(
                    push {static_address_for_field_pointer}
                    // _ **field

                    read_mem
                    // _ **field *field

                    swap 1
                    // _ *field **field

                    pop
                    // *field
                ));

                let mut load_field = dtype.load_from_memory(None, state);

                code.append(&mut load_field);
            }

            code
        }

        // _ *struct
        // if struct is copy, we just copy all bytes, otherwise we need something
        // more complicated.
        if self.is_copy {
            ast_types::DataType::copy_words_from_memory(None, self.stack_size())
        } else {
            load_from_memory_struct_is_not_copy(self, state)
        }
    }

    /// Return the code to get all field pointers. The field pointers are stored
    /// in memory, not on stack. The address for the field pointers is returned.
    /// BEFORE: _ *struct
    /// AFTER:  _
    pub(crate) fn get_all_field_pointers(
        &self,
        state: &mut CompilerState,
    ) -> (BFieldElement, Vec<LabelledInstruction>) {
        let mut code = triton_asm!();

        let pointer_for_result: u64 = state
            .global_compiler_state
            .snippet_state
            .kmalloc(self.field_count() as u32)
            .try_into()
            .unwrap();

        let number_of_fields = self.field_count();
        for (field_count, (_field_id, field_type)) in
            self.field_ids_and_types_reversed().enumerate()
        {
            let pointer_pointer_for_this_field =
                pointer_for_result + number_of_fields as u64 - 1 - field_count as u64;
            match field_type.bfield_codec_length() {
                Some(static_size) => {
                    code.append(&mut triton_asm!(
                        // _ *current_field

                        // Store result in result array
                        push {pointer_pointer_for_this_field}
                        dup 1
                        write_mem
                        pop
                        // _ *current_field

                        // TODO: Not needed for last loop-iteration
                        push {static_size}
                        add
                        // _ *field_or_fieldsize_next

                    ));
                }
                None => {
                    code.append(&mut triton_asm!(
                        // _ *current_field_size

                        read_mem
                        // _ *current_field_size current_field_size

                        swap 1
                        // _ current_field_size *current_field_size

                        push 1
                        add
                        // _ current_field_size *current_field

                        push {pointer_pointer_for_this_field}
                        dup 1
                        // _ current_field_size *current_field **field *current_field

                        write_mem
                        // _ current_field_size *current_field **field

                        pop
                        // _ current_field_size *current_field

                        add
                        // _ *next_field_or_field_size
                    ))
                }
            }
        }

        code.push(triton_instr!(pop));

        (pointer_for_result.into(), code)
    }

    /// Assuming the stack top points to the start of the struct, returns the code
    /// that modifies the top stack value to point to the indicated field. So the top
    /// stack element is consumed and the returned value is a pointer to the requested
    /// field in the struct. Note that the top of the stack is where the field begins,
    /// not the size indication of that field.
    pub(crate) fn get_field_accessor_code_for_reference(
        &self,
        field_id: &ast_types::FieldId,
    ) -> Vec<LabelledInstruction> {
        // This implementation must match `BFieldCodec` for the equivalent Rust types
        let mut instructions = vec![];
        let mut static_pointer_addition = 0;
        let needle_id = field_id;
        let mut needle_type: Option<ast_types::DataType> = None;
        for (haystack_field_id, haystack_type) in self.field_ids_and_types_reversed() {
            if haystack_field_id == *needle_id {
                // If we've found the field the accumulators are in the right state.
                // return them.
                needle_type = Some(haystack_type.to_owned());
                break;
            } else {
                // We have not reached the field yet. If the field has a statically
                // known size, we can just add that number to the accumulator. Otherwise,
                // we have to read the size of the field from RAM, and add that value
                // to the pointer
                match haystack_type.bfield_codec_length() {
                    Some(static_length) => static_pointer_addition += static_length,
                    None => {
                        if !static_pointer_addition.is_zero() {
                            instructions
                                .append(&mut triton_asm!(push {static_pointer_addition} add));
                        }
                        instructions.append(&mut triton_asm!(read_mem add push 1 add));
                        static_pointer_addition = 0;
                    }
                }
            }
        }

        // If the requested field is dynamically sized, add one to address, to point to start
        // of the field instead of the size of the field.
        match needle_type.unwrap().bfield_codec_length() {
            Some(_) => (),
            None => static_pointer_addition += 1,
        }

        if !static_pointer_addition.is_zero() {
            instructions.append(&mut triton_asm!(push {static_pointer_addition} add));
        }

        instructions
    }

    pub(crate) fn stack_size(&self) -> usize {
        match &self.variant {
            StructVariant::TupleStruct(tuple) => tuple.stack_size(),
            StructVariant::NamedFields(struct_named_fields) => struct_named_fields.stack_size(),
        }
    }
}
