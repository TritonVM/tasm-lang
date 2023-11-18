use itertools::Itertools;
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

            let number_of_fields = struct_type.field_count();
            for (field_count, dtype) in struct_type.field_types().enumerate() {
                let static_address_for_field_pointer =
                    pointer_pointer.value() + number_of_fields as u64 - 1 - field_count as u64;

                code.append(&mut triton_asm!(
                    push {static_address_for_field_pointer}

                    // _ **field
                    read_mem
                    // _ **field *field

                    swap 1
                    // _ *field **field

                    pop
                ));

                let mut load_field = dtype.copy_from_memory(None, state, false);

                code.append(&mut load_field);
            }

            code
        }
        // TODO: Fix this once we have a function to get all field pointers!
        // ast_types::DataType::copy_words_from_memory(None, self.stack_size(), false)

        // _ *struct
        // if struct is copy, we just copy all bytes, otherwise we need the
        // list of pointers.
        if self.is_copy {
            ast_types::DataType::copy_words_from_memory(None, self.stack_size(), false)
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
            .kmalloc(self.field_count())
            .try_into()
            .unwrap();

        let mut field_count = 0;
        for (_, field_type) in self.field_ids_and_types_reversed_for_tuples() {
            match field_type.bfield_codec_length() {
                Some(static_size) => {
                    code.append(&mut triton_asm!(
                        // _ *current_field

                        // Store result in result vector
                        push {pointer_for_result + field_count}
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
                    //
                    todo!();
                    code.append(&mut triton_asm!())
                }
            }

            field_count += 1;
        }

        code.push(triton_instr!(pop));

        (pointer_for_result.into(), code)

        // // TODO: We might have to reverse the order, if this is a struct with named fields!
        // for (field_id, field_type) in self.field_ids_and_types_reversed_for_tuples() {
        //     code.push(triton_instr!(dup 0));
        //     // _ [fields] *field_or_fieldsize *field_or_fieldsize

        //     let static_size = field_type.bfield_codec_length();
        //     if static_size.is_none() {
        //         code.append(&mut triton_asm!(push 1 add));
        //     }

        //     code.append(&mut triton_asm!(
        //         // _ [*fields] *field_or_fieldsize *field

        //         swap 1
        //         // _ [*fields] *field *field_or_fieldsize
        //         // _ [*fields'] *field_or_fieldsize
        //     ));

        //     match static_size {
        //         Some(size) => code.append(&mut triton_asm!(
        //             // _ [*fields'] *field
        //             push { size }
        //             add
        //             // _ [*fields'] *next_field
        //         )),
        //         None => code.append(&mut triton_asm!(
        //             // _ [*fields'] *field_size
        //             read_mem
        //             // _ [*fields'] *field_size field_size

        //             push 1
        //             add
        //             add
        //             // _ [*fields'] *next_field
        //         )),
        //     }
        // }

        // code.push(triton_instr!(pop));
        // // _ [*fields]

        // code
    }

    // let mut code = triton_asm!();
    // for (field_id, field_type) in self.field_ids_and_types_reversed_for_tuples() {}
    // let mut code = triton_asm!();
    // for (field_id, field_type) in self.field_ids_and_types_reversed_for_tuples() {
    //     let static_size = field_type.bfield_codec_length();
    //     if static_size.is_none() {
    //         code.append(&mut triton_asm!(push 1 add));
    //     }
    //     code.append(&mut field_type.copy_from_memory(None, state, true));
    //     // _ [field] first_word_of_field
    //     match static_size {
    //         None => {
    //             code.append(&mut triton_asm!(
    //                 // _ [field] first_word_of_field

    //                 push -1
    //                 add
    //                 // [field] *size

    //                 read_mem
    //                 // [field] *size size

    //                 push 1
    //                 add
    //                 add
    //                 // [field] *next_field
    //             ));
    //         }
    //         Some(static_size) => {
    //             code.append(&mut triton_asm!(
    //                 // _ [field] first_word_of_field

    //                 push {static_size}
    //                 add
    //                 // _ [field] *next_field
    //             ));
    //         }
    //     }
    // }

    // code.push(triton_instr!(pop));

    // println!(
    //     "code for loading struct from memory: {}",
    //     code.iter().join("\n")
    // );
    // code
    // }

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
        for (haystack_field_id, haystack_type) in self.field_ids_and_types_reversed_for_tuples() {
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
