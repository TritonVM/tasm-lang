use itertools::Itertools;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::triton_instr;
use triton_vm::BFieldElement;

use crate::ast;
use crate::ast_types;
use crate::ast_types::EnumType;
use crate::libraries::LibraryFunction;
use crate::tasm_code_generator::CompilerState;
use crate::tasm_code_generator::SubRoutine;

impl EnumType {
    /// Return a label for the subroutine which loads this enum type from
    /// memory onto the stack
    fn label_for_memory_loading_subroutine(&self) -> String {
        format!("load_{}_to_stack", self.name)
    }

    /// BEFORE: _ *discriminant
    /// AFTER:  _ [data] [padding] discriminant
    pub(crate) fn load_from_memory(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
        let load_subroutine_label = self.label_for_memory_loading_subroutine();
        if state.contains_subroutine(&load_subroutine_label) {
            return triton_asm!(call {
                load_subroutine_label
            });
        }

        let (field_pointer_pointer, get_variant_data_pointers, subroutines) =
            self.get_variant_data_pointers_with_sizes(state);
        let pointer_for_words_loaded_acc: u64 = state
            .global_compiler_state
            .snippet_state
            .kmalloc(1)
            .try_into()
            .unwrap();
        let pointer_for_discriminant_pointer: u64 = state
            .global_compiler_state
            .snippet_state
            .kmalloc(1)
            .try_into()
            .unwrap();
        let get_variant_data_pointers_label = get_variant_data_pointers.get_label();
        for sr in [vec![get_variant_data_pointers], subroutines].concat() {
            state.add_library_function(sr);
        }

        let mut load_subroutine = triton_asm!(
            {load_subroutine_label}:
                // _ *discriminant


                call {get_variant_data_pointers_label}
                // _ *discriminant
                // memory: [(*field0, _), (*field1, _), ...]

                // Initialize word-counter to zero
                push {pointer_for_words_loaded_acc}
                push 0
                write_mem
                pop
                // _ *discriminant

                // Store discriminant pointer
                push {pointer_for_discriminant_pointer}
                swap 1
                // _ **discriminant *discriminant

                write_mem
                pop
                // _
        );

        // Now match on variant to load the data
        for (haystack_discriminant, (variant_name, variant_type)) in
            self.variants.iter().enumerate()
        {
            let subroutine_for_loading_variant_fields_label =
                format!("{}_load_field_subroutine_{}", self.name, variant_name);

            load_subroutine.append(&mut triton_asm!(
                // _ [data]

                push {pointer_for_discriminant_pointer}
                // _ [data] **discriminant

                read_mem
                // _ [data] **discriminant *discriminant

                read_mem
                // _ [data] **discriminant *discriminant discriminant

                swap 2
                pop
                pop
                // _ [data] discriminant

                push {haystack_discriminant}
                eq
                // _ [data] (discriminant == haystack)

                skiz
                call {subroutine_for_loading_variant_fields_label}

                // _ [data']
            ));

            let data_fields = variant_type.as_tuple_type();
            let mut load_variant_fields = triton_asm!();
            for (field_count, data_field_type) in data_fields.fields.iter().enumerate() {
                let load_field = data_field_type.load_from_memory(None, state);
                let field_stack_size = data_field_type.stack_size();
                load_variant_fields.append(&mut triton_asm!(
                    // _ [data]

                    push {field_pointer_pointer.value() + 2 * field_count as u64}
                    // _ [data] *field[field_count].pointer

                    read_mem
                    swap 1
                    pop

                    // _ [data] *field
                    {&load_field}

                    // _ [data] [field_data]
                    // _ [data']

                    push {field_stack_size}
                    // _ [data'] field_stack_size

                    push {pointer_for_words_loaded_acc}
                    // _ [data'] field_stack_size *word_acc

                    read_mem
                    swap 1
                    pop
                    // _ [data'] field_stack_size word_acc

                    add
                    // _ [data'] word_acc'

                    push {pointer_for_words_loaded_acc}
                    swap 1
                    write_mem
                    pop
                    // _ [data']
                ));
            }

            // acc_code_for_subroutine.append(&mut triton_asm!(return));
            let code_for_subroutine = triton_asm!(
                {subroutine_for_loading_variant_fields_label}:
                    // _
                    {&load_variant_fields}

                    // _ [data]
                    return
            );
            state.add_library_function(code_for_subroutine.try_into().unwrap());
        }

        let size_of_data_plus_padding = self.stack_size() - 1;
        let pad_subroutine = pad_subroutine();
        state.add_library_function(pad_subroutine.clone());

        load_subroutine.append(&mut triton_asm!(
            // _ [data]
            push {pointer_for_words_loaded_acc}
            read_mem
            swap 1
            pop

            // _ [data] words_loaded
            push -1
            mul
            push {size_of_data_plus_padding}
            add
            // _ [data] (stack_size - words_loaded)
            // _ [data] pad_needed

            call {pad_subroutine.get_label()}
            // _ [data] [padding] 0

            pop
            // _ [data] [padding]

            push {pointer_for_discriminant_pointer}
            // _ [data] [padding] **discriminant

            read_mem
            read_mem
            // _ [data] [padding] **discriminant *discriminant discriminant

            swap 2
            pop
            pop
            // _ [data] [padding] discriminant

            return
        ));

        // load_subroutine
        state.add_library_function(load_subroutine.try_into().unwrap());

        triton_asm!(call {
            load_subroutine_label
        })
    }

    // TODO: I don't think we're using the stored field sizes for anything.
    // Get rid of those!
    /// Return the code to get all field pointers for the discriminant whose
    /// pointer lives on top of the stack.
    /// The field pointers are stored in memory at a statically known address.
    /// Returns (**fields, function to call, helper functions)
    /// BEFORE: _ *discriminant
    /// AFTER: _ *discriminant
    /// Builds this list in memory:
    /// [(*field_0, field_size_0), (*field_1, field_size_1), ...]
    fn get_variant_data_pointers_with_sizes(
        &self,
        state: &mut CompilerState,
    ) -> (BFieldElement, SubRoutine, Vec<SubRoutine>) {
        let max_field_count = self
            .variants
            .iter()
            .map(|(_name, data_type)| data_type.as_tuple_type().element_count())
            .max()
            .unwrap_or_default();

        // Allocate two words for each field: field size and field pointer
        let field_pointer_pointer: u64 = state
            .global_compiler_state
            .snippet_state
            .kmalloc(2 * max_field_count)
            .try_into()
            .unwrap();

        let mut subroutines: Vec<SubRoutine> = vec![];
        let mut set_all_field_pointers = vec![];

        for (haystack_discriminant, (variant_name, variant_type)) in
            self.variants.iter().enumerate()
        {
            let subroutine_label =
                format!("{}_find_pointers_subroutine_{}", self.name, variant_name);
            set_all_field_pointers.append(&mut triton_asm!(
                // _ *discriminant

                read_mem
                // _ *discriminant discriminant

                push {haystack_discriminant}
                eq
                // _ *discriminant (discriminant == haystack)

                skiz
                call {subroutine_label}

                // _ *discriminant
            ));

            let data_fields = variant_type.as_tuple_type();
            let subroutine = match data_fields.element_count() {
                0 => triton_asm!(
                    {subroutine_label}:
                        // _ *discriminant

                        return
                ),
                1 => {
                    let data_field = &data_fields[0];
                    let static_encoding_length = data_field.bfield_codec_length();

                    let write_field_pointer_and_size = match static_encoding_length {
                        Some(_static_size) => {
                            // We don't need to store size, since it's statically known
                            triton_asm!(
                                // _ *discriminant

                                push 1
                                add
                                // _ *first_field

                                push {field_pointer_pointer}
                                swap 1
                                // _ *field[0].pointer *first_field

                                write_mem
                                // _ *field[0].pointer

                                pop
                                // _
                            )
                        }
                        None => triton_asm!(
                            // _ *discriminant
                            push 1
                            add
                            // _ *first_field_size

                            read_mem
                            // _ *first_field_size field_size

                            push {field_pointer_pointer + 1}
                            swap 1
                            // _ *first_field_size *field[0].size field_size

                            write_mem
                            // _ *first_field_size *field[0].size

                            pop
                            // _ *first_field_size

                            push 1
                            add
                            // _ *first_field

                            push {field_pointer_pointer}
                            swap 1
                            // _ *field[0].pointer *first_field

                            write_mem
                            // _ *field[0].pointer

                            pop
                            // _
                        ),
                    };

                    triton_asm!(
                        {subroutine_label}:
                        // _ *discriminant

                        dup 0
                        // _ *discriminant *discriminant

                        {&write_field_pointer_and_size}

                        // _ *discriminant
                        return
                    )
                }
                n => {
                    let mut handle_fields = triton_asm!();

                    for field_count in 0..n {
                        let field_index = data_fields.element_count() - 1 - field_count;
                        let data_field = &data_fields[field_index];
                        let static_encoding_length = data_field.bfield_codec_length();
                        let pointer_pointer = field_pointer_pointer + 2 * field_index as u64;
                        let mut write_field_pointer_and_size = match static_encoding_length {
                            Some(static_size) => {
                                // We don't need to store size, since it's statically known
                                triton_asm!(
                                    // _ *field

                                    dup 0
                                    // _ *field *field

                                    push {pointer_pointer}
                                    swap 1
                                    // _ *field *field[field_index].pointer *field

                                    write_mem
                                    // _ *field *field[field_index].pointer

                                    pop
                                    // _ *field

                                    push {static_size}
                                    add
                                    // _ *next_field
                                )
                            }
                            None => triton_asm!(
                                // _ *field_size

                                read_mem
                                // _ *field_size field_size

                                push {pointer_pointer + 1}
                                swap 1
                                // _ *field_size *field[field_index].size field_size

                                write_mem
                                // _ *field_size *field[field_index].size

                                pop
                                // _ *field_size

                                read_mem
                                // _ *field_size field_size

                                swap 1
                                // _ field_size *field_size

                                push 1
                                add
                                // _ field_size *field

                                push {pointer_pointer}
                                dup 1
                                // _ field_size *field *field[field_index].pointer *field

                                write_mem
                                // _ field_size *field[field_index].pointer

                                pop
                                // _ field_size *field

                                add
                                // _ next_field
                            ),
                        };

                        handle_fields.append(&mut write_field_pointer_and_size);
                    }

                    triton_asm!(
                        {subroutine_label}:
                            // _ *discriminant

                            dup 0

                            push 1
                            add
                            // _ *discriminant *first_field_or_field_size

                            {&handle_fields}
                            // _ *discriminant *garbage

                            pop
                            // _ *discriminant
                            return
                    )
                }
            };

            subroutines.push(subroutine.try_into().unwrap());
        }

        let function_label = format!("get_variant_data_pointers_at_runtime_{}", self.name);
        let fn_code = triton_asm!(
            {function_label}:
                {&set_all_field_pointers}
                return
        );

        (
            field_pointer_pointer.into(),
            fn_code.try_into().unwrap(),
            subroutines,
        )
    }

    /// Return the subroutine label for putting pointers to field
    /// elements on top of the stack
    /// BEFORE: _ *discriminant
    /// AFTER: _ *discriminant [*variant-data-fields]
    pub(crate) fn get_variant_data_fields_in_memory(
        &self,
        enum_variant_selector: &ast::EnumVariantSelector,
        state: &mut CompilerState,
    ) -> String {
        let data_types = enum_variant_selector.get_bindings_type(
            &self
                .variant_data_type(&enum_variant_selector.variant_name)
                .as_tuple_type(),
        );

        let label_for_subroutine = enum_variant_selector.label_for_binding_subroutine(true);

        // If subroutine was already constructed and included, just return the label, otherwise
        // build the subroutine and include it in state.
        if state.contains_subroutine(&label_for_subroutine) {
            return label_for_subroutine;
        }

        let mut acc_code = triton_asm!(
            {label_for_subroutine}:

                // _ *discriminant
                dup 0
                push 1
                add
                // _ *discriminant *first_field_or_field_size
        );
        // let mut ret: Vec<Vec<LabelledInstruction>> = vec![];

        // Before this loop: _ *discriminant *first_field_or_field_size
        // Goal: _ *discriminant field_0 field_1 field_2 ...
        for dtype in data_types.clone().into_iter().rev() {
            let mut get_field_pointer = match dtype.bfield_codec_length() {
                Some(size) => {
                    triton_asm!(
                        // _ *discriminant *field

                        dup 0
                        // _ *discriminant *field *field

                        push {size}
                        add
                        // _ *discriminant *field *next_field_or_size
                    )
                }
                None => {
                    triton_asm!(
                        // _ *discriminant *field_size

                        read_mem
                        // _ *discriminant *field_size field_size

                        swap 1
                        push 1
                        add
                        swap 1
                        // _ *discriminant *field field_size

                        dup 1
                        add
                        // _ *discriminant *field *next_field_or_size
                    )
                }
            };

            acc_code.append(&mut get_field_pointer);
        }

        // We have:
        // _ *discriminant [*field_2, *field_1, *field_0]
        // We want:
        // _ *discriminant [*field_0, *field_1, *field_2]

        // So we need to flip top `n` words on the stack
        let flip_code = match data_types.element_count() {
            0 => triton_asm!(),
            1 => triton_asm!(),
            2 => triton_asm!(swap 1),
            3 => triton_asm!(swap 2),
            4 => triton_asm!(
                // _ e3 e2 e1 e0
                swap 3
                // _ e0 e2 e1 e3
                swap 1
                // _ e2 e0 e1 e3
                swap 2
                // _ e1 e0 e2 e3
                swap 1
                // _ e0 e1 e2 e3
            ),
            5 => triton_asm!(
                swap 4
                // _ e0 e3 e2 e1 e4
                swap 1
                // _ e0 e3 e2 e4 e1
                swap 3
                // _ e0 e1 e2 e4 e3

                swap 1
                // _ e0 e1 e2 e3 e4
            ),
            _ => todo!("What?!"),
        };

        // acc_code.append(&mut flip_code);
        let acc_code = triton_asm!(
            {&acc_code}
            // _ *discriminant [*field (hi-to-low)] *garbage

            pop
            // _ *discriminant [*field (hi-to-low)]

            {&flip_code}
            // _ *discriminant [*field (low-to-hi)]

            return
        );

        state.add_library_function(acc_code.try_into().unwrap());

        label_for_subroutine
    }

    /// Return the constructor that is called by an expression evaluating to an
    /// enum type. E.g.: `Foo::A(100u32);`
    pub(crate) fn variant_tuple_constructor(&self, variant_name: &str) -> LibraryFunction {
        let data_tuple = self.variant_data_type(variant_name).as_tuple_type();
        assert!(
            !data_tuple.is_unit(),
            "Variant {variant_name} in enum type {} does not carry data",
            self.name
        );

        let constructor_name = format!("{}::{variant_name}", self.name);
        let constructor_return_type = ast_types::DataType::Enum(Box::new(self.to_owned()));
        let mut constructor = data_tuple.constructor(&constructor_name, constructor_return_type);

        // Append padding code to ensure that all enum variants have the same size
        // on the stack.
        let padding = vec![triton_instr!(push 0); self.padding_size(variant_name)];
        let discriminant = self.variant_discriminant(variant_name);
        let discriminant = triton_asm!(push { discriminant });

        constructor.body = [constructor.body, padding, discriminant].concat();

        constructor
    }
}

/// Return the code to push `n` zeros to stack.
/// BEFORE: _ n
/// AFTER:  _ [0; n] 0
fn pad_subroutine() -> SubRoutine {
    let pad_subroutine_loop_label = "pad_loop_for_enum_to_stack";
    triton_asm!(
        // Invariant: [data] [padding] remaining_pad
        {pad_subroutine_loop_label}:
            // loop condition (remaining_pad == 0)
            dup 0
            push 0
            eq
            skiz
                return

            push 0
            swap 1

            push -1
            add
            // _ [data] [padding'] remaining_pad'

            recurse
    )
    .try_into()
    .unwrap()
}
