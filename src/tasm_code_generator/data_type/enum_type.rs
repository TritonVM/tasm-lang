use itertools::Itertools;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::triton_instr;
use triton_vm::BFieldElement;

use crate::ast_types;
use crate::ast_types::EnumType;
use crate::libraries::LibraryFunction;
use crate::tasm_code_generator::CompilerState;
use crate::tasm_code_generator::SubRoutine;

impl EnumType {
    /// BEFORE: _ *discriminant
    /// AFTER:  _ [data] [padding] discriminant
    pub(crate) fn load_from_memory(&self, state: &mut CompilerState) -> Vec<LabelledInstruction> {
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

        let mut code = triton_asm!(
            // _ *discriminant


            call {get_variant_data_pointers_label}
            // _ *discriminant

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

            code.append(&mut triton_asm!(
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
                load_variant_fields = triton_asm!(
                    // [data] _

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
                );
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

        let value_stack_size = self.stack_size();
        let pad_subroutine = pad_subroutine();
        state.add_library_function(pad_subroutine.clone());

        code.append(&mut triton_asm!(
            // _ [data]
            push {pointer_for_words_loaded_acc}
            read_mem
            swap 1
            pop

            // _ [data] words_loaded
            push -1
            mul
            push {value_stack_size}
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
        ));

        code
    }

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

        assert!(
            self.variants
                .iter()
                .all(|x| x.1.as_tuple_type().element_count() < 2),
            "Cannot handle so much data in enums. enum type: \"{}\"",
            self.name
        );

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
                _n => {
                    todo!()
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

    /// Return the function to get data field pointers for variant data, at
    /// runtime.
    /// BEFORE: _ *discriminant
    /// AFTER: _ [(*data_field, stack_size, is_reference_type)] field_count
    fn get_variant_data_pointers_and_field_sizes_at_runtime_old(
        &self,
    ) -> (SubRoutine, Vec<SubRoutine>) {
        // TODO: Return value could be delivered as pointer to
        // static memory allocation, in case we want to allow
        // more data in the fields. This function puts a limit
        // on the number of fields in a variant type to 7.
        assert!(
            self.variants
                .iter()
                .all(|x| x.1.as_tuple_type().element_count() < 2),
            "Cannot handle so much data in enums. enum type: \"{}\"",
            self.name
        );
        let function_label = format!(
            "get_variant_data_pointers_and_data_sizes_at_runtime_{}",
            self.name
        );
        let mut subroutines: Vec<SubRoutine> = vec![];
        let mut acc_code = triton_asm!(
            {function_label}:
        );

        for (haystack_discriminant, dtype) in self.variant_types().enumerate() {
            let subroutine_label = format!(
                "{}_variant_size_subroutine_{}",
                self.name, haystack_discriminant
            );
            acc_code.append(&mut triton_asm!(
                // <[(*data_field data_field_stack_size is_reference_type)] field_count> *discriminant

                read_mem
                // <variant_info> *discriminant discriminant

                push {haystack_discriminant}
                eq
                // <variant_info> *discriminant (discriminant == haystack)

                skiz
                call {subroutine_label}

                // <variant_info> *discriminant
            ));

            let data_fields = dtype.as_tuple_type();
            let subroutine = match data_fields.element_count() {
                0 => triton_asm!(
                    {subroutine_label}:
                        // _ *discriminant

                        push 0
                        // _ *discriminant field_count

                        swap 1
                        // _ field_count *discriminant

                        return
                ),
                1 => {
                    let data_field = &data_fields[0];
                    let static_encoding_length = data_field.bfield_codec_length();
                    let (field_stack_size, is_reference_type, has_size_indicator) =
                        if data_field.data_always_lives_in_memory() {
                            (1, true, static_encoding_length.is_none())
                        } else {
                            (
                                data_field.stack_size(),
                                false,
                                static_encoding_length.is_none(),
                            )
                        };

                    println!("enum_type");
                    dbg!(data_field);
                    dbg!(static_encoding_length);
                    dbg!(field_stack_size);
                    dbg!(is_reference_type);
                    dbg!(has_size_indicator);

                    triton_asm!(
                        {subroutine_label}:
                            // *discriminant

                            push {field_stack_size}
                            push {is_reference_type as u64}
                            push 1
                            // _ *discriminant field_stack_size is_reference_type field_count

                            dup 3
                            push {1 + has_size_indicator as u64}
                            add
                            // _ *discriminant field_stack_size is_reference_type field_count *data_field

                            swap 4
                            // _ *data_field field_size is_reference_type field_count *discriminant

                            return
                    )
                }
                _n => {
                    todo!()
                }
            };

            subroutines.push(subroutine.try_into().unwrap());
        }

        // _ [(*data_field data_field_stack_size is_reference_type)] field_count *discriminant

        acc_code.append(&mut triton_asm!(
                pop
                // _ [(*data_field data_field_stack_size is_reference_type)] field_count

                return
        ));

        (acc_code.try_into().unwrap(), subroutines)
    }

    /// Return the code to put enum-variant data fields on top of stack.
    /// Does not consume the enum_expr pointer.
    /// BEFORE: _ *enum_expr
    /// AFTER: _ *enum_expr [*variant-data-fields]
    pub(crate) fn get_variant_data_fields_in_memory(
        &self,
        variant_name: &str,
    ) -> Vec<(Vec<LabelledInstruction>, ast_types::DataType)> {
        // TODO: Can we get this code to consume *enum_expr instead?

        // Example: Foo::Bar(Vec<u32>)
        // Memory layout will be:
        // [discriminant, field_size, [u32_list]]
        // In that case we want to return code to get *u32_list.

        // You can assume that the stack has a pointer to `discriminant` on
        // top. So we want to return the code
        // `push 1 add push 1 add`
        let data_types = self.variant_data_type(variant_name);

        // Skip discriminant
        let mut acc_code = vec![triton_instr!(push 1), triton_instr!(add)];
        let mut ret: Vec<Vec<LabelledInstruction>> = vec![];

        // Invariant: _ *enum_expr [*preceding_fields]
        for (field_count, dtype) in data_types.clone().into_iter().enumerate() {
            match dtype.bfield_codec_length() {
                Some(size) => {
                    // field size is known statically, does not need to be read
                    // from memory
                    // stack: _ *enum_expr [*preceding_fields]
                    ret.push(triton_asm!(
                        // _ *enum_expr [*preceding_fields]

                        dup {field_count}
                        // _ *enum_expr [*previous_fields] *enum_expr

                        {&acc_code}
                        // _ *enum_expr [*previous_fields] *current_field
                    ));
                    acc_code.append(&mut triton_asm!(push {size} add));
                }
                None => {
                    // Current field size must be read from memory
                    // stack: _ *enum_expr [*preceding_fields]
                    ret.push(triton_asm!(
                        // _ *enum_expr [*preceding_fields]

                        dup {field_count}
                        // _ *enum_expr [*previous_fields] *enum_expr

                        {&acc_code}
                        // _ *enum_expr [*previous_fields] *current_field_size

                        push 1
                        add
                        // _ *enum_expr [*previous_fields] *current_field
                    ));

                    acc_code.append(&mut triton_asm!(
                        read_mem
                        add
                        push 1
                        add
                    ));
                }
            }
        }

        ret.into_iter().zip_eq(data_types).collect_vec()
    }

    /// Return the constructor that is called by an expression evaluating to an
    /// enum type. E.g.: `Foo::A(100u32);`
    pub(crate) fn variant_constructor(&self, variant_name: &str) -> LibraryFunction {
        let data_tuple = self.variant_data_type(variant_name);
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

fn read_data_size_elements_to_stack_loop() -> [SubRoutine; 3] {
    let reference_type_branch_label = "load_enum_variant_data_field_to_stack_reference_type_branch";

    // BEFORE: _ *discriminant *last_word_in_field data_size next_size is_reference_type
    // AFTER:  _ [data'] *discriminant *garbage data_size 0
    let reference_type_branch = triton_asm!(
        {reference_type_branch_label}:
        // _ *discriminant *last_word_in_field data_size data_size 1

        pop
        // _ *discriminant *last_word_in_field data_size next_size

        swap 2
        // _ *discriminant next_size data_size *last_word_in_field

        swap 3
        // _ *last_word_in_field next_size data_size *discriminant

        swap 2
        // _ *last_word_in_field *discriminant next_size data_size

        push 0
        // _ *last_word_in_field *discriminant garbage next_size 0

        push 0

        // _ *last_word_in_field *discriminant garbage next_size 0 0

        return
    );

    let value_type_branch_label = "load_enum_variant_data_field_to_stack_value_type_branch";
    let value_type_branch = triton_asm!(
        {value_type_branch_label}:
            // Invariant: [data] *discriminant *next_data_word data_size remaining_words
            // loop condition: remaining_words == 0
            dup 0
            push 0
            eq
            skiz
                return

            // [data] *discriminant *next_data_word data_size remaining_words

            swap 2
            // [data] *discriminant remaining_words data_size *next_data_word

            read_mem
            // [data] *discriminant remaining_words data_size *next_data_word data_element

            swap 4
            // [data] data_element remaining_words data_size *next_data_word *discriminant
            // [data'] remaining_words data_size *next_data_word *discriminant

            swap 3
            // [data'] *discriminant data_size *next_data_word remaining_words

            push -1
            add
            // [data'] *discriminant data_size *next_data_word remaining_words'

            swap 1
            push -1
            add
            // [data'] *discriminant data_size remaining_words' *next_data_word'

            swap 2
            // [data'] *discriminant *next_data_word' remaining_words' data_size

            swap 1
            // [data'] *discriminant *next_data_word' data_size remaining_words'

            recurse
    );

    let main_function_label = "load_enum_variant_data_field_to_stack";
    let main_function: SubRoutine = triton_asm!(
        {main_function_label}:
        // *discriminant *last_word_in_field data_size data_size is_reference_type
        push 1
        swap 1

        // *discriminant *last_word_in_field data_size data_size 1 is_reference_type
        skiz
            call {reference_type_branch_label}
        skiz
            call {value_type_branch_label}

        return
    )
    .try_into()
    .unwrap();

    [
        main_function,
        reference_type_branch.try_into().unwrap(),
        value_type_branch.try_into().unwrap(),
    ]
}

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
