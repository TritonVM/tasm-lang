use itertools::Itertools;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::triton_instr;

use crate::ast_types;
use crate::libraries::LibraryFunction;
use crate::tasm_code_generator::CompilerState;
use crate::tasm_code_generator::SubRoutine;

/// BEFORE: _ *discriminant
/// AFTER:  _ [data] [padding] discriminant
pub(super) fn load_enum_from_memory(
    enum_type: &ast_types::EnumType,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    // Strategy:
    // get_variant_data_fields_in_memory
    // _ *discriminant [(*data_field, stack_size)] field_count
    let (subroutine, subroutines) =
        enum_type.get_variant_data_pointers_and_field_sizes_at_runtime();
    let get_variant_data_size_label = subroutine.get_label();
    for sr in [vec![subroutine], subroutines].concat() {
        state.add_library_function(sr);
    }

    let mut code = triton_asm!(
        dup 0
        // _ *discriminant *discriminant

        call {get_variant_data_size_label}
        // _ *discriminant [(*data_field, stack_size, is_reference_type)] field_count
    );

    // For now we assume that max number of fields is 1.
    // If that were to change the field meta data needs to be stored in
    // memory!
    // We *should* be able to statically allocate the correct amount of data for that,
    // I think.

    let [load_field_data_from_memory, load_field_reference_type_branch, load_field_value_branch] =
        read_data_size_elements_to_stack_loop();
    let load_field_data_from_memory_label = load_field_data_from_memory.get_label();
    state.add_library_function(load_field_data_from_memory);
    state.add_library_function(load_field_reference_type_branch);
    state.add_library_function(load_field_value_branch);

    let load_variant_data_from_memory_label = "load_enum_variant_data_to_stack_loop";
    let read_data_size_elements_to_stack_loop: SubRoutine = triton_asm!(
        // Invariant: _ *discriminant [(*data_field, stack_size is_reference_type)] remaining_fields
    {load_variant_data_from_memory_label}:
        // Check loop-condition, remaining_fields == 0
        dup 0
        push 0
        eq
        skiz
            return

        // _ *discriminant [other_fields] *next_field next_size is_reference_type remaining_fields


        // We cannot know how big `data` is, *and* we cannot know how many other
        // fields there are. So the task is impossible. We need to access
        // [(*data_field, stack_size)] from memory!

        // BUT: Let's just assume *one* field for now and get that to work
        // TODO: This is wrong if there are multiple fields in the variant's
        // data.
        // _ *discriminant *next_field next_size remaining_fields

        pop
        // _ *discriminant *next_field next_size is_reference_type

        swap 2
        // _ *discriminant is_reference_type next_size *next_field

        dup 1
        push -1
        add
        add
        // _ *discriminant is_reference_type next_size *last_word_of_field

        swap 2
        // _ *discriminant *last_word_of_field next_size is_reference_type

        dup 1
        swap 1
        // _ *discriminant *last_word_of_field next_size next_size is_reference_type

        call {load_field_data_from_memory_label}
        // _ [field_data] *discriminant garbage data_size 0

        pop
        // _ [field_data] *discriminant garbage data_size

        swap 1
        pop
        // _ [field_data] *discriminant data_size

        // TODO: Change to `recurse` once above logic is fixed!
        return
    )
    .try_into()
    .unwrap();
    state.add_library_function(read_data_size_elements_to_stack_loop);

    let size_of_data_plus_padding = enum_type.stack_size() - 1;
    code.append(&mut triton_asm!(
        // _ *discriminant [(*data_field, stack_size)] remaining_fields
        call {load_variant_data_from_memory_label}
        // _ [field_data] *discriminant data_size

        push -1
        mul
        // _ [field_data] *discriminant -data_size

        push {size_of_data_plus_padding}
        add
        // _ [field_data] *discriminant padding_words
    ));

    let pad_subroutine_loop = pad_subroutine();
    let pad_subroutine_loop_label = pad_subroutine_loop.get_label();
    state.add_library_function(pad_subroutine_loop);
    code.append(&mut triton_asm!(
        // _ [data] *discriminant padding_words
        call { pad_subroutine_loop_label }
        // _ [data] [padding'] *discriminant 0

        pop
        // _ [data] [padding'] *discriminant

        read_mem
        // _ [data] [padding'] *discriminant discriminant

        swap 1
        pop
        // _ [data] [padding'] discriminant
    ));

    code
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
        // Invariant: [data] [padding] *discriminant remaining_pad
        {pad_subroutine_loop_label}:
            // loop condition (remaining_pad == 0)
            dup 0
            push 0
            eq
            skiz
                return

            // _ [data] [padding] *discriminant remaining_pad
            push 0
            // _ [data] [padding] *discriminant remaining_pad 0

            swap 2
            // _ [data] [padding'] remaining_pad *discriminant

            swap 1
            // _ [data] [padding'] *discriminant remaining_pad

            push -1
            add
            // _ [data] [padding'] *discriminant remaining_pad'

            recurse
    )
    .try_into()
    .unwrap()
}

impl ast_types::EnumType {
    /// Return the function to get data field pointers for variant data, at
    /// runtime.
    /// BEFORE: _ *discriminant
    /// AFTER: _ [(*data_field, stack_size, is_reference_type)] field_count
    fn get_variant_data_pointers_and_field_sizes_at_runtime(
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
    pub(super) fn get_variant_data_fields_in_memory(
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
