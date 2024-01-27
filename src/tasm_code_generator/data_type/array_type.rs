use crate::ast::{self, ArrayExpression};
use crate::tasm_code_generator::{
    compile_expr, pop_n, write_n_words_to_memory_leaving_address, CompilerState,
};
use crate::triton_vm::triton_asm;
use crate::type_checker::GetType;
use crate::LabelledInstruction;
use crate::{ast_types, type_checker};
use itertools::Itertools;

pub(crate) fn compile_array_expr(
    state: &mut CompilerState,
    array_expr: &ArrayExpression<type_checker::Typing>,
    array_type: &type_checker::Typing,
) -> Vec<LabelledInstruction> {
    // Compile elements left-to-right and store each element in memory

    // TODO: This does not handle arrays of elements whose sizes
    // are not known at compile time.

    let ast_types::DataType::Array(array_type) = &array_type.get_type() else {
        panic!("Type must be array");
    };

    let total_size_in_memory: usize = array_type.size_in_memory();
    let dyn_malloc_snippet_label =
        state.import_snippet(Box::new(tasm_lib::memory::dyn_malloc::DynMalloc));
    let allocate_memory_for_array = triton_asm!(
        // _

        push {total_size_in_memory}
        call {dyn_malloc_snippet_label}

        // _ *array

        dup 0
        // _ *array *array
    );
    state.new_value_identifier("array_pointer", &(array_type.into()));
    state.new_value_identifier("array_pointer", &(array_type.into()));
    let element_size: usize = array_type.element_type.stack_size();
    let move_element_to_memory = write_n_words_to_memory_leaving_address(element_size);

    let store_array_in_memory = match array_expr {
        ast::ArrayExpression::ElementsSpecified(element_expressions) => {
            element_expressions
                .iter()
                .enumerate()
                .map(|(arg_pos, arg_expr)| {
                    let context = format!("_array_{arg_pos}");
                    let (_, evaluate_element) = compile_expr(arg_expr, &context, state);
                    state.function_state.vstack.pop().unwrap();

                    triton_asm!(
                        // _ *array *free_word

                        {&evaluate_element}
                        // _ *array *free_word [elem]

                        dup {element_size}
                        // _ *array *free_word [elem] *free_word

                        {&move_element_to_memory}
                        // _ *array *free_word *next_free_word

                        swap 1
                        pop 1
                        // _ *array *next_free_word
                    )
                })
                .concat()
        }

        ArrayExpression::Repeat { element, length } => {
            let context = "_array_repeat";
            let (_, evaluate_element) = compile_expr(element, context, state);
            state.function_state.vstack.pop().unwrap();

            let dup_element = format!("dup {}\n", element_size - 1).repeat(element_size);

            let copy_element_to_memory_length_times = (0..*length)
                .flat_map(|_| {
                    triton_asm!(
                        // _ *array *free_word [elem]

                        {dup_element}
                        // _ *array *free_word [elem] [elem]

                        dup {2 * element_size}
                        // _ *array *free_word [elem] [elem] *free_word

                        {&move_element_to_memory}
                        // _ *array *free_word [elem] *next_free_word

                        swap {1 + element_size}
                        pop 1
                        // _ *array *next_free_word [elem]
                    )
                })
                .collect_vec();

            let clean_up_element = pop_n(element_size);

            triton_asm!(
                // _ *array *array

                {&evaluate_element}
                // _ *array *array [elem]

                {&copy_element_to_memory_length_times}
                // _ *array *next_free_word [elem]

                {&clean_up_element}
                // _ *array *next_free_word
            )
        }
    };

    // Remove the added array pointers from compiler's view of stack
    state.function_state.vstack.pop().unwrap();
    state.function_state.vstack.pop().unwrap();

    triton_asm!(
        // _
        {&allocate_memory_for_array}
        // _ *array *array

        {&store_array_in_memory}
        // _ *array *next_free_word

        pop 1
        // _ *array
    )
}
