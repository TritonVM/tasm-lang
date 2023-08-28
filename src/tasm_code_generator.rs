pub mod subroutine;

use itertools::{Either, Itertools};
use num::{One, Zero};
use std::collections::{HashMap, HashSet};
use tasm_lib::library::Library as SnippetState;
use tasm_lib::memory::dyn_malloc::DynMalloc;
use tasm_lib::snippet::BasicSnippet;
use tasm_lib::{arithmetic, hashing};
use triton_vm::instruction::LabelledInstruction;
use triton_vm::op_stack::OpStackElement;
use triton_vm::{triton_asm, triton_instr, Program};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use self::subroutine::SubRoutine;
use crate::ast::AbstractArgument;
use crate::libraries::{self};
use crate::stack::Stack;
use crate::types::{is_list_type, GetType};
use crate::{ast, types};

// the compiler's view of the stack, including information about whether value has been spilled to memory
type VStack = Stack<(ValueIdentifier, (ast::DataType, Option<u32>))>;
type VarAddr = HashMap<String, ValueIdentifier>;

impl VStack {
    /// Returns (stack_position, data_type, maybe_memory_location) where `stack_position` is the top of
    /// the value on the stack, i.e. the most shallow part of the value. Top of stack has index 0.
    /// May return a depth that exceeds the addressable space (16 elements) in which case spilling
    /// may be required.
    pub fn find_stack_value(
        &self,
        seek_addr: &ValueIdentifier,
    ) -> (usize, ast::DataType, Option<u32>) {
        let mut position: usize = 0;
        for (_i, (found_addr, (data_type, spilled))) in self.inner.iter().rev().enumerate() {
            if seek_addr == found_addr {
                return (position, data_type.to_owned(), spilled.to_owned());
            }

            position += data_type.stack_size();

            // By asserting after `+= data_type.size_of()`, we check that the deepest part
            // of the sought value is addressable, not just the top part of the value.
        }

        panic!("Cannot find {seek_addr} on vstack")
    }

    /// Return the stack position of an element inside a tuple.
    /// Returns (stack_position, data_type, maybe_memory_location).
    fn find_tuple_element(
        &self,
        seek_addr: &ValueIdentifier,
        tuple_index: usize,
    ) -> (usize, ast::DataType, Option<u32>) {
        let (tuple_value_position, tuple_type, spilled) = self.find_stack_value(seek_addr);
        let element_types = if let ast::DataType::Tuple(ets) = &tuple_type {
            ets
        } else {
            panic!("Expected type was tuple.")
        };

        // Last elemen of the tuple is stored on top of the stack
        let tuple_depth: usize = element_types
            .iter()
            .enumerate()
            .filter(|(i, _x)| *i > tuple_index)
            .map(|(_i, x)| x.stack_size())
            .sum::<usize>();

        (
            tuple_value_position + tuple_depth,
            element_types[tuple_index].clone(),
            spilled.map(|x| x + tuple_depth as u32),
        )
    }

    fn get_stack_height(&self) -> usize {
        self.inner
            .iter()
            .map(|(_, (data_type, _))| data_type.stack_size())
            .sum()
    }

    fn get_code_to_spill_to_memory(
        &self,
        values_to_spill: &[ValueIdentifier],
    ) -> Vec<LabelledInstruction> {
        let mut code = vec![];
        for value_to_spill in values_to_spill {
            let (stack_position, data_type, memory_spill_address) =
                self.find_stack_value(value_to_spill);
            let memory_spill_address = memory_spill_address.unwrap();
            let top_of_value = stack_position;
            let mut spill_code =
                copy_value_to_memory(memory_spill_address, data_type.stack_size(), top_of_value);
            code.append(&mut spill_code);
        }

        code
    }
}

#[derive(Clone, Debug, Default)]
/// State for managing the compilation of a single function

struct FunctionState {
    vstack: VStack,
    var_addr: VarAddr,
    spill_required: HashSet<ValueIdentifier>,
    subroutines: Vec<SubRoutine>,
}

impl FunctionState {
    fn assert_subroutines_look_sane(&self) {
        assert!(
            self.subroutines.iter().all(|sub| sub.has_valid_structure()),
            "All subroutines must have a valid structure"
        );
    }

    /// Add a compiled function and its subroutines to the list of subroutines, without
    /// concatenating instructions needlessly which would delete information
    fn add_compiled_fn_to_subroutines(&mut self, mut function: InnerFunctionTasmCode) {
        self.subroutines.append(&mut function.sub_routines);
        self.subroutines.push(function.call_depth_zero_code.into());
    }
}

struct InnerFunctionTasmCode {
    name: String,
    call_depth_zero_code: Vec<LabelledInstruction>,
    sub_routines: Vec<SubRoutine>,
}

impl InnerFunctionTasmCode {
    pub fn remove_own_name(&self) -> Vec<LabelledInstruction> {
        assert!(
            matches!(
                self.call_depth_zero_code.first().unwrap(),
                LabelledInstruction::Label(_)
            ),
            "1st line must be a label when removing it"
        );
        self.call_depth_zero_code[1..].to_vec()
    }
}

pub struct OuterFunctionTasmCode {
    function_data: InnerFunctionTasmCode,
    external_dependencies: Vec<SubRoutine>,
    dyn_malloc_init_code: Vec<LabelledInstruction>,
}

impl OuterFunctionTasmCode {
    pub fn compose(&self) -> Vec<LabelledInstruction> {
        // TODO: Should we end this function with a `halt` such that it works as
        // a standalone program?

        // Remove the first label.
        // This is done to ensure that the code wrapping this function in a `call <fn_name>, halt` logic still
        // sets the dynamic allocator initial value. Otherwise, the dynamic memory allocation would not be
        // included in the code executed with this wrapper code. If this code is *not* run through tests,
        // this rearrangement is inconsequential as it just moves the initialization to after a label, which is
        // removed later by the linker. It's not super elegant but it works. Sue me.
        let call_depth_zero_code = self.function_data.remove_own_name();

        let name = &self.function_data.name;
        let dyn_malloc_init = self.dyn_malloc_init_code.clone();
        let external_dependencies_code = self
            .external_dependencies
            .iter()
            .map(|x| x.get_code())
            .concat();
        let subroutines = self
            .function_data
            .sub_routines
            .iter()
            .map(|sr| sr.get_code())
            .concat();

        let ret = triton_asm!(
            {name}:
                {&dyn_malloc_init}
                {&call_depth_zero_code}
                {&subroutines}
                {&external_dependencies_code}
        );

        // Verify that code parses by wrapping it in a program, panics
        // if assembly is invalid.
        let _program = Program::new(&ret);

        ret
    }
}

/// State that is preserved across the compilation of functions
#[derive(Clone, Debug, Default)]
pub struct GlobalCompilerState {
    counter: usize,
    snippet_state: SnippetState,
}

#[derive(Clone, Debug, Default)]
pub struct CompilerState {
    /// The part of the compiler state that applies across function calls to locally defined
    global_compiler_state: GlobalCompilerState,

    /// The part of the compiler state that only applies within a function
    function_state: FunctionState,
}

impl CompilerState {
    /// Construct a new compiler state with no known values that must be spilled.
    fn new(global_compiler_state: &GlobalCompilerState) -> Self {
        Self {
            global_compiler_state: global_compiler_state.to_owned(),
            function_state: FunctionState::default(),
        }
    }

    /// Construct a new compiler state with a defined set of values that should be stored in memory,
    /// rather than on the stack, i.e. "spilled".
    fn with_known_spills(
        global_compiler_state: &GlobalCompilerState,
        required_spills: HashSet<ValueIdentifier>,
    ) -> Self {
        Self {
            global_compiler_state: global_compiler_state.to_owned(),
            function_state: FunctionState {
                vstack: Default::default(),
                var_addr: VarAddr::default(),
                spill_required: required_spills,
                subroutines: Vec::default(),
            },
        }
    }

    /// At the beginning of a function call, the caller will have placed all arguments
    /// on top of the stack. This must be reflected in the vstack.
    /// Returns a list of values that must be spilled to memory such that the code
    /// generator can start the function with the spilling of these values.
    fn add_input_arguments_to_vstack_and_return_spilled_fn_args(
        &mut self,
        input_arguments: &[AbstractArgument],
    ) -> Vec<ValueIdentifier> {
        const FN_ARG_NAME_PREFIX: &str = "fn_arg";

        let mut fn_arg_spilling = vec![];
        for input_arg in input_arguments {
            match input_arg {
                AbstractArgument::FunctionArgument(_) => todo!(),
                AbstractArgument::ValueArgument(abstract_input) => {
                    let (fn_arg_addr, spill) =
                        self.new_value_identifier(FN_ARG_NAME_PREFIX, &abstract_input.data_type);
                    self.function_state
                        .var_addr
                        .insert(abstract_input.name.clone(), fn_arg_addr.clone());

                    if let Some(_spill_addr) = spill {
                        fn_arg_spilling.push(fn_arg_addr);
                    }
                }
            }
        }

        fn_arg_spilling
    }

    /// Return the set of values that the compiler has determined must be spilled to memory.
    fn get_required_spills(&self) -> HashSet<ValueIdentifier> {
        self.function_state.spill_required.to_owned()
    }

    /// Return code for a function, without including its external dependencies and without
    /// initialization of the dynamic memory allocator, as these pieces of code should be
    /// included only once, by the outer function.
    fn compose_code_for_inner_function(
        &self,
        fn_name: &str,
        call_depth_zero_code: Vec<LabelledInstruction>,
    ) -> InnerFunctionTasmCode {
        let with_own_label = triton_asm!(
            {fn_name}:
                {&call_depth_zero_code}
                return);
        InnerFunctionTasmCode {
            name: fn_name.to_owned(),
            call_depth_zero_code: with_own_label,
            sub_routines: self.function_state.subroutines.clone(),
        }
    }

    /// Return code including those compilation steps that should only be performed once per compilation:
    /// - initialization of dynamic memory allocator
    /// - importing of all external dependencies
    fn compose_code_for_outer_function(
        &self,
        inner_function: InnerFunctionTasmCode,
    ) -> OuterFunctionTasmCode {
        // After the spilling has been done, and after all dependencies have been loaded, the `library` field
        // in the `state` now contains the information about how to initialize the dynamic memory allocator
        // such that dynamically allocated memory does not overwrite statically allocated memory. The `library`
        // field contains the number of words that were statically allocated.
        let dyn_malloc_init_code = DynMalloc::get_initialization_code(
            self.global_compiler_state
                .snippet_state
                .get_next_free_address()
                .try_into()
                .unwrap(),
        );

        let external_dependencies: Vec<SubRoutine> = self
            .global_compiler_state
            .snippet_state
            .all_external_dependencies()
            .into_iter()
            .map(|x| x.into())
            .collect_vec();

        OuterFunctionTasmCode {
            function_data: inner_function,
            external_dependencies,
            dyn_malloc_init_code,
        }
    }
}

pub const SIZE_OF_ACCESSIBLE_STACK: usize = triton_vm::op_stack::NUM_OP_STACK_REGISTERS;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueIdentifier {
    pub name: String,
}

impl std::fmt::Display for ValueIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl CompilerState {
    fn get_binding_name(&self, value_identifier: &ValueIdentifier) -> String {
        match self
            .function_state
            .var_addr
            .iter()
            .find(|x| x.1 == value_identifier)
        {
            Some(binding) => binding.0.to_owned(),
            None => "Unbound value".to_string(),
        }
    }

    /// Get a new, guaranteed unique, identifier for a value. Returns an address to
    /// spill the value to, iff spilling of this value is required. A previous run
    /// of the compiler will have determined whether spilling is required.
    pub fn new_value_identifier(
        &mut self,
        prefix: &str,
        data_type: &ast::DataType,
    ) -> (ValueIdentifier, Option<u32>) {
        let name = format!(
            "_{}_{}_{}",
            prefix, data_type, self.global_compiler_state.counter
        );
        let address = ValueIdentifier { name };

        // Get a statically known memory address if value needs to be spilled to
        // memory.
        let spilled = if self.function_state.spill_required.contains(&address) {
            let spill_address = self
                .global_compiler_state
                .snippet_state
                .kmalloc(data_type.stack_size())
                .try_into()
                .unwrap();
            eprintln!("Warning: spill required of {address}. Spilling to address: {spill_address}");
            Some(spill_address)
        } else {
            None
        };

        self.function_state
            .vstack
            .push((address.clone(), (data_type.clone(), spilled)));
        self.global_compiler_state.counter += 1;

        (address, spilled)
    }

    pub fn import_snippet(&mut self, snippet: Box<dyn BasicSnippet>) -> String {
        self.global_compiler_state.snippet_state.import(snippet)
    }

    pub fn mark_as_spilled(&mut self, value_identifier: &ValueIdentifier) {
        eprintln!(
            "Warning: Marking {value_identifier} as spilled. Binding: {}",
            self.get_binding_name(value_identifier)
        );
        self.function_state
            .spill_required
            .insert(value_identifier.to_owned());
    }

    fn verify_same_ordering_of_bindings(
        &self,
        previous_stack: &VStack,
        previous_var_addr: &VarAddr,
    ) {
        // make a list of tuples (binding name, stack position, spilled_value) as the stack looked at the beginning of the loop
        let bindings_start: HashMap<String, (usize, Option<u32>)> = previous_var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, _data_type, spilled) =
                    previous_stack.find_stack_value(v);
                (binding_name, (previous_stack_depth, spilled))
            })
            .collect();

        // make a list of tuples (binding name, stack position, spilled_value) as the stack looks now
        let bindings_end: HashMap<String, (usize, Option<u32>)> = self
            .function_state
            .var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, _data_type, spilled) =
                    self.function_state.vstack.find_stack_value(v);
                (binding_name, (previous_stack_depth, spilled))
            })
            .collect();

        // Sanity check that all bindings at the start of the code block still exists.
        // I think that should be the case.
        for (var_name_start, _) in bindings_start.iter() {
            assert!(
                bindings_end.contains_key(var_name_start),
                "Bindings at end of block must contain all at start of block"
            );
        }

        // Generate a flip/flop list of tuple values (var_name, old_position, new_position)
        // If assignments overwrite existing stack space, that means all values declared in
        // the block will be on top of the stack, and that the flip_flop list will always
        // be empty.
        let mut flip_flop_list = vec![];
        for (start_binding, (start_stack_pos, _spilled)) in bindings_start.iter() {
            let (end_stack_pos, _end_data_type) = bindings_end[start_binding];
            if end_stack_pos != *start_stack_pos {
                flip_flop_list.push((start_binding, end_stack_pos, start_stack_pos));
            }
        }

        if !flip_flop_list.is_empty() {
            panic!("non-empty flip-flop list is not yet supported!\n\n\nflip-flop list: {flip_flop_list:#?}");
        }

        // TODO: If we want to, we could change this function to return the code to recover from this
        // re-ordering of value bindings. We can also just accept that we must generate code that does
        // not change the stack-ordering of bindings when a value is re-assigned.
    }

    /// Return the code to overwrite a stack value with the value that's on top of the stack
    /// Note that the top value and the value to be removed *must* be of the same type.
    /// Updates the `vstack` but not the `var_addr` as this is assumed to be handled by the caller.
    fn overwrite_value(
        &mut self,
        value_identifier_to_remove: &ValueIdentifier,
    ) -> Vec<LabelledInstruction> {
        let (stack_position_of_value_to_remove, type_to_remove, old_value_spilled) = self
            .function_state
            .vstack
            .find_stack_value(value_identifier_to_remove);

        // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
        // this value must be marked as a value to be spilled, and the compiler must be run again.
        let accessible = stack_position_of_value_to_remove < SIZE_OF_ACCESSIBLE_STACK;
        if old_value_spilled.is_none() && !accessible {
            self.mark_as_spilled(value_identifier_to_remove);
        }

        let (_top_element_id, (top_element_type, _new_value_spilled)) = self
            .function_state
            .vstack
            .pop()
            .expect("vstack cannot be empty");

        assert_eq!(
            top_element_type, type_to_remove,
            "Top stack value and value to remove must match"
        );

        // Overwrite the value, whether it lives on stack or in memory
        let value_size = type_to_remove.stack_size();

        match old_value_spilled {
            Some(spill_addr) => move_top_stack_value_to_memory(spill_addr, value_size),
            None => {
                if stack_position_of_value_to_remove > 0 && accessible {
                    let swap_pop_instructions =
                        format!("swap {stack_position_of_value_to_remove} pop\n")
                            .repeat(value_size);
                    triton_asm!({ swap_pop_instructions })
                } else if !accessible {
                    eprintln!("Compiler must run again because of {value_identifier_to_remove}");
                    triton_asm!(push 0 assert)
                } else {
                    vec![]
                }
            }
        }
    }

    fn replace_tuple_element(
        &mut self,
        tuple_identifier: &ValueIdentifier,
        tuple_index: usize,
    ) -> Vec<LabelledInstruction> {
        // This function assumes that the last element of the tuple is placed on top of the stack
        let (stack_position_of_tuple, tuple_type, tuple_spilled) = self
            .function_state
            .vstack
            .find_stack_value(tuple_identifier);
        let element_types = if let ast::DataType::Tuple(ets) = &tuple_type {
            ets
        } else {
            panic!("Original value must have type tuple")
        };

        let (_top_element_id, (_top_element_type, _)) = self
            .function_state
            .vstack
            .pop()
            .expect("vstack cannot be empty");

        // Last element of tuple is on top of stack. How many machine
        // words deep is the value we want to replace?
        let tuple_depth: usize = element_types
            .iter()
            .enumerate()
            .filter(|(i, _x)| *i > tuple_index)
            .map(|(_i, x)| x.stack_size())
            .sum::<usize>();

        // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
        // this value must be marked as a value to be spilled, and the compiler must be run again.
        let stack_position_of_value_to_remove = stack_position_of_tuple + tuple_depth;
        let accessible = stack_position_of_value_to_remove < SIZE_OF_ACCESSIBLE_STACK;
        if tuple_spilled.is_none() && !accessible {
            self.mark_as_spilled(tuple_identifier);
        }

        // Replace the overwritten value on stack, or in memory
        let value_size = element_types[tuple_index].stack_size();
        match tuple_spilled {
            Some(spill_addr) => {
                let element_address = spill_addr + tuple_depth as u32;
                move_top_stack_value_to_memory(element_address, value_size)
            }
            None => {
                if !accessible {
                    eprintln!("Compiler must run again because of {tuple_identifier}");
                    triton_asm!(push 0 assert)
                } else if stack_position_of_value_to_remove > 0 && accessible {
                    let swap_pop_instructions =
                        format!("swap {stack_position_of_value_to_remove} pop\n")
                            .repeat(value_size);
                    triton_asm!({ swap_pop_instructions })
                } else {
                    vec![]
                }
            }
        }
    }

    /// Restore the vstack to a previous state, representing the state the stack had
    /// at the beginning of a codeblock. Also returns the code to achieve this.
    // TODO: Should also handle spilled_values
    fn restore_stack_code(
        &mut self,
        previous_stack: &VStack,
        previous_var_addr: &VarAddr,
    ) -> Vec<LabelledInstruction> {
        // Clear stack, vstack, and var_addr of locally declared values for those that are on top of the stack
        let mut code = vec![];
        self.show_vstack_values();
        loop {
            let (addr, (dt, spilled)) = self.function_state.vstack.peek().unwrap().to_owned();
            let binding_name = self
                .function_state
                .var_addr
                .iter()
                .find(|(_var_name, ident)| **ident == addr)
                //.unwrap_or_else(|| panic!("Cannot handle stack cleanup of unbound values"))
                .map(|x| x.0.clone());
            // .0
            // .clone();
            if binding_name.is_some()
                && previous_var_addr.contains_key(&binding_name.clone().unwrap())
                || binding_name.is_none()
                    && previous_stack
                        .inner
                        .contains(&(addr, (dt.clone(), spilled)))
            {
                break;
            } else {
                code.append(&mut triton_asm![pop; dt.stack_size()]);
                self.function_state.vstack.pop();
                if let Some(binding) = binding_name {
                    let removed = self.function_state.var_addr.remove(&binding);
                    assert!(removed.is_some());
                }
            }
        }

        // Verify that ordering of previously declared bindings did not change from the execution of
        // this code block.
        self.verify_same_ordering_of_bindings(previous_stack, previous_var_addr);

        code
    }

    /// Return code that clears the stack above a certain height but leaves the value
    /// that's on the top of the stack when this function is called.
    /// Also updates vstack to reflect this.
    fn clear_all_but_top_stack_value_above_height(
        &mut self,
        height: usize,
    ) -> Vec<LabelledInstruction> {
        /// Code generation for removing elements from the bottom of the stack while preserving
        /// the order of the values above.
        fn clear_bottom_of_stack(
            top_value_size: usize,
            words_to_remove: usize,
        ) -> Vec<LabelledInstruction> {
            match (top_value_size, words_to_remove) {
                (3, 2) => triton_asm!(swap 2 swap 4 pop swap 2 pop),
                (4, 2) => triton_asm!(swap 1 swap 3 swap 5 pop swap 1 swap 3 pop),
                (6, 2) => triton_asm!(swap 2 swap 4 swap 6 pop swap 2 swap 4 swap 6 pop),
                (6, 4) => triton_asm!(swap 4 swap 8 pop swap 4 swap 8 pop swap 4 pop swap 4 pop),
                (n, 1) => {
                    let mut swaps = vec![];
                    for i in 1..=n {
                        swaps.append(&mut triton_asm!(swap {i}));
                    }

                    triton_asm!({&swaps} pop)
                }
                _ => panic!("Unsupported. Please cover more special cases. Got: {top_value_size}, {words_to_remove}"),
            }
        }

        let height_of_affected_stack: usize =
            self.function_state.vstack.get_stack_height() - height;

        let top_element = self
            .function_state
            .vstack
            .pop()
            .expect("Cannot remove all but top element from an empty stack");
        let (_top_elem_ident, (top_element_type, top_element_spilled)) = top_element.clone();

        // Verify that top element is not spilled -- as we haven't covered that case yet
        // TODO: Fix this case. That should be quite easy, I think. Not sure this can ever
        // happen though... Please write a test demonstrating this case, before fixing it.
        assert!(
            top_element_spilled.is_none(),
            "Cannot handle spilled value as top element in stack clearing yet"
        );

        let top_value_size = top_element_type.stack_size();

        // Generate code to move value to the bottom of the requested stack range
        let words_to_remove = height_of_affected_stack - top_value_size;
        let code = if words_to_remove != 0
            && top_value_size <= words_to_remove
            && words_to_remove < SIZE_OF_ACCESSIBLE_STACK
        {
            // If we can handle the stack clearing by just swapping the top value
            // lower onto the stack and removing everything above, we do that.
            let mut code = vec![triton_asm!(swap {words_to_remove} pop); top_value_size].concat();

            // Generate code to remove any remaining values from the requested stack range
            let remaining_pops = if words_to_remove > top_value_size {
                words_to_remove - top_value_size
            } else {
                0
            };

            code.append(&mut triton_asm![pop; remaining_pops]);

            code
        } else if words_to_remove >= SIZE_OF_ACCESSIBLE_STACK {
            // In this case, we have to clear more elements from the stack than we can
            // access with dup15/swap15. Our solution is to store the return value in
            // memory, clear the stack, and read it back from memory.
            let memory_location: u32 = self
                .global_compiler_state
                .snippet_state
                .kmalloc(top_value_size)
                .try_into()
                .unwrap();
            let mut code = copy_top_stack_value_to_memory(memory_location, top_value_size);
            code.append(&mut triton_asm![pop; height_of_affected_stack]);
            code.append(&mut load_from_memory(Some(memory_location), top_value_size));

            code
        } else if words_to_remove != 0 {
            // Here, the number of words under the top element is less than the size of
            // the top element. We special-case this as the order on the stack
            // must be preserved, which is non-trivial.
            clear_bottom_of_stack(top_value_size, words_to_remove)
        } else {
            // The case where nothing has to be done since there is nothing below the
            // top value that needs to be removed
            vec![]
        };

        // Clean up vstack
        while self.function_state.vstack.get_stack_height() > height {
            self.function_state.vstack.pop();
        }

        // Sanity check that input argument was aligned with a value
        assert_eq!(
            height,
            self.function_state.vstack.get_stack_height(),
            "Cannot clear stack to position that is not alligned with a value"
        );

        self.function_state.vstack.push(top_element);

        code
    }

    /// Helper function for debugging
    #[allow(dead_code)]
    fn show_vstack_values(&self) {
        print!("vstack: ");
        for (addr, (data_type, spilled)) in self.function_state.vstack.inner.iter() {
            let var_names = self
                .function_state
                .var_addr
                .iter()
                .filter(|(_k, v)| **v == *addr)
                .map(|(k, _v)| k)
                .collect_vec();
            let var_name = match var_names.len() {
                0 => "unnamed",
                _ => var_names[0],
            };
            match spilled {
                Some(spilled_addr) => {
                    println!("{addr}: {var_name} <{data_type}> spilled to: {spilled_addr}")
                }
                None => println!("{addr}: {var_name} <{data_type}>"),
            }
        }
        println!();
    }
}

/// Compile a function, returning the code for the function body. Inherits the `global_compiler_state`
/// from the caller but starts with an empty virtual stack and an empty variable mapping.
fn compile_function_inner(
    function: &ast::Fn<types::Typing>,
    global_compiler_state: &mut GlobalCompilerState,
) -> InnerFunctionTasmCode {
    let fn_name = &function.fn_signature.name;
    let _fn_stack_output_sig = format!("{}", function.fn_signature.output);

    // Run the compilation 1st time to learn which values need to be spilled to memory
    let mut state = CompilerState::new(global_compiler_state);
    let fn_arg_spilling =
        state.add_input_arguments_to_vstack_and_return_spilled_fn_args(&function.fn_signature.args);
    assert!(
        fn_arg_spilling.is_empty(),
        "Cannot memory-spill function arguments first time the code generator runs"
    );

    // Compiling the function body allows us to learn which values need to be spilled to memory
    let _fn_body_code = function
        .body
        .iter()
        .map(|stmt| compile_stmt(stmt, function, &mut state))
        .concat();

    // Run the compilation again now that we know which values to spill
    println!("\n\n\nRunning compiler again\n\n\n");
    let mut state =
        CompilerState::with_known_spills(global_compiler_state, state.get_required_spills());

    // Add function arguments to the compiler's view of the stack.
    let fn_arg_spilling =
        state.add_input_arguments_to_vstack_and_return_spilled_fn_args(&function.fn_signature.args);

    // Create code to spill required function arguments to memory
    let mut fn_body_code = state
        .function_state
        .vstack
        .get_code_to_spill_to_memory(&fn_arg_spilling);

    // Append the code for the function body
    fn_body_code.append(
        &mut function
            .body
            .iter()
            .map(|stmt| compile_stmt(stmt, function, &mut state))
            .concat(),
    );

    // Sanity check: Assert that all subroutines start with a label and end with a return
    state.function_state.assert_subroutines_look_sane();

    // Update global compiler state to propagate this to caller, such that external
    // dependencies that were loaded here will not be loaded again.
    *global_compiler_state = state.global_compiler_state.to_owned();

    state.compose_code_for_inner_function(fn_name, fn_body_code)
}

// TODO: Remove this attribute once we have a sane `main` function that uses this step
#[allow(dead_code)]
pub(crate) fn compile_function(function: &ast::Fn<types::Typing>) -> OuterFunctionTasmCode {
    let mut state = CompilerState::default();
    let compiled_function = compile_function_inner(function, &mut state.global_compiler_state);

    state.compose_code_for_outer_function(compiled_function)
}

fn compile_stmt(
    stmt: &ast::Stmt<types::Typing>,
    function: &ast::Fn<types::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    match stmt {
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
            mutable: _,
        }) => {
            let (expr_addr, expr_code) = compile_expr(expr, var_name, data_type, state);
            state
                .function_state
                .var_addr
                .insert(var_name.clone(), expr_addr);
            expr_code
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            let data_type = expr.get_type();

            // When overwriting a value, we ignore the identifier of the new expression as
            // it's simply popped from the stack and the old identifier is used.
            let (_expr_addr, expr_code) = compile_expr(expr, "assign", &data_type, state);
            match identifier {
                ast::Identifier::String(var_name, _known_type) => {
                    let value_identifier = state.function_state.var_addr[var_name].clone();
                    let overwrite_code = state.overwrite_value(&value_identifier);

                    vec![expr_code, overwrite_code].concat()
                }
                ast::Identifier::TupleIndex(ident, tuple_index) => {
                    let var_name =
                        if let ast::Identifier::String(var_name, _known_type) = *ident.to_owned() {
                            var_name
                        } else {
                            panic!("Nested tuple expressions not yet supported");
                        };

                    let value_identifier = state.function_state.var_addr[&var_name].clone();

                    let overwrite_code =
                        state.replace_tuple_element(&value_identifier, *tuple_index);

                    vec![expr_code, overwrite_code].concat()
                }
                ast::Identifier::ListIndex(ident, index_expr) => {
                    let fn_name =
                        state.import_snippet(Box::new(tasm_lib::list::safe_u32::set::SafeSet(
                            data_type.clone().try_into().unwrap(),
                        )));

                    let ident_expr = ast::Expr::Var(*ident.to_owned());
                    let (_ident_addr, ident_code) =
                        compile_expr(&ident_expr, "ident_on_assign", &data_type, state);
                    let (_index_expr, index_code) =
                        compile_expr(index_expr, "index_on_assign", &index_expr.get_type(), state);
                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![
                        expr_code,
                        ident_code,
                        index_code,
                        triton_asm!(call { fn_name }),
                    ]
                    .concat()
                }
            }
        }

        // 'return;': Clean stack
        ast::Stmt::Return(None) => {
            let mut code = vec![];
            while let Some((_addr, (data_type, _spilled))) = state.function_state.vstack.pop() {
                code.push(triton_asm![pop; data_type.stack_size()])
            }

            code.concat()
        }

        ast::Stmt::Return(Some(ret_expr)) => {
            // special-case on returning variable, without unnecessary dup-instructions
            let expr_code = if let ast::Expr::Var(ast::Identifier::String(var_name, _known_type)) =
                ret_expr
            {
                // Case: Returning a bound variable

                // Remove everything above returned value
                let needle = state
                    .function_state
                    .var_addr
                    .get(var_name)
                    .expect("Returned value must exist in value/addr map");
                let mut code = vec![];

                // If the returned value is a spilled value, we pop everything and
                // load the value from memory.
                // If the returned value is not spilled, we pop until the sought
                // value is on top of the stack and then call a helper function
                // to remove everything below it.
                loop {
                    // This loop pops values from the stack until the value we want to return.
                    // Then the below if-expression takes over when the value is found.
                    let (haystack, (dt, spilled)) = state.function_state.vstack.peek().unwrap();
                    if *haystack == *needle {
                        match spilled {
                            Some(spill_addr) => {
                                // Value is spilled, so we load it from memory and clear that stack.
                                code.append(&mut triton_asm![pop; state.function_state.vstack.get_stack_height()]);
                                code.append(&mut load_from_memory(
                                    Some(*spill_addr),
                                    dt.stack_size(),
                                ))
                            }

                            None => {
                                // Value is not spilled and is on top of the stack now. Remove everything
                                // below it.
                                code.append(
                                    &mut state.clear_all_but_top_stack_value_above_height(0),
                                );
                            }
                        }
                        break;
                    }

                    code.append(&mut triton_asm![pop; dt.stack_size()]);
                    state.function_state.vstack.pop();
                }

                code
            } else {
                // Case: Returning an expression that must be computed
                let code =
                    compile_expr(ret_expr, "ret_expr", &function.fn_signature.output, state).1;

                // Remove all but top value from stack
                let cleanup_code = state.clear_all_but_top_stack_value_above_height(0);

                vec![code, cleanup_code].concat()
            };

            // We don't need to clear `var_addr` here since we are either done with the code generation
            // (if this is an outer function return) or we're returning from a local function that does
            // not share `vstack` or `var_addr` with the caller.

            expr_code
        }

        ast::Stmt::FnCall(fn_call) => compile_fn_call(fn_call, state),

        ast::Stmt::MethodCall(method_call) => compile_method_call(method_call, state),

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            // The code generated here is a subroutine that contains the while loop code
            // and then just a call to this subroutine.
            let (cond_addr, cond_evaluation_code) =
                compile_expr(condition, "while_condition", &condition.get_type(), state);

            let while_loop_subroutine_name = format!("{cond_addr}_while_loop");

            // condition evaluation is not visible to loop body, so pop this from vstack
            state.function_state.vstack.pop();

            let loop_body_code = compile_stmt(&ast::Stmt::Block(block.to_owned()), function, state);
            let while_loop_code = triton_asm!(
                    {while_loop_subroutine_name}:
                        {&cond_evaluation_code}
                        push 0 eq skiz return
                        {&loop_body_code}
                        recurse
            );

            state
                .function_state
                .subroutines
                .push(while_loop_code.into());

            triton_asm!(call {
                while_loop_subroutine_name
            })
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let (cond_addr, cond_code) =
                compile_expr(condition, "if_condition", &condition.get_type(), state);

            // Pop condition result from vstack as it's not on the stack inside the branches
            let _condition_addr = state.function_state.vstack.pop();

            let then_body_code =
                compile_stmt(&ast::Stmt::Block(then_branch.to_owned()), function, state);

            let else_body_code =
                compile_stmt(&ast::Stmt::Block(else_branch.to_owned()), function, state);

            let then_subroutine_name = format!("{cond_addr}_then");
            let else_subroutine_name = format!("{cond_addr}_else");
            let if_code = triton_asm!(
                {&cond_code}
                push 1
                swap 1
                skiz
                call {then_subroutine_name}
                skiz
                call {else_subroutine_name}
            );

            let then_code = triton_asm!(
                {then_subroutine_name}:
                    pop
                    {&then_body_code}
                    push 0
                    return
            );

            let else_code = triton_asm!(
                {else_subroutine_name}:
                    {&else_body_code}
                    return
            );

            state.function_state.subroutines.push(then_code.into());
            state.function_state.subroutines.push(else_code.into());

            if_code
        }

        ast::Stmt::Block(ast::BlockStmt { stmts }) => {
            let vstack_init = state.function_state.vstack.clone();
            let var_addr_init = state.function_state.var_addr.clone();
            let block_body_code = stmts
                .iter()
                .map(|stmt| compile_stmt(stmt, function, state))
                .collect_vec()
                .concat();

            let restore_stack_code = state.restore_stack_code(&vstack_init, &var_addr_init);

            vec![block_body_code, restore_stack_code].concat()
        }
        ast::Stmt::Assert(ast::AssertStmt { expression }) => {
            let (_addr, assert_expr_code) =
                compile_expr(expression, "assert-expr", &expression.get_type(), state);

            // evaluated expression value is not visible after `assert` instruction has been executed
            state.function_state.vstack.pop();

            triton_asm!(
                {&assert_expr_code}
                assert
            )
        }
        ast::Stmt::FnDeclaration(function) => {
            let compiled_fn = compile_function_inner(function, &mut state.global_compiler_state);
            state
                .function_state
                .add_compiled_fn_to_subroutines(compiled_fn);

            vec![]
        }
    }
}

fn compile_fn_call(
    fn_call: &ast::FnCall<types::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let ast::FnCall {
        name,
        args,
        annot: _return_type, // unit for statement-level fn calls
        type_parameter,
        arg_evaluation_order,
    } = fn_call.clone();

    // Compile function arguments either left-to-right or right-to-left depending
    // on definition in function.
    let args_iter = if arg_evaluation_order == ast::ArgEvaluationOrder::LeftToRight {
        Either::Left(args.iter().enumerate())
    } else {
        Either::Right(args.iter().enumerate().rev())
    };

    let (_args_idents, args_code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) =
        args_iter
            .map(|(arg_pos, arg_expr)| {
                let context = format!("_{name}_arg_{arg_pos}");
                compile_expr(arg_expr, &context, &arg_expr.get_type(), state)
            })
            .unzip();

    let mut call_fn_code = vec![];
    for lib in libraries::all_libraries() {
        if let Some(fn_name) = lib.get_function_name(&name) {
            call_fn_code.append(&mut lib.call_function(&fn_name, type_parameter, &args, state));
            break;
        }
    }

    if call_fn_code.is_empty() {
        // Function is not a library function, but type checker has guaranteed that it is in
        // scope. So we just call it.
        call_fn_code.append(&mut triton_asm!(call { name }));
    }

    // Remove function arguments from vstack since they're not visible after the function call
    for _ in 0..args.len() {
        state.function_state.vstack.pop();
    }

    vec![args_code.concat(), call_fn_code].concat()
}

fn compile_method_call(
    method_call: &ast::MethodCall<types::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let method_name = method_call.method_name.clone();
    let receiver_type = method_call.args[0].get_type();

    // Compile arguments, including receiver, left-to-right
    let (_args_idents, args_code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) =
        method_call
            .args
            .iter()
            .enumerate()
            .map(|(arg_pos, arg_expr)| {
                let context = format!("_{method_name}_arg_{arg_pos}");
                compile_expr(arg_expr, &context, &arg_expr.get_type(), state)
            })
            .unzip();

    let mut call_code = vec![];
    for lib in libraries::all_libraries() {
        if let Some(fn_name) = lib.get_method_name(&method_name, &receiver_type) {
            call_code.append(&mut lib.call_method(
                &fn_name,
                &receiver_type,
                &method_call.args,
                state,
            ));
            break;
        }
    }

    assert!(!call_code.is_empty(), "Unknown method: {method_name}");

    // Update vstack to reflect that all input arguments, including receiver
    // were consumed.
    for _ in 0..method_call.args.len() {
        state.function_state.vstack.pop();
    }

    vec![args_code.concat(), call_code].concat()
}

fn compile_expr(
    expr: &ast::Expr<types::Typing>,
    _context: &str,
    _data_type: &ast::DataType,
    state: &mut CompilerState,
) -> (ValueIdentifier, Vec<LabelledInstruction>) {
    let result_type = expr.get_type();
    let code = match expr {
        ast::Expr::Lit(expr_lit) => match expr_lit {
            ast::ExprLit::Bool(value) => {
                triton_asm!(push {*value as u64})
            }

            ast::ExprLit::U32(value) => {
                triton_asm!(push {*value as u64})
            }

            ast::ExprLit::BFE(value) => triton_asm!(push {*value }),

            ast::ExprLit::U64(value) => {
                let as_u32s = U32s::<2>::try_from(*value).unwrap().encode();
                let stack_serialized: Vec<_> = as_u32s.iter().rev().collect();

                let code = stack_serialized
                    .iter()
                    .flat_map(|bfe| triton_asm!(push {**bfe}))
                    .collect_vec();

                code
            }

            ast::ExprLit::XFE(value) => {
                // In the VM, the 1st element of the array is expected to be on top of the stack.
                // So the elements must be pushed onto the stack in reversed order.
                triton_asm!(
                    push {value.coefficients[2]}
                    push {value.coefficients[1]}
                    push {value.coefficients[0]}
                )
            }
            ast::ExprLit::Digest(_) => todo!(),
            ast::ExprLit::GenericNum(n, _) => {
                panic!("Type of number literal {n} not resolved")
            }
            ast::ExprLit::MemPointer(ast::MemPointerLiteral {
                mem_pointer_address,
                struct_name: _,
            }) => triton_asm!(push {
                mem_pointer_address
            }),
        },

        ast::Expr::Var(identifier) => match identifier {
            ast::Identifier::String(var_name, _known_type) => {
                let var_addr = state.function_state.var_addr.get(var_name).cloned();
                match &var_addr {
                    Some(var_addr) => {
                        // Identifier is a value that must be living on the stack
                        let (position, old_data_type, spilled) =
                            state.function_state.vstack.find_stack_value(var_addr);

                        // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
                        // this value must be marked as a value to be spilled, and the compiler must be run again.
                        let bottom_position = position + result_type.stack_size() - 1;
                        let accessible = bottom_position < SIZE_OF_ACCESSIBLE_STACK;
                        if spilled.is_none() && !accessible {
                            state.mark_as_spilled(var_addr);
                        }

                        // sanity check
                        assert_eq!(old_data_type, result_type, "type must match expected type");

                        match spilled {
                            Some(spill_address) => {
                                // The value has been spilled to memory. Get it from there.
                                load_from_memory(Some(spill_address), result_type.stack_size())
                            }
                            None => {
                                if !accessible {
                                    // The compiler needs to run again. Produce unusable code.
                                    triton_asm!(push 0 assert)
                                } else {
                                    // The value is accessible on the stack. Copy it from there.
                                    dup_value_from_stack_code(
                                        position.try_into().unwrap(),
                                        &result_type,
                                    )
                                }
                            }
                        }
                    }
                    // Identifier is a function and must be in scope since typechecker was passed
                    None => vec![],
                }
            }
            ast::Identifier::TupleIndex(ident, tuple_index) => {
                let var_name = if let ast::Identifier::String(var_name, _) = *ident.to_owned() {
                    var_name
                } else {
                    panic!("Nested tuple references not yet supported");
                };
                let var_addr = state
                    .function_state
                    .var_addr
                    .get(&var_name)
                    .expect("variable exists")
                    .to_owned();

                // Note that this function returns the address of the *tuple element*, both
                // on the stack and in memory if spilled. So the stack position/spilled address
                // should not be shifted with the elements position inside the tuple.
                let (position, element_type, spilled) = state
                    .function_state
                    .vstack
                    .find_tuple_element(&var_addr, *tuple_index);

                // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
                // this value must be marked as a value to be spilled, and the compiler must be run again.
                let bottom_position = position + element_type.stack_size() - 1;
                let accessible = bottom_position < SIZE_OF_ACCESSIBLE_STACK;
                if spilled.is_none() && !accessible {
                    state.mark_as_spilled(&var_addr);
                }

                match spilled {
                    Some(spill_address) => {
                        // The value has been spilled to memory. Get it from there.
                        load_from_memory(Some(spill_address), element_type.stack_size())
                    }
                    None => {
                        if !accessible {
                            // The compiler needs to run again. Produce unusable code.
                            triton_asm!(push 0 assert)
                        } else {
                            // The value is accessible on the stack. Copy it from there.
                            dup_value_from_stack_code(position.try_into().unwrap(), &element_type)
                        }
                    }
                }
            }
            ast::Identifier::ListIndex(ident, index_expr) => {
                let ident_type = ident.get_type();
                let type_param = ident_type.type_parameter().unwrap_or_else(|| {
                    panic!("identifier must have type parameter when reading through indexing")
                });
                let fn_name = state.import_snippet(Box::new(
                    tasm_lib::list::safe_u32::get::SafeGet(type_param.try_into().unwrap()),
                ));

                // TODO: Remove these sanity checks
                let ident_as_string = match *ident.to_owned() {
                    ast::Identifier::String(as_str, _) => as_str,
                    _ => panic!("Nested list indexing not yet supported"),
                };
                let var_addr = state
                    .function_state
                    .var_addr
                    .get(&ident_as_string)
                    .expect("variable exists")
                    .to_owned();
                let (_position, old_data_type, _spilled) =
                    state.function_state.vstack.find_stack_value(&var_addr);
                assert_eq!(
                    ident_type, old_data_type,
                    "Type found on vstack must match expected type"
                );
                assert!(
                    is_list_type(&old_data_type),
                    "Can only index into list types"
                );

                // The recursive calls below should be able to handle spilled values, so we don't
                // need to handle it here.

                let ident_expr = ast::Expr::Var(*ident.to_owned());
                let (_ident_addr, ident_code) =
                    compile_expr(&ident_expr, "ident_on_assign", &ident_type, state);
                let (_index_expr, index_code) =
                    compile_expr(index_expr, "index_on_assign", &index_expr.get_type(), state);
                state.function_state.vstack.pop();
                state.function_state.vstack.pop();

                triton_asm!(
                    {&ident_code}
                    {&index_code}
                    call {fn_name}
                )
            }
        },

        ast::Expr::Tuple(exprs) => {
            // Compile arguments left-to-right
            let (idents, code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) = exprs
                .iter()
                .enumerate()
                .map(|(arg_pos, arg_expr)| {
                    let context = format!("_tuple_{arg_pos}");
                    compile_expr(arg_expr, &context, &arg_expr.get_type(), state)
                })
                .unzip();

            // Combine vstack entries into one Tuple entry
            for _ in idents {
                state.function_state.vstack.pop();
            }

            code.concat()
        }

        ast::Expr::FnCall(fn_call) => compile_fn_call(fn_call, state),

        ast::Expr::MethodCall(method_call) => compile_method_call(method_call, state),

        ast::Expr::Unary(unaryop, inner_expr, _known_type) => {
            let inner_type = inner_expr.get_type();
            let (_inner_expr_addr, inner_expr_code) =
                compile_expr(inner_expr, "_binop_lhs", &inner_type, state);
            let code = match unaryop {
                ast::UnaryOp::Neg => match inner_type {
                    ast::DataType::BFE => triton_asm!(push -1 mul),
                    ast::DataType::XFE => triton_asm!(push -1 xbmul),
                    _ => panic!("Unsupported negation of type {inner_type}"),
                },
                ast::UnaryOp::Not => match inner_type {
                    ast::DataType::Bool => triton_asm!(push 0 eq),
                    ast::DataType::U32 => triton_asm!(push {u32::MAX as u64} xor),
                    ast::DataType::U64 => triton_asm!(
                        swap 1
                        push {u32::MAX as u64}
                        xor
                        swap 1
                        push {u32::MAX as u64}
                        xor
                    ),
                    _ => panic!("Unsupported not of type {inner_type}"),
                },
                ast::UnaryOp::Deref => {
                    let size_of_value = inner_type.stack_size();
                    // Push `size_of_value` many words from memory onto stack.
                    // We probably have a function for loading from memory!
                    // Use that.
                    // todo!()
                    load_from_memory(None, size_of_value)
                }
            };

            state.function_state.vstack.pop();

            vec![inner_expr_code, code].concat()
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, _known_type) => {
            let lhs_type = lhs_expr.get_type();
            let rhs_type = rhs_expr.get_type();

            match binop {
                ast::BinOp::Add => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let add_code = match result_type {
                        ast::DataType::U32 => {
                            // We use the safe, overflow-checking, add code as default
                            let safe_add_u32 =
                                state.import_snippet(Box::new(arithmetic::u32::safe_add::SafeAdd));
                            triton_asm!(call { safe_add_u32 })
                        }
                        ast::DataType::U64 => {
                            // We use the safe, overflow-checking, add code as default
                            let add_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::add_u64::AddU64));

                            triton_asm!(call { add_u64 })
                        }
                        ast::DataType::BFE => triton_asm!(add),
                        ast::DataType::XFE => {
                            triton_asm!(xxadd swap 3 pop swap 3 pop swap 3 pop)
                        }
                        _ => panic!("Operator add is not supported for type {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, add_code].concat()
                }
                ast::BinOp::And => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let and_code = match result_type {
                        ast::DataType::Bool => triton_asm!(add push 2 eq),
                        _ => panic!("Logical AND operator is not supported for {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, and_code].concat()
                }

                ast::BinOp::BitAnd => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let bitwise_and_code = match result_type {
                        ast::DataType::U32 => triton_asm!(and),
                        ast::DataType::U64 => {
                            let and_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::and_u64::AndU64));
                            triton_asm!(call { and_u64 })
                        }
                        _ => panic!("Logical AND operator is not supported for {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, bitwise_and_code].concat()
                }

                ast::BinOp::BitXor => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;
                    let xor_code = match result_type {
                        U32 => triton_asm!(xor),
                        U64 => triton_asm!(
                            swap 3
                            xor
                            swap 2
                            xor
                        ),
                        _ => panic!("xor on {result_type} is not supported"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, xor_code].concat()
                }
                ast::BinOp::BitOr => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;

                    let bitwise_or_code = match result_type {
                        U32 => {
                            let or_u32 = state.import_snippet(Box::new(arithmetic::u32::or::OrU32));
                            triton_asm!(call { or_u32 })
                        }
                        U64 => {
                            let or_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::or_u64::OrU64));
                            triton_asm!(call { or_u64 })
                        }
                        _ => panic!("bitwise `or` on {result_type} is not supported"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, bitwise_or_code].concat()
                }

                ast::BinOp::Div => {
                    use ast::DataType::*;
                    match result_type {
                        U32 => {
                            // TODO: Consider evaluating in opposite order to save a clock-cycle by removing `swap1`
                            // below. This would change the "left-to-right" convention though.
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                swap 1
                                div
                                pop
                            )
                        }
                        U64 => {
                            // Division is very expensive in the general case!
                            // Try to do right shifting instead, if you can.
                            let rhs_expr_owned = *rhs_expr.to_owned();
                            if matches!(rhs_expr_owned, ast::Expr::Lit(ast::ExprLit::U64(2))) {
                                let (_lhs_expr_addr, lhs_expr_code) =
                                    compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                                let div2 = state
                                    .import_snippet(Box::new(arithmetic::u64::div2_u64::Div2U64));

                                // Pop the numerator that was divided by two
                                state.function_state.vstack.pop();

                                triton_asm!(
                                    {&lhs_expr_code}
                                    call {div2}
                                )
                            } else {
                                let (_lhs_expr_addr, lhs_expr_code) =
                                    compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                                let (_rhs_expr_addr, rhs_expr_code) =
                                    compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                                let div_mod_u64 = state.import_snippet(Box::new(
                                    arithmetic::u64::div_mod_u64::DivModU64,
                                ));

                                state.function_state.vstack.pop();
                                state.function_state.vstack.pop();

                                // Call the div-mod function and throw away the remainder
                                triton_asm!(
                                    {&lhs_expr_code}
                                    {&rhs_expr_code}
                                    call {div_mod_u64}
                                    pop
                                    pop
                                )
                            }
                        }
                        BFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            // vec![lhs_expr_code, rhs_expr_code, bfe_div_code].concat()
                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                invert
                                mul
                            )
                        }
                        XFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                xinvert
                                xxmul
                                swap 3
                                pop
                                swap 3
                                pop
                                swap 3
                                pop
                            )
                        }
                        _ => panic!("Unsupported div for type {result_type}"),
                    }
                }

                ast::BinOp::Rem => {
                    use ast::DataType::*;
                    match result_type {
                        U32 => {
                            // TODO: Consider evaluating in opposite order to save a clock-cycle by removing `swap1`
                            // below. This would change the "left-to-right" convention though.
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // Pop numerator and denominator
                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                swap 1
                                div
                                swap 1
                                pop
                            )
                        }
                        U64 => {
                            // divsion and remainder are very expensive in the general case!
                            // Try to use a bitmask instead.
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            let div_mod_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::div_mod_u64::DivModU64));

                            state.function_state.vstack.pop();
                            state.function_state.vstack.pop();

                            // Call the div-mod function and throw away the quotient
                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {div_mod_u64}
                                swap 2
                                pop
                                swap 2
                                pop
                            )
                        }
                        _ => panic!("Unsupported remainder of type {lhs_type}"),
                    }
                }

                ast::BinOp::Eq => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let eq_code = compile_eq_code(&lhs_type, state);

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, eq_code].concat()
                }

                ast::BinOp::Lt => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    match lhs_type {
                        U32 => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            swap 1
                            lt
                        ),

                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64));
                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                swap 3
                                swap 1
                                swap 3
                                swap 2
                                call {lt_u64}
                            )
                        }
                        _ => panic!("Unsupported < for type {lhs_type}"),
                    }
                }
                // ast::Expr::Binop(lhs_expr, binop, rhs_expr, _known_type)
                ast::BinOp::Gt => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;
                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    match lhs_type {
                        U32 => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            lt
                        ),
                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                call {lt_u64}
                            )
                        }
                        _ => panic!("Unsupported < for type {lhs_type}"),
                    }
                }
                ast::BinOp::Mul => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    match lhs_type {
                        U32 => {
                            let fn_name =
                                state.import_snippet(Box::new(arithmetic::u32::safe_mul::SafeMul));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {fn_name}
                            )
                        }
                        U64 => {
                            let fn_name = state.import_snippet(Box::new(
                                arithmetic::u64::safe_mul_u64::SafeMulU64,
                            ));

                            triton_asm!(
                                {&lhs_expr_code}
                                {&rhs_expr_code}
                                call {fn_name}
                            )
                        }
                        BFE => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            mul
                        ),
                        XFE => triton_asm!(
                            {&lhs_expr_code}
                            {&rhs_expr_code}
                            xxmul
                            swap 3
                            pop
                            swap 3
                            pop
                            swap 3
                            pop
                        ),
                        _ => panic!("Unsupported MUL for type {lhs_type}"),
                    }
                }
                ast::BinOp::Neq => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let eq_code = compile_eq_code(&lhs_type, state);

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        {&eq_code}
                        push 0
                        eq
                    )
                }

                ast::BinOp::Or => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        add
                        push 0
                        eq
                        push 0
                        eq
                    )
                }

                ast::BinOp::Shl => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let lhs_type = lhs_expr.get_type();
                    let shl = if matches!(lhs_type, ast::DataType::U32) {
                        state.import_snippet(Box::new(arithmetic::u32::shift_left::ShiftLeftU32))
                    } else if matches!(lhs_type, ast::DataType::U64) {
                        state
                            .import_snippet(Box::new(arithmetic::u64::shift_left_u64::ShiftLeftU64))
                    } else {
                        panic!("Unsupported SHL of type {lhs_type}");
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        call {shl}
                    )
                }

                ast::BinOp::Shr => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let lhs_type = lhs_expr.get_type();
                    let shr = if matches!(lhs_type, ast::DataType::U32) {
                        state.import_snippet(Box::new(arithmetic::u32::shift_right::ShiftRightU32))
                    } else if matches!(lhs_type, ast::DataType::U64) {
                        state.import_snippet(Box::new(
                            arithmetic::u64::shift_right_u64::ShiftRightU64,
                        ))
                    } else {
                        panic!("Unsupported SHL of type {lhs_type}");
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        call {shr}
                    )
                }

                ast::BinOp::Sub => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let sub_code: Vec<LabelledInstruction> = match result_type {
                        ast::DataType::U32 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let safe_sub_u32 =
                                state.import_snippet(Box::new(arithmetic::u32::safe_sub::SafeSub));
                            triton_asm!(
                                swap 1
                                call {safe_sub_u32}
                            )
                        }
                        ast::DataType::U64 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let sub_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::sub_u64::SubU64));
                            triton_asm!(
                                //     // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                swap 3
                                swap 1
                                swap 3
                                swap 2
                                call {sub_u64}
                            )
                        }
                        ast::DataType::BFE => {
                            triton_asm!(
                                push -1
                                mul
                                add
                            )
                        }
                        ast::DataType::XFE => {
                            triton_asm!(
                                  // multiply top element with -1
                                push -1
                                xbmul
                                // Perform (lhs - rhs)
                                xxadd
                                // Get rid of the lhs, only leaving the result
                                swap 3
                                pop
                                swap 3
                                pop
                                swap 3
                                pop
                            )
                        }
                        _ => panic!("subtraction operator is not supported for {result_type}"),
                    };

                    state.function_state.vstack.pop();
                    state.function_state.vstack.pop();

                    triton_asm!(
                        {&lhs_expr_code}
                        {&rhs_expr_code}
                        {&sub_code}
                    )
                }
            }
        }

        ast::Expr::If(ast::ExprIf {
            condition,
            then_branch,
            else_branch,
        }) => {
            let (_cond_addr, cond_code) =
                compile_expr(condition, "if_cond", &condition.get_type(), state);

            // Condition is handled immediately and it is not on the stack when
            // the `then` or `else` branches are entered.
            state.function_state.vstack.pop();

            let branch_start_vstack = state.function_state.vstack.clone();
            let branch_start_var_addr = state.function_state.var_addr.clone();
            let return_type = then_branch.get_type();

            // Compile `then` branch
            let (then_addr, mut then_body_code) =
                compile_expr(then_branch, "then", &return_type, state);

            // Cleanup stack and variable name mapping after `then` body. Preserve the return
            // value from the `then` branch on the stack, but not on vstack as this value is
            // not visible to the `else` branch.
            let mut then_body_cleanup_code = state
                .clear_all_but_top_stack_value_above_height(branch_start_vstack.get_stack_height());
            then_body_code.append(&mut then_body_cleanup_code);
            let _returned_value_from_then_block = state.function_state.vstack.pop().unwrap();
            state.verify_same_ordering_of_bindings(&branch_start_vstack, &branch_start_var_addr);
            state.function_state.var_addr = branch_start_var_addr.clone();

            // Compile `else` branch
            let (_else_addr, mut else_body_code) =
                compile_expr(else_branch, "else", &return_type, state);

            // Cleanup stack and variable name mapping after `else` body. Preserve the return
            // value from the `else` branch on the stack, but not on vstack, as this is added
            // later.
            let mut else_body_cleanup_code = state
                .clear_all_but_top_stack_value_above_height(branch_start_vstack.get_stack_height());
            else_body_code.append(&mut else_body_cleanup_code);
            let _returned_value_from_else_block = state.function_state.vstack.pop().unwrap();
            state.verify_same_ordering_of_bindings(&branch_start_vstack, &branch_start_var_addr);
            state.function_state.var_addr = branch_start_var_addr;

            // Both branches are compiled as subroutines which are called depending on what `cond`
            // evaluates to.
            let then_subroutine_name = format!("{then_addr}_then");
            let else_subroutine_name = format!("{then_addr}_else");

            let code = triton_asm!(
                {&cond_code}
                push 1
                swap 1
                skiz
                call {then_subroutine_name}
                skiz
                call {else_subroutine_name}
            );

            let then_code = triton_asm!(
                {then_subroutine_name}:
                    pop
                    {&then_body_code}
                    push 0
                    return
            );

            let else_code = triton_asm!(
                {else_subroutine_name}:
                    {&else_body_code}
                    return
            );

            state.function_state.subroutines.push(then_code.into());
            state.function_state.subroutines.push(else_code.into());

            code
        }

        ast::Expr::Cast(expr, _as_type) => {
            let previous_type = expr.get_type();
            let (_expr_addr, expr_code) = compile_expr(expr, "as", &previous_type, state);
            let (_, (_old_data_type, spilled)) = state.function_state.vstack.pop().unwrap();

            // I don't think this value *can* be spilled unless maybe if you make a tuple
            // that's longer than 16 words which we probably can't handle anyway
            assert!(
                spilled.is_none(),
                "Can't handle spilled values in casting yet"
            );

            match (&previous_type, &result_type) {
                (ast::DataType::U64, ast::DataType::U32) => {
                    triton_asm!(
                        {&expr_code}
                        swap 1
                        pop
                    )
                }
                (ast::DataType::U32, ast::DataType::U64) => {
                    triton_asm!(
                        {&expr_code}
                        push 0
                        swap 1
                    )
                }
                // Allow identity-casting since we might need this to make the types
                // agree with code compiled by rustc.
                (ast::DataType::U32, ast::DataType::U32) => expr_code,
                (ast::DataType::U64, ast::DataType::U64) => expr_code,
                (ast::DataType::Bool, ast::DataType::U64) => {
                    triton_asm!(
                        {&expr_code}
                        push 0
                        swap 1
                    )
                }
                (ast::DataType::Bool, ast::DataType::U32) => expr_code,
                (ast::DataType::Bool, ast::DataType::BFE) => expr_code,
                _ => todo!(),
            }
        }
        ast::Expr::Field(expr, field_name) => {
            // Here, `receiver_type` *must* be of type `Struct` or of type `mempoint(Struct)`,
            // where the Struct is known. We should handle those cases separately.
            let receiver_type = expr.get_type();

            match receiver_type {
                ast::DataType::MemPointer(inner_type) => match *inner_type {
                    ast::DataType::Struct(inner_struct) => {
                        inner_struct.get_field_accessor_code(field_name)
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }
    };

    let (addr, spill) = state.new_value_identifier(&format!("{expr}_{result_type}"), &result_type);
    let spill_code = spill
        .map(|x| copy_top_stack_value_to_memory(x, result_type.stack_size()))
        .unwrap_or_default();

    (addr, vec![code, spill_code].concat())
}

/// Return the code to store a stack-value in memory
fn copy_value_to_memory(
    memory_location: u32,
    value_size: usize,
    stack_location_for_top_of_value: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = triton_asm!(push {memory_location as u64});

    for i in 0..value_size {
        // ret.push(dup(1 + i as u64 + stack_location_for_top_of_value as u64));
        ret.append(&mut triton_asm!(dup {1 + i as u64 + stack_location_for_top_of_value as u64}));
        // _ [elements] mem_address element

        ret.push(triton_instr!(write_mem));
        // _ [elements] mem_address

        if i != value_size - 1 {
            ret.append(&mut triton_asm!(push 1 add));
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(triton_instr!(pop));

    ret
}

/// Return the code to move the top stack element at a
/// specific memory address. Deletes top stack value.
fn move_top_stack_value_to_memory(
    memory_location: u32,
    top_value_size: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = triton_asm!(push {memory_location as u64});

    for i in 0..top_value_size {
        ret.push(triton_instr!(swap 1));
        // _ mem_address element

        ret.push(triton_instr!(write_mem));
        // _ mem_address

        if i != top_value_size - 1 {
            ret.append(&mut triton_asm!(push 1 add));
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(triton_instr!(pop));

    // TODO: Needs to pop from vstack!

    ret
}

/// Return the code to store the top stack element at a
/// specific memory address. Leaves the stack unchanged.
fn copy_top_stack_value_to_memory(
    memory_location: u32,
    top_value_size: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = triton_asm!(push {memory_location as u64});

    for i in 0..top_value_size {
        ret.append(&mut triton_asm!(dup {1 + i as u64}));
        // _ [elements] mem_address element

        ret.push(triton_instr!(write_mem));
        // _ [elements] mem_address

        if i != top_value_size - 1 {
            ret.append(&mut triton_asm!(push 1 add));
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(triton_instr!(pop));

    ret
}

/// Return the code to load a value from memory. If memory location is not provided, the memory
/// address is assumed to be on top of the stack. Leaves the stack unchanged in either case.
fn load_from_memory(
    memory_location: Option<u32>,
    top_value_size: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address. So we read the value at the highes memory location first.
    // TODO: Consider making subroutines out of this in
    // order to get shorter programs.
    // let mut ret = triton_asm!(push {memory_location as u64 + top_value_size as u64 - 1});
    let mut ret = match memory_location {
        Some(mem_location) => triton_asm!(push {mem_location as u64 + top_value_size as u64 - 1}),
        None => {
            if top_value_size.is_one() {
                triton_asm!(dup 0)
            } else {
                triton_asm!(dup 0 push {top_value_size as u64 - 1} add)
            }
        }
    };

    for i in 0..top_value_size {
        // Stack: _ memory_address

        ret.push(triton_instr!(read_mem));
        // Stack: _ memory_address value

        ret.push(triton_instr!(swap 1));

        // Decrement memory address to prepare for next loop iteration
        if i != top_value_size - 1 {
            ret.append(&mut triton_asm!(push {BFieldElement::MAX} add))
            // Stack: _ (memory_address - 1)
        }
    }

    // Remove memory address from top of stack
    ret.push(triton_instr!(pop));

    // Stack: _ element_N element_{N - 1} ... element_0

    ret
}

fn compile_eq_code(
    lhs_type: &ast::DataType,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    use ast::DataType::*;
    match lhs_type {
        Bool | U32 | BFE | VoidPointer => triton_asm!(eq),
        U64 => triton_asm!(
            // _ a_hi a_lo b_hi b_lo
            swap 3
            eq
            swap 2
            eq
            mul
        ),
        U128 => todo!(),

        XFE => triton_asm!(
             // _ a_2 a_1 a_0 b_2 b_1 b_0
            swap 4 // _ a_2 b_0 a_0 b_2 b_1 a_1
            eq     // _ a_2 b_0 a_0 b_2 (b_1 == a_1)
            swap 4 // _ (b_1 == a_1) b_0 a_0 b_2 a_2
            eq     // _ (b_1 == a_1) b_0 a_0 (b_2 == a_2)
            swap 2 // _ (b_1 == a_1) (b_2 == a_2) a_0 b_0
            eq     // _ (b_1 == a_1) (b_2 == a_2) (a_0 == b_0)
            mul    // _ (b_1 == a_1) ((b_2 == a_2)·(a_0 == b_0))
            mul    // _ ((b_1 == a_1)·(b_2 == a_2)·(a_0 == b_0))
        ),
        Digest => {
            let eq_digest = state.import_snippet(Box::new(hashing::eq_digest::EqDigest));
            triton_asm!(call { eq_digest })
        }
        List(_) => todo!(),
        Tuple(_) => todo!(),
        Function(_) => todo!(),
        Struct(_) => todo!(),
        MemPointer(_) => todo!("Comparison of MemPointer not supported yet"),
        Unresolved(name) => panic!("Cannot compare unresolved type {name}"),
    }
}

/// Copy a value at a position on the stack to the top
fn dup_value_from_stack_code(
    position: OpStackElement,
    data_type: &ast::DataType,
) -> Vec<LabelledInstruction> {
    let elem_size = data_type.stack_size();

    // the position of the deepest element of the value.
    let n: usize = Into::<usize>::into(position) + elem_size - 1;

    let instrs_as_str = format!("dup {}\n", n);
    let instrs_as_str = instrs_as_str.repeat(elem_size);

    triton_asm!({ instrs_as_str })
}

impl ast::StructType {
    /// Assuming the stack top points to the start of the struct, returns the code
    /// that modifies the top stack value to point to the indicated field.
    pub fn get_field_accessor_code(&self, field_name: &str) -> Vec<LabelledInstruction> {
        // This implementation must match `BFieldCodec` for the equivalent Rust types
        let mut instructions = vec![];
        let mut static_pointer_addition = 0;
        let needle = field_name;
        for (haystack_field_name, haystack_type) in self.fields.iter() {
            if haystack_field_name == needle {
                // If we've found the field the accumulators are in the right state.
                // return them.
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

        if !static_pointer_addition.is_zero() {
            instructions.append(&mut triton_asm!(push {static_pointer_addition} add));
        }

        instructions
    }
}
