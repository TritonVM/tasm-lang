use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use tasm_lib::library::Library;
use tasm_lib::snippet::Snippet;
use tasm_lib::{arithmetic, hashing};
use triton_opcodes::instruction::{AnInstruction::*, LabelledInstruction::*};
use triton_opcodes::ord_n::Ord16;
use triton_opcodes::parser::{parse, to_labelled};
use triton_opcodes::shortcuts::*;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::libraries::{tasm, unsigned_integers, vector};
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

            position += data_type.size_of();

            // By asserting after `+= data_type.size_of()`, we check that the deepest part
            // of the sought value is addressable, not just the top part of the value.
            // TODO: REMOVE THIS ASSERT!
            // assert!(position < STACK_SIZE, "Addressing beyond the {STACK_SIZE}'th stack element requires spilling and register-allocation.");
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
            .map(|(_i, x)| x.size_of())
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
            .map(|(_, (data_type, _))| data_type.size_of())
            .sum()
    }
}

#[derive(Debug, Default)]
pub struct CompilerState {
    pub counter: usize,

    // Where on stack is the variable placed?
    pub vstack: VStack,

    // TODO: `VStack` might be able to handle this `spilled_values` info.
    // Variables that have been spilled to memory.
    // Mapping from value identifier to memory location.
    // pub spilled_values: HashMap<ValueIdentifier, u32>,
    pub spill_required: HashSet<ValueIdentifier>,

    // Mapping from variable name to its internal identifier
    pub var_addr: VarAddr,

    // A library struct to keep check of which snippets are already in the namespace
    pub library: Library,

    // A list of call sites for ad-hoc branching
    pub subroutines: Vec<Vec<LabelledInstruction>>,
}

// TODO: Use this value from Triton-VM
pub const STACK_SIZE: usize = 16;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueIdentifier {
    pub name: String,
}

use triton_opcodes::instruction::LabelledInstruction;

impl std::fmt::Display for ValueIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl CompilerState {
    fn get_binding_name(&self, value_identifier: &ValueIdentifier) -> String {
        match self.var_addr.iter().find(|x| x.1 == value_identifier) {
            Some(binding) => binding.0.to_owned(),
            None => "Unbound value".to_string(),
        }
    }

    /// Get a new, guaranteed unique, identifier for a value. Returns an address to
    /// spill the value to, iff spilling of this value is required.
    pub fn new_value_identifier(
        &mut self,
        prefix: &str,
        data_type: &ast::DataType,
    ) -> (ValueIdentifier, Option<u32>) {
        let name = format!("_{}_{}_{}", prefix, data_type, self.counter);
        let address = ValueIdentifier { name };

        // Get a statically known memory address if value needs to be spilled to
        // memory.
        let spilled = if self.spill_required.contains(&address) {
            let spill_address = self
                .library
                .kmalloc(data_type.size_of())
                .try_into()
                .unwrap();
            println!("Warning: spill required of {address}. Spilling to address: {spill_address}");
            Some(spill_address)
        } else {
            None
        };

        self.vstack
            .push((address.clone(), (data_type.clone(), spilled)));
        self.counter += 1;

        (address, spilled)
    }

    pub fn import_snippet(&mut self, snippet: Box<dyn Snippet>) -> String {
        self.library.import(snippet)
    }

    pub fn mark_as_spilled(&mut self, value_identifier: &ValueIdentifier) {
        println!(
            "Warning: Marking {value_identifier} as spilled. Binding: {}",
            self.get_binding_name(value_identifier)
        );
        self.spill_required.insert(value_identifier.to_owned());
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
            .var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, _data_type, spilled) = self.vstack.find_stack_value(v);
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
        let (stack_position_of_value_to_remove, type_to_remove, old_value_spilled) =
            self.vstack.find_stack_value(value_identifier_to_remove);

        // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
        // this value must be marked as a value to be spilled, and the compiler must be run again.
        let accessible = stack_position_of_value_to_remove < STACK_SIZE;
        if old_value_spilled.is_none() && !accessible {
            self.mark_as_spilled(value_identifier_to_remove);
        }

        let (_top_element_id, (top_element_type, _new_value_spilled)) =
            self.vstack.pop().expect("vstack cannot be empty");

        assert_eq!(
            top_element_type, type_to_remove,
            "Top stack value and value to remove must match"
        );

        // Overwrite the value, whether it lives on stack or in memory
        let value_size = type_to_remove.size_of();

        match old_value_spilled {
            Some(spill_addr) => move_top_stack_value_to_memory(spill_addr, value_size),
            None => {
                if stack_position_of_value_to_remove > 0 && accessible {
                    vec![
                        vec![
                            Instruction(Swap(
                                stack_position_of_value_to_remove.try_into().unwrap()
                            )),
                            pop()
                        ];
                        value_size
                    ]
                    .concat()
                } else if !accessible {
                    println!("Compiler must run again because of {value_identifier_to_remove}");
                    vec![push(0), assert_()]
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
        let (stack_position_of_tuple, tuple_type, tuple_spilled) =
            self.vstack.find_stack_value(tuple_identifier);
        let element_types = if let ast::DataType::Tuple(ets) = &tuple_type {
            ets
        } else {
            panic!("Original value must have type tuple")
        };

        let (_top_element_id, (_top_element_type, _)) =
            self.vstack.pop().expect("vstack cannot be empty");

        // Last element of tuple is on top of stack. How many machine
        // words deep is the value we want to replace?
        let tuple_depth: usize = element_types
            .iter()
            .enumerate()
            .filter(|(i, _x)| *i > tuple_index)
            .map(|(_i, x)| x.size_of())
            .sum::<usize>();

        // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
        // this value must be marked as a value to be spilled, and the compiler must be run again.
        let stack_position_of_value_to_remove = stack_position_of_tuple + tuple_depth;
        let accessible = stack_position_of_value_to_remove < STACK_SIZE;
        if tuple_spilled.is_none() && !accessible {
            self.mark_as_spilled(tuple_identifier);
        }

        // Replace the overwritten value on stack, or in memory
        let value_size = element_types[tuple_index].size_of();
        match tuple_spilled {
            Some(spill_addr) => {
                let element_address = spill_addr + tuple_depth as u32;
                move_top_stack_value_to_memory(element_address, value_size)
            }
            None => {
                if !accessible {
                    println!("Compiler must run again because of {tuple_identifier}");
                    vec![push(0), assert_()]
                } else if stack_position_of_value_to_remove > 0 && accessible {
                    vec![
                        vec![
                            Instruction(Swap(
                                stack_position_of_value_to_remove.try_into().unwrap()
                            )),
                            pop()
                        ];
                        value_size
                    ]
                    .concat()
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
            let (addr, (dt, spilled)) = self.vstack.peek().unwrap().to_owned();
            let binding_name = self
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
                code.append(&mut vec![pop(); dt.size_of()]);
                self.vstack.pop();
                if let Some(binding) = binding_name {
                    let removed = self.var_addr.remove(&binding);
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
                (4, 2) => vec![swap(1), swap(3), swap(5), pop(), swap(1), swap(3), pop()],
                (6, 2) => vec![swap(2), swap(4), swap(6), pop(), swap(2), swap(4), swap(6), pop()],
                (n, 1) => {
                    let mut swaps = vec![];
                    for i in 1..n {
                        swaps.push(swap(i as u64));
                    }

                    vec![swaps, vec![pop()]].concat()
                }
                _ => panic!("Unsupported. Please cover more special cases. Got: {top_value_size}, {words_to_remove}"),
            }
        }

        let height_of_affected_stack: usize = self.vstack.get_stack_height() - height;

        let top_element = self
            .vstack
            .pop()
            .expect("Cannot remove all but top element from an empty stack");
        let top_value_size = top_element.1 .0.size_of();

        // Generate code to move value to the bottom of the requested stack range
        let words_to_remove = height_of_affected_stack - top_value_size;
        let code = if words_to_remove != 0
            && top_value_size <= words_to_remove
            && words_to_remove < STACK_SIZE
        {
            // If we can handle the stack clearing by just swapping the top value
            // lower onto the stack and removing everything above, we do that.
            let swap_instruction = Instruction(Swap(words_to_remove.try_into().unwrap()));
            let mut code = vec![vec![swap_instruction, pop()]; top_value_size].concat();

            // Generate code to remove any remaining values from the requested stack range
            let remaining_pops = if words_to_remove > top_value_size {
                words_to_remove - top_value_size
            } else {
                0
            };

            code.append(&mut vec![pop(); remaining_pops]);

            code
        } else if words_to_remove >= STACK_SIZE {
            // In this case, we have to clear more elements from the stack than we can
            // access with dup15/swap15. Our solution is to store the return value in
            // memory, clear the stack, and read it back from memory.
            let memory_location: u32 = self.library.kmalloc(top_value_size).try_into().unwrap();
            let mut code = copy_top_stack_value_to_memory(memory_location, top_value_size);
            code.append(&mut vec![pop(); height_of_affected_stack]);
            code.append(&mut load_from_memory(memory_location, top_value_size));

            // let mut words_to_remove = words_to_remove;
            // let mut code = vec![];
            // while words_to_remove > 0 {
            //     let swap_arg = if words_to_remove >= STACK_SIZE {
            //         STACK_SIZE as u64 - 1
            //     } else {
            //         words_to_remove as u64
            //     };
            //     let swap_instruction = swap(swap_arg);
            //     code.append(&mut vec![vec![swap_instruction, pop()]; top_value_size].concat());
            //     code.append(&mut vec![pop(); words_to_remove - top_value_size]);
            //     words_to_remove -= swap_arg as usize;
            // }
            code
        } else if words_to_remove != 0 {
            // Here, the number of words under the top element is less than the size of
            // the top element. We special-case this as the order on the stack
            // must be preserverved, which is non-trivial.
            clear_bottom_of_stack(top_value_size, words_to_remove)
        } else {
            // The case where nothing has to be done since there is nothing below the
            // top value that needs to be removed
            vec![]
        };

        // Clean up vstack
        while self.vstack.get_stack_height() > height {
            self.vstack.pop();
        }

        // Sanity check that input argument was aligned with a value
        assert_eq!(
            height,
            self.vstack.get_stack_height(),
            "Cannot clear stack to position that is not alligned with a value"
        );

        self.vstack.push(top_element);

        code
    }

    /// Helper function for debugging
    #[allow(dead_code)]
    fn show_vstack_values(&self) {
        print!("vstack: ");
        for (addr, (data_type, spilled)) in self.vstack.inner.iter() {
            let var_names = self
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

pub fn compile(function: &ast::Fn<types::Typing>) -> Vec<LabelledInstruction> {
    const FN_ARG_NAME_PREFIX: &str = "fn_arg";
    let fn_name = &function.fn_signature.name;
    let _fn_stack_input_sig = function
        .fn_signature
        .args
        .iter()
        .map(|arg| format!("({arg})"))
        .join(" ");
    let _fn_stack_output_sig = format!("{}", function.fn_signature.output);

    // Run the compilation 1st time to learn which values need to be spilled to memory
    let mut state = CompilerState::default();

    for arg in function.fn_signature.args.iter() {
        let (fn_arg_addr, spill) = state.new_value_identifier(FN_ARG_NAME_PREFIX, &arg.data_type);
        state.var_addr.insert(arg.name.clone(), fn_arg_addr);

        assert!(
            spill.is_none(),
            "Cannot handle spill 1st time code generator runs"
        );
    }

    let _fn_body_code = function
        .body
        .iter()
        .map(|stmt| compile_stmt(stmt, function, &mut state))
        .concat();

    // Run the compilation again know that we know which values to spill
    println!("\n\n\nRunning compiler again\n\n\n");
    let spill_required = state.spill_required;
    state = CompilerState::default();
    state.spill_required = spill_required;
    let mut fn_arg_spilling = vec![];
    for arg in function.fn_signature.args.iter() {
        let (fn_arg_addr, spill) = state.new_value_identifier(FN_ARG_NAME_PREFIX, &arg.data_type);
        state.var_addr.insert(arg.name.clone(), fn_arg_addr.clone());

        if let Some(_spill_addr) = spill {
            fn_arg_spilling.push(fn_arg_addr);
        }
    }

    let mut fn_body_code = vec![];

    // Spill required function arguments to memory
    for value_id in fn_arg_spilling {
        // Get stack depth of value
        let (stack_depth, data_type, spill_addr) = state.vstack.find_stack_value(&value_id);

        // Produce the code to spill the function argument
        let top_of_value = stack_depth;
        let mut spill_code =
            store_value_in_memory(spill_addr.unwrap(), data_type.size_of(), top_of_value);

        fn_body_code.append(&mut spill_code);
    }

    fn_body_code.append(
        &mut function
            .body
            .iter()
            .map(|stmt| compile_stmt(stmt, function, &mut state))
            .concat(),
    );

    // TODO: Use this function once triton-opcodes reaches 0.15.0
    // let dependencies = state.library.all_imports_as_instruction_lists();
    let dependencies = state.library.all_imports();
    let dependencies = parse(&dependencies)
        .map(|instructions| to_labelled(&instructions))
        .unwrap_or_else(|_| panic!("Must be able to parse dependencies code:\n{dependencies}"));

    // Assert that all subroutines start with a label and end with a return
    assert!(
        state.subroutines.iter().all(|subroutine| {
            let begins_with_label = matches!(*subroutine.first().unwrap(), Label(_));
            let ends_with_return_or_recurse = *subroutine.last().unwrap() == return_()
                || *subroutine.last().unwrap() == recurse();
            let contains_return = subroutine.iter().any(|x| *x == return_());
            begins_with_label && ends_with_return_or_recurse && contains_return
        }),
        "Each subroutine must begin with a label, contain a return and end with a return or a recurse"
    );

    let ret = vec![
        vec![Label(fn_name.to_owned())],
        fn_body_code,
        vec![Instruction(Return)],
        state.subroutines.concat(),
        dependencies,
    ]
    .concat();

    // Check that no label-duplicates are present. This could happen if a dependency
    // and the compiled function shared name. We do this by assembling the code and
    // then parsing it again. A duplicated label should be caught by the parser.
    // I wanted to add a test for this, but I couldn't find a good way of doing that.
    let assembler = ret.iter().map(|x| x.to_string()).join("\n");
    // println!("{assembler}");
    parse(&assembler)
        .map(|instructions| to_labelled(&instructions))
        .map_err(|err| anyhow::anyhow!("{}", err))
        .expect("Produced code must parse")
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
            state.var_addr.insert(var_name.clone(), expr_addr);
            expr_code
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            let data_type = expr.get_type();

            // When overwriting a value, we ignore the identifier of the new expression as
            // it's simply popped from the stack and the old identifier is used.
            let (_expr_addr, expr_code) = compile_expr(expr, "assign", &data_type, state);
            match identifier {
                ast::Identifier::String(var_name, _known_type) => {
                    let value_identifier = state.var_addr[var_name].clone();
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

                    let value_identifier = state.var_addr[&var_name].clone();

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
                    state.vstack.pop();
                    state.vstack.pop();
                    state.vstack.pop();

                    vec![expr_code, ident_code, index_code, vec![call(fn_name)]].concat()
                }
            }
        }

        // 'return;': Clean stack
        ast::Stmt::Return(None) => {
            let mut code = vec![];
            while let Some((_addr, (data_type, _spilled))) = state.vstack.pop() {
                code.push(vec![pop(); data_type.size_of()]);
            }

            code.concat()
        }

        ast::Stmt::Return(Some(ret_expr)) => {
            // special-case on returning variable, without unnecessary dup-instructions
            let expr_code = if let ast::Expr::Var(ast::Identifier::String(var_name, _known_type)) =
                ret_expr
            {
                // Remove everything above returned value
                let needle = state
                    .var_addr
                    .get(var_name)
                    .expect("Returned value must exist in value/addr map");
                let mut code = vec![];
                loop {
                    let (haystack, (dt, spilled)) = state.vstack.peek().unwrap();
                    if *haystack == *needle {
                        match spilled {
                            // If the returned value is a spilled value, we pop everything and
                            // load the value from memory. Otherwise, we pop until the sought
                            // value is on top of the stack and then call a helper function
                            // to remove everything below it.
                            Some(spill_addr) => {
                                code.append(&mut vec![pop(); state.vstack.get_stack_height()]);
                                code.append(&mut load_from_memory(*spill_addr, dt.size_of()))
                            }
                            None => code
                                .append(&mut state.clear_all_but_top_stack_value_above_height(0)),
                        }
                        break;
                    }

                    code.append(&mut vec![pop(); dt.size_of()]);
                    state.vstack.pop();
                }

                // TODO: Cleanup `var_addr`

                code
            } else {
                let code =
                    compile_expr(ret_expr, "ret_expr", &function.fn_signature.output, state).1;

                // Remove all but top value from stack
                let cleanup_code = state.clear_all_but_top_stack_value_above_height(0);

                // TODO: Cleanup `var_addr`

                vec![code, cleanup_code].concat()
            };

            expr_code
        }

        ast::Stmt::FnCall(fn_call) => compile_fn_call(fn_call, state),

        ast::Stmt::MethodCall(method_call) => compile_method_call(method_call, state),

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            // The code generated here is a subroutine that contains the while loop code
            // and then just a call to this subroutine.
            let (cond_addr, cond_code) =
                compile_expr(condition, "while_condition", &condition.get_type(), state);

            let while_loop_subroutine_name = format!("{cond_addr}_while_loop");

            // condition evaluation is not visible to loop body, so pop this from vstack
            state.vstack.pop();

            let loop_body_code = compile_stmt(&ast::Stmt::Block(block.to_owned()), function, state);
            let while_loop_code = vec![
                vec![Label(while_loop_subroutine_name.clone())],
                // condition
                cond_code,
                vec![push(0), eq(), skiz(), return_()],
                // body
                loop_body_code,
                // loop back (goto)
                vec![recurse()],
            ]
            .concat();

            state.subroutines.push(while_loop_code);

            vec![call(while_loop_subroutine_name)]
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let (cond_addr, cond_code) =
                compile_expr(condition, "if_condition", &condition.get_type(), state);

            // Pop condition result from vstack as it's not on the stack inside the branches
            let _condition_addr = state.vstack.pop();

            let then_body_code =
                compile_stmt(&ast::Stmt::Block(then_branch.to_owned()), function, state);

            let else_body_code =
                compile_stmt(&ast::Stmt::Block(else_branch.to_owned()), function, state);

            let then_subroutine_name = format!("{cond_addr}_then");
            let else_subroutine_name = format!("{cond_addr}_else");
            let mut if_code = cond_code;
            if_code.append(&mut vec![
                push(1),                            // _ cond 1
                swap(1),                            // _ 1 cond
                skiz(),                             // _ 1
                call(then_subroutine_name.clone()), // _ [then_branch_value] 0|1
                skiz(),                             // _ [then_branch_value]
                call(else_subroutine_name.clone()), // _ then_branch_value|else_branch_value
            ]);

            let then_code = vec![
                vec![Label(then_subroutine_name), pop()],
                then_body_code,
                vec![push(0), return_()],
            ]
            .concat();

            let else_code = vec![
                vec![Label(else_subroutine_name)],
                else_body_code,
                vec![return_()],
            ]
            .concat();

            state.subroutines.push(then_code);
            state.subroutines.push(else_code);

            if_code
        }

        ast::Stmt::Block(ast::BlockStmt { stmts }) => {
            let vstack_init = state.vstack.clone();
            let var_addr_init = state.var_addr.clone();
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
            state.vstack.pop();

            vec![assert_expr_code, vec![assert_()]].concat()
        }
    }
}

fn compile_fn_call(
    fn_call: &ast::FnCall<types::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let ast::FnCall {
        mut name,
        args,
        annot: _return_type, // unit for statement-level fn calls
        type_parameter,
    } = fn_call.clone();

    // Compile arguments left-to-right
    let (_args_idents, args_code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) = args
        .iter()
        .enumerate()
        .map(|(arg_pos, arg_expr)| {
            let context = format!("_{name}_arg_{arg_pos}");
            compile_expr(arg_expr, &context, &arg_expr.get_type(), state)
        })
        .unzip();

    // If function is from tasm-lib, import it
    if let Some(snippet_name) = tasm::get_function_name(&name) {
        name = tasm::import_tasm_snippet(snippet_name, state);
    }

    // If function is from vector-lib, import it
    if let Some(snippet_name) = vector::get_function_name(&name) {
        name = vector::import_tasm_snippet(snippet_name, &type_parameter, state);
    }

    // If function is from unsigned_lib, import it
    if let Some(snippet_name) = unsigned_integers::get_function_name(&name) {
        name = vector::import_tasm_snippet(snippet_name, &type_parameter, state);
    }

    for _ in 0..args.len() {
        state.vstack.pop();
    }

    let mut fn_call_code = args_code;
    fn_call_code.push(vec![call(name.to_string())]);

    fn_call_code.concat()
}

fn compile_method_call(
    method_call: &ast::MethodCall<types::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let mut name = method_call.method_name.clone();
    let receiver_type = method_call.args[0].get_type();
    let type_parameter = receiver_type.type_parameter();

    // Compile arguments, including receiver, left-to-right
    let (_args_idents, args_code): (Vec<ValueIdentifier>, Vec<Vec<LabelledInstruction>>) =
        method_call
            .args
            .iter()
            .enumerate()
            .map(|(arg_pos, arg_expr)| {
                let context = format!("_{name}_arg_{arg_pos}");
                compile_expr(arg_expr, &context, &arg_expr.get_type(), state)
            })
            .unzip();

    // If method is from vector-lib, ...
    if let Some(snippet_name) = vector::get_method_name(&name) {
        name = vector::import_tasm_snippet(snippet_name, &type_parameter, state);
    }

    // If method is from unsigned-integer-lib, ...
    if let Some(snippet_name) = unsigned_integers::get_method_name(&name) {
        name = unsigned_integers::import_tasm_snippet(snippet_name, &receiver_type, state);
    }

    for _ in 0..method_call.args.len() {
        state.vstack.pop();
    }

    let mut fn_call_code = args_code;
    fn_call_code.push(vec![call(name.to_string())]);

    fn_call_code.concat()
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
                vec![Instruction(Push(BFieldElement::new(*value as u64)))]
            }

            ast::ExprLit::U32(value) => {
                vec![Instruction(Push(BFieldElement::new(*value as u64)))]
            }

            ast::ExprLit::BFE(value) => vec![Instruction(Push(*value))],

            ast::ExprLit::U64(value) => {
                let as_u32s = U32s::<2>::try_from(*value).unwrap().to_sequence();
                let stack_serialized: Vec<_> = as_u32s.iter().rev().collect();

                let code = stack_serialized
                    .iter()
                    .map(|bfe| Instruction(Push(**bfe)))
                    .collect_vec();

                code
            }

            ast::ExprLit::XFE(_) => todo!(),
            ast::ExprLit::Digest(_) => todo!(),
            ast::ExprLit::GenericNum(n, _) => {
                panic!("Type of number literal {n} not resolved")
            }
        },

        ast::Expr::Var(identifier) => match identifier {
            ast::Identifier::String(var_name, _known_type) => {
                let var_addr = state
                    .var_addr
                    .get(var_name)
                    .expect("variable exists")
                    .to_owned();
                let (position, old_data_type, spilled) = state.vstack.find_stack_value(&var_addr);

                // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
                // this value must be marked as a value to be spilled, and the compiler must be run again.
                let bottom_position = position + result_type.size_of() - 1;
                let accessible = bottom_position < STACK_SIZE;
                if spilled.is_none() && !accessible {
                    state.mark_as_spilled(&var_addr);
                }

                // sanity check
                assert_eq!(old_data_type, result_type, "type must match expected type");

                match spilled {
                    Some(spill_address) => {
                        // The value has been spilled to memory. Get it from there.
                        load_from_memory(spill_address, result_type.size_of())
                    }
                    None => {
                        if !accessible {
                            // The compiler needs to run again. Produce unusable code.
                            vec![push(0), assert_()]
                        } else {
                            // The value is accessible on the stack. Copy it from there.
                            dup_value_from_stack_code(position.try_into().unwrap(), &result_type)
                        }
                    }
                }
            }
            ast::Identifier::TupleIndex(ident, tuple_index) => {
                let var_name = if let ast::Identifier::String(var_name, _) = *ident.to_owned() {
                    var_name
                } else {
                    panic!("Nested tuple references not yet supported");
                };
                let var_addr = state
                    .var_addr
                    .get(&var_name)
                    .expect("variable exists")
                    .to_owned();

                // Note that this function returns the address of the *tuple element*, both
                // on the stack and in memory if spilled. So the stack position/spilled address
                // should not be shifted with the elements position inside the tuple.
                let (position, element_type, spilled) =
                    state.vstack.find_tuple_element(&var_addr, *tuple_index);

                // If value is not marked as a spilled value and it's inacessible through swap(n)/dup(n),
                // this value must be marked as a value to be spilled, and the compiler must be run again.
                let bottom_position = position + element_type.size_of() - 1;
                let accessible = bottom_position < STACK_SIZE;
                if spilled.is_none() && !accessible {
                    state.mark_as_spilled(&var_addr);
                }

                match spilled {
                    Some(spill_address) => {
                        // The value has been spilled to memory. Get it from there.
                        load_from_memory(spill_address, element_type.size_of())
                    }
                    None => {
                        if !accessible {
                            // The compiler needs to run again. Produce unusable code.
                            vec![push(0), assert_()]
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
                    .var_addr
                    .get(&ident_as_string)
                    .expect("variable exists")
                    .to_owned();
                let (_position, old_data_type, _spilled) = state.vstack.find_stack_value(&var_addr);
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
                state.vstack.pop();
                state.vstack.pop();

                vec![ident_code, index_code, vec![call(fn_name)]].concat()
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
                state.vstack.pop();
            }

            code.concat()
        }

        ast::Expr::FnCall(fn_call) => compile_fn_call(fn_call, state),

        ast::Expr::MethodCall(method_call) => compile_method_call(method_call, state),

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
                            vec![call(safe_add_u32)]
                        }
                        ast::DataType::U64 => {
                            // We use the safe, overflow-checking, add code as default
                            let add_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::add_u64::AddU64));

                            vec![call(add_u64)]
                        }
                        ast::DataType::BFE => vec![add()],
                        ast::DataType::XFE => {
                            vec![xxadd(), swap(3), pop(), swap(3), pop(), swap(3), pop()]
                        }
                        _ => panic!("Operator add is not supported for type {result_type}"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, add_code].concat()
                }
                ast::BinOp::And => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let and_code = match result_type {
                        ast::DataType::Bool => vec![add(), push(2), eq()],
                        _ => panic!("Logical AND operator is not supported for {result_type}"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, and_code].concat()
                }

                ast::BinOp::BitAnd => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let bitwise_and_code = match result_type {
                        ast::DataType::U32 => vec![and()],
                        ast::DataType::U64 => {
                            let and_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::and_u64::AndU64));
                            vec![call(and_u64)]
                        }
                        _ => panic!("Logical AND operator is not supported for {result_type}"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, bitwise_and_code].concat()
                }

                ast::BinOp::BitXor => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;
                    let xor_code = match result_type {
                        U32 => vec![xor()],
                        U64 => vec![
                            // a_hi a_lo b_hi b_lo
                            swap(3), // b_lo a_lo b_hi a_hi
                            xor(),   // b_lo a_lo (b_hi ⊻ a_hi)
                            swap(2), // (b_hi ⊻ a_hi) b_lo a_lo
                            xor(),   // (b_hi ⊻ a_hi) (b_lo ⊻ a_lo)
                        ],
                        _ => panic!("xor on {result_type} is not supported"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();

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
                            vec![call(or_u32)]
                        }
                        U64 => {
                            let or_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::or_u64::OrU64));
                            vec![call(or_u64)]
                        }
                        _ => panic!("bitwise `or` on {result_type} is not supported"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();

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
                            state.vstack.pop();
                            state.vstack.pop();

                            vec![lhs_expr_code, rhs_expr_code, vec![swap(1), div(), pop()]].concat()
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
                                state.vstack.pop();

                                vec![lhs_expr_code, vec![call(div2)]].concat()
                            } else {
                                let (_lhs_expr_addr, lhs_expr_code) =
                                    compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                                let (_rhs_expr_addr, rhs_expr_code) =
                                    compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                                let div_mod_u64 = state.import_snippet(Box::new(
                                    arithmetic::u64::div_mod_u64::DivModU64,
                                ));

                                state.vstack.pop();
                                state.vstack.pop();

                                // Call the div-mod function and throw away the remainder
                                vec![
                                    lhs_expr_code,
                                    rhs_expr_code,
                                    vec![call(div_mod_u64), pop(), pop()],
                                ]
                                .concat()
                            }
                        }
                        BFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // div num
                            let bfe_div_code = vec![
                                swap(1),  // _ num div
                                invert(), // _ num (1/div)
                                mul(),    // _ num·(1/div), or (num/div)
                            ];

                            // Pop numerator and denominator
                            state.vstack.pop();
                            state.vstack.pop();

                            vec![lhs_expr_code, rhs_expr_code, bfe_div_code].concat()
                        }
                        XFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // div_2 div_1 div_0 num_2 num_1 num_0
                            let xfe_div_code = vec![
                                swap(5),   // num_0 div_1 div_0 num_2 num_1 div_2
                                swap(2),   // num_0 div_1 div_0 div_2 num_1 num_2
                                swap(5),   // num_2 div_1 div_0 div_2 num_1 num_0
                                swap(4),   // num_2 num_0 div_0 div_2 num_1 div_1
                                swap(1),   // num_2 num_0 div_0 div_2 div_1 num_1
                                swap(4),   // num_2 num_1 div_0 div_2 div_1 num_0
                                swap(3),   // num_2 num_1 num_0 div_2 div_1 div_0
                                xinvert(), // num_2 num_1 num_0 (1/div)_2 (1/div)_1 (1/div)_0
                                xxmul(),   // num_2 num_1 num_0 (num/div)_2 (num/div)_1 (num/div)_0
                                swap(3),   // num_2 num_1 (num/div)_0 (num/div)_2 (num/div)_1 num_0
                                pop(),     // num_2 num_1 (num/div)_0 (num/div)_2 (num/div)_1
                                swap(3),   // num_2 (num/div)_1 (num/div)_0 (num/div)_2 num_1
                                pop(),     // num_2 (num/div)_1 (num/div)_0 (num/div)_2
                                swap(3),   // (num/div)_2 (num/div)_1 (num/div)_0 num_2
                                pop(),     // (num/div)_2 (num/div)_1 (num/div)_0
                            ];

                            // Pop numerator and denominator
                            state.vstack.pop();
                            state.vstack.pop();

                            vec![lhs_expr_code, rhs_expr_code, xfe_div_code].concat()
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
                            state.vstack.pop();
                            state.vstack.pop();

                            vec![
                                lhs_expr_code,
                                rhs_expr_code,
                                vec![swap(1), div(), swap(1), pop()],
                            ]
                            .concat()
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

                            state.vstack.pop();
                            state.vstack.pop();

                            // Call the div-mod function and throw away the remainder
                            vec![
                                lhs_expr_code,
                                rhs_expr_code,
                                vec![call(div_mod_u64), swap(2), pop(), swap(2), pop()],
                            ]
                            .concat()
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

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, eq_code].concat()
                }

                ast::BinOp::Lt => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;

                    state.vstack.pop();
                    state.vstack.pop();

                    match lhs_type {
                        U32 => vec![lhs_expr_code, rhs_expr_code, vec![swap(1), lt()]].concat(),

                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64));

                            vec![
                                lhs_expr_code,
                                rhs_expr_code,
                                vec![
                                    // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                    swap(3),      // _ rhs_lo lhs_lo rhs_hi lhs_hi
                                    swap(1),      // _ rhs_lo lhs_lo lhs_hi rhs_hi
                                    swap(3),      // _ rhs_hi lhs_lo lhs_hi rhs_lo
                                    swap(2),      // _ rhs_hi rhs_lo lhs_hi lhs_lo
                                    call(lt_u64), // _ (lhs < rhs)
                                ],
                            ]
                            .concat()
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
                    state.vstack.pop();
                    state.vstack.pop();

                    match lhs_type {
                        U32 => vec![lhs_expr_code, rhs_expr_code, vec![lt()]].concat(),

                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64));

                            vec![
                                lhs_expr_code,
                                rhs_expr_code,
                                vec![
                                    // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                    call(lt_u64), // _ (lhs < rhs)
                                ],
                            ]
                            .concat()
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

                    state.vstack.pop();
                    state.vstack.pop();

                    match lhs_type {
                        U32 => {
                            let fn_name =
                                state.import_snippet(Box::new(arithmetic::u32::safe_mul::SafeMul));

                            vec![lhs_expr_code, rhs_expr_code, vec![call(fn_name)]].concat()
                        }
                        U64 => {
                            let fn_name = state.import_snippet(Box::new(
                                arithmetic::u64::safe_mul_u64::SafeMulU64,
                            ));

                            vec![lhs_expr_code, rhs_expr_code, vec![call(fn_name)]].concat()
                        }
                        BFE => vec![lhs_expr_code, rhs_expr_code, vec![mul()]].concat(),
                        _ => panic!("Unsupported MUL for type {lhs_type}"),
                    }
                }
                ast::BinOp::Neq => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let mut neq_code = compile_eq_code(&lhs_type, state);
                    neq_code.append(&mut vec![push(0), eq()]);

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, neq_code].concat()
                }

                ast::BinOp::Or => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let or_code = vec![
                        add(),   // _ (a + b)
                        push(0), // _ (a + b) 0
                        eq(),    // _ ((a + b) == 0)
                        push(0), // _ ((a + b) == 0) 0
                        eq(),    // _ ((a + b) != 0), or (a ∨ b)
                    ];

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, or_code].concat()
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

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, vec![call(shl)]].concat()
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

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, vec![call(shr)]].concat()
                }

                ast::BinOp::Sub => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let neg_1 = BFieldElement::P - 1;

                    let sub_code: Vec<LabelledInstruction> = match result_type {
                        ast::DataType::U32 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let safe_sub_u32 =
                                state.import_snippet(Box::new(arithmetic::u32::safe_sub::SafeSub));
                            vec![swap(1), call(safe_sub_u32)]
                        }
                        ast::DataType::U64 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let sub_u64 =
                                state.import_snippet(Box::new(arithmetic::u64::sub_u64::SubU64));
                            vec![
                                // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                swap(3),       // _ rhs_lo lhs_lo rhs_hi lhs_hi
                                swap(1),       // _ rhs_lo lhs_lo lhs_hi rhs_hi
                                swap(3),       // _ rhs_hi lhs_lo lhs_hi rhs_lo
                                swap(2),       // _ rhs_hi rhs_lo lhs_hi lhs_lo
                                call(sub_u64), // _ (lhs - rhs)_hi (lhs - rhs)_lo
                            ]
                        }
                        ast::DataType::BFE => {
                            vec![swap(1), push(neg_1), mul(), add()]
                        }
                        ast::DataType::XFE => {
                            vec![
                                // flip the x operands
                                swap(3),
                                swap(1),
                                swap(4),
                                swap(1),
                                swap(3),
                                swap(5),
                                // multiply top element with -1
                                push(neg_1),
                                xbmul(),
                                // Perform (lhs - rhs)
                                xxadd(),
                                // Get rid of the rhs, only leaving the result
                                swap(3),
                                pop(),
                                swap(3),
                                pop(),
                                swap(3),
                                pop(),
                            ]
                        }
                        _ => panic!("subtraction operator is not supported for {result_type}"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();

                    vec![lhs_expr_code, rhs_expr_code, sub_code].concat()
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
            state.vstack.pop();

            let branch_start_vstack = state.vstack.clone();
            let branch_start_var_addr = state.var_addr.clone();
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
            let _returned_value_from_then_block = state.vstack.pop().unwrap();
            state.verify_same_ordering_of_bindings(&branch_start_vstack, &branch_start_var_addr);
            state.var_addr = branch_start_var_addr.clone();

            // Compile `else` branch
            let (_else_addr, mut else_body_code) =
                compile_expr(else_branch, "else", &return_type, state);

            // Cleanup stack and variable name mapping after `else` body. Preserve the return
            // value from the `else` branch on the stack, but not on vstack, as this is added
            // later.
            let mut else_body_cleanup_code = state
                .clear_all_but_top_stack_value_above_height(branch_start_vstack.get_stack_height());
            else_body_code.append(&mut else_body_cleanup_code);
            let _returned_value_from_else_block = state.vstack.pop().unwrap();
            state.verify_same_ordering_of_bindings(&branch_start_vstack, &branch_start_var_addr);
            state.var_addr = branch_start_var_addr;

            // Both branches are compiled as subroutines which are called depending on what `cond`
            // evaluates to.
            let then_subroutine_name = format!("{then_addr}_then");
            let else_subroutine_name = format!("{then_addr}_else");
            let mut code = cond_code;
            code.append(&mut vec![
                push(1),                            // _ cond 1
                swap(1),                            // _ 1 cond
                skiz(),                             // _ 1
                call(then_subroutine_name.clone()), // _ [then_branch_value] 0|1
                skiz(),                             // _ [then_branch_value]
                call(else_subroutine_name.clone()), // _ then_branch_value|else_branch_value
            ]);

            let then_code = vec![
                vec![Label(then_subroutine_name), pop()],
                then_body_code,
                vec![push(0), return_()],
            ]
            .concat();

            let else_code = vec![
                vec![Label(else_subroutine_name)],
                else_body_code,
                vec![return_()],
            ]
            .concat();

            state.subroutines.push(then_code);
            state.subroutines.push(else_code);

            code
        }

        ast::Expr::Cast(expr, _as_type) => {
            let previous_type = expr.get_type();
            let (_expr_addr, expr_code) = compile_expr(expr, "as", &previous_type, state);
            let (_, (_old_data_type, spilled)) = state.vstack.pop().unwrap();

            // I don't think this value *can* be spilled unless maybe if you make a tuple
            // that's longer than 16 words which we probably can't handle anyway
            assert!(
                spilled.is_none(),
                "Can't handle spilled values in casting yet"
            );

            match (&previous_type, &result_type) {
                (ast::DataType::U64, ast::DataType::U32) => {
                    vec![expr_code, vec![swap(1), pop()]].concat()
                }
                (ast::DataType::U32, ast::DataType::U64) => {
                    vec![expr_code, vec![push(0), swap(1)]].concat()
                }
                // Allow identity-casting since we might need this to make the types
                // agree with code compiled by rustc.
                (ast::DataType::U32, ast::DataType::U32) => expr_code,
                (ast::DataType::U64, ast::DataType::U64) => expr_code,
                (ast::DataType::Bool, ast::DataType::U64) => {
                    vec![expr_code, vec![push(0), swap(1)]].concat()
                }
                (ast::DataType::Bool, ast::DataType::U32) => expr_code,
                (ast::DataType::Bool, ast::DataType::BFE) => expr_code,
                _ => todo!(),
            }
        }
    };

    let (addr, spill) = state.new_value_identifier(&format!("{expr}_{result_type}"), &result_type);
    let spill_code = spill
        .map(|x| copy_top_stack_value_to_memory(x, result_type.size_of()))
        .unwrap_or_default();

    (addr, vec![code, spill_code].concat())
}

/// Return the code to store a stack-value in memory
fn store_value_in_memory(
    memory_location: u32,
    value_size: usize,
    stack_location_for_top_of_value: usize,
) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address.
    let mut ret = vec![];
    ret.push(push(memory_location as u64));

    for i in 0..value_size {
        ret.push(dup(1 + i as u64 + stack_location_for_top_of_value as u64));
        // _ [elements] mem_address element

        ret.push(write_mem());
        // _ [elements] mem_address

        if i != value_size - 1 {
            ret.push(push(1));
            ret.push(add());
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(pop());

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
    let mut ret = vec![];
    ret.push(push(memory_location as u64));

    for i in 0..top_value_size {
        ret.push(swap(1));
        // _ mem_address element

        ret.push(write_mem());
        // _ mem_address

        if i != top_value_size - 1 {
            ret.push(push(1));
            ret.push(add());
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(pop());

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
    let mut ret = vec![];
    ret.push(push(memory_location as u64));

    for i in 0..top_value_size {
        ret.push(dup(1 + i as u64));
        // _ [elements] mem_address element

        ret.push(write_mem());
        // _ [elements] mem_address

        if i != top_value_size - 1 {
            ret.push(push(1));
            ret.push(add());
            // _ (mem_address + 1)
        }
    }

    // remove memory address from top of stack
    ret.push(pop());

    ret
}

fn load_from_memory(memory_location: u32, top_value_size: usize) -> Vec<LabelledInstruction> {
    // A stack value of the form `_ val2 val1 val0`, with `val0` being on the top of the stack
    // is stored in memory as: `val0 val1 val2`, where `val0` is stored on the `memory_location`
    // address. So we read the value at the highes memory location first.
    // TODO: Consider making subroutines out of this in
    // order to get shorter programs.
    let mut ret = vec![];
    ret.push(push(memory_location as u64 + top_value_size as u64 - 1));

    for i in 0..top_value_size {
        // Stack: _ memory_address

        ret.push(read_mem());
        // Stack: _ memory_address value

        ret.push(swap(1));

        // Decrement memory address to prepare for next loop iteration
        if i != top_value_size - 1 {
            ret.push(push(BFieldElement::MAX));
            ret.push(add());
            // Stack: _ (memory_address - 1)
        }
    }

    // Remove memory address from top of stack
    ret.push(pop());

    // Stack: _ element_N element_{N - 1} ... element_0

    ret
}

fn compile_eq_code(
    lhs_type: &ast::DataType,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    use ast::DataType::*;
    match lhs_type {
        Bool | U32 | BFE => vec![eq()],
        U64 => vec![
            // _ a_hi a_lo b_hi b_lo
            swap(3), // _ b_lo a_lo b_hi a_hi
            eq(),    // _ b_lo a_lo (b_hi == a_hi)
            swap(2), // _ (b_hi == a_hi) a_lo b_lo
            eq(),    // _ (b_hi == a_hi) (a_lo == b_lo)
            mul(),   // _ (b_hi == a_hi && a_lo == b_lo)
        ],
        U128 => todo!(),

        XFE => vec![
            // _ a_2 a_1 a_0 b_2 b_1 b_0
            swap(4), // _ a_2 b_0 a_0 b_2 b_1 a_1
            eq(),    // _ a_2 b_0 a_0 b_2 (b_1 == a_1)
            swap(4), // _ (b_1 == a_1) b_0 a_0 b_2 a_2
            eq(),    // _ (b_1 == a_1) b_0 a_0 (b_2 == a_2)
            swap(2), // _ (b_1 == a_1) (b_2 == a_2) a_0 b_0
            eq(),    // _ (b_1 == a_1) (b_2 == a_2) (a_0 == b_0)
            mul(),   // _ (b_1 == a_1) (b_2 == a_2)·(a_0 == b_0)
            mul(),   // _ (b_1 == a_1)·(b_2 == a_2)·(a_0 == b_0)
        ],
        Digest => {
            let eq_digest = state.import_snippet(Box::new(hashing::eq_digest::EqDigest));
            vec![call(eq_digest)]
        }
        List(_) => todo!(),
        Tuple(_) => todo!(),
    }
}

/// Copy a value at a position on the stack to the top
fn dup_value_from_stack_code(
    position: Ord16,
    data_type: &ast::DataType,
) -> Vec<LabelledInstruction> {
    let elem_size = data_type.size_of();

    // the position of the deepest element of the value.
    let n: usize = Into::<usize>::into(position) + elem_size - 1;

    vec![Instruction(Dup(n.try_into().unwrap())); elem_size]
}
