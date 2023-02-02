use std::collections::HashMap;

use itertools::Itertools;
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

use crate::ast;
use crate::stack::Stack;
use crate::tasm_function_signatures::{get_tasm_lib_fn, import_tasm_snippet};

type VStack = Stack<(ValueIdentifier, ast::DataType)>;
type VarAddr = HashMap<String, ValueIdentifier>;

impl VStack {
    /// Returns (stack_position, data_type, vstack_position). Top of stack has index 0.
    pub fn find_stack_value(&self, seek_addr: &ValueIdentifier) -> (Ord16, ast::DataType, usize) {
        let mut position: usize = 0;
        for (i, (found_addr, data_type)) in self.inner.iter().rev().enumerate() {
            if seek_addr == found_addr {
                return (
                    position
                        .try_into()
                        .expect("Found stack position must match a stack element"),
                    data_type.to_owned(),
                    i,
                );
            }

            position += size_of(data_type);

            // By asserting after `+= size_of(data_type)`, we check that the deepest part
            // of the sought value is addressable, not just the top part of the value.
            assert!(position < STACK_SIZE, "Addressing beyond the {STACK_SIZE}'th stack element requires spilling and register-allocation.");
        }

        panic!("Cannot find {seek_addr} on vstack")
    }

    /// Return the code to overwrite a stack value with the value that's on top of the stack
    /// Note that the top value and the value to be removed *must* be of the same type.
    /// Updates the `vstack` but not the `var_addr` as this is assumed to be handled by the caller.
    fn overwrite_stack_value_with_same_data_type(
        &mut self,
        value_identifier_to_remove: &ValueIdentifier,
    ) -> Vec<LabelledInstruction> {
        let (stack_position_of_value_to_remove, type_to_remove, _) =
            self.find_stack_value(value_identifier_to_remove);
        let (top_element_id, top_element_type) = self.pop().expect("vstack cannot be empty");

        assert_eq!(
            top_element_type, type_to_remove,
            "Top stack value and value to remove must match"
        );

        let value_size = size_of(&type_to_remove);

        // Remove the overwritten value from stack
        let code: Vec<LabelledInstruction> =
            if Into::<usize>::into(stack_position_of_value_to_remove) > 0 {
                vec![vec![Instruction(Swap(stack_position_of_value_to_remove)), pop()]; value_size]
                    .concat()
            } else {
                vec![]
            };

        // Remove the overwritten value from vstack
        let old_value = (value_identifier_to_remove.to_owned(), type_to_remove);
        let new_value = (top_element_id, top_element_type);
        self.replace_value(&old_value, new_value);

        code
    }

    /// Return code that clears the stack but leaves the value that's on the top of the stack
    /// when this function is called.
    fn remove_all_but_top_stack_value(&mut self) -> Vec<LabelledInstruction> {
        let stack_height = self.get_stack_height();
        let top_element = self
            .pop()
            .expect("Cannot remove all but top element from an empty stack");
        let top_value_size = size_of(&top_element.1);
        assert!(
            stack_height < STACK_SIZE,
            "For now, we only support functions with max {STACK_SIZE} elements on the stack"
        );

        // Generate code to move value to the bottom of the stack
        let words_to_remove = stack_height - top_value_size;
        let code = if words_to_remove != 0 {
            let swap_instruction = Instruction(Swap(words_to_remove.try_into().unwrap()));
            vec![vec![swap_instruction, pop()]; top_value_size].concat()
        } else {
            vec![]
        };

        // Clean up vstack
        while let Some(_elem) = self.pop() {}
        self.push(top_element);

        // Generate code to remove any remaining values from the stack
        let remaining_pops = if words_to_remove > top_value_size {
            words_to_remove - top_value_size
        } else {
            0
        };
        vec![code, vec![pop(); remaining_pops]].concat()
    }

    fn get_stack_height(&self) -> usize {
        self.inner
            .iter()
            .map(|(_, data_type)| size_of(data_type))
            .sum()
    }
}

#[derive(Debug, Default)]
pub struct CompilerState {
    pub counter: usize,

    // Where on stack is the variable placed?
    pub vstack: VStack,

    // Mapping from variable name to its internal identifier
    pub var_addr: VarAddr,

    // A library struct to keep check of which snippets are already in the namespace
    pub library: Library,

    // A list of call sites for ad-hoc branching
    pub subroutines: Vec<Vec<LabelledInstruction>>,
}

// TODO: Use this value from Triton-VM
const STACK_SIZE: usize = 16;

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
    pub fn new_value_identifier(
        &mut self,
        prefix: &str,
        data_type: &ast::DataType,
    ) -> ValueIdentifier {
        let name = format!("_{}_{}_{}", prefix, data_type, self.counter);
        let address = ValueIdentifier { name };
        self.vstack.push((address.clone(), data_type.clone()));
        self.counter += 1;
        address
    }

    pub fn import_snippet(&mut self, snippet: Box<dyn Snippet>) -> &'static str {
        self.library.import(snippet)
    }

    /// Restore the vstack to a previous state, representing the state the stack had
    /// at the beginning of a codeblock.
    fn restore_stack_code(
        &self,
        previous_stack: &VStack,
        previous_var_addr: &VarAddr,
    ) -> Vec<LabelledInstruction> {
        // Get the variable names of all bindings at the start of the codeblock

        // make a list of tuples (binding name, vstack position, stack position, data_type) as the stack looked at the beginning of the loop
        let bindings_start: HashMap<String, (usize, Ord16, ast::DataType)> = previous_var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, data_type, previous_vstack_depth) =
                    previous_stack.find_stack_value(v);
                (
                    binding_name,
                    (previous_vstack_depth, previous_stack_depth, data_type),
                )
            })
            .collect();

        // make a list of tuples (variable names, vstack position, stack position, data_type) as the stack looks now
        let bindings_end: HashMap<String, (usize, Ord16, ast::DataType)> = self
            .var_addr
            .iter()
            .map(|(k, v)| {
                let binding_name = k.to_string();
                let (previous_stack_depth, data_type, previous_vstack_depth) =
                    self.vstack.find_stack_value(v);
                (
                    binding_name,
                    (previous_vstack_depth, previous_stack_depth, data_type),
                )
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

        // TODO:
        // Sanity check that all data types of matching bindings are of the same data_type?

        // Generate a flip/flop list of tuple values (var_name, old_position, new_position)
        let mut flip_flop_list = vec![];
        for (start_binding, (_start_vstack_pos, start_stack_pos, _start_data_type)) in
            bindings_start.iter()
        {
            let (_end_vstack_pow, end_stack_pos, _end_data_type) =
                bindings_end[start_binding].clone();
            if end_stack_pos != *start_stack_pos {
                flip_flop_list.push((start_binding, end_stack_pos, start_stack_pos));
            }
        }

        if !flip_flop_list.is_empty() {
            panic!("non-empty flip-flop list is not yet supported!\n\n\nflip-flop list: {flip_flop_list:#?}");
        }

        // TODO:
        // For each value that is in a new position, generate the code to swap it back.
        // For each binding that wasn't there at start, remove it from both own vstack and from the stack by returning the code
        // to do that.

        vec![]
    }

    /// Helper function for debugging
    #[allow(dead_code)]
    fn show_vstack_values(&self) {
        print!("vstack: ");
        for (addr, data_type) in self.vstack.inner.iter() {
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
            print!("{var_name} <{data_type}>, ");
        }
        println!();
    }
}

pub fn compile(function: &ast::Fn<ast::Typing>) -> Vec<LabelledInstruction> {
    let fn_name = &function.fn_signature.name;
    let _fn_stack_input_sig = function
        .fn_signature
        .args
        .iter()
        .map(|arg| format!("({arg})"))
        .join(" ");
    let _fn_stack_output_sig = format!("{}", function.fn_signature.output);

    let mut state = CompilerState::default();

    for arg in function.fn_signature.args.iter() {
        let fn_arg_addr = state.new_value_identifier("_fn_arg", &arg.data_type);
        state.var_addr.insert(arg.name.clone(), fn_arg_addr);
    }

    let fn_body_code = function
        .body
        .iter()
        .map(|stmt| compile_stmt(stmt, function, &mut state))
        .concat();

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
            begins_with_label && ends_with_return_or_recurse
        }),
        "Each subroutine must begin with a label and end with a return or a recurse"
    );

    let ret = vec![
        vec![Label(fn_name.to_owned())],
        fn_body_code,
        vec![Instruction(Return)],
        dependencies,
        state.subroutines.concat(),
    ]
    .concat();

    // Check that no label-duplicates are present. This could happen if a dependency
    // and the compiled function shared name. We do this by assembling the code and
    // then parsing it again. A duplicated label should be caught by the parser.
    // I wanted to add a test for this, but I couldn't find a good way of doing that.
    let assembler = ret.iter().map(|x| x.to_string()).join("\n");
    parse(&assembler)
        .map(|instructions| to_labelled(&instructions))
        .expect("Produced code must parse")
}

fn compile_stmt(
    stmt: &ast::Stmt<ast::Typing>,
    function: &ast::Fn<ast::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    match stmt {
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
        }) => {
            let (expr_addr, expr_code) = compile_expr(expr, var_name, data_type, state);
            state.var_addr.insert(var_name.clone(), expr_addr);
            expr_code
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => match identifier {
            ast::Identifier::String(var_name, known_type) => {
                let data_type = known_type.get_type();
                let (expr_addr, expr_code) = compile_expr(expr, "assign", &data_type, state);
                let old_value_identifier = state
                    .var_addr
                    .insert(var_name.clone(), expr_addr.clone())
                    .expect("Value identifier must exist in var_addr");

                // Currently, assignments just get the same place on the stack as the value
                // that it is overwriting had. This may or may not be efficient.
                // Get code to overwrite old value, and update the compiler's vstack
                let overwrite_code = state
                    .vstack
                    .overwrite_stack_value_with_same_data_type(&old_value_identifier);

                vec![expr_code, overwrite_code].concat()
            }
            ast::Identifier::TupleIndex(_, _) => todo!(),
            ast::Identifier::ListIndex(_, _) => todo!(),
        },

        // 'return;': Clean stack
        ast::Stmt::Return(None) => {
            let mut code = vec![];
            while let Some((_addr, data_type)) = state.vstack.pop() {
                code.push(vec![pop(); size_of(&data_type)]);
            }

            code.concat()
        }

        ast::Stmt::Return(Some(ret_expr)) => {
            // special-case on returning variable, without unnecessary dup-instructions
            let expr_code =
                if let ast::Expr::Var(ast::Identifier::String(var_name, _known_type)) = ret_expr {
                    // Remove everything above returned value
                    let needle = state
                        .var_addr
                        .get(var_name)
                        .expect("Returned value must exist in value/addr map");
                    let mut code = vec![];
                    loop {
                        let (haystack, dt) = state.vstack.peek().unwrap();
                        if *haystack == *needle {
                            break;
                        }

                        code.append(&mut vec![pop(); size_of(&dt)]);
                        state.vstack.pop();
                    }

                    // Now returned value is top of stack. Remove everything below it
                    code.append(&mut state.vstack.remove_all_but_top_stack_value());

                    code
                } else {
                    let expr_code =
                        compile_expr(ret_expr, "ret_expr", &function.fn_signature.output, state).1;

                    // Remove all but top value from stack
                    let remove_elements_code = state.vstack.remove_all_but_top_stack_value();
                    vec![expr_code, remove_elements_code].concat()
                };

            expr_code
        }

        ast::Stmt::FnCall(fn_call) => compile_fn_call(fn_call, state),
        ast::Stmt::While(ast::WhileStmt { condition, stmts }) => {
            // The code generated here is a subroutine that contains the while loop code
            // and then just a call to this subroutine.
            let (cond_addr, cond_code) =
                compile_expr(condition, "while_condition", &condition.get_type(), state);

            let while_loop_subroutine_name = format!("{cond_addr}_while_loop");

            // condition evaluation is not visible to loop body, so pop this from vstack
            state.vstack.pop();
            let vstack_init = state.vstack.clone();
            let var_addr_init = state.var_addr.clone();
            let mut loop_body_code = stmts
                .iter()
                .map(|stmt| compile_stmt(stmt, function, state))
                .collect_vec()
                .concat();

            let mut restore_stack_code = state.restore_stack_code(&vstack_init, &var_addr_init);

            loop_body_code.append(&mut restore_stack_code);

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
        ast::Stmt::If(_) => todo!(),
    }
}

fn compile_fn_call(
    fn_call: &ast::FnCall<ast::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction> {
    let ast::FnCall {
        mut name,
        args,
        annot: _return_type, // void
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
    if let Some(snippet_name) = get_tasm_lib_fn(&name) {
        import_tasm_snippet(snippet_name, None, state);
        name = snippet_name.to_string();
    }

    for _ in 0..args.len() {
        state.vstack.pop();
    }

    let mut fn_call_code = args_code;
    fn_call_code.push(vec![
        //
        call(name.to_string()),
    ]);

    fn_call_code.concat()
}

fn compile_expr(
    expr: &ast::Expr<ast::Typing>,
    _context: &str,
    _data_type: &ast::DataType,
    state: &mut CompilerState,
) -> (ValueIdentifier, Vec<LabelledInstruction>) {
    match expr {
        ast::Expr::Lit(expr_lit, known_type) => {
            let res_type = known_type.get_type();

            match expr_lit {
                ast::ExprLit::Bool(value) => {
                    let addr = state.new_value_identifier("_bool_lit", &res_type);
                    (
                        addr,
                        vec![Instruction(Push(BFieldElement::new(*value as u64)))],
                    )
                }

                ast::ExprLit::U32(value) => {
                    let addr = state.new_value_identifier("_u32_lit", &res_type);
                    (
                        addr,
                        vec![Instruction(Push(BFieldElement::new(*value as u64)))],
                    )
                }

                ast::ExprLit::BFE(value) => {
                    let addr = state.new_value_identifier("_bfe_lit", &res_type);
                    (addr, vec![Instruction(Push(*value))])
                }

                ast::ExprLit::U64(value) => {
                    let addr = state.new_value_identifier("_u64_lit", &res_type);
                    let as_u32s = U32s::<2>::try_from(*value).unwrap().to_sequence();
                    let stack_serialized: Vec<_> = as_u32s.iter().rev().collect();

                    let code = stack_serialized
                        .iter()
                        .map(|bfe| Instruction(Push(**bfe)))
                        .collect_vec();

                    (addr, code)
                }

                ast::ExprLit::XFE(_) => todo!(),
                ast::ExprLit::Digest(_) => todo!(),
            }
        }

        ast::Expr::Var(identifier) => match identifier {
            ast::Identifier::String(var_name, known_type) => {
                let data_type = known_type.get_type();
                let var_addr = state.var_addr.get(var_name).expect("variable exists");
                let (position, old_data_type, _) = state.vstack.find_stack_value(var_addr);

                // sanity check
                assert_eq!(old_data_type, data_type, "type must match expected type");

                let var_copy_code = dup_value_from_stack_code(position, &data_type);
                let var_copy_addr = state.new_value_identifier("_var_copy", &data_type);

                (var_copy_addr, var_copy_code)
            }
            ast::Identifier::TupleIndex(_, _) => todo!(),
            ast::Identifier::ListIndex(_, _) => todo!(),
        },

        ast::Expr::FlatList(_) => todo!(),
        ast::Expr::FnCall(fn_call) => {
            let fn_call_code = compile_fn_call(fn_call, state);
            let fn_call_ident_prefix = format!("_fn_call_{}", fn_call.name);
            let fn_call_ident =
                state.new_value_identifier(&fn_call_ident_prefix, &fn_call.annot.get_type());

            (fn_call_ident, fn_call_code)
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, known_type) => {
            let res_type = known_type.get_type();
            let lhs_type = lhs_expr.get_type();
            let rhs_type = rhs_expr.get_type();

            // // LHS is expected to be on the top of the stack, so we get RHS first
            // let (_rhs_expr_addr, rhs_expr_code) =
            //     compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);
            // let (_lhs_expr_addr, lhs_expr_code) =
            //     compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
            // let lhs_expr_owned: ast::Expr<ast::Typing> = *(*lhs_expr).to_owned();
            // let rhs_expr_owned: ast::Expr<ast::Typing> = *(*rhs_expr).to_owned();

            let (addr, code) = match binop {
                ast::BinOp::Add => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let add_code = match res_type {
                        ast::DataType::U32 => {
                            // We use the safe, overflow-checking, add code as default
                            let safe_add_u32 = state
                                .import_snippet(Box::new(arithmetic::u32::safe_add::SafeAdd))
                                .to_string();
                            vec![call(safe_add_u32)]
                        }
                        ast::DataType::U64 => {
                            // We use the safe, overflow-checking, add code as default
                            let add_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::add_u64::AddU64))
                                .to_string();
                            vec![call(add_u64)]
                        }
                        ast::DataType::BFE => vec![add()],
                        ast::DataType::XFE => {
                            vec![xxadd(), swap3(), pop(), swap3(), pop(), swap3(), pop()]
                        }
                        _ => panic!("Operator add is not supported for type {res_type}"),
                    };

                    let code = vec![lhs_expr_code, rhs_expr_code, add_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_add", &res_type);
                    (addr, code)
                }
                ast::BinOp::And => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let and_code = match res_type {
                        ast::DataType::Bool => vec![add(), push(2), eq()],
                        _ => panic!("Logical AND operator is not supported for {res_type}"),
                    };

                    let code = vec![lhs_expr_code, rhs_expr_code, and_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_add", &res_type);
                    (addr, code)
                }

                ast::BinOp::BitAnd => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let bitwise_and_code = match res_type {
                        ast::DataType::U32 => vec![and()],
                        ast::DataType::U64 => {
                            let and_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::and_u64::AndU64))
                                .to_string();
                            vec![call(and_u64)]
                        }
                        _ => panic!("Logical AND operator is not supported for {res_type}"),
                    };

                    let code = vec![lhs_expr_code, rhs_expr_code, bitwise_and_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_add", &res_type);

                    (addr, code)
                }

                ast::BinOp::BitXor => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;
                    let xor_code = match res_type {
                        U32 => vec![xor()],
                        U64 => vec![
                            // a_hi a_lo b_hi b_lo
                            swap3(), // b_lo a_lo b_hi a_hi
                            xor(),   // b_lo a_lo (b_hi ⊻ a_hi)
                            swap2(), // (b_hi ⊻ a_hi) b_lo a_lo
                            xor(),   // (b_hi ⊻ a_hi) (b_lo ⊻ a_lo)
                        ],
                        _ => panic!("xor on {res_type} is not supported"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_xor", &res_type);

                    (addr, vec![lhs_expr_code, rhs_expr_code, xor_code].concat())
                }

                ast::BinOp::Div => {
                    use ast::DataType::*;
                    match res_type {
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
                            let addr = state.new_value_identifier("_binop_div", &res_type);

                            (
                                addr,
                                vec![lhs_expr_code, rhs_expr_code, vec![swap1(), div(), pop()]]
                                    .concat(),
                            )
                        }
                        U64 => {
                            // For now we can only divide u64s by 2.
                            let rhs_expr_owned = *rhs_expr.to_owned();
                            if !matches!(rhs_expr_owned, ast::Expr::Lit(ast::ExprLit::U64(2), _)) {
                                panic!("Unsupported division with denominator: {rhs_expr_owned:#?}")
                            }

                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let div2 = state
                                .import_snippet(Box::new(arithmetic::u64::div2_u64::Div2U64))
                                .to_string();

                            // Pop the numerator that was divided by two
                            state.vstack.pop();
                            let addr = state.new_value_identifier("_binop_div", &res_type);

                            (addr, vec![lhs_expr_code, vec![call(div2)]].concat())
                        }
                        BFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // div num
                            let bfe_div_code = vec![
                                swap1(),  // _ num div
                                invert(), // _ num (1/div)
                                mul(),    // _ num·(1/div), or (num/div)
                            ];

                            // Pop numerator and denominator
                            state.vstack.pop();
                            state.vstack.pop();
                            let addr = state.new_value_identifier("_binop_div", &res_type);

                            (
                                addr,
                                vec![lhs_expr_code, rhs_expr_code, bfe_div_code].concat(),
                            )
                        }
                        XFE => {
                            let (_lhs_expr_addr, lhs_expr_code) =
                                compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);
                            let (_rhs_expr_addr, rhs_expr_code) =
                                compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                            // div_2 div_1 div_0 num_2 num_1 num_0
                            let xfe_div_code = vec![
                                swap5(),   // num_0 div_1 div_0 num_2 num_1 div_2
                                swap2(),   // num_0 div_1 div_0 div_2 num_1 num_2
                                swap5(),   // num_2 div_1 div_0 div_2 num_1 num_0
                                swap4(),   // num_2 num_0 div_0 div_2 num_1 div_1
                                swap1(),   // num_2 num_0 div_0 div_2 div_1 num_1
                                swap4(),   // num_2 num_1 div_0 div_2 div_1 num_0
                                swap3(),   // num_2 num_1 num_0 div_2 div_1 div_0
                                xinvert(), // num_2 num_1 num_0 (1/div)_2 (1/div)_1 (1/div)_0
                                xxmul(),   // num_2 num_1 num_0 (num/div)_2 (num/div)_1 (num/div)_0
                                swap3(),   // num_2 num_1 (num/div)_0 (num/div)_2 (num/div)_1 num_0
                                pop(),     // num_2 num_1 (num/div)_0 (num/div)_2 (num/div)_1
                                swap3(),   // num_2 (num/div)_1 (num/div)_0 (num/div)_2 num_1
                                pop(),     // num_2 (num/div)_1 (num/div)_0 (num/div)_2
                                swap3(),   // (num/div)_2 (num/div)_1 (num/div)_0 num_2
                                pop(),     // (num/div)_2 (num/div)_1 (num/div)_0
                            ];

                            // Pop numerator and denominator
                            state.vstack.pop();
                            state.vstack.pop();
                            let addr = state.new_value_identifier("_binop_div", &res_type);

                            (
                                addr,
                                vec![lhs_expr_code, rhs_expr_code, xfe_div_code].concat(),
                            )
                        }
                        _ => panic!("Unsupported div for type {res_type}"),
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
                    let addr = state.new_value_identifier("_binop_eq", &res_type);

                    (addr, vec![lhs_expr_code, rhs_expr_code, eq_code].concat())
                }

                ast::BinOp::Lt => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;
                    let code = match lhs_type {
                        U32 => vec![lhs_expr_code, rhs_expr_code, vec![swap1(), lt()]].concat(),

                        U64 => {
                            let lt_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::lt_u64::LtStandardU64))
                                .to_string();

                            vec![
                                lhs_expr_code,
                                rhs_expr_code,
                                vec![
                                    // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                    swap3(),      // _ rhs_lo lhs_lo rhs_hi lhs_hi
                                    swap1(),      // _ rhs_lo lhs_lo lhs_hi rhs_hi
                                    swap3(),      // _ rhs_hi lhs_lo lhs_hi rhs_lo
                                    swap2(),      // _ rhs_hi rhs_lo lhs_hi lhs_lo
                                    call(lt_u64), // _ (lhs < rhs)
                                ],
                            ]
                            .concat()
                        }
                        _ => panic!("Unsupported < for type {lhs_type}"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_lt", &res_type);

                    (addr, code)
                }
                ast::BinOp::Mul => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    use ast::DataType::*;
                    let code = match lhs_type {
                        U32 => {
                            let fn_name =
                                state.import_snippet(Box::new(arithmetic::u32::safe_mul::SafeMul));

                            vec![
                                rhs_expr_code,
                                lhs_expr_code,
                                vec![call(fn_name.to_string())],
                            ]
                            .concat()
                        }
                        _ => panic!("Unsupported MUL for type {lhs_type}"),
                    };

                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_lt", &res_type);

                    (addr, code)
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
                    let addr = state.new_value_identifier("_binop_neq", &res_type);

                    (addr, vec![lhs_expr_code, rhs_expr_code, neq_code].concat())
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
                    let addr = state.new_value_identifier("_binop_or", &res_type);

                    (addr, vec![lhs_expr_code, rhs_expr_code, or_code].concat())
                }

                ast::BinOp::Rem => todo!(),

                ast::BinOp::Shl => {
                    // For now we can only `1 << n` for some n
                    let lhs_expr_owned = *lhs_expr.to_owned();
                    if !matches!(lhs_expr_owned, ast::Expr::Lit(ast::ExprLit::U64(1), _)) {
                        panic!("Unsupported shift left: {lhs_expr_owned:#?}")
                    }

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let pow2_fn = state
                        .import_snippet(Box::new(arithmetic::u64::pow2_u64::Pow2U64))
                        .to_string();
                    let code = vec![rhs_expr_code, vec![call(pow2_fn)]].concat();

                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_shl", &res_type);

                    (addr, code)
                }

                ast::BinOp::Shr => todo!(),

                ast::BinOp::Sub => {
                    let (_lhs_expr_addr, lhs_expr_code) =
                        compile_expr(lhs_expr, "_binop_lhs", &lhs_type, state);

                    let (_rhs_expr_addr, rhs_expr_code) =
                        compile_expr(rhs_expr, "_binop_rhs", &rhs_type, state);

                    let neg_1 = BFieldElement::P - 1;

                    let sub_code: Vec<LabelledInstruction> = match res_type {
                        ast::DataType::U32 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let safe_sub_u32 = state
                                .import_snippet(Box::new(arithmetic::u32::safe_sub::SafeSub))
                                .to_string();
                            vec![swap1(), call(safe_sub_u32)]
                        }
                        ast::DataType::U64 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let sub_u64 = state
                                .import_snippet(Box::new(arithmetic::u64::sub_u64::SubU64))
                                .to_string();
                            vec![
                                // _ lhs_hi lhs_lo rhs_hi rhs_lo
                                swap3(),       // _ rhs_lo lhs_lo rhs_hi lhs_hi
                                swap1(),       // _ rhs_lo lhs_lo lhs_hi rhs_hi
                                swap3(),       // _ rhs_hi lhs_lo lhs_hi rhs_lo
                                swap2(),       // _ rhs_hi rhs_lo lhs_hi lhs_lo
                                call(sub_u64), // _ (lhs - rhs)_hi (lhs - rhs)_lo
                            ]
                        }
                        ast::DataType::BFE => {
                            vec![swap1(), push(neg_1), mul(), add()]
                        }
                        ast::DataType::XFE => {
                            vec![
                                // flip the x operands
                                swap3(),
                                swap1(),
                                swap4(),
                                swap1(),
                                swap3(),
                                swap5(),
                                // multiply top element with -1
                                push(neg_1),
                                xbmul(),
                                // Perform (lhs - rhs)
                                xxadd(),
                                // Get rid of the rhs, only leaving the result
                                swap3(),
                                pop(),
                                swap3(),
                                pop(),
                                swap3(),
                                pop(),
                            ]
                        }
                        _ => panic!("subtraction operator is not supported for {res_type}"),
                    };

                    let code = vec![lhs_expr_code, rhs_expr_code, sub_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_sub", &res_type);

                    (addr, code)
                }
            };

            (addr, code)
        }
        ast::Expr::If(ast::ExprIf {
            condition,
            then_branch,
            else_branch,
        }) => {
            let (_cond_addr, cond_code) =
                compile_expr(condition, "if_cond", &condition.get_type(), state);

            // Condition is handled immediately.
            state.vstack.pop();

            let branch_start_vstack = state.vstack.clone();
            let (then_addr, then_body_code) =
                compile_expr(then_branch, "then", &then_branch.get_type(), state);

            // Pop all vstack elements produced by `then_body`
            state.vstack = branch_start_vstack.clone();

            let (_else_addr, else_body_code) =
                compile_expr(else_branch, "else", &else_branch.get_type(), state);

            // Pop all vstack elements produced by `else_body`
            state.vstack = branch_start_vstack;

            let mut if_code = cond_code;

            // This naming should make it easier for the programmer to see which
            // subroutines belong to the same if/else expression
            let then_subroutine_name = format!("{then_addr}_then");
            let else_subroutine_name = format!("{then_addr}_else");
            if_code.append(&mut vec![
                push(1),                            // _ cond 1
                swap1(),                            // _ 1 cond
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

            let if_res_addr = state.new_value_identifier("if_then_else", &then_branch.get_type());

            // TODO: Clear `var_addr` if then/else branches did assignment; this is not currently possible in if-expr,
            // but it is possible in if-stmt. Either solve this when compiling if-stmt, or extend if-expr to allow for
            // multi-statement blocks.

            (if_res_addr, if_code)
        }

        ast::Expr::Cast(expr, as_type) => {
            let expr_type = expr.get_type();
            let (_expr_addr, expr_code) = compile_expr(expr, "as", &expr_type, state);

            match (expr_type, as_type) {
                (ast::DataType::U64, ast::DataType::U32) => {
                    // No value check is performed here
                    let (_, old_data_type) = state.vstack.pop().unwrap();

                    // sanity check
                    assert_eq!(ast::DataType::U64, old_data_type);

                    let addr = state.new_value_identifier("_as_u32", as_type);
                    let cast_code = vec![swap1(), pop()];

                    (addr, vec![expr_code, cast_code].concat())
                }
                (ast::DataType::U32, ast::DataType::U64) => {
                    let (_, old_data_type) = state.vstack.pop().unwrap();

                    // sanity check
                    assert_eq!(ast::DataType::U32, old_data_type);

                    let addr = state.new_value_identifier("_as_u64", as_type);
                    let cast_code = vec![push(0), swap1()];

                    (addr, vec![expr_code, cast_code].concat())
                }
                _ => todo!(),
            }
        }
    }
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
            swap3(), // _ b_lo a_lo b_hi a_hi
            eq(),    // _ b_lo a_lo (b_hi == a_hi)
            swap2(), // _ (b_hi == a_hi) a_lo b_lo
            eq(),    // _ (b_hi == a_hi) (a_lo == b_lo)
            mul(),   // _ (b_hi == a_hi && a_lo == b_lo)
        ],

        XFE => vec![
            // _ a_2 a_1 a_0 b_2 b_1 b_0
            swap4(), // _ a_2 b_0 a_0 b_2 b_1 a_1
            eq(),    // _ a_2 b_0 a_0 b_2 (b_1 == a_1)
            swap4(), // _ (b_1 == a_1) b_0 a_0 b_2 a_2
            eq(),    // _ (b_1 == a_1) b_0 a_0 (b_2 == a_2)
            swap2(), // _ (b_1 == a_1) (b_2 == a_2) a_0 b_0
            eq(),    // _ (b_1 == a_1) (b_2 == a_2) (a_0 == b_0)
            mul(),   // _ (b_1 == a_1) (b_2 == a_2)·(a_0 == b_0)
            mul(),   // _ (b_1 == a_1)·(b_2 == a_2)·(a_0 == b_0)
        ],
        Digest => {
            let eq_digest = state
                .import_snippet(Box::new(hashing::eq_digest::EqDigest))
                .to_string();
            vec![call(eq_digest)]
        }
        List(_) => todo!(),
        FlatList(_) => todo!(),
    }
}

pub fn size_of(data_type: &ast::DataType) -> usize {
    use ast::DataType::*;
    match data_type {
        Bool => 1,
        U32 => 1,
        U64 => 2,
        BFE => 1,
        XFE => 3,
        Digest => 5,
        List(_list_type) => 1,
        FlatList(tuple_type) => tuple_type.iter().map(size_of).sum(),
    }
}

/// Copy a value at a position on the stack to the top
fn dup_value_from_stack_code(
    position: Ord16,
    data_type: &ast::DataType,
) -> Vec<LabelledInstruction> {
    let elem_size = size_of(data_type);

    // the position of the deepest element of the value.
    let n: usize = Into::<usize>::into(position) + elem_size - 1;

    vec![Instruction(Dup(n.try_into().unwrap())); elem_size]
}
