use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use tasm_lib::arithmetic;
use tasm_lib::library::Library;
use tasm_lib::snippet::Snippet;
use triton_opcodes::instruction::{AnInstruction::*, LabelledInstruction::*};
use triton_opcodes::ord_n::Ord16;
use triton_opcodes::parser::{parse, to_labelled};
use triton_opcodes::shortcuts::*;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::ast;
use crate::stack::Stack;

#[derive(Debug, Default)]
pub struct CompilerState {
    pub counter: usize,

    // Where on stack is the variable placed?
    pub vstack: Stack<(ValueIdentifier, ast::DataType)>,

    // Mapping from variable name to its internal identifier
    pub var_addr: HashMap<String, ValueIdentifier>,

    // A library struct to keep check of which snippets are already in the namespace
    pub library: Library,
}

// TODO: Use this value from Triton-VM
const STACK_SIZE: usize = 16;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueIdentifier {
    name: String,
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

    fn import_snippet<S: Snippet>(&mut self) -> &'static str {
        self.library.import::<S>()
    }

    /// Return code that clears the stack but leaves the value that's on the top of the stack
    /// when this function is called.
    fn remove_all_but_top_stack_value(&mut self) -> Vec<LabelledInstruction> {
        let stack_height = self.get_stack_height();
        let top_element = self
            .vstack
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
            vec![vec![swap_instruction, Instruction(Pop)]; top_value_size].concat()
        } else {
            vec![]
        };

        // Clean up vstack
        while let Some(_elem) = self.vstack.pop() {}
        self.vstack.push(top_element);

        // Generate code to remove any remaining values from the stack
        vec![
            code,
            vec![Instruction(Pop); words_to_remove - top_value_size],
        ]
        .concat()
    }

    /// Return the code to overwrite a stack value with the value that's on top of the stack
    /// Note that the top value and the value to be removed *must* be of the same type.
    /// Updates the `vstack` but not the `var_addr` as this is assumed to be handled by the caller.
    fn overwrite_stack_value_with_same_data_type(
        &mut self,
        value_identifier_to_remove: &ValueIdentifier,
    ) -> Vec<LabelledInstruction> {
        let (_top_element_id, top_element_type) =
            self.vstack.pop().expect("vstack cannot be empty");
        let (stack_position_of_value_to_remove, type_to_remove) =
            self.find_stack_value(value_identifier_to_remove);

        assert_eq!(
            top_element_type, type_to_remove,
            "Top stack value and value to remove must match"
        );

        let value_size = size_of(&type_to_remove);

        // Remove the overwritten value from stack
        let code: Vec<LabelledInstruction> = vec![
            vec![
                Instruction(Swap(stack_position_of_value_to_remove)),
                Instruction(Pop)
            ];
            value_size
        ]
        .concat();

        // Remove the overwritten value from vstack
        self.vstack
            .remove_value(&(value_identifier_to_remove.to_owned(), type_to_remove));

        code
    }

    fn get_stack_height(&self) -> usize {
        self.vstack
            .inner
            .iter()
            .map(|(_, data_type)| size_of(data_type))
            .sum()
    }

    fn find_stack_value(&self, seek_addr: &ValueIdentifier) -> (Ord16, ast::DataType) {
        let mut position: usize = 0;
        for (found_addr, data_type) in self.vstack.inner.iter().rev() {
            if seek_addr == found_addr {
                return (
                    position
                        .try_into()
                        .expect("Found stack position must match a stack element"),
                    data_type.to_owned(),
                );
            }

            position += size_of(data_type);

            // By asserting after `+= size_of(data_type)`, we check that the deepest part
            // of the sought value is addressable, not just the top part of the value.
            assert!(position < STACK_SIZE, "Addressing beyond the {STACK_SIZE}'th stack element requires spilling and register-allocation.");
        }

        panic!("Cannot find {seek_addr} on vstack")
    }
}

pub fn compile(function: &ast::Fn<ast::Typing>) -> Vec<LabelledInstruction> {
    let fn_name = &function.name;
    let _fn_stack_input_sig = function.args.iter().map(|arg| format!("({arg})")).join(" ");
    let _fn_stack_output_sig = function
        .output
        .as_ref()
        .map(|data_type| format!("{data_type}"))
        .unwrap_or_default();

    let mut state = CompilerState::default();

    // TODO: Initialize vstack to reflect that the arguments are present on it.
    for arg in function.args.iter() {
        let fn_arg_addr = state.new_value_identifier("_fn_arg", &arg.data_type);
        state.var_addr.insert(arg.name.clone(), fn_arg_addr);
    }

    let fn_body_code = function
        .body
        .iter()
        .map(|stmt| compile_stmt(stmt, function, &mut state))
        .concat();

    // TODO: I wanted to use the result from `all_imports_as_instruction_lists`
    // here but I got a borrow-checker error. Probably bc of the &'static str
    // it returns.
    let dependencies = state.library.all_imports();
    let dependencies = parse(&dependencies)
        .map(|instructions| to_labelled(&instructions))
        .unwrap_or_else(|_| panic!("Must be able to parse dependencies code:\n{dependencies}"));

    let ret = vec![
        vec![Label(fn_name.to_owned())],
        fn_body_code,
        vec![Instruction(Return)],
        dependencies,
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
                let data_type = known_type.unwrap();
                let (expr_addr, expr_code) = compile_expr(expr, "assign", &data_type, state);
                let old_value_identifier = state
                    .var_addr
                    .insert(var_name.clone(), expr_addr.clone())
                    .expect("Value identifier must exist in var_addr");

                // Get code to overwrite old value, and update the compiler's vstack
                state.vstack.push((expr_addr, data_type));
                let overwrite_code =
                    state.overwrite_stack_value_with_same_data_type(&old_value_identifier);

                vec![expr_code, overwrite_code].concat()
            }
            ast::Identifier::TupleIndex(_, _) => todo!(),
            ast::Identifier::ListIndex(_, _) => todo!(),
        },

        // 'return;': Clean stack
        ast::Stmt::Return(None) => {
            let mut code = vec![];
            while let Some((_addr, data_type)) = state.vstack.pop() {
                code.push(vec![Instruction(Pop); size_of(&data_type)]);
            }

            code.concat()
        }

        ast::Stmt::Return(Some(ret_expr)) => {
            let ret_type = function.output.as_ref().expect("a return type");
            // special-case on returning variable, without unnecessary dup-instructions
            let expr_code = if let ast::Expr::Var(ast::Identifier::String(var_name, known_type)) =
                ret_expr
            {
                let data_type = known_type.unwrap();
                let var_addr = state.var_addr.get(var_name).expect("variable exists");
                let (position, old_data_type) = state.find_stack_value(var_addr);
                let mut stack_height = state.get_stack_height();

                // sanity check
                assert_eq!(old_data_type, data_type, "type must match expected type");

                // Pop everything prior to sought value
                let first_pop_code = vec![Instruction(Pop); position.into()];
                stack_height -= Into::<usize>::into(position);

                // Swap returned value to bottom of stack
                let data_type_size = size_of(&data_type);
                let swap_instr =
                    Instruction(Swap((stack_height - data_type_size).try_into().unwrap()));
                let swap_code = vec![vec![swap_instr, Instruction(Pop)]; data_type_size].concat();
                stack_height -= data_type_size;

                let last_pop_code = vec![Instruction(Pop); stack_height - data_type_size];

                vec![first_pop_code, swap_code, last_pop_code].concat()
            } else {
                let expr_code = compile_expr(ret_expr, "ret_expr", ret_type, state).1;

                // Remove all but top value from stack
                let remove_elements_code = state.remove_all_but_top_stack_value();
                vec![expr_code, remove_elements_code].concat()
            };

            expr_code
        }

        ast::Stmt::FnCall(_) => todo!(),
        ast::Stmt::While(_) => todo!(),
        ast::Stmt::If(_) => todo!(),
    }
}

fn compile_expr(
    expr: &ast::Expr<ast::Typing>,
    _context: &str,
    _data_type: &ast::DataType,
    state: &mut CompilerState,
) -> (ValueIdentifier, Vec<LabelledInstruction>) {
    match expr {
        ast::Expr::Lit(expr_lit, known_type) => {
            let data_type = known_type.unwrap();
            match expr_lit {
                ast::ExprLit::Bool(value) => {
                    let addr = state.new_value_identifier("_bool_lit", &data_type);
                    (
                        addr,
                        vec![Instruction(Push(BFieldElement::new(*value as u64)))],
                    )
                }

                ast::ExprLit::U32(value) => {
                    let addr = state.new_value_identifier("_u32_lit", &data_type);
                    (
                        addr,
                        vec![Instruction(Push(BFieldElement::new(*value as u64)))],
                    )
                }

                ast::ExprLit::BFE(value) => {
                    let addr = state.new_value_identifier("_bfe_lit", &data_type);
                    (addr, vec![Instruction(Push(*value))])
                }

                ast::ExprLit::U64(value) => {
                    let addr = state.new_value_identifier("_u64_lit", &data_type);
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
                let data_type = known_type.unwrap();
                let var_addr = state.var_addr.get(var_name).expect("variable exists");
                let (position, old_data_type) = state.find_stack_value(var_addr);

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
        ast::Expr::FnCall(_) => todo!(),
        ast::Expr::Binop(lhs_expr, binop, rhs_expr, known_type) => {
            let data_type = known_type.unwrap();

            // LHS is expected to be on the top of the stack, so we get RHS first
            let (_rhs_expr_addr, rhs_expr_code) =
                compile_expr(rhs_expr, "_binop_rhs", &rhs_expr.get_type(), state);
            let (_lhs_expr_addr, lhs_expr_code) =
                compile_expr(lhs_expr, "_binop_lhs", &lhs_expr.get_type(), state);

            let (addr, code) = match binop {
                ast::BinOp::Add => {
                    let add_code = match data_type {
                        ast::DataType::U32 => {
                            // We use the safe, overflow-checking, add code as default
                            let fn_name =
                                state.import_snippet::<arithmetic::u32::safe_add::SafeAdd>();
                            vec![Instruction(Call(fn_name.to_string()))]
                        }
                        ast::DataType::U64 => {
                            // We use the safe, overflow-checking, add code as default
                            let fn_name =
                                state.import_snippet::<arithmetic::u64::add_u64::AddU64>();
                            vec![Instruction(Call(fn_name.to_string()))]
                        }
                        ast::DataType::BFE => vec![Instruction(Add)],
                        ast::DataType::XFE => {
                            vec![
                                Instruction(XxAdd),
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Pop),
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Pop),
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Pop),
                            ]
                        }
                        _ => panic!("Operator add is not supported for type {data_type}"),
                    };

                    let code = vec![rhs_expr_code, lhs_expr_code, add_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_add", &data_type);
                    (addr, code)
                }
                ast::BinOp::And => {
                    let and_code = match data_type {
                        ast::DataType::Bool => {
                            vec![
                                Instruction(Add),
                                Instruction(Push(2u64.into())),
                                Instruction(Eq),
                            ]
                        }
                        _ => panic!("Logical AND operator is not supported for {data_type}"),
                    };

                    let code = vec![rhs_expr_code, lhs_expr_code, and_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_add", &data_type);
                    (addr, code)
                }
                ast::BinOp::BitAnd => {
                    let bitwise_and_code = match data_type {
                        ast::DataType::U32 => vec![Instruction(And)],
                        ast::DataType::U64 => {
                            let fn_name =
                                state.import_snippet::<arithmetic::u64::and_u64::AndU64>();
                            vec![Instruction(Call(fn_name.to_string()))]
                        }
                        _ => panic!("Logical AND operator is not supported for {data_type}"),
                    };

                    let code = vec![rhs_expr_code, lhs_expr_code, bitwise_and_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_add", &data_type);
                    (addr, code)
                }
                ast::BinOp::BitXor => todo!(),
                ast::BinOp::Div => todo!(),

                ast::BinOp::Eq => {
                    use ast::DataType::*;
                    let _code = match data_type {
                        Bool | U32 | BFE => vec![eq()],
                        U64 => vec![
                            // _ a_hi a_lo b_hi b_lo
                            swap3(), // _ b_lo a_lo b_hi a_hi
                            eq(),    // _ b_lo a_lo (b_hi == a_hi)
                            swap2(), // _ (b_hi == a_hi) a_lo b_lo
                            eq(),    // _ (b_hi == a_hi) (a_lo == b_lo)
                            mul(),   // _ (b_hi == a_hi && a_lo == b_lo)
                        ],

                        XFE => todo!(),
                        Digest => todo!(),
                        List(_) => todo!(),
                        FlatList(_) => todo!(),
                    };

                    todo!()
                }

                ast::BinOp::Lt => todo!(),
                ast::BinOp::Mul => todo!(),
                ast::BinOp::Neq => todo!(),
                ast::BinOp::Or => todo!(),
                ast::BinOp::Rem => todo!(),

                ast::BinOp::Shl => {
                    let lhs_expr_owned: ast::Expr<ast::Typing> = *(*lhs_expr).to_owned();

                    if !matches!(lhs_expr_owned, ast::Expr::Lit(ast::ExprLit::U64(1), _)) {
                        panic!("Unsupported shift left: {lhs_expr_owned:#?}")
                    }

                    let pow2_fn = state.import_snippet::<arithmetic::u64::pow2_u64::Pow2U64>();
                    let code = vec![Instruction(Call(pow2_fn.to_string()))];
                    let code = vec![rhs_expr_code, code].concat();

                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_shl", &data_type);

                    (addr, code)
                }

                ast::BinOp::Shr => todo!(),
                ast::BinOp::Sub => {
                    let sub_code: Vec<LabelledInstruction> = match data_type {
                        ast::DataType::U32 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let fn_name =
                                state.import_snippet::<arithmetic::u32::safe_sub::SafeSub>();
                            vec![Instruction(Call(fn_name.to_string()))]
                        }
                        ast::DataType::U64 => {
                            // As standard, we use safe arithmetic that crashes on overflow
                            let fn_name =
                                state.import_snippet::<arithmetic::u64::sub_u64::SubU64>();
                            vec![Instruction(Call(fn_name.to_string()))]
                        }
                        ast::DataType::BFE => {
                            vec![
                                Instruction(Swap(1u32.try_into().unwrap())),
                                Instruction(Push(-BFieldElement::one())),
                                Instruction(Mul),
                                Instruction(Add),
                            ]
                        }
                        ast::DataType::XFE => {
                            vec![
                                // flip the x operands
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Swap(1u32.try_into().unwrap())),
                                Instruction(Swap(4u32.try_into().unwrap())),
                                Instruction(Swap(1u32.try_into().unwrap())),
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Swap(5u32.try_into().unwrap())),
                                // multiply top element with -1
                                Instruction(Push(-BFieldElement::one())),
                                Instruction(XbMul),
                                // Perform (lhs - rhs)
                                Instruction(XxAdd),
                                // Get rid of the rhs, only leaving the result
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Pop),
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Pop),
                                Instruction(Swap(3u32.try_into().unwrap())),
                                Instruction(Pop),
                            ]
                        }
                        _ => panic!("subtraction operator is not supported for {data_type}"),
                    };

                    let code = vec![rhs_expr_code, lhs_expr_code, sub_code].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_binop_sub", &data_type);
                    (addr, code)
                }
            };

            // let code = vec![rhs_expr_code, lhs_expr_code, code].concat();
            // state.vstack.pop();
            // state.vstack.pop();
            // let addr = state.new_value_identifier(&format!("_binop_{binop:?}"), &data_type);

            (addr, code)
        }
        ast::Expr::If(_) => todo!(),

        ast::Expr::Cast(expr, as_type) => {
            let expr_type = expr.get_type();
            let (_expr_addr, expr_code) = compile_expr(expr, "as", &expr_type, state);

            match (expr_type, as_type) {
                (ast::DataType::U64, ast::DataType::U32) => {
                    // No value check is performed here
                    state.vstack.pop();
                    let addr = state.new_value_identifier("_as_u32", as_type);
                    let cast_code = vec![Instruction(Swap(Ord16::ST1)), Instruction(Pop)];

                    (addr, vec![expr_code, cast_code].concat())
                }
                _ => todo!(),
            }
        }
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
