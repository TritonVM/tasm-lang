use std::collections::HashMap;

use itertools::Itertools;
use triton_opcodes::instruction::{AnInstruction, LabelledInstruction};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::ast;
use crate::stack::Stack;

#[derive(Debug, Default)]
pub struct CompilerState {
    pub counter: usize,
    pub vstack: Stack<(Address, ast::DataType)>,
    pub var_addr: HashMap<String, Address>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Address {
    name: String,
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl CompilerState {
    pub fn new_address(&mut self, prefix: &str, data_type: &ast::DataType) -> Address {
        let name = format!("_{}_{}_{}", prefix, data_type, self.counter);
        let address = Address { name };
        self.vstack.push((address.clone(), data_type.clone()));
        self.counter += 1;
        address
    }

    fn find_stack_position(&self, seek_addr: &Address) -> usize {
        let mut position: usize = 0;
        for (found_addr, data_type) in self.vstack.inner.iter().rev() {
            if seek_addr == found_addr {
                return position;
            }

            position += size_of(data_type);

            // By asserting after `+= size_of(data_type)`, we check that the deepest part
            // of the sought value is addressable, not just the top part of the value.
            const STACK_SIZE: usize = 16;
            assert!(position < STACK_SIZE, "Addressing beyond the {}'th stack element requires spilling and register-allocation.", STACK_SIZE);
        }

        panic!("Cannot find {} on vstack", seek_addr)
    }
}

pub fn compile(function: &ast::Fn<ast::Typing>) -> Vec<LabelledInstruction<'static>> {
    let fn_name = &function.name;
    let _fn_stack_input_sig = function.args.iter().map(|arg| format!("({arg})")).join(" ");
    let _fn_stack_output_sig = function
        .output
        .as_ref()
        .map(|data_type| format!("{}", data_type))
        .unwrap_or_default();

    let mut state = CompilerState::default();

    // TODO: Initialize vstack to reflect that the arguments are present on it.
    for arg in function.args.iter() {
        let fn_arg_addr = state.new_address("_fn_arg", &arg.data_type);
        state.var_addr.insert(arg.name.clone(), fn_arg_addr);
    }

    let fn_body_code = function
        .body
        .iter()
        .map(|stmt| compile_stmt(stmt, function, &mut state))
        .concat();

    vec![
        vec![LabelledInstruction::Label(fn_name.to_owned(), "")],
        fn_body_code,
    ]
    .concat()
}

fn compile_stmt(
    stmt: &ast::Stmt<ast::Typing>,
    function: &ast::Fn<ast::Typing>,
    state: &mut CompilerState,
) -> Vec<LabelledInstruction<'static>> {
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

        ast::Stmt::Assign(_) => todo!(),

        // 'return;': Clean stack
        ast::Stmt::Return(None) => {
            let mut code = vec![];
            while let Some((_addr, data_type)) = state.vstack.pop() {
                code.push(vec![
                    LabelledInstruction::Instruction(AnInstruction::Pop, "",);
                    size_of(&data_type)
                ]);
            }

            code.concat()
        }

        // 'return <expr>;': Reorder and clean stack.
        ast::Stmt::Return(Some(ret_expr)) => {
            let ret_type = function.output.as_ref().expect("a return type");
            let (_expr_addr, expr_code) = compile_expr(ret_expr, "ret_expr", ret_type, state);

            // TODO: Clean up vstack
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
) -> (Address, Vec<LabelledInstruction<'static>>) {
    match expr {
        ast::Expr::Lit(expr_lit, known_type) => {
            let data_type = known_type.unwrap();
            match expr_lit {
                ast::ExprLit::Bool(value) => {
                    let addr = state.new_address("_bool_lit", &data_type);
                    (
                        addr,
                        vec![LabelledInstruction::Instruction(
                            AnInstruction::Push(BFieldElement::new(*value as u64)),
                            "",
                        )],
                    )
                }

                ast::ExprLit::U32(value) => {
                    let addr = state.new_address("_u32_lit", &data_type);
                    (
                        addr,
                        vec![LabelledInstruction::Instruction(
                            AnInstruction::Push(BFieldElement::new(*value as u64)),
                            "",
                        )],
                    )
                }

                ast::ExprLit::BFE(value) => {
                    let addr = state.new_address("_bfe_lit", &data_type);
                    (
                        addr,
                        vec![LabelledInstruction::Instruction(
                            AnInstruction::Push(*value),
                            "",
                        )],
                    )
                }

                ast::ExprLit::U64(value) => {
                    let addr = state.new_address("_u64_lit", &data_type);
                    let as_u32s = U32s::<2>::try_from(*value).unwrap().to_sequence();
                    let stack_serialized: Vec<_> = as_u32s.iter().rev().collect();

                    let code = stack_serialized
                        .iter()
                        .map(|bfe| LabelledInstruction::Instruction(AnInstruction::Push(**bfe), ""))
                        .collect_vec();

                    (addr, code)
                }

                ast::ExprLit::XFE(_) => todo!(),
                ast::ExprLit::Digest(_) => todo!(),
            }
        }

        ast::Expr::Var(identifier) => {
            //
            match identifier {
                ast::Identifier::String(var_name, known_type) => {
                    let data_type = known_type.unwrap();
                    let var_addr = state.var_addr.get(var_name).expect("variable exists");
                    let position = state.find_stack_position(var_addr);
                    let var_copy_code = dup_value_from_stack_code(position, &data_type);
                    let var_copy_addr = state.new_address("_var_copy", &data_type);

                    (var_copy_addr, var_copy_code)
                }
                ast::Identifier::TupleIndex(_, _) => todo!(),
                ast::Identifier::ListIndex(_, _) => todo!(),
            }
        }

        ast::Expr::FlatList(_) => todo!(),
        ast::Expr::FnCall(_) => todo!(),
        ast::Expr::Binop(lhs_expr, binop, rhs_expr, known_type) => {
            let data_type = known_type.unwrap();

            // FIXME: '&data_type' below is wrong; it is the operand's type, not the binop's.
            let (_lhs_expr_addr, lhs_expr_code) =
                compile_expr(lhs_expr, "_binop_lhs", &data_type, state);

            let (_rhs_expr_addr, rhs_expr_code) =
                compile_expr(rhs_expr, "_binop_rhs", &data_type, state);

            match binop {
                ast::BinOp::Add => {
                    // FIXME: Don't assume u32/bfe.
                    let add_code = vec![
                        lhs_expr_code,
                        rhs_expr_code,
                        vec![LabelledInstruction::Instruction(AnInstruction::Add, "")],
                    ]
                    .concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let add_addr = state.new_address("_add_result", &data_type);

                    (add_addr, add_code)
                }
                ast::BinOp::And => todo!(),
                ast::BinOp::BitAnd => todo!(),
                ast::BinOp::BitXor => todo!(),
                ast::BinOp::Div => todo!(),
                ast::BinOp::Eq => todo!(),
                ast::BinOp::Lt => todo!(),
                ast::BinOp::Mul => todo!(),
                ast::BinOp::Neq => todo!(),
                ast::BinOp::Or => todo!(),
                ast::BinOp::Rem => todo!(),
                ast::BinOp::Shl => todo!(),
                ast::BinOp::Shr => todo!(),
                ast::BinOp::Sub => todo!(),
            }
        }
        ast::Expr::If(_) => todo!(),
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

fn dup_value_from_stack_code(
    position: usize,
    data_type: &ast::DataType,
) -> Vec<LabelledInstruction<'static>> {
    let elem_size = size_of(data_type);

    // the position of the deepest element of the value.
    let n = position + elem_size - 1;

    vec![LabelledInstruction::Instruction(
        AnInstruction::Dup(n.try_into().unwrap()),
        "",
    )]
}
