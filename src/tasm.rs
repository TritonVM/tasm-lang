use std::collections::HashMap;

use itertools::Itertools;
use triton_opcodes::instruction::{AnInstruction::*, LabelledInstruction::*};
use triton_opcodes::ord_n::Ord16;
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
}

// TODO: Use this value from Triton-VM
const STACK_SIZE: usize = 16;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueIdentifier {
    name: String,
}

use triton_opcodes::instruction::LabelledInstruction;
type Labeled = LabelledInstruction<'static>;

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

    /// Return code that clears the stack but leaves the value that's on the top of the stack
    /// when this function is called.
    fn remove_all_but_top_stack_value(&mut self) -> Vec<Labeled> {
        let top_element = self
            .vstack
            .pop()
            .expect("Cannot remove all but top element from an empty stack");
        let top_value_size = size_of(&top_element.1);
        let stack_height = self.get_stack_height();
        assert!(
            stack_height < STACK_SIZE,
            "For now, we only support functions with max {} elements on the stack",
            STACK_SIZE
        );

        let words_to_remove = stack_height - top_value_size;
        let swap_instruction = Instruction(Swap(words_to_remove.try_into().unwrap()), "");
        let code = vec![vec![swap_instruction, Instruction(Pop, "")]; top_value_size].concat();

        // Clean up vstack
        while let Some(_elem) = self.vstack.pop() {}
        self.vstack.push(top_element);

        vec![code, vec![Instruction(Pop, ""); words_to_remove]].concat()
    }

    /// Return the code to overwrite a stack value with the value that's on top of the stack
    /// Note that the top value and the value to be removed *must* be of the same type.
    /// Updates the `vstack` but not the `var_addr` as this is assumed to be handled by the caller.
    fn overwrite_stack_value_with_same_data_type(
        &mut self,
        value_identifier_to_remove: &ValueIdentifier,
    ) -> Vec<Labeled> {
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
        let code: Vec<Labeled> = vec![
            vec![
                Instruction(Swap(stack_position_of_value_to_remove), ""),
                Instruction(Pop, "")
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
            assert!(position < STACK_SIZE, "Addressing beyond the {}'th stack element requires spilling and register-allocation.", STACK_SIZE);
        }

        panic!("Cannot find {} on vstack", seek_addr)
    }
}

pub fn compile(function: &ast::Fn<ast::Typing>) -> Vec<Labeled> {
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
        let fn_arg_addr = state.new_value_identifier("_fn_arg", &arg.data_type);
        state.var_addr.insert(arg.name.clone(), fn_arg_addr);
    }

    let fn_body_code = function
        .body
        .iter()
        .map(|stmt| compile_stmt(stmt, function, &mut state))
        .concat();

    vec![
        vec![Label(fn_name.to_owned(), "")],
        fn_body_code,
        vec![Instruction(Return, "")],
    ]
    .concat()
}

fn compile_stmt(
    stmt: &ast::Stmt<ast::Typing>,
    function: &ast::Fn<ast::Typing>,
    state: &mut CompilerState,
) -> Vec<Labeled> {
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
                code.push(vec![Instruction(Pop, "",); size_of(&data_type)]);
            }

            code.concat()
        }

        ast::Stmt::Return(Some(ret_expr)) => {
            let ret_type = function.output.as_ref().expect("a return type");
            // special-case on returning variable, without unnecessary dup-instructions
            let expr_code =
                if let ast::Expr::Var(ast::Identifier::String(var_name, known_type)) = ret_expr {
                    let data_type = known_type.unwrap();
                    let var_addr = state.var_addr.get(var_name).expect("variable exists");
                    let (position, old_data_type) = state.find_stack_value(var_addr);
                    let mut stack_height = state.get_stack_height();

                    // sanity check
                    assert_eq!(old_data_type, data_type, "type must match expected type");

                    // Pop everything prior to sought value
                    let first_pop_code = vec![Instruction(Pop, ""); position.into()];
                    stack_height -= Into::<usize>::into(position);

                    // Swap returned value to bottom of stack
                    let data_type_size = size_of(&data_type);
                    let swap_instr = Instruction(
                        Swap((stack_height - data_type_size).try_into().unwrap()),
                        "",
                    );
                    let swap_code =
                        vec![vec![swap_instr, Instruction(Pop, "")]; data_type_size].concat();
                    stack_height -= data_type_size;

                    let last_pop_code = vec![Instruction(Pop, ""); stack_height - data_type_size];

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
) -> (ValueIdentifier, Vec<Labeled>) {
    match expr {
        ast::Expr::Lit(expr_lit, known_type) => {
            let data_type = known_type.unwrap();
            match expr_lit {
                ast::ExprLit::Bool(value) => {
                    let addr = state.new_value_identifier("_bool_lit", &data_type);
                    (
                        addr,
                        vec![Instruction(Push(BFieldElement::new(*value as u64)), "")],
                    )
                }

                ast::ExprLit::U32(value) => {
                    let addr = state.new_value_identifier("_u32_lit", &data_type);
                    (
                        addr,
                        vec![Instruction(Push(BFieldElement::new(*value as u64)), "")],
                    )
                }

                ast::ExprLit::BFE(value) => {
                    let addr = state.new_value_identifier("_bfe_lit", &data_type);
                    (addr, vec![Instruction(Push(*value), "")])
                }

                ast::ExprLit::U64(value) => {
                    let addr = state.new_value_identifier("_u64_lit", &data_type);
                    let as_u32s = U32s::<2>::try_from(*value).unwrap().to_sequence();
                    let stack_serialized: Vec<_> = as_u32s.iter().rev().collect();

                    let code = stack_serialized
                        .iter()
                        .map(|bfe| Instruction(Push(**bfe), ""))
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

            let (_lhs_expr_addr, lhs_expr_code) =
                compile_expr(lhs_expr, "_binop_lhs", &lhs_expr.get_type(), state);

            let (_rhs_expr_addr, rhs_expr_code) =
                compile_expr(rhs_expr, "_binop_rhs", &rhs_expr.get_type(), state);

            match binop {
                ast::BinOp::Add => {
                    // FIXME: Don't assume u32/bfe.
                    let add_code =
                        vec![lhs_expr_code, rhs_expr_code, vec![Instruction(Add, "")]].concat();
                    state.vstack.pop();
                    state.vstack.pop();
                    let add_addr = state.new_value_identifier("_add_result", &data_type);

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

fn dup_value_from_stack_code(position: Ord16, data_type: &ast::DataType) -> Vec<Labeled> {
    let elem_size = size_of(data_type);

    // the position of the deepest element of the value.
    let n: usize = Into::<usize>::into(position) + elem_size - 1;

    vec![Instruction(Dup(n.try_into().unwrap()), "")]
}
