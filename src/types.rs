use std::collections::HashMap;

use crate::ast;

pub fn annotate_fn(function: &mut ast::Fn<ast::Typing>) {
    // Populate vtable with function arguments
    let mut vtable: HashMap<String, ast::DataType> = function
        .args
        .iter()
        .map(|arg| (arg.name.clone(), arg.data_type.clone()))
        .collect();

    // Type-annotate each statement in-place
    function
        .body
        .iter_mut()
        .for_each(|stmt| annotate_stmt(stmt, &mut vtable, &function.name, &function.output));
}

fn annotate_stmt(
    stmt: &mut ast::Stmt<ast::Typing>,
    vtable: &mut HashMap<String, ast::DataType>,
    fn_name: &str,
    fn_output: &Option<ast::DataType>,
) {
    match stmt {
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
        }) => {
            if vtable.contains_key(var_name) {
                panic!("let-assign cannot shadow existing variable '{}'!", var_name);
            }

            let derived_type = derive_annotate_expr_type(expr, vtable);
            assert_type_equals(&derived_type, data_type, "let-statement");
            vtable.insert(var_name.clone(), data_type.clone());
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            let identifier_type = annotate_identifier_type(identifier, vtable);
            let expr_type = derive_annotate_expr_type(expr, vtable);
            assert_type_equals(&identifier_type, &expr_type, "assign-statement");
        }

        ast::Stmt::Return(opt_expr) => match (opt_expr, fn_output) {
            (None, None) => (),
            (None, Some(return_type)) => panic!(
                "Return without value; expect function {} to return {}",
                fn_name, return_type
            ),
            (Some(ret_expr), None) => {
                let expr_ret_type = derive_annotate_expr_type(ret_expr, vtable);
                panic!(
                    "Return with value; expect function {} to return nothing, but returns {}",
                    fn_name, expr_ret_type
                )
            }
            (Some(ret_expr), Some(fn_ret_type)) => {
                let expr_ret_type = derive_annotate_expr_type(ret_expr, vtable);
                assert_type_equals(fn_ret_type, &expr_ret_type, "return stmt");
            }
        },

        ast::Stmt::FnCall(ast::FnCall {
            name: _name,
            args,
            annot: _call_return_type,
        }) => {
            let _arg_types: Vec<ast::DataType> = args
                .iter_mut()
                .map(|arg_expr| derive_annotate_expr_type(arg_expr, vtable))
                .collect();

            // TODO: Check that function exists and that its input types match with args provided.
            // TODO: Type-check that functions not bound to variables don't return anything
        }

        ast::Stmt::While(ast::WhileStmt { condition, stmts }) => {
            let condition_type = derive_annotate_expr_type(condition, vtable);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "while-condition");

            stmts
                .iter_mut()
                .for_each(|stmt| annotate_stmt(stmt, vtable, fn_name, fn_output));
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            if_branch,
            else_branch,
        }) => {
            let condition_type = derive_annotate_expr_type(condition, vtable);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "if-condition");

            // TODO: By sharing 'vtable' between branches, you cannot have the same 'let'
            // statement in each branch, since type-checking the 'let' in the else-branch
            // would trigger "let-assign cannot shadow existing variable".

            if_branch
                .iter_mut()
                .for_each(|stmt| annotate_stmt(stmt, vtable, fn_name, fn_output));

            else_branch
                .iter_mut()
                .for_each(|stmt| annotate_stmt(stmt, vtable, fn_name, fn_output));
        }
    }
}

fn assert_type_equals(derived_type: &ast::DataType, data_type: &ast::DataType, context: &str) {
    if derived_type != data_type {
        panic!(
            "Type mismatch between type '{}' and derived type '{}' for {}",
            data_type, derived_type, context
        );
    }
}

fn annotate_identifier_type(
    identifier: &mut ast::Identifier<ast::Typing>,
    vtable: &mut HashMap<String, ast::DataType>,
) -> ast::DataType {
    match identifier {
        // x
        ast::Identifier::String(var_name, var_type) => {
            let found_type = vtable.get(var_name).expect("variable must exist");
            *var_type = ast::Typing::KnownType(found_type.clone());
            found_type.clone()
        }

        // x.0
        ast::Identifier::TupleIndex(tuple_identifier, index) => {
            // For syntax like 'x.0.1', find the type of 'x.0'; we don't currently
            // generate ASTs with nested tuple indexing, but it's easier to solve
            // the type inference generally here.
            let tuple_type = annotate_identifier_type(tuple_identifier, vtable);

            if let ast::DataType::FlatList(elem_types) = tuple_type {
                if elem_types.len() < *index {
                    panic!(
                        "Cannot index tuple of {} elements with index {}",
                        elem_types.len(),
                        index
                    );
                }

                elem_types[*index].clone()
            } else {
                panic!("Cannot index non-tuple with tuple index {}", index);
            }
        }

        // x[e]
        ast::Identifier::ListIndex(list_identifier, index_expr) => {
            let index_type = derive_annotate_expr_type(index_expr, vtable);
            if !is_index_type(&index_type) {
                panic!("Cannot index list with type '{}'", index_type);
            }

            // TODO: It could make sense to support var.1[i] if there were better support for tuples.
            if !is_string_identifier(list_identifier) {
                panic!("Cannot index anything but variables: {:?}", list_identifier);
            }

            let list_type = annotate_identifier_type(list_identifier, vtable);
            if !is_primitive_type(&list_type) {
                panic!("Cannot index list of type '{}", list_type);
            }

            list_type
        }
    }
}

fn derive_annotate_expr_type(
    expr: &mut ast::Expr<ast::Typing>,
    vtable: &mut HashMap<String, ast::DataType>,
) -> ast::DataType {
    let bool_type = ast::DataType::U32;

    match expr {
        ast::Expr::Lit(expr_lit, var_type) => {
            let found_type = expr_lit_type(expr_lit);
            *var_type = ast::Typing::KnownType(found_type.clone());
            found_type
        }

        ast::Expr::Var(identifier) => annotate_identifier_type(identifier, vtable),

        ast::Expr::FlatList(tuple_exprs) => {
            let tuple_types: Vec<ast::DataType> = tuple_exprs
                .iter_mut()
                .map(|expr| derive_annotate_expr_type(expr, vtable))
                .collect();
            ast::DataType::FlatList(tuple_types)
        }

        ast::Expr::FnCall(ast::FnCall {
            name,
            args,
            annot: _call_return_type,
        }) => {
            // TODO: Check that function exists and that its input types match with args provided.
            // TODO: Cannot annotate return type when there's no function table.
            let _arg_types: Vec<ast::DataType> = args
                .iter_mut()
                .map(|arg_expr| derive_annotate_expr_type(arg_expr, vtable))
                .collect();

            panic!("TODO: Don't know what type of value '{}' returns!", name)
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, binop_type) => {
            let lhs_type = derive_annotate_expr_type(lhs_expr, vtable);
            let rhs_type = derive_annotate_expr_type(rhs_expr, vtable);

            use ast::BinOp::*;
            match binop {
                // Overloaded for all arithmetic types.
                Add => {
                    assert_type_equals(&lhs_type, &rhs_type, "add-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot add non-arithmetic type '{}'",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Restricted to bool only.
                And => {
                    assert_type_equals(&lhs_type, &bool_type, "and-expr lhs");
                    assert_type_equals(&rhs_type, &bool_type, "and-expr rhs");
                    *binop_type = ast::Typing::KnownType(bool_type.clone());
                    bool_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitAnd => {
                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-and-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-and type '{}' (not u32-based)",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitXor => {
                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-xor-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-xor type '{}' (not u32-based)",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all arithmetic types.
                Div => {
                    assert_type_equals(&lhs_type, &rhs_type, "div-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot divide non-arithmetic type '{}'",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all primitive types.
                Eq => {
                    assert_type_equals(&lhs_type, &rhs_type, "eq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare non-primitive type '{}' for equality",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(bool_type.clone());
                    bool_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Lt => {
                    assert_type_equals(&lhs_type, &rhs_type, "lt-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot compare type '{}' with less-than (not u32-based)",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(bool_type.clone());
                    bool_type
                }

                // Overloaded for all primitive types.
                Mul => {
                    assert_type_equals(&lhs_type, &rhs_type, "mul-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot multiply non-arithmetic type '{}'",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all primitive types.
                Neq => {
                    assert_type_equals(&lhs_type, &rhs_type, "neq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare type '{}' with not-equal (not primitive)",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(bool_type.clone());
                    bool_type
                }

                // Restricted to bool only.
                Or => {
                    assert_type_equals(&lhs_type, &bool_type, "or-expr lhs");
                    assert_type_equals(&rhs_type, &bool_type, "or-expr rhs");
                    *binop_type = ast::Typing::KnownType(bool_type.clone());
                    bool_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Rem => {
                    assert_type_equals(&lhs_type, &rhs_type, "rem-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot find remainder for type '{}' (not u32-based)",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());

                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shl => {
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-left for type '{}' (not u32-based)",
                        lhs_type
                    );
                    assert_type_equals(&rhs_type, &ast::DataType::U32, "shl-rhs-expr");
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());

                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shr => {
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-right for type '{}' (not u32-based)",
                        lhs_type
                    );
                    assert_type_equals(&rhs_type, &ast::DataType::U32, "shr-rhs-expr");
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());

                    lhs_type
                }

                // Overloaded for all arithmetic types.
                Sub => {
                    assert_type_equals(&lhs_type, &rhs_type, "sub-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot subtract non-arithmetic type '{}'",
                        lhs_type
                    );
                    *binop_type = ast::Typing::KnownType(lhs_type.clone());
                    lhs_type
                }
            }
        }

        ast::Expr::If(ast::ExprIf {
            condition,
            then_branch,
            else_branch,
        }) => {
            let condition_type = derive_annotate_expr_type(condition, vtable);
            assert_type_equals(&condition_type, &bool_type, "if-condition-expr");

            let then_type = derive_annotate_expr_type(then_branch, vtable);
            let else_type = derive_annotate_expr_type(else_branch, vtable);
            assert_type_equals(&then_type, &else_type, "if-then-else-expr");

            then_type
        }
    }
}

pub fn expr_lit_type(expr_lit: &ast::ExprLit) -> ast::DataType {
    match expr_lit {
        ast::ExprLit::Bool(_) => ast::DataType::Bool,
        ast::ExprLit::U32(_) => ast::DataType::U32,
        ast::ExprLit::U64(_) => ast::DataType::U64,
        ast::ExprLit::BFE(_) => ast::DataType::BFE,
        ast::ExprLit::XFE(_) => ast::DataType::XFE,
        ast::ExprLit::Digest(_) => ast::DataType::Digest,
    }
}

fn is_string_identifier<T>(identifier: &ast::Identifier<T>) -> bool {
    matches!(identifier, ast::Identifier::String(_, _))
}

/// A type that can be used as address in `read_mem` and `write_mem` calls.
///
/// Since memory addresses are essentially `BFieldElement`s, only types
/// that are subsets of `BFE`s can be used. The only such type is `U32`.
fn is_index_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, U32 | BFE)
}

/// A type for which basic arithmetic operators can be overloaded.
///
/// Note that not all operators work for all arithmetic types.
///
/// E.g. the bitwise operators only work for `is_u32_based_type()`.
fn is_arithmetic_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, U32 | U64 | BFE | XFE)
}

/// A type that is implemented in terms of `U32` values.
///
/// E.g. `U32` and `U64`.
fn is_u32_based_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, U32 | U64)
}

/// A non-composite fixed-length type.
fn is_primitive_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, Bool | U32 | U64 | BFE | XFE | Digest)
}
