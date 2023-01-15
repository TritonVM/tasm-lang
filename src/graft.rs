use crate::ast;
use itertools::Itertools;
use syn::ExprCall;

fn rust_type_path_to_data_type(rust_type_path: &syn::TypePath) -> ast::DataType {
    assert_eq!(
        1,
        rust_type_path.path.segments.len(),
        "Length other than one not supported"
    );
    rust_type_path.path.segments[0]
        .ident
        .to_string()
        .parse::<ast::DataType>()
        .expect("a valid DataType")
}

fn rust_type_to_data_type(x: &syn::Type) -> ast::DataType {
    match x {
        syn::Type::Path(data_type) => rust_type_path_to_data_type(data_type),
        ty => panic!("Unsupported type {ty:#?}"),
    }
}

fn pat_type_to_data_type(rust_type_path: &syn::PatType) -> ast::DataType {
    match rust_type_path.ty.as_ref() {
        syn::Type::Path(path) => rust_type_path_to_data_type(path),
        other_type => panic!("Unsupported {other_type:#?}"),
    }
}

fn pat_to_name(pat: &syn::Pat) -> String {
    let name = match pat {
        syn::Pat::Ident(ident) => ident.ident.to_string(),
        other => panic!("unsupported: {other:?}"),
    };
    name
}

fn path_to_ident(path: &syn::Path) -> String {
    // We just join identifiers with `::` to get the full function name / identifier name
    let identifiers: Vec<String> = path.segments.iter().map(|x| x.ident.to_string()).collect();
    identifiers.join("::")
}

fn graft_fn_arg(rust_fn_arg: &syn::FnArg) -> ast::FnArg {
    match rust_fn_arg {
        syn::FnArg::Typed(pat_type) => {
            let name = pat_to_name(&pat_type.pat);
            let data_type: ast::DataType = match pat_type.ty.as_ref() {
                syn::Type::Path(type_path) => rust_type_path_to_data_type(type_path),
                other => panic!("unsupported: {other:?}"),
            };

            ast::FnArg { name, data_type }
        }
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_return_type(rust_return_type: &syn::ReturnType) -> Vec<ast::DataType> {
    let ret_type = match rust_return_type {
        syn::ReturnType::Type(_, path) => match path.as_ref() {
            syn::Type::Path(type_path) => vec![rust_type_path_to_data_type(type_path)],
            syn::Type::Tuple(tuple_type) => {
                let tuple_type = tuple_type;
                let elements = tuple_type
                    .elems
                    .iter()
                    .map(rust_type_to_data_type)
                    .collect_vec();

                elements
            }
            _ => panic!("unsupported: {path:?}"),
        },
        other => panic!("unsupported: {other:?}"),
    };

    ret_type
}

// TODO: Consider moving this to the `ast` file and implement it as a conversion function
fn graft_call_exp(expr_call: &ExprCall) -> ast::FnCall {
    let fun_name: String = match expr_call.func.as_ref() {
        syn::Expr::Path(path) => path_to_ident(&path.path),
        other => panic!("unsupported: {other:?}"),
    };
    let args: Vec<ast::Expr> = expr_call.args.iter().map(|x| graft_expr(x)).collect_vec();

    ast::FnCall {
        name: fun_name,
        args,
    }
}

pub fn graft_expr(rust_exp: &syn::Expr) -> ast::Expr {
    match rust_exp {
        syn::Expr::Binary(bin_expr) => {
            let left = graft_expr(&bin_expr.left);
            let ast_binop: ast::BinOperator = bin_expr.op.into();
            let right = graft_expr(&bin_expr.right);

            ast::Expr::Binop(Box::new(left), ast_binop, Box::new(right))
        }
        syn::Expr::Path(path) => {
            let path = &path.path;
            let ident: String = path_to_ident(path);
            ast::Expr::Var(ident)
        }
        syn::Expr::Tuple(tuple_expr) => {
            let exprs = tuple_expr.elems.iter().map(|x| graft_expr(x)).collect_vec();
            ast::Expr::FlatList(exprs)
        }
        syn::Expr::Lit(litexp) => {
            let lit = &litexp.lit;
            ast::Expr::Lit(lit.into())
        }
        syn::Expr::Call(call_exp) => ast::Expr::FnCall(graft_call_exp(call_exp)),
        syn::Expr::Paren(paren_exp) => {
            // I *think* this is sufficient to handle parentheses correctly
            let a = graft_expr(&paren_exp.expr);

            a
        }
        syn::Expr::If(expr_if) => {
            let condition = graft_expr(&expr_if.cond);
            let if_branch = &expr_if.then_branch.stmts;
            assert_eq!(1, if_branch.len(), "Max one line in if/else expressions");
            let then_branch = match &if_branch[0] {
                syn::Stmt::Expr(expr) => graft_expr(&expr),
                other => panic!("unsupported: {other:?}"),
            };
            let else_branch = &expr_if.else_branch.as_ref().unwrap().1;
            let else_branch = match else_branch.as_ref() {
                syn::Expr::Block(block) => {
                    let else_branch = &block.block.stmts;
                    assert_eq!(1, else_branch.len(), "Max one line in if/else expressions");
                    let else_branch = match &else_branch[0] {
                        syn::Stmt::Expr(expr) => graft_expr(&expr),
                        other => panic!("unsupported: {other:?}"),
                    };
                    else_branch
                }
                other => panic!("unsupported: {other:?}"),
            };

            ast::Expr::If(ast::ExprIf {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            })
        }
        other => panic!("unsupported: {other:?}"),
    }
}

pub fn graft_stmt(rust_stmt: &syn::Stmt) -> ast::Stmt {
    match rust_stmt {
        syn::Stmt::Local(local) => {
            let (ident, data_type): (String, ast::DataType) = match &local.pat {
                syn::Pat::Type(pat_type) => {
                    let dt: ast::DataType = pat_type_to_data_type(pat_type);
                    let ident: String = pat_to_name(&pat_type.pat);

                    (ident, dt)
                }
                syn::Pat::Ident(d) => {
                    // This would indicate that the explicit type is missing
                    let ident = d.ident.to_string();
                    panic!("Missing type parameter in declaration of {ident}");
                }
                other => panic!("unsupported: {other:?}"),
            };

            let init = local.init.as_ref().unwrap();
            let init_expr = init.1.as_ref();
            let ast_expt = graft_expr(init_expr);
            let let_stmt = ast::LetStmt {
                var_name: ident,
                data_type,
                expr: ast_expt,
            };
            ast::Stmt::Let(let_stmt)
        }
        syn::Stmt::Item(_) => todo!(),
        syn::Stmt::Expr(expr) => match expr {
            syn::Expr::While(while_stmt) => {
                let expr_while = while_stmt;
                let while_condition = graft_expr(&expr_while.cond);
                let while_stmts: Vec<ast::Stmt> = while_stmt
                    .body
                    .stmts
                    .iter()
                    .map(|x| graft_stmt(x))
                    .collect_vec();

                let while_stmt = ast::WhileStmt {
                    condition: while_condition,
                    stmts: while_stmts,
                };
                ast::Stmt::While(while_stmt)
            }
            syn::Expr::If(if_expr) => {
                let if_condition = graft_expr(&if_expr.cond);
                let if_stmts: Vec<ast::Stmt> = if_expr
                    .then_branch
                    .stmts
                    .iter()
                    .map(|x| graft_stmt(x))
                    .collect_vec();
                let else_stmts: Vec<ast::Stmt> = match if_expr.else_branch.as_ref() {
                    Some(else_stmts) => match else_stmts.1.as_ref() {
                        syn::Expr::Block(block) => {
                            block.block.stmts.iter().map(|x| graft_stmt(x)).collect()
                        }
                        other => panic!("unsupported: {other:?}"),
                    },
                    None => vec![],
                };

                let if_stmt = ast::IfStmt {
                    condition: if_condition,
                    if_branch: if_stmts,
                    else_branch: else_stmts,
                };
                ast::Stmt::If(if_stmt)
            }
            other => panic!("unsupported: {other:?}"),
        },
        syn::Stmt::Semi(semi, _b) => match semi {
            syn::Expr::Return(ret) => {
                // Handle a return statement
                let a = ret.expr.as_ref().unwrap();
                let b = graft_expr(a);

                ast::Stmt::Return(b)
            }
            syn::Expr::Call(call_exp) => {
                // Handle a function call that's not an assignment or a return expression
                let ast_fn_call = graft_call_exp(call_exp);

                ast::Stmt::FnCall(ast_fn_call)
            }
            syn::Expr::Assign(assign) => {
                let identifier = assign.left.as_ref();
                let identifier = match identifier {
                    syn::Expr::Path(path) => path_to_ident(&path.path),
                    other => panic!("unsupported: {other:?}"),
                };
                let assign_expr: ast::Expr = graft_expr(assign.right.as_ref());
                let assign_stmt: ast::AssignStmt = ast::AssignStmt {
                    var_name: identifier,
                    expr: assign_expr,
                };

                ast::Stmt::Assign(assign_stmt)
            }
            other => panic!("unsupported: {other:?}"),
        },
    }
}

pub fn graft(input: &syn::ItemFn) -> ast::Fn {
    let function_name = input.sig.ident.to_string();
    let fn_arguments = input.sig.inputs.iter().map(graft_fn_arg).collect_vec();
    let output_values = graft_return_type(&input.sig.output);

    let body = input
        .block
        .stmts
        .iter()
        .map(|stmt| graft_stmt(stmt))
        .collect_vec();
    let ret = ast::Fn {
        name: function_name,
        args: fn_arguments,
        body, // TODO: Implement!
        output: output_values,
    };

    ret
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn leaf_count_to_node_count() {
        let tokens: syn::Item = parse_quote! {
                fn leaf_count_to_node_count(leaf_count: u64) -> u64 {
                    if leaf_count == 0u64 {
                        return 0u64;
                    }

                    let rightmost_leaf_leaf_index: u64 = leaf_count - 1u64;
                    let non_leaf_nodes_left: u64 = non_leaf_nodes_left(rightmost_leaf_leaf_index);
                    let node_index_of_rightmost_leaf: u64 = leaf_index_to_node_index(rightmost_leaf_leaf_index);

                    let mut non_leaf_nodes_after: u64 = 0u64;
                    let mut node_index: u64 = node_index_of_rightmost_leaf;
                    let mut right_count: u64 = right_lineage_length(node_index);
                    while right_count != 0u64 {
                        non_leaf_nodes_after = non_leaf_nodes_after + 1u64;
                        // go to parent (parent of right child has node index plus 1)
                        node_index = node_index + 1u64;
                        right_count = right_count - 1u64;
                    }

                    // Number of nodes is: non-leafs after, non-leafs before, and leaf count
                    return non_leaf_nodes_after + non_leaf_nodes_left + leaf_count;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn right_lineage_length() {
        let tokens: syn::Item = parse_quote! {
            fn right_lineage_length(node_index: u64) -> u64 {
                let bit_width: u64 = u64::BITS - u64::leading_zeros(node_index);
                let npo2: u64 = 1u64 << bit_width;

                let dist: u64 = npo2 - node_index;

                let ret: u64 = if (bit_width) < dist {
                    right_lineage_length(node_index - (npo2 >> 1u64) + 1u64)
                } else {
                    (dist - 1u64)
                };

                return ret;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn mmr_leftmost_ancestor() {
        let tokens: syn::Item = parse_quote! {
            fn leftmost_ancestor(node_index: u64) -> (u64, u32) {
                // let h = u128::BITS - node_index.leading_zeros() - 1;
                let h: u32 = u64::BITS - u64::leading_zeros(node_index) - 1u32;
                let ret: u64 = (1u64 << (h + 1u64)) - 1u64;

                return (ret, h);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn mmr_left_child() {
        let tokens: syn::Item = parse_quote! {
                fn left_child(node_index: u64, height: u64) -> u64 {
                    return node_index - (1u64 << height);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn method_call_no_args() {
        let tokens: syn::Item = parse_quote! {
            fn method_call() -> () {
                pop();
                push();
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn method_call_with_args() {
        let tokens: syn::Item = parse_quote! {
            fn method_call(lhs: u32, pointer: BFieldElement) -> () {
                pop(lhs);
                push(pointer, lhs);
                let foo: u32 = barbarian(7u32);

                return (pointer, foo, greek(barbarian(barbarian(greek(199u64)))));
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u64_algebra() {
        let tokens: syn::Item = parse_quote! {
            fn u64_algebra(lhs: u64, rhs: u64) -> (u64, u64, u64, u64, u64, u64, u64) {
                let a: u64 = lhs + rhs;
                let b: u64 = lhs - rhs;
                let c: u64 = lhs * rhs;
                let d: u64 = lhs / rhs;
                let e: u64 = 1u64 << 17u64;
                let f: u64 = 1u64 << lhs;
                let g: u64 = 1u64 >> rhs;

                return (a, b, c, d, e, f, g);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u32_algebra() {
        let tokens: syn::Item = parse_quote! {
            fn u32_algebra(lhs: u32, rhs: u32) -> (u32, u32, u32, u32) {
                let a: u32 = lhs + rhs;
                let b: u32 = lhs - rhs;
                let c: u32 = lhs * rhs;
                let d: u32 = lhs / rhs;
                let e: u32 = 1u32 << 17u32;
                let f: u32 = 1u32 << lhs;
                let g: u32 = 1u32 >> rhs;
                let h: u32 = lhs % 2u32;
                let i: bool = (lhs % 2u32) == 0u32;

                // Verify correct precedence handling
                let j: bool = (lhs + 14u32) * 117u32 - ((1u32 - (2u32 - rhs)) - (lhs - rhs));

                return (d, e, f, g);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn boolean_algebra() {
        let tokens: syn::Item = parse_quote! {
            fn boolean_algebra(lhs: bool, rhs: bool) -> (bool, bool, bool, bool, bool, bool) {
                let a: bool = lhs && rhs;
                let b: bool = lhs ^ rhs;
                let c: bool = lhs || rhs;
                let d: bool = true;
                let e: bool = false;
                let f: bool = true && false || false ^ false;

                return (a, b, c, d, e);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn and_and_xor_u32() {
        let tokens: syn::Item = parse_quote! {
            fn and_and_xor_u32(lhs: u32, rhs: u32) -> (u32, u32) {
                let a: u32 = lhs & rhs;
                let b: u32 = lhs ^ rhs;
                return (a, b);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn bfe_add_return_expr() {
        let tokens: syn::Item = parse_quote! {
            fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
                return lhs + rhs;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn bfe_add_return_var() {
        let tokens: syn::Item = parse_quote! {
            fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
                let sum: BFieldElement = lhs + rhs;
                return sum;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u32_add() {
        let tokens: syn::Item = parse_quote! {
            fn add_u32(lhs: u32, rhs: u32) -> u32 {
                let c: u32 = lhs + rhs;
                return c;
            }
        };
        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u32_swap() {
        let tokens: syn::Item = parse_quote! {
            fn swap_u32(lhs: u32, rhs: u32) -> (u32, u32) {
                return (rhs, lhs);
            }
        };
        match &tokens {
            syn::Item::Fn(item_fn) => {
                println!("{item_fn:#?}");
                let ret = graft(item_fn);
                println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }
}
