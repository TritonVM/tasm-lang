use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::ast;
use crate::types;

type Annotation = types::Typing;

pub fn graft(input: &syn::ItemFn) -> ast::Fn<Annotation> {
    let name = input.sig.ident.to_string();
    let args = input.sig.inputs.iter().map(graft_fn_arg).collect_vec();
    let output = graft_return_type(&input.sig.output);
    let body = input.block.stmts.iter().map(graft_stmt).collect_vec();

    ast::Fn {
        body,
        fn_signature: ast::FnSignature { name, args, output },
    }
}

fn rust_type_path_to_data_type(rust_type_path: &syn::TypePath) -> ast::DataType {
    assert_eq!(
        1,
        rust_type_path.path.segments.len(),
        "Length other than one not supported"
    );
    let rust_type_as_string = rust_type_path.path.segments[0].ident.to_string();
    let primitive_type_parse_result = rust_type_as_string.parse::<ast::DataType>();

    if let Ok(data_type) = primitive_type_parse_result {
        return data_type;
    }

    // Type is not primitive. Is it a vector?
    if rust_type_as_string == "Vec" {
        return rust_vec_to_data_type(&rust_type_path.path.segments[0].arguments);
    }

    panic!("Unsupported type {rust_type_as_string:#?}");
}

fn rust_vec_to_data_type(path_args: &syn::PathArguments) -> ast::DataType {
    match path_args {
        syn::PathArguments::AngleBracketed(ab) => {
            assert_eq!(1, ab.args.len(), "Must be Vec<T> for *one* generic T.");
            match &ab.args[0] {
                syn::GenericArgument::Type(syn::Type::Path(path)) => {
                    ast::DataType::List(Box::new(rust_type_path_to_data_type(path)))
                }
                other => panic!("Unsupported type {other:#?}"),
            }
        }
        other => panic!("Unsupported type {other:#?}"),
    }
}

fn rust_type_to_data_type(x: &syn::Type) -> ast::DataType {
    match x {
        syn::Type::Path(data_type) => rust_type_path_to_data_type(data_type),
        ty => panic!("Unsupported type {ty:#?}"),
    }
}

// Extract type and mutability from syn::PatType
fn pat_type_to_data_type(rust_type_path: &syn::PatType) -> (ast::DataType, bool) {
    let mutable = match *rust_type_path.pat.to_owned() {
        syn::Pat::Ident(syn::PatIdent {
            attrs: _,
            by_ref: _,
            mutability,
            ident: _,
            subpat: _,
        }) => mutability.is_some(),
        other_type => panic!("Unsupported {other_type:#?}"),
    };
    match rust_type_path.ty.as_ref() {
        syn::Type::Path(path) => (rust_type_path_to_data_type(path), mutable),
        syn::Type::Tuple(tuple) => {
            let types = tuple.elems.iter().map(rust_type_to_data_type).collect_vec();

            // I think this is the correct handling interpretation of mutability as long
            // as we don't allow destructuring for tuple definitions.
            (ast::DataType::Tuple(types), mutable)
        }
        other_type => panic!("Unsupported {other_type:#?}"),
    }
}

fn pat_to_name(pat: &syn::Pat) -> String {
    match pat {
        syn::Pat::Ident(ident) => ident.ident.to_string(),
        other => panic!("unsupported: {other:?}"),
    }
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
            let (data_type, mutable): (ast::DataType, bool) = match pat_type.ty.as_ref() {
                // Input is an owned value
                syn::Type::Path(type_path) => (rust_type_path_to_data_type(type_path), false),

                // Input is a mutable reference
                syn::Type::Reference(syn::TypeReference {
                    and_token: _,
                    lifetime: _,
                    mutability,
                    elem,
                }) => {
                    assert!(mutability.is_some(), "Reference input must be mutable");
                    match *elem.to_owned() {
                        syn::Type::Path(type_path) => {
                            (rust_type_path_to_data_type(&type_path), true)
                        }
                        _ => todo!(),
                    }
                }
                other => panic!("unsupported: {other:?}"),
            };

            ast::FnArg {
                name,
                data_type,
                mutable,
            }
        }
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_return_type(rust_return_type: &syn::ReturnType) -> ast::DataType {
    match rust_return_type {
        syn::ReturnType::Type(_, path) => match path.as_ref() {
            syn::Type::Path(type_path) => rust_type_path_to_data_type(type_path),
            syn::Type::Tuple(tuple_type) => {
                let tuple_type = tuple_type;
                let output_elements = tuple_type
                    .elems
                    .iter()
                    .map(rust_type_to_data_type)
                    .collect_vec();

                ast::DataType::Tuple(output_elements)
            }
            _ => panic!("unsupported: {path:?}"),
        },
        syn::ReturnType::Default => ast::DataType::Tuple(vec![]),
    }
}

/// Return type argument found in path
fn path_to_type_parameter(path: &syn::Path) -> Option<ast::DataType> {
    let mut type_parameter: Option<ast::DataType> = None;
    for segment in path.segments.iter() {
        match &segment.arguments {
            syn::PathArguments::None => continue,
            syn::PathArguments::AngleBracketed(abga) => {
                assert_eq!(
                    1,
                    abga.args.len(),
                    "Only one type parameter argument is supported"
                );
                if let syn::GenericArgument::Type(rdt) = &abga.args[0] {
                    assert!(
                        type_parameter.is_none(),
                        "only one type parameter supported"
                    );
                    type_parameter = Some(rust_type_to_data_type(rdt));
                } else {
                    panic!("unsupported GenericArgument: {:#?}", abga.args[0]);
                }
            }
            syn::PathArguments::Parenthesized(_) => panic!("unsupported: {path:#?}"),
        }
    }

    type_parameter
}

fn graft_call_exp(expr_call: &syn::ExprCall) -> ast::Expr<Annotation> {
    let (name, type_parameter) = match expr_call.func.as_ref() {
        syn::Expr::Path(path) => (
            path_to_ident(&path.path),
            path_to_type_parameter(&path.path),
        ),
        other => panic!("unsupported: {other:?}"),
    };
    let args = expr_call.args.iter().map(graft_expr).collect_vec();
    let annot = Default::default();

    // Special case for consturcting BFEs and XFEs from literals.
    if name == "BFieldElement::new" && args.len() == 1 && matches!(args[0], ast::Expr::Lit(_)) {
        if let ast::Expr::Lit(ast::ExprLit::U64(value)) = args[0] {
            ast::Expr::Lit(ast::ExprLit::BFE(BFieldElement::new(value)))
        } else {
            panic!("Can only instantiate BFE with u64-literal. Please use casting for conversion to `BFieldElement`");
        }
    } else {
        ast::Expr::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
        })
    }
}

/// Return identifier if expression is a Path/identifier
fn expr_to_maybe_ident(rust_exp: &syn::Expr) -> Option<String> {
    match rust_exp {
        syn::Expr::Path(path_expr) => Some(path_to_ident(&path_expr.path)),
        _ => None,
    }
}

/// Interpret an expression as an identifier
fn expr_as_identifier(rust_exp: &syn::Expr) -> ast::Identifier<Annotation> {
    match rust_exp {
        syn::Expr::Path(path) => {
            ast::Identifier::String(path_to_ident(&path.path), Default::default())
        }
        syn::Expr::Field(field_expr) => {
            // This is for tuple support. E.g.: `a.2 = 14u32;`
            let path = field_expr.base.as_ref();
            let ident = match expr_to_maybe_ident(path) {
                Some(ident) => ident,
                None => panic!("unsupported: {field_expr:?}"),
            };
            let tuple_index = match &field_expr.member {
                syn::Member::Named(_) => panic!("unsupported: {field_expr:?}"),
                syn::Member::Unnamed(tuple_index) => tuple_index,
            };

            ast::Identifier::TupleIndex(
                Box::new(ast::Identifier::String(ident, Default::default())),
                tuple_index.index as usize,
            )
        }
        syn::Expr::Index(index_expr) => {
            let ident = match expr_to_maybe_ident(&index_expr.expr) {
                Some(ident) => ident,
                None => panic!("unsupported: {index_expr:?}"),
            };
            let index_expr = graft_expr(index_expr.index.as_ref());
            ast::Identifier::ListIndex(
                Box::new(ast::Identifier::String(ident, Default::default())),
                Box::new(index_expr),
            )
        }
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_method_call(rust_method_call: &syn::ExprMethodCall) -> ast::MethodCall<Annotation> {
    // TODO: This code only supports method calls on variable names and not on
    // list elements or on tuple elements. We definitely want to support this
    // on tuple elements, though. Expand!
    match rust_method_call.receiver.as_ref() {
        syn::Expr::Path(path) => {
            let identifier = ast::Identifier::String(path_to_ident(&path.path), Default::default());
            let method_name = rust_method_call.method.to_string();
            let mut args = vec![ast::Expr::Var(identifier)];
            args.append(&mut rust_method_call.args.iter().map(graft_expr).collect_vec());
            let annot = Default::default();
            ast::MethodCall {
                method_name,
                args,
                annot,
            }
        }
        syn::Expr::MethodCall(rust_inner_method_call) => {
            let inner_method_call = graft_method_call(rust_inner_method_call);
            let identifier = match &inner_method_call.args[0] {
                ast::Expr::Var(ident) => ident.to_owned(),
                _ => todo!(),
            };
            assert_eq!("pop", inner_method_call.method_name, ".pop().unwrap() only");
            let method_name = "pop".to_string();
            let mut args = vec![ast::Expr::Var(identifier)];
            args.append(
                &mut rust_inner_method_call
                    .args
                    .iter()
                    .map(graft_expr)
                    .collect_vec(),
            );
            let annot = Default::default();
            ast::MethodCall {
                method_name,
                args,
                annot,
            }
        }
        other => panic!("unsupported: {other:?}"),
    }
}

/// Handle Rust expressions of the type i += 1
pub fn graft_binop_eq_expr(
    left: &syn::Expr,
    op: &syn::BinOp,
    right: &syn::Expr,
) -> ast::Expr<Annotation> {
    let left = graft_expr(left);
    let ast_binop: ast::BinOp = graft_eq_binop(op);
    let right = graft_expr(right);

    ast::Expr::Binop(
        Box::new(left),
        ast_binop,
        Box::new(right),
        Default::default(),
    )
}

pub fn graft_expr(rust_exp: &syn::Expr) -> ast::Expr<Annotation> {
    match rust_exp {
        syn::Expr::Binary(bin_expr) => {
            let left = graft_expr(&bin_expr.left);
            let ast_binop: ast::BinOp = graft_binop(bin_expr.op);
            let right = graft_expr(&bin_expr.right);

            ast::Expr::Binop(
                Box::new(left),
                ast_binop,
                Box::new(right),
                Default::default(),
            )
        }
        syn::Expr::Path(path) => {
            let path = &path.path;
            let ident: String = path_to_ident(path);
            ast::Expr::Var(ast::Identifier::String(ident, Default::default()))
        }
        syn::Expr::Tuple(tuple_expr) => {
            let exprs = tuple_expr.elems.iter().map(graft_expr).collect_vec();
            ast::Expr::Tuple(exprs)
        }
        syn::Expr::Lit(litexp) => {
            let lit = &litexp.lit;
            ast::Expr::Lit(graft_lit(lit))
        }
        syn::Expr::Call(call_exp) => graft_call_exp(call_exp),
        syn::Expr::Paren(paren_exp) => graft_expr(&paren_exp.expr),
        syn::Expr::If(expr_if) => {
            let condition = graft_expr(&expr_if.cond);
            let if_branch = &expr_if.then_branch.stmts;
            assert_eq!(1, if_branch.len(), "Max one line in if/else expressions");
            let then_branch = match &if_branch[0] {
                syn::Stmt::Expr(expr) => graft_expr(expr),
                other => panic!("unsupported: {other:?}"),
            };
            let else_branch = &expr_if.else_branch.as_ref().unwrap().1;
            let else_branch = match else_branch.as_ref() {
                syn::Expr::Block(block) => {
                    let else_branch = &block.block.stmts;
                    assert_eq!(1, else_branch.len(), "Max one line in if/else expressions");

                    match &else_branch[0] {
                        syn::Stmt::Expr(expr) => graft_expr(expr),
                        other => panic!("unsupported: {other:?}"),
                    }
                }
                other => panic!("unsupported: {other:?}"),
            };

            ast::Expr::If(ast::ExprIf {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            })
        }
        syn::Expr::MethodCall(method_call_expr) => {
            ast::Expr::MethodCall(graft_method_call(method_call_expr))
        }
        syn::Expr::Field(field_expr) => {
            // This branch is for tuple support.
            // Nested tuples are not supported, and that's probably preferable
            let path = field_expr.base.as_ref();
            let ident = match expr_to_maybe_ident(path) {
                Some(ident) => ident,
                None => panic!("unsupported: {field_expr:?}"),
            };
            let tuple_index = match &field_expr.member {
                syn::Member::Named(_) => panic!("unsupported: {field_expr:?}"),
                syn::Member::Unnamed(tuple_index) => tuple_index,
            };

            ast::Expr::Var(ast::Identifier::TupleIndex(
                Box::new(ast::Identifier::String(ident, Default::default())),
                tuple_index.index as usize,
            ))
        }
        syn::Expr::Index(index_expr) => {
            let expr = graft_expr(&index_expr.expr);
            let index = graft_expr(&index_expr.index);

            if let ast::Expr::Var(identifier) = expr {
                ast::Expr::Var(ast::Identifier::ListIndex(
                    Box::new(identifier),
                    Box::new(index),
                ))
            } else {
                panic!("unsupported index expression: {index_expr:#?}");
            }
        }
        syn::Expr::Cast(syn::ExprCast {
            attrs: _attrs,
            expr,
            as_token: _as_token,
            ty,
        }) => {
            let unboxed_ty: syn::Type = *(*ty).to_owned();
            let as_type = rust_type_to_data_type(&unboxed_ty);
            let ast_expr = graft_expr(&(*expr).to_owned());
            ast::Expr::Cast(Box::new(ast_expr), as_type)
        }
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_lit(rust_val: &syn::Lit) -> ast::ExprLit<Annotation> {
    use ast::ExprLit::*;

    match rust_val {
        syn::Lit::Bool(b) => Bool(b.value),
        syn::Lit::Int(int_lit) => {
            let int_lit = int_lit.token().to_string();
            if let Some(int_lit) = int_lit.strip_suffix("u32") {
                if let Ok(int_u32) = int_lit.parse::<u32>() {
                    return ast::ExprLit::U32(int_u32);
                }
            }

            if let Some(int_lit) = int_lit.strip_suffix("u64") {
                if let Ok(int_u64) = int_lit.parse::<u64>() {
                    return ast::ExprLit::U64(int_u64);
                }
            }

            let literal_err = format!("unsupported integer literal: {int_lit}");
            let literal: u64 = int_lit.parse::<u64>().expect(&literal_err);
            ast::ExprLit::GenericNum(literal, Default::default())
        }
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_binop(rust_binop: syn::BinOp) -> ast::BinOp {
    match rust_binop {
        syn::BinOp::Add(_) => ast::BinOp::Add,
        syn::BinOp::And(_) => ast::BinOp::And,
        syn::BinOp::BitAnd(_) => ast::BinOp::BitAnd,
        syn::BinOp::BitXor(_) => ast::BinOp::BitXor,
        syn::BinOp::Div(_) => ast::BinOp::Div,
        syn::BinOp::Eq(_) => ast::BinOp::Eq,
        syn::BinOp::Lt(_) => ast::BinOp::Lt,
        syn::BinOp::Gt(_) => ast::BinOp::Gt,
        syn::BinOp::Mul(_) => ast::BinOp::Mul,
        syn::BinOp::Ne(_) => ast::BinOp::Neq,
        syn::BinOp::Or(_) => ast::BinOp::Or,
        syn::BinOp::Rem(_) => ast::BinOp::Rem,
        syn::BinOp::Shl(_) => ast::BinOp::Shl,
        syn::BinOp::Shr(_) => ast::BinOp::Shr,
        syn::BinOp::Sub(_) => ast::BinOp::Sub,
        other => panic!("unsupported: {other:?}"),
    }
}

fn graft_eq_binop(rust_eq_binop: &syn::BinOp) -> ast::BinOp {
    match rust_eq_binop {
        syn::BinOp::AddEq(_) => ast::BinOp::Add,
        syn::BinOp::SubEq(_) => ast::BinOp::Sub,
        syn::BinOp::MulEq(_) => ast::BinOp::Mul,
        syn::BinOp::DivEq(_) => ast::BinOp::Div,
        syn::BinOp::RemEq(_) => ast::BinOp::Rem,
        syn::BinOp::BitXorEq(_) => ast::BinOp::BitXor,
        syn::BinOp::BitAndEq(_) => ast::BinOp::BitAnd,
        syn::BinOp::ShlEq(_) => ast::BinOp::Shl,
        syn::BinOp::ShrEq(_) => ast::BinOp::Shr,
        other => panic!("unsupported for eq binop: {other:?}"),
    }
}

pub fn graft_stmt(rust_stmt: &syn::Stmt) -> ast::Stmt<Annotation> {
    match rust_stmt {
        syn::Stmt::Local(local) => {
            // Handle variable declarations
            let (ident, data_type, mutable): (String, ast::DataType, bool) = match &local.pat {
                syn::Pat::Type(pat_type) => {
                    let (dt, mutable): (ast::DataType, bool) = pat_type_to_data_type(pat_type);
                    let ident: String = pat_to_name(&pat_type.pat);

                    (ident, dt, mutable)
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
                mutable
            };
            ast::Stmt::Let(let_stmt)
        }
        syn::Stmt::Item(_) => todo!(),
        syn::Stmt::Expr(expr) => match expr {
            syn::Expr::While(while_stmt) => {
                let expr_while = while_stmt;
                let while_condition = graft_expr(&expr_while.cond);
                let while_stmts: Vec<ast::Stmt<Annotation>> =
                    while_stmt.body.stmts.iter().map(graft_stmt).collect_vec();

                let while_stmt = ast::WhileStmt {
                    condition: while_condition,
                    block: ast::BlockStmt { stmts: while_stmts },
                };
                ast::Stmt::While(while_stmt)
            }
            syn::Expr::If(if_expr) => {
                let if_condition = graft_expr(&if_expr.cond);
                let then_stmts: Vec<ast::Stmt<Annotation>> = if_expr
                    .then_branch
                    .stmts
                    .iter()
                    .map(graft_stmt)
                    .collect_vec();
                let else_stmts: Vec<ast::Stmt<Annotation>> = match if_expr.else_branch.as_ref() {
                    Some(else_stmts) => match else_stmts.1.as_ref() {
                        syn::Expr::Block(block) => {
                            block.block.stmts.iter().map(graft_stmt).collect()
                        }
                        other => panic!("unsupported: {other:?}"),
                    },
                    None => vec![],
                };

                let if_stmt = ast::IfStmt {
                    condition: if_condition,
                    then_branch: ast::BlockStmt { stmts: then_stmts },
                    else_branch: ast::BlockStmt { stmts: else_stmts },
                };
                ast::Stmt::If(if_stmt)
            }
            syn::Expr::Block(syn::ExprBlock { attrs: _attrs, label: _label, block }) => {
                let stmts: Vec<ast::Stmt<Annotation>> = block
                    .stmts
                    .iter()
                    .map(graft_stmt)
                    .collect_vec();
                ast::Stmt::Block(ast::BlockStmt { stmts })
            },
            other => panic!("unsupported expression. make sure to end statements by semi-colon and to explicitly 'return': {other:?}"),
        },
        syn::Stmt::Semi(semi, _b) => match semi {
            syn::Expr::Return(ret_expr) => {
                let optional_ret_expr = ret_expr.expr.as_ref().map(|ret_expr| graft_expr(ret_expr));
                ast::Stmt::Return(optional_ret_expr)
            }
            syn::Expr::Call(call_exp) => {
                // Handle a function call that's not an assignment or a return expression
                let ast_fn_call = graft_call_exp(call_exp);

                match ast_fn_call {
                    ast::Expr::FnCall(fncall) => ast::Stmt::FnCall(fncall),
                    _ => panic!("function call as a statement cannot be a literal")
                }
            }
            syn::Expr::Assign(assign) => {
                let identifier_expr = assign.left.as_ref();
                let identifier = expr_as_identifier(identifier_expr);
                let assign_expr = graft_expr(assign.right.as_ref());
                let assign_stmt = ast::AssignStmt {
                    identifier,
                    expr: assign_expr,
                };
                ast::Stmt::Assign(assign_stmt)
            }
            // Handle expressions of the type `i += 1`
            syn::Expr::AssignOp(syn::ExprAssignOp { attrs: _, left, op, right }) => {
                let identifier_expr = left.as_ref();
                let identifier = expr_as_identifier(identifier_expr);
                let assign_expr = graft_binop_eq_expr(left, op, right);
                let assign_stmt = ast::AssignStmt {
                    identifier,
                    expr: assign_expr,
                };

                ast::Stmt::Assign(assign_stmt)
            },
            syn::Expr::MethodCall(method_call_expr) => {
                ast::Stmt::MethodCall(graft_method_call(method_call_expr))
            }
            other => panic!("unsupported: {other:?}"),
        },
    }
}

pub fn item_fn(item: syn::Item) -> syn::ItemFn {
    match item {
        syn::Item::Fn(item_fn) => item_fn,
        other => panic!("item_fn: expected fn, found: {other:#?}"),
    }
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn big_mmr_function() {
        let tokens: syn::Item = parse_quote! {
            fn calculate_new_peaks_from_leaf_mutation(
                old_peaks: Vec<Digest>,
                new_leaf: Digest,
                leaf_count: u64,
                auth_path: Vec<Digest>,
                leaf_index: u64,
            ) -> Vec<Digest> {
                // let (mut acc_mt_index, peak_index) =
                let acc_mt_index_and_peak_index: (u64, u32) = leaf_index_to_mt_index_and_peak_index(leaf_index, leaf_count);
                let mut acc_hash: Digest = new_leaf;
                let mut i: u32 = 0u32;
                while acc_mt_index_and_peak_index.0 != 1u64 {
                    let ap_element: Digest = auth_path[i];
                    if acc_mt_index_and_peak_index.0 % 2u64 == 1u64 {
                        // Node with `acc_hash` is a right child
                        acc_hash = H::hash_pair(ap_element, acc_hash);
                    } else {
                        // Node with `acc_hash` is a left child
                        acc_hash = H::hash_pair(acc_hash, ap_element);
                    }

                    acc_mt_index_and_peak_index.0 = acc_mt_index_and_peak_index.0 / 2u64;
                    i = i + 1u32;
                }

                let mut calculated_peaks: Vec<Digest> = old_peaks.to_vec();
                calculated_peaks[acc_mt_index_and_peak_index.1] = acc_hash;

                return calculated_peaks;
        }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                let _ret = graft(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn make_a_list() {
        let tokens: syn::Item = parse_quote! {
            fn make_and_return_a_list() -> Vec<u64> {
                let mut a: Vec<u64> = Vec::<u64>::default();
                let mut b: Vec<u64> = Vec::default();
                a.push(43u64);
                a.push(10u64);
                a.pop().unwrap();

                return a;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                let _ret = graft(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn function_call_no_args() {
        let tokens: syn::Item = parse_quote! {
            fn method_call() -> () {
                pop();
                push();
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn function_call_with_args() {
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                let _ret = graft(item_fn);
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
                let _ret = graft(item_fn);
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
                let _ret = graft(item_fn);
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
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
                // println!("{item_fn:#?}");
                let _ret = graft(item_fn);
                // println!("{ret:#?}");
            }
            _ => panic!("unsupported"),
        }
    }
}
