use itertools::Itertools;
use std::collections::HashMap;

use crate::ast;
use crate::libraries::tasm;
use crate::libraries::vector;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Typing {
    UnknownType,
    KnownType(ast::DataType),
}

impl Default for Typing {
    fn default() -> Self {
        Typing::UnknownType
    }
}

impl GetType for Typing {
    fn get_type(&self) -> ast::DataType {
        match self {
            Typing::UnknownType => {
                panic!("Cannot unpack type before complete type annotation.")
            }
            Typing::KnownType(data_type) => data_type.clone(),
        }
    }
}

pub trait GetType {
    fn get_type(&self) -> ast::DataType;
}

impl<T: GetType> GetType for ast::ExprLit<T> {
    fn get_type(&self) -> ast::DataType {
        match self {
            ast::ExprLit::Bool(_) => ast::DataType::Bool,
            ast::ExprLit::U32(_) => ast::DataType::U32,
            ast::ExprLit::U64(_) => ast::DataType::U64,
            ast::ExprLit::BFE(_) => ast::DataType::BFE,
            ast::ExprLit::XFE(_) => ast::DataType::XFE,
            ast::ExprLit::Digest(_) => ast::DataType::Digest,
            ast::ExprLit::GenericNum(_, ty) => ty.get_type(),
        }
    }
}

impl<T: GetType> GetType for ast::Expr<T> {
    fn get_type(&self) -> ast::DataType {
        match self {
            ast::Expr::Lit(lit) => lit.get_type(),
            ast::Expr::Var(id) => id.get_type(),
            ast::Expr::FlatList(t_list) => {
                let types = t_list.iter().map(|elem| elem.get_type()).collect_vec();
                ast::DataType::FlatList(types)
            }
            ast::Expr::FnCall(fn_call) => fn_call.get_type(),
            ast::Expr::MethodCall(method_call) => method_call.get_type(),
            ast::Expr::Binop(_, _, _, t) => t.get_type(),
            ast::Expr::If(if_expr) => if_expr.get_type(),
            ast::Expr::Cast(_expr, t) => t.to_owned(),
        }
    }
}

impl<T: GetType> GetType for ast::ExprIf<T> {
    fn get_type(&self) -> ast::DataType {
        self.then_branch.get_type()
    }
}

impl<T: GetType> GetType for ast::Identifier<T> {
    fn get_type(&self) -> ast::DataType {
        match self {
            ast::Identifier::String(_, t) => t.get_type(),
            ast::Identifier::TupleIndex(id, idx) => {
                let rec = id.get_type();
                match rec {
                    ast::DataType::FlatList(list) => list[*idx].clone(),
                    dt => panic!("Type error. Expected FlatList got: {dt:?}"),
                }
            }
            ast::Identifier::ListIndex(id, _) => {
                let rec = id.get_type();
                match rec {
                    ast::DataType::List(element_type) => *element_type,
                    dt => panic!("Type error. Expected List got: {dt:?}"),
                }
            }
        }
    }
}

impl<T: GetType> GetType for ast::FnCall<T> {
    fn get_type(&self) -> ast::DataType {
        self.annot.get_type()
    }
}

impl<T: GetType> GetType for ast::MethodCall<T> {
    fn get_type(&self) -> ast::DataType {
        self.annot.get_type()
    }
}

#[derive(Debug, Default)]
pub struct CheckState {
    pub vtable: HashMap<String, DataTypeAndMutability>,
    pub ftable: HashMap<String, ast::FnSignature>,
}

#[derive(Clone, Debug)]
pub struct DataTypeAndMutability {
    pub data_type: ast::DataType,
    pub mutable: bool,
}

impl From<ast::FnArg> for DataTypeAndMutability {
    fn from(value: ast::FnArg) -> Self {
        Self {
            data_type: value.data_type,
            mutable: value.mutable,
        }
    }
}

impl DataTypeAndMutability {
    pub fn new(data_type: &ast::DataType, mutable: bool) -> Self {
        Self {
            data_type: data_type.to_owned(),
            mutable,
        }
    }
}

pub fn annotate_fn(function: &mut ast::Fn<Typing>) {
    // Initialize `CheckState`
    let vtable: HashMap<String, DataTypeAndMutability> =
        HashMap::with_capacity(function.fn_signature.args.len());
    let mut ftable: HashMap<String, ast::FnSignature> = HashMap::new();
    // Insert self into ftable; TODO: Handle multiple functions
    ftable.insert(
        function.fn_signature.name.clone(),
        function.fn_signature.clone(),
    );
    let mut state = CheckState { vtable, ftable };

    // Populate vtable with function arguments
    for arg in function.fn_signature.args.iter() {
        let duplicate_fn_arg = state
            .vtable
            .insert(arg.name.clone(), arg.to_owned().into())
            .is_some();
        if duplicate_fn_arg {
            panic!("Duplicate function argument {}", arg.name);
        }
    }

    // Type-annotate each statement in-place
    function.body.iter_mut().for_each(|stmt| {
        annotate_stmt(
            stmt,
            &mut state,
            &function.fn_signature.name,
            &function.fn_signature.output,
        )
    });
}

fn annotate_stmt(
    stmt: &mut ast::Stmt<Typing>,
    state: &mut CheckState,
    fn_name: &str,
    fn_output: &ast::DataType,
) {
    match stmt {
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
            mutable,
        }) => {
            if state.vtable.contains_key(var_name) {
                panic!("let-assign cannot shadow existing variable '{var_name}'!");
            }

            let derived_type = derive_annotate_expr_type(expr, state);
            assert_type_equals(&derived_type, data_type, "let-statement");
            state.vtable.insert(
                var_name.clone(),
                DataTypeAndMutability::new(data_type, *mutable),
            );
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            let (identifier_type, mutable) = annotate_identifier_type(identifier, state);
            let expr_type = derive_annotate_expr_type(expr, state);

            // Only allow assignment if binding was declared as mutable
            assert_type_equals(&identifier_type, &expr_type, "assign-statement");
            assert!(
                mutable,
                "Cannot re-assign non-mutable binding: {identifier}"
            )
        }

        ast::Stmt::Return(opt_expr) => match (opt_expr, fn_output) {
            (None, ast::DataType::FlatList(tys)) => assert_eq!(
                0,
                tys.len(),
                "Return without value; expect {fn_name} to return nothing."
            ),
            (None, _) => {
                panic!("Return without value; expect function {fn_name} to return {fn_output}")
            }
            (Some(ret_expr), _) => {
                let expr_ret_type = derive_annotate_expr_type(ret_expr, state);
                assert_type_equals(fn_output, &expr_ret_type, "return stmt");
            }
        },

        ast::Stmt::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
        }) => {
            let fn_signature = get_fn_signature(name, state, type_parameter);
            assert!(
                is_void_type(&fn_signature.output),
                "Function call '{name}' at statement-level must return the unit type."
            );

            derive_annotate_fn_call_args(&fn_signature, args, state);

            *annot = Typing::KnownType(fn_signature.output);
        }

        ast::Stmt::MethodCall(ast::MethodCall {
            method_name,
            args,
            annot,
        }) => {
            let receiver = if let ast::Expr::Var(rec) = &mut args[0] {
                rec
            } else {
                panic!("Receiver must be an identifier")
            };
            let (receiver_type, _mutable) = annotate_identifier_type(receiver, state);
            let type_parameter = receiver_type.type_parameter();
            let method_signature: ast::FnSignature =
                get_method_signature(method_name, state, &type_parameter);
            assert!(
                is_void_type(&method_signature.output),
                "Method call {receiver}.'{method_name}' at statement-level must return the unit type."
            );

            // TODO: Check that receiver_type corresponds to method's FnSignature

            derive_annotate_fn_call_args(&method_signature, args, state);

            *annot = Typing::KnownType(method_signature.output)
        }

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            let condition_type = derive_annotate_expr_type(condition, state);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "while-condition");
            annotate_block_stmt(block, fn_name, fn_output, state);
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let condition_type = derive_annotate_expr_type(condition, state);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "if-condition");

            annotate_block_stmt(then_branch, fn_name, fn_output, state);
            annotate_block_stmt(else_branch, fn_name, fn_output, state);
        }

        ast::Stmt::Block(block_stmt) => {
            annotate_block_stmt(block_stmt, fn_name, fn_output, state);
        }
    }
}

fn annotate_block_stmt(
    block: &mut ast::BlockStmt<Typing>,
    fn_name: &str,
    fn_output: &ast::DataType,
    state: &mut CheckState,
) {
    let vtable_before = state.vtable.clone();
    block
        .stmts
        .iter_mut()
        .for_each(|stmt| annotate_stmt(stmt, state, fn_name, fn_output));
    state.vtable = vtable_before;
}

fn assert_type_equals(derived_type: &ast::DataType, data_type: &ast::DataType, context: &str) {
    if derived_type != data_type {
        panic!(
            "Type mismatch between type '{data_type}' and derived type '{derived_type}' for {context}",
        );
    }
}

/// Set type and return type and whether the identifier was declared as mutable
fn annotate_identifier_type(
    identifier: &mut ast::Identifier<Typing>,
    state: &mut CheckState,
) -> (ast::DataType, bool) {
    match identifier {
        // x
        ast::Identifier::String(var_name, var_type) => {
            let found_type = state
                .vtable
                .get(var_name)
                .unwrap_or_else(|| panic!("variable {var_name} must have known type"));
            *var_type = Typing::KnownType(found_type.data_type.clone());
            (found_type.data_type.clone(), found_type.mutable)
        }

        // x.0
        ast::Identifier::TupleIndex(tuple_identifier, index) => {
            // For syntax like 'x.0.1', find the type of 'x.0'; we don't currently
            // generate ASTs with nested tuple indexing, but it's easier to solve
            // the type inference generally here.
            let tuple_type = annotate_identifier_type(tuple_identifier, state);

            if let ast::DataType::FlatList(elem_types) = tuple_type.0 {
                if elem_types.len() < *index {
                    panic!(
                        "Cannot index tuple of {} elements with index {}",
                        elem_types.len(),
                        index
                    );
                }

                (elem_types[*index].clone(), tuple_type.1)
            } else {
                panic!("Cannot index non-tuple with tuple index {index}");
            }
        }

        // x[e]
        ast::Identifier::ListIndex(list_identifier, index_expr) => {
            // if let ast::Expr::Lit(ast::ExprLit::Num(val, t)) = *index_expr.to_owned() {
            //     *index_expr = Box::new(ast::Expr::Lit(
            //         ast::ExprLit::U32(val as u32),
            //         Typing::default(),
            //     ));
            // };
            let index_type = derive_annotate_expr_type(index_expr, state);
            if !is_index_type(&index_type) {
                panic!("Cannot index list with type '{index_type}'");
            }

            // TODO: It could make sense to support var.1[i] if there were better support for tuples.
            if !is_string_identifier(list_identifier) {
                panic!("Cannot index anything but variables: {list_identifier:?}");
            }

            let list_type = annotate_identifier_type(list_identifier, state);

            (list_type.0.type_parameter().unwrap(), list_type.1)
        }
    }
}

fn get_fn_signature(
    name: &str,
    state: &CheckState,
    type_parameter: &Option<ast::DataType>,
) -> ast::FnSignature {
    // all functions from `tasm-lib` are in scope
    if let Some(snippet_name) = tasm::get_function_name(name) {
        return tasm::function_name_to_signature(snippet_name);
    }

    // Functions for lists are in scope
    if let Some(function_name) = vector::get_function_name(name) {
        return vector::function_name_to_signature(function_name, type_parameter);
    }

    state
        .ftable
        .get(name)
        .unwrap_or_else(|| panic!("Don't know what type of value '{name}' returns! Type parameter was: {type_parameter:?}"))
        .clone()
}

fn get_method_signature(
    name: &str,
    _state: &CheckState,
    type_parameter: &Option<ast::DataType>,
) -> ast::FnSignature {
    if let Some(snippet_name) = tasm::get_method_name(name) {
        return tasm::function_name_to_signature(snippet_name);
    }

    // Functions for lists are in scope
    if let Some(function_name) = vector::get_method_name(name) {
        return vector::method_name_to_signature(function_name, type_parameter);
    }

    panic!("Don't know what type of value '{name}' returns! Type parameter was: {type_parameter:?}")
}

fn derive_annotate_fn_call_args(
    fn_signature: &ast::FnSignature,
    args: &mut [ast::Expr<Typing>],
    state: &mut CheckState,
) {
    let fn_name = &fn_signature.name;
    let arg_types: Vec<ast::DataType> = args
        .iter_mut()
        .map(|arg_expr| derive_annotate_expr_type(arg_expr, state))
        .collect();
    assert_eq!(
        fn_signature.args.len(),
        arg_types.len(),
        "Wrong number of arguments in function call to '{}'; expected {} arguments, got {}.",
        fn_name,
        fn_signature.args.len(),
        arg_types.len(),
    );
    for (arg_pos, (fn_arg, expr_type)) in fn_signature
        .args
        .iter()
        .zip_eq(arg_types.iter())
        .enumerate()
    {
        let arg_pos = arg_pos + 1;
        let ast::FnArg {
            name: arg_name,
            data_type: arg_type,
            mutable: _mutable,
        } = fn_arg;
        assert_eq!(
        arg_type, expr_type,
        "Wrong type of function argument {arg_pos} '{arg_name}' in '{fn_name}'; expected {arg_type}, got {expr_type}.",
    )
    }
}

fn derive_annotate_expr_type(
    expr: &mut ast::Expr<Typing>,
    state: &mut CheckState,
) -> ast::DataType {
    match expr {
        ast::Expr::Lit(ast::ExprLit::Bool(_)) => ast::DataType::Bool,
        ast::Expr::Lit(ast::ExprLit::U32(_)) => ast::DataType::U32,
        ast::Expr::Lit(ast::ExprLit::U64(_)) => ast::DataType::U64,
        ast::Expr::Lit(ast::ExprLit::BFE(_)) => ast::DataType::BFE,
        ast::Expr::Lit(ast::ExprLit::XFE(_)) => ast::DataType::XFE,
        ast::Expr::Lit(ast::ExprLit::Digest(_)) => ast::DataType::Digest,
        ast::Expr::Lit(ast::ExprLit::GenericNum(_n, _t)) => {
            panic!("TODO: Inject type hint into `t`")
        }

        ast::Expr::Var(identifier) => {
            let identifier_type = annotate_identifier_type(identifier, state);
            identifier_type.0
        }

        ast::Expr::FlatList(tuple_exprs) => {
            let tuple_types: Vec<ast::DataType> = tuple_exprs
                .iter_mut()
                .map(|expr| derive_annotate_expr_type(expr, state))
                .collect();
            ast::DataType::FlatList(tuple_types)
        }

        ast::Expr::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
        }) => {
            let fn_signature = get_fn_signature(name, state, type_parameter);
            assert!(
                !is_void_type(&fn_signature.output),
                "Function calls in expressions cannot return the unit type"
            );

            derive_annotate_fn_call_args(&fn_signature, args, state);

            *annot = Typing::KnownType(fn_signature.output.clone());

            fn_signature.output
        }

        ast::Expr::MethodCall(ast::MethodCall {
            method_name,
            args,
            annot,
        }) => {
            let receiver = if let ast::Expr::Var(rec) = &mut args[0] {
                rec
            } else {
                panic!("Receiver must be an identifier")
            };
            let (receiver_type, _mutable) = annotate_identifier_type(receiver, state);
            let type_parameter = receiver_type.type_parameter();
            let method_signature: ast::FnSignature =
                get_method_signature(method_name, state, &type_parameter);
            assert!(
                !is_void_type(&method_signature.output),
                "Method calls in expressions cannot return the unit type"
            );

            // TODO: Do more type-checking here if we need

            derive_annotate_fn_call_args(&method_signature, args, state);

            *annot = Typing::KnownType(method_signature.output.clone());

            method_signature.output
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, binop_type) => {
            let lhs_type = derive_annotate_expr_type(lhs_expr, state);
            let rhs_type = derive_annotate_expr_type(rhs_expr, state);

            use ast::BinOp::*;
            match binop {
                // Overloaded for all arithmetic types.
                Add => {
                    assert_type_equals(&lhs_type, &rhs_type, "add-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot add non-arithmetic type '{lhs_type}'",
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Restricted to bool only.
                And => {
                    assert_type_equals(&lhs_type, &ast::DataType::Bool, "and-expr lhs");
                    assert_type_equals(&rhs_type, &ast::DataType::Bool, "and-expr rhs");
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitAnd => {
                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-and-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-and type '{lhs_type}' (not u32-based)",
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitXor => {
                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-xor-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-xor type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all arithmetic types.
                Div => {
                    assert_type_equals(&lhs_type, &rhs_type, "div-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot divide non-arithmetic type '{lhs_type}'"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all primitive types.
                Eq => {
                    assert_type_equals(&lhs_type, &rhs_type, "eq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare non-primitive type '{lhs_type}' for equality"
                    );
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Lt => {
                    assert_type_equals(&lhs_type, &rhs_type, "lt-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot compare type '{lhs_type}' with less-than (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Overloaded for all primitive types.
                Mul => {
                    assert_type_equals(&lhs_type, &rhs_type, "mul-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot multiply non-arithmetic type '{lhs_type}'"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all primitive types.
                Neq => {
                    assert_type_equals(&lhs_type, &rhs_type, "neq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare type '{lhs_type}' with not-equal (not primitive)"
                    );
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to bool only.
                Or => {
                    assert_type_equals(&lhs_type, &ast::DataType::Bool, "or-expr lhs");
                    assert_type_equals(&rhs_type, &ast::DataType::Bool, "or-expr rhs");
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Rem => {
                    assert_type_equals(&lhs_type, &rhs_type, "rem-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot find remainder for type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());

                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shl => {
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-left for type '{lhs_type}' (not u32-based)"
                    );
                    assert_type_equals(&rhs_type, &ast::DataType::U32, "shl-rhs-expr");
                    *binop_type = Typing::KnownType(lhs_type.clone());

                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shr => {
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-right for type '{lhs_type}' (not u32-based)"
                    );
                    assert_type_equals(&rhs_type, &ast::DataType::U32, "shr-rhs-expr");
                    *binop_type = Typing::KnownType(lhs_type.clone());

                    lhs_type
                }

                // Overloaded for all arithmetic types.
                Sub => {
                    assert_type_equals(&lhs_type, &rhs_type, "sub-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot subtract non-arithmetic type '{lhs_type}'"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }
            }
        }

        ast::Expr::If(ast::ExprIf {
            condition,
            then_branch,
            else_branch,
        }) => {
            let condition_type = derive_annotate_expr_type(condition, state);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "if-condition-expr");

            let then_type = derive_annotate_expr_type(then_branch, state);
            let else_type = derive_annotate_expr_type(else_branch, state);
            assert_type_equals(&then_type, &else_type, "if-then-else-expr");

            then_type
        }
        ast::Expr::Cast(expr, as_type) => {
            let expr_type = derive_annotate_expr_type(expr, state);
            assert!(
                is_u32_based_type(&expr_type),
                "Can only cast from u32 and u64"
            );
            assert!(is_u32_based_type(as_type), "Can only cast to u32 and u64");

            as_type.to_owned()
        }
    }
}

pub fn is_string_identifier<T>(identifier: &ast::Identifier<T>) -> bool {
    matches!(identifier, ast::Identifier::String(_, _))
}

pub fn is_list_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, List(_))
}

/// A type that can be used as address in `read_mem` and `write_mem` calls.
///
/// Since memory addresses are essentially `BFieldElement`s, only types
/// that are subsets of `BFE`s can be used. The only such type is `U32`.
pub fn is_index_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, U32 | BFE)
}

/// A type for which basic arithmetic operators can be overloaded.
///
/// Note that not all operators work for all arithmetic types.
///
/// E.g. the bitwise operators only work for `is_u32_based_type()`.
pub fn is_arithmetic_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, U32 | U64 | BFE | XFE)
}

/// A type that is implemented in terms of `U32` values.
///
/// E.g. `U32` and `U64`.
pub fn is_u32_based_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, U32 | U64)
}

/// A non-composite fixed-length type.
pub fn is_primitive_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, Bool | U32 | U64 | BFE | XFE | Digest)
}

pub fn is_void_type(data_type: &ast::DataType) -> bool {
    if let ast::DataType::FlatList(tys) = data_type {
        tys.is_empty()
    } else {
        false
    }
}
