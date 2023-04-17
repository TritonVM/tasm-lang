use itertools::Itertools;
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tasm_code_generator::STACK_SIZE;
use crate::{ast, libraries};

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub enum Typing {
    /// An `UnknownType` has not been determined; this is produced by the parser/grafter.
    #[default]
    UnknownType,

    /// A `KnownType` has been determined; this is performed by the type checker.
    KnownType(ast::DataType),
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
            ast::ExprLit::GenericNum(_, t) => t.get_type(),
        }
    }
}

impl<T: GetType> GetType for ast::Expr<T> {
    fn get_type(&self) -> ast::DataType {
        match self {
            ast::Expr::Lit(lit) => lit.get_type(),
            ast::Expr::Var(id) => id.get_type(),
            ast::Expr::Tuple(t_list) => {
                ast::DataType::Tuple(t_list.iter().map(|elem| elem.get_type()).collect())
            }
            ast::Expr::FnCall(fn_call) => fn_call.get_type(),
            ast::Expr::MethodCall(method_call) => method_call.get_type(),
            ast::Expr::Binop(_, _, _, t) => t.get_type(),
            ast::Expr::If(if_expr) => if_expr.get_type(),
            ast::Expr::Cast(_expr, t) => t.to_owned(),
            ast::Expr::Unary(_unaryop, inner_expr, _) => inner_expr.get_type(),
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
                    ast::DataType::Tuple(list) => list[*idx].clone(),
                    dt => panic!("Type error. Expected Tuple got: {dt:?}"),
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
    /// The `vtable` maps variable names to their type.
    ///
    /// This is used for determining the type of variables in expressions.
    pub vtable: HashMap<String, DataTypeAndMutability>,

    /// The `ftable` maps function names to their signature (argument and output) types.
    ///
    /// This is used for determining the type of function calls in expressions.
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

    // Verify that input arguments do not exceed 15 words
    assert!(
        state
            .vtable
            .values()
            .map(|x| x.data_type.size_of())
            .sum::<usize>()
            < STACK_SIZE,
        "{}: Cannot handle function signatures with input size exceeding {} words",
        function.fn_signature.name,
        STACK_SIZE - 1
    );

    // Verify that last statememnt of function exists, and that it is a `return` statement
    let last_stmt = function
        .body
        .iter()
        .last()
        .unwrap_or_else(|| panic!("{}: Function cannot be emtpy.", function.fn_signature.name));
    assert!(
        matches!(last_stmt, ast::Stmt::Return(_)),
        "Last line of function must be a `return`"
    );

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

            let let_expr_hint: Option<&ast::DataType> = Some(data_type);
            let derived_type = derive_annotate_expr_type(expr, let_expr_hint, state);
            assert_type_equals(&derived_type, data_type, "let-statement");
            state.vtable.insert(
                var_name.clone(),
                DataTypeAndMutability::new(data_type, *mutable),
            );
        }

        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            let (identifier_type, mutable) = annotate_identifier_type(identifier, state);
            let assign_expr_hint = &identifier_type;
            let expr_type = derive_annotate_expr_type(expr, Some(assign_expr_hint), state);

            // Only allow assignment if binding was declared as mutable
            assert_type_equals(&identifier_type, &expr_type, "assign-statement");
            assert!(
                mutable,
                "Cannot re-assign non-mutable binding: {identifier}"
            )
        }

        ast::Stmt::Return(opt_expr) => match (opt_expr, fn_output) {
            (None, ast::DataType::Tuple(tys)) => assert_eq!(
                0,
                tys.len(),
                "Return without value; expect {fn_name} to return nothing."
            ),
            (None, _) => {
                panic!("Return without value; expect function {fn_name} to return {fn_output}")
            }
            (Some(ret_expr), _) => {
                let hint = &fn_output;
                let expr_ret_type = derive_annotate_expr_type(ret_expr, Some(hint), state);
                assert_type_equals(fn_output, &expr_ret_type, "return stmt");
            }
        },

        ast::Stmt::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
            arg_evaluation_order,
        }) => {
            let fn_signature = get_fn_signature(name, state, type_parameter);
            assert!(
                is_void_type(&fn_signature.output),
                "Function call '{name}' at statement-level must return the unit type."
            );

            derive_annotate_fn_call_args(&fn_signature, args, state);

            *arg_evaluation_order = fn_signature.arg_evaluation_order;
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

            let method_signature: ast::FnSignature =
                get_method_signature(method_name, state, receiver_type);
            assert!(
                is_void_type(&method_signature.output),
                "Method call {receiver}.'{method_name}' at statement-level must return the unit type."
            );

            // TODO: Check that receiver_type corresponds to method's FnSignature

            derive_annotate_fn_call_args(&method_signature, args, state);

            *annot = Typing::KnownType(method_signature.output)
        }

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            let condition_hint = ast::DataType::Bool;
            let condition_type = derive_annotate_expr_type(condition, Some(&condition_hint), state);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "while-condition");
            annotate_block_stmt(block, fn_name, fn_output, state);
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let condition_hint = ast::DataType::Bool;
            let condition_type = derive_annotate_expr_type(condition, Some(&condition_hint), state);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "if-condition");

            annotate_block_stmt(then_branch, fn_name, fn_output, state);
            annotate_block_stmt(else_branch, fn_name, fn_output, state);
        }

        ast::Stmt::Block(block_stmt) => {
            annotate_block_stmt(block_stmt, fn_name, fn_output, state);
        }
        ast::Stmt::Assert(assert_expr) => {
            let expr_type = derive_annotate_expr_type(
                &mut assert_expr.expression,
                Some(&ast::DataType::Bool),
                state,
            );
            assert_type_equals(&expr_type, &ast::DataType::Bool, "assert expression");
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

pub fn assert_type_equals(derived_type: &ast::DataType, data_type: &ast::DataType, context: &str) {
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

            if let ast::DataType::Tuple(elem_types) = tuple_type.0 {
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
            let index_hint = ast::DataType::U32;
            let index_type = derive_annotate_expr_type(index_expr, Some(&index_hint), state);
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
    // Function from libraries are in scope
    for lib in libraries::all_libraries() {
        if let Some(fn_name) = lib.get_function_name(name) {
            return lib.function_name_to_signature(&fn_name, type_parameter.to_owned());
        }
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
    receiver_type: ast::DataType,
) -> ast::FnSignature {
    // Only methods from libraries are in scope. New methods cannot be declared.
    for lib in libraries::all_libraries() {
        if let Some(method_name) = lib.get_method_name(name, &receiver_type) {
            return lib.method_name_to_signature(&method_name, &receiver_type);
        }
    }

    panic!(
        "Don't know what type of value '{name}' returns! Receiver parameter was: {receiver_type:?}"
    )
}

fn derive_annotate_fn_call_args(
    fn_signature: &ast::FnSignature,
    args: &mut [ast::Expr<Typing>],
    state: &mut CheckState,
) {
    let fn_name = &fn_signature.name;
    assert_eq!(
        fn_signature.args.len(),
        args.len(),
        "Wrong number of arguments in function call to '{}'; expected {} arguments, got {}.",
        fn_name,
        fn_signature.args.len(),
        args.len(),
    );
    let arg_types: Vec<ast::DataType> = args
        .iter_mut()
        .zip_eq(fn_signature.args.iter())
        .map(|(arg_expr, fn_arg)| {
            let arg_hint = Some(&fn_arg.data_type);
            derive_annotate_expr_type(arg_expr, arg_hint, state)
        })
        .collect();

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
    hint: Option<&ast::DataType>,
    state: &mut CheckState,
) -> ast::DataType {
    // println!("derive_annotate_expr_type({expr:?}, hint: {hint:?})");
    match expr {
        ast::Expr::Lit(ast::ExprLit::Bool(_)) => ast::DataType::Bool,
        ast::Expr::Lit(ast::ExprLit::U32(_)) => ast::DataType::U32,
        ast::Expr::Lit(ast::ExprLit::U64(_)) => ast::DataType::U64,
        ast::Expr::Lit(ast::ExprLit::BFE(_)) => ast::DataType::BFE,
        ast::Expr::Lit(ast::ExprLit::XFE(_)) => ast::DataType::XFE,
        ast::Expr::Lit(ast::ExprLit::Digest(_)) => ast::DataType::Digest,
        ast::Expr::Lit(ast::ExprLit::GenericNum(n, _t)) => {
            use ast::DataType::*;

            match hint {
                Some(&U32) => {
                    let err = format!("Cannot infer number literal {n} as u32");
                    let n: u32 = (*n).try_into().expect(&err);
                    *expr = ast::Expr::Lit(ast::ExprLit::U32(n));
                    U32
                }
                Some(&U64) => {
                    *expr = ast::Expr::Lit(ast::ExprLit::U64(*n));
                    U64
                }
                Some(&BFE) => {
                    assert!(*n <= BFieldElement::MAX);
                    let n = BFieldElement::new(*n);
                    *expr = ast::Expr::Lit(ast::ExprLit::BFE(n));
                    BFE
                }
                Some(&XFE) => {
                    assert!(*n <= BFieldElement::MAX);
                    let n = BFieldElement::new(*n);
                    let n = XFieldElement::new_const(n);
                    *expr = ast::Expr::Lit(ast::ExprLit::XFE(n));
                    XFE
                }
                Some(hint) => panic!("GenericNum does not infer as type hint {hint}"),
                None => panic!("GenericNum does not infer in context with no type hint. Missing type hint for: {:?}", expr),
            }
        }

        ast::Expr::Var(identifier) => {
            let identifier_type = annotate_identifier_type(identifier, state);
            identifier_type.0
        }

        ast::Expr::Tuple(tuple_exprs) => {
            let no_hint = None;
            let tuple_types: Vec<ast::DataType> = tuple_exprs
                .iter_mut()
                .map(|expr| derive_annotate_expr_type(expr, no_hint, state))
                .collect();
            ast::DataType::Tuple(tuple_types)
        }

        ast::Expr::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
            arg_evaluation_order,
        }) => {
            let fn_signature = get_fn_signature(name, state, type_parameter);
            assert!(
                !is_void_type(&fn_signature.output),
                "Function calls in expressions cannot return the unit type"
            );

            derive_annotate_fn_call_args(&fn_signature, args, state);
            *annot = Typing::KnownType(fn_signature.output.clone());
            *arg_evaluation_order = fn_signature.arg_evaluation_order;

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
            let method_signature: ast::FnSignature =
                get_method_signature(method_name, state, receiver_type);
            assert!(
                !is_void_type(&method_signature.output),
                "Method calls in expressions cannot return the unit type"
            );

            // TODO: Do more type-checking here if we need

            derive_annotate_fn_call_args(&method_signature, args, state);

            *annot = Typing::KnownType(method_signature.output.clone());

            method_signature.output
        }

        ast::Expr::Unary(unaryop, inner_expr, unaryop_type) => {
            use ast::UnaryOp::*;
            let inner_expr_type = derive_annotate_expr_type(inner_expr, hint, state);
            match unaryop {
                Neg => {
                    assert!(
                        is_negatable_type(&inner_expr_type),
                        "Cannot negate type '{inner_expr_type}'",
                    );
                    *unaryop_type = Typing::KnownType(inner_expr_type.clone());
                    inner_expr_type
                }
                Not => {
                    assert!(
                        is_not_compatible_type(&inner_expr_type),
                        "Cannot negate type '{inner_expr_type}'",
                    );
                    *unaryop_type = Typing::KnownType(inner_expr_type.clone());
                    inner_expr_type
                }
            }
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, binop_type) => {
            use ast::BinOp::*;
            match binop {
                // Overloaded for all arithmetic types.
                Add => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

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
                    let bool_hint = Some(&ast::DataType::Bool);
                    let lhs_type = derive_annotate_expr_type(lhs_expr, bool_hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, bool_hint, state);

                    assert_type_equals(&lhs_type, &ast::DataType::Bool, "and-expr lhs");
                    assert_type_equals(&rhs_type, &ast::DataType::Bool, "and-expr rhs");
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitAnd => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

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
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-xor-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-xor type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitOr => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-or-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-or type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all arithmetic types.
                Div => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

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
                    // FIXME: Cannot provide parent `hint` (since it's Bool)
                    let no_hint = None;
                    let lhs_type = derive_annotate_expr_type(lhs_expr, no_hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, no_hint, state);

                    assert_type_equals(&lhs_type, &rhs_type, "eq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare non-primitive type '{lhs_type}' for equality"
                    );
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Lt | Gt => {
                    // FIXME: Cannot provide parent `hint` (since it's Bool)
                    let no_hint = None;
                    let lhs_type = derive_annotate_expr_type(lhs_expr, no_hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, no_hint, state);

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
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

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
                    // FIXME: Cannot provide parent `hint` (since it's Bool)
                    let no_hint = None;
                    let lhs_type = derive_annotate_expr_type(lhs_expr, no_hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, no_hint, state);

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
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

                    assert_type_equals(&lhs_type, &ast::DataType::Bool, "or-expr lhs");
                    assert_type_equals(&rhs_type, &ast::DataType::Bool, "or-expr rhs");
                    *binop_type = Typing::KnownType(ast::DataType::Bool);
                    ast::DataType::Bool
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Rem => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

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
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);

                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-left for type '{lhs_type}' (not u32-based)"
                    );

                    let rhs_hint = Some(&ast::DataType::U32);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, rhs_hint, state);

                    assert_type_equals(&rhs_type, &ast::DataType::U32, "shl-rhs-expr");
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shr => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);

                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-right for type '{lhs_type}' (not u32-based)"
                    );

                    let rhs_hint = Some(&ast::DataType::U32);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, rhs_hint, state);

                    assert_type_equals(&rhs_type, &ast::DataType::U32, "shr-rhs-expr");
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    lhs_type
                }

                // Overloaded for all arithmetic types.
                Sub => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

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
            let condition_hint = ast::DataType::Bool;
            let condition_type = derive_annotate_expr_type(condition, Some(&condition_hint), state);
            assert_type_equals(&condition_type, &ast::DataType::Bool, "if-condition-expr");

            // FIXME: Unify branches
            let branch_hint = None;
            let then_type = derive_annotate_expr_type(then_branch, branch_hint, state);
            let else_type = derive_annotate_expr_type(else_branch, branch_hint, state);
            assert_type_equals(&then_type, &else_type, "if-then-else-expr");

            then_type
        }

        ast::Expr::Cast(expr, to_type) => {
            let from_type = derive_annotate_expr_type(expr, None, state);
            let valid_cast = is_u32_based_type(&from_type) && is_u32_based_type(to_type)
                || from_type == ast::DataType::Bool && is_arithmetic_type(to_type);

            assert!(valid_cast, "Cannot cast from {from_type} to {to_type}");

            to_type.to_owned()
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

/// A type from which expressions such as `-value` can be formed
pub fn is_negatable_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, BFE | XFE)
}

/// A type from which expressions such as `!value` can be formed
pub fn is_not_compatible_type(data_type: &ast::DataType) -> bool {
    use ast::DataType::*;
    matches!(data_type, Bool | U32 | U64)
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
    if let ast::DataType::Tuple(tys) = data_type {
        tys.is_empty()
    } else {
        false
    }
}
