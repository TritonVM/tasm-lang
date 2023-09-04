use itertools::Itertools;
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::tasm_code_generator::SIZE_OF_ACCESSIBLE_STACK;
use crate::{ast, ast_types, libraries};

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub enum Typing {
    /// An `UnknownType` has not been determined; this is produced by the parser/grafter.
    #[default]
    UnknownType,

    /// A `KnownType` has been determined; this is performed by the type checker.
    KnownType(ast_types::DataType),
}

impl GetType for Typing {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            Typing::UnknownType => {
                panic!("Cannot unpack type before complete type annotation.")
            }
            Typing::KnownType(data_type) => data_type.clone(),
        }
    }
}

pub trait GetType {
    fn get_type(&self) -> ast_types::DataType;
}

impl<T: GetType> GetType for ast::ExprLit<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::ExprLit::Bool(_) => ast_types::DataType::Bool,
            ast::ExprLit::U32(_) => ast_types::DataType::U32,
            ast::ExprLit::U64(_) => ast_types::DataType::U64,
            ast::ExprLit::BFE(_) => ast_types::DataType::BFE,
            ast::ExprLit::XFE(_) => ast_types::DataType::XFE,
            ast::ExprLit::Digest(_) => ast_types::DataType::Digest,
            ast::ExprLit::GenericNum(_, t) => t.get_type(),
            ast::ExprLit::MemPointer(ast::MemPointerLiteral {
                mem_pointer_address: _,
                struct_name: _,
                resolved_type,
            }) => resolved_type.get_type(),
        }
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::Expr<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::Expr::Lit(lit) => lit.get_type(),
            ast::Expr::Var(id) => id.get_type(),
            ast::Expr::Tuple(t_list) => {
                ast_types::DataType::Tuple(t_list.iter().map(|elem| elem.get_type()).collect())
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

impl<T: GetType + std::fmt::Debug> GetType for ast::ExprIf<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.then_branch.get_type()
    }
}

impl<T: GetType> GetType for ast::Identifier<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::Identifier::String(_, t) => t.get_type(),
            ast::Identifier::ListIndex(id, _, t) => t.get_type(),
            ast::Identifier::TupleIndex(id, idx, t) => t.get_type(),
            ast::Identifier::Field(ident, field_name, t) => t.get_type(),
        }
    }
}

impl<T: GetType> GetType for ast::FnCall<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.annot.get_type()
    }
}

impl<T: GetType> GetType for ast::MethodCall<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.annot.get_type()
    }
}

#[derive(Debug)]
pub struct CheckState<'a> {
    pub libraries: &'a [Box<dyn libraries::Library>],

    /// The `vtable` maps variable names to their type.
    ///
    /// This is used for determining the type of variables in expressions.
    pub vtable: HashMap<String, DataTypeAndMutability>,

    /// The `ftable` maps function names to their signature (argument and output) types.
    ///
    /// This is used for determining the type of function calls in expressions.
    pub ftable: HashMap<String, ast::FnSignature>,

    pub declared_structs: HashMap<String, ast_types::StructType>,
}

#[derive(Clone, Debug)]
pub struct DataTypeAndMutability {
    pub data_type: ast_types::DataType,
    pub mutable: bool,
}

impl From<ast_types::AbstractValueArg> for DataTypeAndMutability {
    fn from(value: ast_types::AbstractValueArg) -> Self {
        Self {
            data_type: value.data_type,
            mutable: value.mutable,
        }
    }
}

impl DataTypeAndMutability {
    pub fn new(data_type: &ast_types::DataType, mutable: bool) -> Self {
        Self {
            data_type: data_type.to_owned(),
            mutable,
        }
    }
}

pub fn annotate_fn(
    function: &mut ast::Fn<Typing>,
    declared_structs: HashMap<String, ast_types::StructType>,
    libraries: &[Box<dyn libraries::Library>],
) {
    // Initialize `CheckState`
    let vtable: HashMap<String, DataTypeAndMutability> =
        HashMap::with_capacity(function.fn_signature.args.len());
    let mut ftable: HashMap<String, ast::FnSignature> = HashMap::new();
    // Insert self into ftable; TODO: Handle multiple functions
    ftable.insert(
        function.fn_signature.name.clone(),
        function.fn_signature.clone(),
    );
    let mut state = CheckState {
        libraries,
        vtable,
        ftable,
        declared_structs,
    };

    // Populate vtable with function arguments
    for arg in function.fn_signature.args.iter() {
        match arg {
            ast_types::AbstractArgument::FunctionArgument(_) => todo!(),
            ast_types::AbstractArgument::ValueArgument(value_fn_arg) => {
                let duplicate_fn_arg = state
                    .vtable
                    .insert(value_fn_arg.name.clone(), value_fn_arg.to_owned().into())
                    .is_some();
                if duplicate_fn_arg {
                    panic!("Duplicate function argument {}", value_fn_arg.name);
                }
            }
        }
    }

    // Verify that input arguments do not exceed 15 words
    assert!(
        state
            .vtable
            .values()
            .map(|x| x.data_type.stack_size())
            .sum::<usize>()
            < SIZE_OF_ACCESSIBLE_STACK,
        "{}: Cannot handle function signatures with input size exceeding {} words",
        function.fn_signature.name,
        SIZE_OF_ACCESSIBLE_STACK - 1
    );

    // Verify that last statement of function exists, and that it is a `return` statement
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
    fn_output: &ast_types::DataType,
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

            // Map types to those declared in program, i.e. resolve local `struct` declarations
            *data_type = data_type.resolve_types(&state.declared_structs);

            let let_expr_hint: Option<&ast_types::DataType> = Some(data_type);
            let derived_type = derive_annotate_expr_type(expr, let_expr_hint, state);
            assert_type_equals(
                &derived_type,
                data_type,
                &format!("let-statement of {var_name}"),
            );
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
            (None, ast_types::DataType::Tuple(tys)) => assert_eq!(
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
            let fn_signature = get_fn_signature(name, state, type_parameter, args);
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
            derive_annotate_expr_type(&mut args[0], None, state);
            let receiver_type = args[0].get_type();

            let method_signature: ast::FnSignature =
                get_method_signature(method_name, state, receiver_type.clone(), args);
            assert!(
                is_void_type(&method_signature.output),
                "Method call {receiver_type}.'{method_name}' at statement-level must return the unit type."
            );

            // TODO: Check that receiver_type corresponds to method's FnSignature

            derive_annotate_fn_call_args(&method_signature, args, state);

            *annot = Typing::KnownType(method_signature.output)
        }

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            let condition_hint = ast_types::DataType::Bool;
            let condition_type = derive_annotate_expr_type(condition, Some(&condition_hint), state);
            assert_type_equals(
                &condition_type,
                &ast_types::DataType::Bool,
                "while-condition",
            );
            annotate_block_stmt(block, fn_name, fn_output, state);
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let condition_hint = ast_types::DataType::Bool;
            let condition_type = derive_annotate_expr_type(condition, Some(&condition_hint), state);
            assert_type_equals(&condition_type, &ast_types::DataType::Bool, "if-condition");

            annotate_block_stmt(then_branch, fn_name, fn_output, state);
            annotate_block_stmt(else_branch, fn_name, fn_output, state);
        }

        ast::Stmt::Block(block_stmt) => {
            annotate_block_stmt(block_stmt, fn_name, fn_output, state);
        }
        ast::Stmt::Assert(assert_expr) => {
            let expr_type = derive_annotate_expr_type(
                &mut assert_expr.expression,
                Some(&ast_types::DataType::Bool),
                state,
            );
            assert_type_equals(&expr_type, &ast_types::DataType::Bool, "assert expression");
        }
        ast::Stmt::FnDeclaration(function) => {
            annotate_fn(function, HashMap::default(), state.libraries);
            state.ftable.insert(
                function.fn_signature.name.clone(),
                function.fn_signature.clone(),
            );
        }
    }
}

fn annotate_block_stmt(
    block: &mut ast::BlockStmt<Typing>,
    fn_name: &str,
    fn_output: &ast_types::DataType,
    state: &mut CheckState,
) {
    let vtable_before = state.vtable.clone();
    block
        .stmts
        .iter_mut()
        .for_each(|stmt| annotate_stmt(stmt, state, fn_name, fn_output));
    state.vtable = vtable_before;
}

pub fn assert_type_equals(
    derived_type: &ast_types::DataType,
    data_type: &ast_types::DataType,
    context: &str,
) {
    if derived_type != data_type {
        panic!(
            "Type mismatch between type\n'{data_type}'\n and derived type\n'{derived_type}'\n for {context}",
        );
    }
}

/// Set type and return type and whether the identifier was declared as mutable
pub fn annotate_identifier_type(
    identifier: &mut ast::Identifier<Typing>,
    state: &mut CheckState,
) -> (ast_types::DataType, bool) {
    match identifier {
        // x
        ast::Identifier::String(var_name, var_type) => match state.vtable.get(var_name) {
            Some(found_type) => {
                *var_type = Typing::KnownType(found_type.data_type.clone());
                (found_type.data_type.clone(), found_type.mutable)
            }
            None => match state.ftable.get(var_name) {
                Some(function) => {
                    *var_type = Typing::KnownType(function.into());
                    let function_datatype: ast_types::DataType = function.into();
                    (function_datatype, false)
                }
                None => panic!("variable {var_name} must have known type"),
            },
        },

        // x.0
        ast::Identifier::TupleIndex(tuple_identifier, index, known_type) => {
            // For syntax like 'x.0.1', find the type of 'x.0'; we don't currently
            // generate ASTs with nested tuple indexing, but it's easier to solve
            // the type inference generally here.
            let (tuple_type, mutable) = annotate_identifier_type(tuple_identifier, state);

            let element_type = if let ast_types::DataType::Tuple(elem_types) = tuple_type {
                if elem_types.len() > *index {
                    elem_types[*index].clone()
                } else {
                    panic!(
                        "Cannot index tuple of {} elements with index {}",
                        elem_types.len(),
                        index
                    );
                }
            } else {
                panic!("Cannot index non-tuple with tuple index {index}");
            };

            *known_type = Typing::KnownType(element_type.clone());

            (element_type, mutable)
        }

        // x[e]
        ast::Identifier::ListIndex(list_identifier, index_expr, known_type) => {
            let index_hint = ast_types::DataType::U32;
            let index_type = derive_annotate_expr_type(index_expr, Some(&index_hint), state);
            if !is_index_type(&index_type) {
                panic!("Cannot index list with type '{index_type}'");
            }

            // Only `a: Vec<T>` can be indexed, so if type is e.g. `MemPointer(Vec<T>)`, then the
            // type of `a` need to be forced to `Vec<T>`.
            // TODO: It's possible that the type of `list_identifier` needs to be forced to. But to
            // do that, this function probably needs the expression, and not just the identifier.
            let (maybe_list_type, mutable) = annotate_identifier_type(list_identifier, state);
            let mut forced_list_type = maybe_list_type.clone();
            let element_type = loop {
                if let ast_types::DataType::List(elem_ty, _) = &forced_list_type {
                    break elem_ty;
                } else if let ast_types::DataType::MemPointer(inner_type) = forced_list_type {
                    forced_list_type = *inner_type.to_owned();
                } else {
                    panic!("Cannot index into {list_identifier} of type {maybe_list_type}");
                }
            };

            // If list_identifier is a MemPointer, and the element type is not copyable,
            // then the element type statys a MemPointer.
            let element_type = if let ast_types::DataType::MemPointer(_) = maybe_list_type {
                if element_type.is_copy() {
                    *element_type.to_owned()
                } else {
                    ast_types::DataType::MemPointer(element_type.to_owned())
                }
            } else {
                *element_type.to_owned()
            };

            list_identifier.force_type(&forced_list_type);
            *known_type = Typing::KnownType(element_type.clone());

            (element_type, mutable)
        }
        ast::Identifier::Field(ident, field_name, annot) => {
            let (receiver_type, mutable) = annotate_identifier_type(ident, state);
            // Only structs have fields, so receiver_type must be a struct, or a pointer to a struct.
            let data_type = receiver_type.field_access_returned_type(field_name);
            *annot = Typing::KnownType(data_type.clone());

            (data_type, mutable)
        }
    }
}

fn get_fn_signature(
    name: &str,
    state: &CheckState,
    type_parameter: &Option<ast_types::DataType>,
    args: &[ast::Expr<Typing>],
) -> ast::FnSignature {
    // Function from libraries are in scope
    for lib in state.libraries.iter() {
        if let Some(fn_name) = lib.get_function_name(name) {
            return lib.function_name_to_signature(&fn_name, type_parameter.to_owned(), args);
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
    state: &CheckState,
    original_receiver_type: ast_types::DataType,
    args: &mut [ast::Expr<Typing>],
) -> ast::FnSignature {
    // Only methods from libraries are in scope. New methods cannot be declared.
    // Implemented following the description from: https://stackoverflow.com/a/28552082/2574407
    // TODO: Handle automatic dereferencing and referencing of MemPointer types
    let mut forced_type = original_receiver_type.clone();
    let mut try_again = true;
    while try_again {
        // 1. if there's a method `bar` where the receiver type (the type of self
        // in the method) matches `forced_type` exactly , use it (a "by value method")
        for lib in state.libraries.iter() {
            if let Some(method_name) = lib.get_method_name(name, &forced_type) {
                if let ast::Expr::Var(var) = &mut args[0] {
                    var.force_type(&forced_type);
                }

                return lib.method_name_to_signature(&method_name, &forced_type, args, state);
            }
        }

        // 2. therwise, add one auto-ref (take & or &mut of the receiver), and,
        // if some method's receiver matches &U, use it (an "autorefd method")
        let auto_refd_forced_type = ast_types::DataType::MemPointer(Box::new(forced_type.clone()));
        for lib in state.libraries.iter() {
            if let Some(method_name) = lib.get_method_name(name, &auto_refd_forced_type) {
                if let ast::Expr::Var(var) = &mut args[0] {
                    var.force_type(&auto_refd_forced_type);
                }
                return lib.method_name_to_signature(
                    &method_name,
                    &auto_refd_forced_type,
                    args,
                    state,
                );
            }
        }

        // Keep stripping `MemPointer` until we find a match
        if let ast_types::DataType::MemPointer(inner_type) = forced_type {
            forced_type = *inner_type.to_owned();
        } else {
            try_again = false;
        }
    }

    panic!(
        "Don't know what type of value '{name}' returns! Receiver type was: {original_receiver_type:?}"
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

    // Get list of concrete arguments used in function call
    let concrete_arguments: Vec<ast_types::DataType> = args
        .iter_mut()
        .zip_eq(fn_signature.args.iter())
        .map(|(arg_expr, fn_arg)| match fn_arg {
            ast_types::AbstractArgument::FunctionArgument(abstract_function) => {
                // arg_expr must evaluate to a FnCall here

                let arg_hint = Some(&abstract_function.function_type.input_argument);
                derive_annotate_expr_type(arg_expr, arg_hint, state)
            }
            ast_types::AbstractArgument::ValueArgument(abstract_value) => {
                let arg_hint = Some(&abstract_value.data_type);
                derive_annotate_expr_type(arg_expr, arg_hint, state)
            }
        })
        .collect();

    // Compare list of concrete arguments with function signature, i.e. expected arguments
    for (arg_pos, (fn_arg, expr_type)) in fn_signature
        .args
        .iter()
        .zip_eq(concrete_arguments.iter())
        .enumerate()
    {
        match fn_arg {
            ast_types::AbstractArgument::FunctionArgument(ast_types::AbstractFunctionArg {
                abstract_name: _,
                function_type,
            }) => assert_type_equals(
                &ast_types::DataType::Function(Box::new(function_type.to_owned())),
                expr_type,
                "Function argument",
            ),
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: arg_name,
                data_type: arg_type,
                mutable: _mutable,
            }) => {
                let arg_pos = arg_pos + 1;
                assert_eq!(
                arg_type, expr_type,
                "Wrong type of function argument {arg_pos} function call '{arg_name}' in '{fn_name}'\n expected type \"{arg_type:#?}\", but got type  \"{expr_type:#?}\".",
            );
            }
        }
    }
}

fn derive_annotate_expr_type(
    expr: &mut ast::Expr<Typing>,
    hint: Option<&ast_types::DataType>,
    state: &mut CheckState,
) -> ast_types::DataType {
    match expr {
        ast::Expr::Lit(ast::ExprLit::Bool(_)) => ast_types::DataType::Bool,
        ast::Expr::Lit(ast::ExprLit::U32(_)) => ast_types::DataType::U32,
        ast::Expr::Lit(ast::ExprLit::U64(_)) => ast_types::DataType::U64,
        ast::Expr::Lit(ast::ExprLit::BFE(_)) => ast_types::DataType::BFE,
        ast::Expr::Lit(ast::ExprLit::XFE(_)) => ast_types::DataType::XFE,
        ast::Expr::Lit(ast::ExprLit::Digest(_)) => ast_types::DataType::Digest,
        ast::Expr::Lit(ast::ExprLit::MemPointer(ast::MemPointerLiteral {
            mem_pointer_address: _,
            struct_name,
            resolved_type,
        })) => {
            // First get the type from the declared structs list, then
            // resolve types on the declared struct in case of nested
            // structs.
            let resolved_inner_type = state
                .declared_structs
                .get(struct_name)
                .expect("{type_name} not known to type checker");
            let resolved_inner_type = ast_types::DataType::Struct(resolved_inner_type.to_owned())
                .resolve_types(&state.declared_structs);
            let ret = ast_types::DataType::MemPointer(Box::new(resolved_inner_type));
            *resolved_type = Typing::KnownType(ret.clone());

            ret
        }
        ast::Expr::Lit(ast::ExprLit::GenericNum(n, _t)) => {
            use ast_types::DataType::*;

            match hint {
                Some(&U32) => {
                    let err = format!("Cannot infer number literal {n} as u32");
                    let n: u32 = (*n).try_into().expect(&err);
                    *expr = ast::Expr::Lit(ast::ExprLit::U32(n));
                    U32
                }
                Some(&U64) => {
                    *expr = ast::Expr::Lit(ast::ExprLit::U64(TryInto::<u64>::try_into(*n).unwrap()));
                    U64
                }
                Some(&BFE) => {
                    assert!(*n <= BFieldElement::MAX as u128);
                    let n = BFieldElement::new(TryInto::<u64>::try_into(*n).unwrap());
                    *expr = ast::Expr::Lit(ast::ExprLit::BFE(n));
                    BFE
                }
                Some(&XFE) => {
                    assert!(*n <= BFieldElement::MAX as u128);
                    let n = BFieldElement::new(TryInto::<u64>::try_into(*n).unwrap());
                    let n = XFieldElement::new_const(n);
                    *expr = ast::Expr::Lit(ast::ExprLit::XFE(n));
                    XFE
                }
                Some(hint) => panic!("GenericNum does not infer as type hint {hint}"),
                None => panic!("GenericNum does not infer in context with no type hint. Missing type hint for: {:?}", expr),
            }
        }

        ast::Expr::Var(identifier) => match identifier.resolved() {
            Some(ty) => ty,
            None => {
                annotate_identifier_type(identifier, state);
                identifier.get_type()
            }
        },

        ast::Expr::Tuple(tuple_exprs) => {
            let no_hint = None;
            let tuple_types: Vec<ast_types::DataType> = tuple_exprs
                .iter_mut()
                .map(|expr| derive_annotate_expr_type(expr, no_hint, state))
                .collect();
            ast_types::DataType::Tuple(tuple_types)
        }

        ast::Expr::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
            arg_evaluation_order,
        }) => {
            let fn_signature = get_fn_signature(name, state, type_parameter, args);
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
            derive_annotate_expr_type(&mut args[0], None, state);
            let receiver_type = args[0].get_type();
            let method_signature: ast::FnSignature =
                get_method_signature(method_name, state, receiver_type, args);
            assert!(
                !is_void_type(&method_signature.output),
                "Method calls in expressions cannot return the unit type"
            );

            // We don't need to check receiver type here since that is done by
            // `derive_annotate_fn_call_args` below.

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
                        type_compatible_with_not(&inner_expr_type),
                        "Cannot negate type '{inner_expr_type}'",
                    );
                    *unaryop_type = Typing::KnownType(inner_expr_type.clone());
                    inner_expr_type
                }
                Deref => {
                    match inner_expr_type {
                        ast_types::DataType::MemPointer(inner_inner_type) => {
                            *unaryop_type = Typing::KnownType(*inner_inner_type.clone());
                            *inner_inner_type
                        },
                        _ =>  panic!("Cannot dereference type of `{inner_expr_type}` as this expression has:\n{inner_expr:#?}")
                    }
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
                    let bool_hint = Some(&ast_types::DataType::Bool);
                    let lhs_type = derive_annotate_expr_type(lhs_expr, bool_hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, bool_hint, state);

                    assert_type_equals(&lhs_type, &ast_types::DataType::Bool, "and-expr lhs");
                    assert_type_equals(&rhs_type, &ast_types::DataType::Bool, "and-expr rhs");
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    ast_types::DataType::Bool
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
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    ast_types::DataType::Bool
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
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    ast_types::DataType::Bool
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
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    ast_types::DataType::Bool
                }

                // Restricted to bool only.
                Or => {
                    let lhs_type = derive_annotate_expr_type(lhs_expr, hint, state);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state);

                    assert_type_equals(&lhs_type, &ast_types::DataType::Bool, "or-expr lhs");
                    assert_type_equals(&rhs_type, &ast_types::DataType::Bool, "or-expr rhs");
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    ast_types::DataType::Bool
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

                    let rhs_hint = Some(&ast_types::DataType::U32);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, rhs_hint, state);

                    assert_type_equals(&rhs_type, &ast_types::DataType::U32, "shl-rhs-expr");
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

                    let rhs_hint = Some(&ast_types::DataType::U32);
                    let rhs_type = derive_annotate_expr_type(rhs_expr, rhs_hint, state);

                    assert_type_equals(&rhs_type, &ast_types::DataType::U32, "shr-rhs-expr");
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
            let condition_hint = ast_types::DataType::Bool;
            let condition_type = derive_annotate_expr_type(condition, Some(&condition_hint), state);
            assert_type_equals(
                &condition_type,
                &ast_types::DataType::Bool,
                "if-condition-expr",
            );

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
                || from_type == ast_types::DataType::Bool && is_arithmetic_type(to_type);

            assert!(valid_cast, "Cannot cast from {from_type} to {to_type}");

            to_type.to_owned()
        }
    }
}

pub fn is_string_identifier<T>(identifier: &ast::Identifier<T>) -> bool {
    matches!(identifier, ast::Identifier::String(_, _))
}

pub fn is_list_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, List(_, _))
}

/// A type that can be used as address in `read_mem` and `write_mem` calls.
///
/// Since memory addresses are essentially `BFieldElement`s, only types
/// that are subsets of `BFE`s can be used. The only such type is `U32`.
pub fn is_index_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | BFE)
}

/// A type for which basic arithmetic operators can be overloaded.
///
/// Note that not all operators work for all arithmetic types.
///
/// E.g. the bitwise operators only work for `is_u32_based_type()`.
pub fn is_arithmetic_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | U64 | BFE | XFE)
}

/// A type from which expressions such as `-value` can be formed
pub fn is_negatable_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, BFE | XFE)
}

/// A type from which expressions such as `!value` can be formed
pub fn type_compatible_with_not(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, Bool | U32 | U64)
}

/// A type that is implemented in terms of `U32` values.
///
/// E.g. `U32` and `U64`.
pub fn is_u32_based_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | U64)
}

/// A non-composite fixed-length type.
pub fn is_primitive_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, Bool | U32 | U64 | BFE | XFE | Digest)
}

pub fn is_void_type(data_type: &ast_types::DataType) -> bool {
    if let ast_types::DataType::Tuple(tys) = data_type {
        tys.is_empty()
    } else {
        false
    }
}
