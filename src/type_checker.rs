use anyhow::bail;
use itertools::Itertools;
use num::One;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast::TupleDestructStmt;
use crate::ast::{MethodCall, UnaryOp};
use crate::ast_types;
use crate::composite_types::CompositeTypes;
use crate::libraries;
use crate::tasm_code_generator::SIZE_OF_ACCESSIBLE_STACK;
use crate::type_checker::Typing::KnownType;

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Typing {
    /// An `UnknownType` has not been determined; this is produced by the parser/grafter.
    #[default]
    UnknownType,

    /// A `KnownType` has been determined; this is performed by the type checker.
    KnownType(ast_types::DataType),
}

impl GetType for Typing {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            Typing::UnknownType => panic!("Cannot unpack type before complete type annotation."),
            Typing::KnownType(data_type) => data_type.clone(),
        }
    }
}

pub(crate) trait GetType {
    fn get_type(&self) -> ast_types::DataType;
}

impl<T: GetType> GetType for ast::ExprLit<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::ExprLit::Bool(_) => ast_types::DataType::Bool,
            ast::ExprLit::U32(_) => ast_types::DataType::U32,
            ast::ExprLit::U64(_) => ast_types::DataType::U64,
            ast::ExprLit::U128(_) => ast_types::DataType::U128,
            ast::ExprLit::Bfe(_) => ast_types::DataType::Bfe,
            ast::ExprLit::Xfe(_) => ast_types::DataType::Xfe,
            ast::ExprLit::Digest(_) => ast_types::DataType::Digest,
            ast::ExprLit::GenericNum(_, t) => t.get_type(),
        }
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::Expr<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::Expr::Lit(lit) => lit.get_type(),
            ast::Expr::Var(id) => id.get_type(),
            ast::Expr::Array(_, t) => t.get_type(),
            ast::Expr::Tuple(t_list) => ast_types::DataType::Tuple(
                t_list
                    .iter()
                    .map(|elem| elem.get_type())
                    .collect_vec()
                    .into(),
            ),
            ast::Expr::Struct(struct_expr) => struct_expr.get_type(),
            ast::Expr::EnumDeclaration(enum_decl) => enum_decl.get_type(),
            ast::Expr::FnCall(fn_call) => fn_call.get_type(),
            ast::Expr::MethodCall(method_call) => method_call.get_type(),
            ast::Expr::Binop(_, _, _, t) => t.get_type(),
            ast::Expr::If(if_expr) => if_expr.get_type(),
            ast::Expr::Cast(_expr, t) => t.to_owned(),
            ast::Expr::Unary(_, _, t) => t.get_type(),
            ast::Expr::ReturningBlock(ret_block) => ret_block.get_type(),
            ast::Expr::Match(match_expr) => match_expr.arms.first().unwrap().body.get_type(),
            ast::Expr::Panic(_, t) => t.get_type(),
            ast::Expr::MemoryLocation(ast::MemPointerExpression { resolved_type, .. }) => {
                resolved_type.get_type()
            }
        }
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::ReturningBlock<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.return_expr.get_type()
    }
}

impl GetType for ast::EnumDeclaration {
    fn get_type(&self) -> ast_types::DataType {
        self.enum_type.clone()
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::StructExpr<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.struct_type.clone()
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::ExprIf<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.then_branch.get_type()
    }
}

impl<T: GetType + std::fmt::Debug> GetType for ast::MatchExpr<T> {
    fn get_type(&self) -> ast_types::DataType {
        self.arms.first().unwrap().body.get_type()
    }
}

impl<T: GetType> GetType for ast::Identifier<T> {
    fn get_type(&self) -> ast_types::DataType {
        match self {
            ast::Identifier::String(_, t) => t.get_type(),
            ast::Identifier::Index(_, _, t) => t.get_type(),
            ast::Identifier::Field(_, _, t) => t.get_type(),
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

#[derive(Debug, Clone)]
pub(crate) struct CheckState<'a> {
    pub(crate) libraries: &'a [Box<dyn libraries::Library>],

    /// The `vtable` maps variable names to their type.
    ///
    /// This is used for determining the type of variables in expressions.
    pub(crate) vtable: HashMap<String, DataTypeAndMutability>,

    /// The `ftable` maps function names to their signature (argument and output) types.
    ///
    /// This is used for determining the type of function calls in expressions.
    pub(crate) ftable: HashMap<String, Vec<ast::FnSignature>>,

    /// All non-atomic types that are in scope
    pub(crate) composite_types: &'a CompositeTypes,
}

#[derive(Clone, Debug)]
pub(crate) struct DataTypeAndMutability {
    pub(crate) data_type: ast_types::DataType,
    pub(crate) mutable: bool,
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
    pub(crate) fn new(data_type: &ast_types::DataType, mutable: bool) -> Self {
        Self {
            data_type: data_type.to_owned(),
            mutable,
        }
    }
}

// TODO: Delete `annotate_method`, use `annotate_function` instead
fn annotate_method(
    method: &mut ast::Method<Typing>,
    composite_types: &CompositeTypes,
    libraries: &[Box<dyn libraries::Library>],
    ftable: HashMap<String, Vec<ast::FnSignature>>,
) {
    // Initialize `CheckState`
    let vtable: HashMap<String, DataTypeAndMutability> =
        HashMap::with_capacity(method.signature.args.len());

    // Insert self into ftable
    let mut state = CheckState {
        libraries,
        vtable,
        ftable,
        composite_types,
    };

    // Populate vtable with function arguments
    for arg in method.signature.args.iter() {
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
        method.signature.input_arguments_stack_size() < SIZE_OF_ACCESSIBLE_STACK,
        "{}: Cannot handle method signatures with input size exceeding {} words",
        method.signature.name,
        SIZE_OF_ACCESSIBLE_STACK - 1
    );

    if let ast::RoutineBody::Ast(stmts) = &mut method.body {
        // Verify that last statement of function exists, and that it is a `return` statement
        let last_stmt = stmts.iter().last().unwrap_or_else(|| {
            panic!(
                "Method `{}`: Function cannot be emtpy.",
                method.signature.name
            )
        });
        assert!(
            matches!(last_stmt, ast::Stmt::Return(_)),
            "Method: `{}`: Last line of method must be a `return`",
            method.signature.name
        );

        // Type-annotate each statement in-place
        stmts
            .iter_mut()
            .for_each(|stmt| annotate_stmt(stmt, &mut state, &method.signature));
    }
}

fn annotate_fn_inner(
    function: &mut ast::Fn<Typing>,
    composite_types: &CompositeTypes,
    libraries: &[Box<dyn libraries::Library>],
    mut ftable: HashMap<String, Vec<ast::FnSignature>>,
) {
    // Initialize `CheckState`
    let vtable: HashMap<String, DataTypeAndMutability> =
        HashMap::with_capacity(function.signature.args.len());

    // Insert self into ftable
    ftable.insert(
        function.signature.name.clone(),
        vec![function.signature.clone()],
    );
    let mut state = CheckState {
        libraries,
        vtable,
        ftable,
        composite_types,
    };

    // Populate vtable with function arguments
    for arg in function.signature.args.iter() {
        match arg {
            ast_types::AbstractArgument::FunctionArgument(_) => todo!(),
            ast_types::AbstractArgument::ValueArgument(value_fn_arg) => {
                let duplicate_fn_arg = state
                    .vtable
                    .insert(value_fn_arg.name.clone(), value_fn_arg.to_owned().into())
                    .is_some();
                if duplicate_fn_arg {
                    panic!(
                        "Duplicate function argument {} in function \"{}\"",
                        value_fn_arg.name, function.signature.name
                    );
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
        "Function `{}`: Cannot handle function signatures with input size exceeding {} words",
        function.signature.name,
        SIZE_OF_ACCESSIBLE_STACK - 1
    );

    if let ast::RoutineBody::Ast(stmts) = &mut function.body {
        // Verify that last statement of function exists, and that it is a `return` statement
        let last_stmt = stmts.iter().last().unwrap_or_else(|| {
            panic!(
                "Function `{}`: Function cannot be emtpy.",
                function.signature.name
            )
        });
        assert!(
            matches!(last_stmt, ast::Stmt::Return(_)),
            "Function `{}`: Last line of function must be a `return` statement.",
            function.signature.name
        );

        // Type-annotate each statement in-place
        stmts
            .iter_mut()
            .for_each(|stmt| annotate_stmt(stmt, &mut state, &function.signature));
    }
}

pub(crate) fn annotate_fn_outer(
    function: &mut ast::Fn<Typing>,
    composite_types: &mut CompositeTypes,
    libraries: &[Box<dyn libraries::Library>],
) {
    // Populate `ftable` with tuple constructors, allowing initialization of tuple structs
    // using `struct Foo(u32); let a = Foo(200);` as well as data-carrying enum-type
    // variants such as `Bar::A(200u32);`.
    // TODO: `ftable` probably needs a type parameter as well, so we can call
    // both `Ok(1u32)` and `Ok(1u64)`.
    let ftable = composite_types.get_all_constructor_signatures();

    // Type annotate the function
    let ftable_outer = ftable.clone();
    annotate_fn_inner(function, composite_types, libraries, ftable);

    // Type annotate all declared methods and associated functions
    let composite_type_copy = composite_types.clone();
    composite_types.methods_mut().for_each(|method| {
        annotate_method(
            method,
            &composite_type_copy,
            libraries,
            ftable_outer.clone(),
        );
    });
    composite_types.associated_functions_mut().for_each(|func| {
        annotate_fn_inner(func, &composite_type_copy, libraries, ftable_outer.clone());
    });
}

fn annotate_stmt(
    stmt: &mut ast::Stmt<Typing>,
    state: &mut CheckState,
    env_fn_signature: &ast::FnSignature,
) {
    match stmt {
        // `let a: u32 = 4;`
        ast::Stmt::Let(ast::LetStmt {
            var_name,
            data_type,
            expr,
            mutable,
        }) => {
            if state.vtable.contains_key(var_name) {
                panic!("let-assign cannot shadow existing variable '{var_name}'!");
            }

            let let_expr_hint: Option<ast_types::DataType> = Some(data_type.to_owned());
            let derived_type =
                derive_annotate_expr_type(expr, let_expr_hint, state, env_fn_signature).unwrap();
            assert_type_equals(
                &derived_type,
                data_type,
                format!("let-statement of {var_name}"),
            );
            state.vtable.insert(
                var_name.clone(),
                DataTypeAndMutability::new(data_type, *mutable),
            );
        }

        // `let Digest([d0, d1, d2, d3, d4]) = digest;`
        ast::Stmt::TupleDestructuring(TupleDestructStmt { ident, bindings }) => {
            annotate_identifier_type(
                ident,
                Some(ast_types::DataType::Digest),
                state,
                env_fn_signature,
            );
            let ast::Identifier::String(binding_name, _) = &ident else {
                panic!("Can only destructure bindings for now");
            };
            assert_type_equals(
                &ident.get_type(),
                &ast_types::DataType::Digest,
                "bindings destructuring",
            );
            state.vtable.remove(binding_name);
            for binding in bindings.iter() {
                state.vtable.insert(
                    binding.name.to_string(),
                    DataTypeAndMutability::new(&ast_types::DataType::Bfe, binding.mutable),
                );
            }
        }

        // `a = 4;`, where `a` is declared as `mut`
        ast::Stmt::Assign(ast::AssignStmt { identifier, expr }) => {
            let (identifier_type, mutable, new_expr) =
                annotate_identifier_type(identifier, None, state, env_fn_signature);
            assert!(
                new_expr.is_none(),
                "Cannot rewrite expression outside of atomic variable expression"
            );
            let assign_expr_hint = &identifier_type;
            let expr_type = derive_annotate_expr_type(
                expr,
                Some(assign_expr_hint.to_owned()),
                state,
                env_fn_signature,
            )
            .unwrap();

            // Only allow assignment if binding was declared as mutable
            assert_type_equals(&identifier_type, &expr_type, "assign-statement");
            assert!(
                mutable,
                "Cannot re-assign non-mutable binding: {identifier}"
            )
        }

        ast::Stmt::Return(opt_expr) => match (opt_expr, &env_fn_signature.output) {
            (None, ast_types::DataType::Tuple(tys)) => assert_eq!(
                0,
                tys.element_count(),
                "Return without value; expect {} to return nothing.",
                env_fn_signature.name
            ),
            (None, _) => {
                panic!(
                    "Return without value; expect function {} to return {}",
                    env_fn_signature.name, env_fn_signature.output
                )
            }
            (Some(ret_expr), _) => {
                let hint = env_fn_signature.output.to_owned();
                let expr_ret_type =
                    derive_annotate_expr_type(ret_expr, Some(hint), state, env_fn_signature)
                        .unwrap();
                assert_type_equals(&env_fn_signature.output, &expr_ret_type, "return stmt");
            }
        },

        ast::Stmt::FnCall(ast::FnCall {
            name,
            args,
            annot,
            type_parameter,
            arg_evaluation_order,
        }) => {
            // Attempt to annotate all arguments before getting the function signature
            for arg in args.iter_mut() {
                // It's OK if this fails, as a later invocation of the argument expressions
                // might pass.
                let _ = derive_annotate_expr_type(arg, None, state, env_fn_signature);
            }

            let callees_fn_signature =
                get_fn_signature(name, state, type_parameter, args, env_fn_signature, None);
            assert!(
                callees_fn_signature.output.is_unit(),
                "Function call '{name}' at statement-level must return the unit type."
            );

            derive_annotate_fn_call_args(&callees_fn_signature, args, state);

            *arg_evaluation_order = callees_fn_signature.arg_evaluation_order;
            *annot = Typing::KnownType(callees_fn_signature.output);
        }

        ast::Stmt::MethodCall(method_call) => {
            // Attempt to annotate all arguments before getting the function signature
            for arg in method_call.args.iter_mut() {
                // It's OK if this fails, as a later invocation of the argument expressions
                // might pass.
                let _ = derive_annotate_expr_type(arg, None, state, env_fn_signature);
            }

            let receiver_type = method_call.args[0].get_type();

            let callees_method_signature: ast::FnSignature =
                get_method_signature(state, method_call, env_fn_signature);
            assert!(
                callees_method_signature.output.is_unit(),
                "Method call {receiver_type}.'{}' at statement-level must return the unit type.",
                method_call.method_name
            );

            // TODO: Check that receiver_type corresponds to method's FnSignature

            derive_annotate_fn_call_args(&callees_method_signature, &mut method_call.args, state);

            method_call.annot = Typing::KnownType(callees_method_signature.output)
        }

        ast::Stmt::While(ast::WhileStmt { condition, block }) => {
            let condition_hint = ast_types::DataType::Bool;
            let condition_type =
                derive_annotate_expr_type(condition, Some(condition_hint), state, env_fn_signature)
                    .unwrap();
            assert_type_equals(
                &condition_type,
                &ast_types::DataType::Bool,
                "while-condition",
            );
            annotate_block_stmt(block, env_fn_signature, state);
        }

        ast::Stmt::If(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
        }) => {
            let condition_hint = ast_types::DataType::Bool;
            let condition_type =
                derive_annotate_expr_type(condition, Some(condition_hint), state, env_fn_signature)
                    .unwrap();
            assert_type_equals(&condition_type, &ast_types::DataType::Bool, "if-condition");

            annotate_block_stmt(then_branch, env_fn_signature, state);
            annotate_block_stmt(else_branch, env_fn_signature, state);
        }

        ast::Stmt::Match(ast::MatchStmt {
            arms,
            match_expression,
        }) => {
            // Verify that match-expression returns an enum-type
            let match_expression_type =
                derive_annotate_expr_type(match_expression, None, state, env_fn_signature).unwrap();
            let (enum_type, is_boxed) = match match_expression_type {
                ast_types::DataType::Enum(enum_type) => (enum_type, false),
                ast_types::DataType::Boxed(inner) => match *inner.to_owned() {
                    ast_types::DataType::Enum(enum_type) => (enum_type, true),
                    other => panic!(
                        "`match` statements are only supported on enum types. For now.\
                        Got {other}"
                    ),
                },
                _ => panic!(
                    "`match` statements are only supported on enum types. For now.\
                    Got {match_expression_type}"
                ),
            };

            let mut variants_encountered: HashSet<String> = HashSet::default();
            let arm_count = arms.len();
            let mut contains_catch_all_arm = false;
            for (i, arm) in arms.iter_mut().enumerate() {
                match &arm.match_condition {
                    ast::MatchCondition::CatchAll => {
                        assert_eq!(
                            i,
                            arm_count - 1,
                            "When using catch_all in match statement, catch_all must be used \
                            in last match arm. Match expression was for type {}",
                            enum_type.name
                        );
                        contains_catch_all_arm = true;

                        annotate_block_stmt(&mut arm.body, env_fn_signature, state);
                    }
                    ast::MatchCondition::EnumVariant(ast::EnumVariantSelector {
                        type_name,
                        variant_name,
                        data_bindings,
                    }) => {
                        assert!(
                            variants_encountered.insert(variant_name.clone()),
                            "Repeated variant name {} in match statement on {} encounter",
                            variant_name,
                            enum_type.name
                        );

                        match type_name {
                            Some(enum_type_name) => {
                                assert_eq!(
                                    &enum_type.name, enum_type_name,
                                    "Match conditions on type {} must all be of same type. \
                                    Got bad type: {enum_type_name}",
                                    enum_type.name
                                );
                            }
                            None => {
                                assert!(
                                    enum_type.is_prelude,
                                    "Only enums specified in prelude may \
                                    use only the variant name in a match arm"
                                );
                            }
                        };

                        let variant_data_tuple =
                            enum_type.variant_data_type(variant_name).as_tuple_type();
                        assert!(data_bindings.is_empty() || variant_data_tuple.element_count() == data_bindings.len(), "Number of bindings must match number of elements in variant data tuple");
                        assert!(
                            data_bindings.iter().map(|x| &x.name).all_unique(),
                            "Name repetition in pattern matching not allowed"
                        );

                        data_bindings.iter().enumerate().for_each(|(i, x)| {
                            let new_binding_type = if is_boxed {
                                ast_types::DataType::Boxed(Box::new(
                                    variant_data_tuple.fields[i].to_owned(),
                                ))
                            } else {
                                variant_data_tuple.fields[i].to_owned()
                            };

                            state.vtable.insert(
                                x.name.to_owned(),
                                DataTypeAndMutability::new(&new_binding_type, x.mutable),
                            );
                        });

                        annotate_block_stmt(&mut arm.body, env_fn_signature, state);

                        // Remove bindings set in match-arm after body's type check
                        data_bindings.iter().for_each(|x| {
                            state.vtable.remove(&x.name);
                        });
                    }
                }
            }

            // Verify that all cases where covered, *or* a catch_all was encountered.
            assert!(
                variants_encountered.len() == enum_type.variants.len() || contains_catch_all_arm,
                "All cases must be covered for match-expression for {}. Missing variants: {}.",
                enum_type.name,
                enum_type
                    .variants
                    .iter()
                    .map(|x| &x.0)
                    .filter(|x| !variants_encountered.contains(*x))
                    .join(",")
            );
        }

        ast::Stmt::Block(block_stmt) => {
            annotate_block_stmt(block_stmt, env_fn_signature, state);
        }
        ast::Stmt::Assert(assert_expr) => {
            let expr_type = derive_annotate_expr_type(
                &mut assert_expr.expression,
                Some(ast_types::DataType::Bool),
                state,
                env_fn_signature,
            )
            .unwrap();
            assert_type_equals(&expr_type, &ast_types::DataType::Bool, "assert expression");
        }
        ast::Stmt::Panic(_) => (),
        ast::Stmt::FnDeclaration(function) => {
            // A local function can see all functions available in the outer scope.
            annotate_fn_inner(
                function,
                state.composite_types,
                state.libraries,
                state.ftable.clone(),
            );
            state.ftable.insert(
                function.signature.name.clone(),
                vec![function.signature.clone()],
            );
        }
    }
}

fn annotate_block_stmt(
    block: &mut ast::BlockStmt<Typing>,
    fn_signature: &ast::FnSignature,
    state: &mut CheckState,
) {
    let vtable_before = state.vtable.clone();
    block
        .stmts
        .iter_mut()
        .for_each(|stmt| annotate_stmt(stmt, state, fn_signature));
    state.vtable = vtable_before;
}

pub(crate) fn assert_type_equals(
    derived_type: &ast_types::DataType,
    data_type: &ast_types::DataType,
    context: impl Display,
) {
    if derived_type != data_type {
        panic!(
            "Type mismatch between type\n'{data_type:?}'\n and derived type\n'{derived_type:?}'\n for {context}",
        );
    }
}

/// Set type and return (type, mutable, new_expr) tuple. `new_expr` indicates that the
/// type of the node in the AST should be changed from identifier to something else.
pub(crate) fn annotate_identifier_type(
    identifier: &mut ast::Identifier<Typing>,
    hint: Option<ast_types::DataType>,
    state: &mut CheckState,
    fn_signature: &ast::FnSignature,
) -> (ast_types::DataType, bool, Option<ast::Expr<Typing>>) {
    match identifier {
        // x
        ast::Identifier::String(var_name, var_type) => {
            match state.vtable.get(var_name) {
                Some(found_type) => {
                    // Identifier is a declared variable
                    *var_type = Typing::KnownType(found_type.data_type.clone());
                    (found_type.data_type.clone(), found_type.mutable, None)
                }
                None => match state.ftable.get(var_name) {
                    // Identifier is a declared function
                    Some(functions) => {
                        assert!(
                            functions.len().is_one(),
                            "Duplicate function name: {var_name}"
                        );
                        let function = functions[0].clone();
                        *var_type = Typing::KnownType(function.clone().into());
                        let function_datatype: ast_types::DataType = function.into();
                        (function_datatype, false, None)
                    }
                    None => {
                        // Identifier is variant type declared in `prelude` without associated data, like `None`.
                        // Here, the identifier must be rewritten to an `EnumDeclaration` by the caller.
                        let type_hint = match hint {
                        Some(ty) => ty,
                        None => panic!("type of variable or identifier \"{var_name}\" could not be resolved"),
                    };
                        let prelude_type = state
                            .composite_types
                            .prelude_variant_match(var_name, &type_hint).expect("type of variable or identifier \"{var_name}\" could not be resolved");
                        *var_type = Typing::KnownType(prelude_type.clone().into());

                        (
                            prelude_type.clone().into(),
                            false,
                            Some(ast::Expr::EnumDeclaration(ast::EnumDeclaration {
                                enum_type: prelude_type.into(),
                                variant_name: var_name.to_owned(),
                            })),
                        )
                    }
                },
            }
        }

        // x[e]
        ast::Identifier::Index(list_identifier, index_expr, known_type) => {
            let index_hint = ast_types::DataType::U32;
            let index_type = match index_expr.as_mut() {
                ast::IndexExpr::Dynamic(index_expr) => {
                    derive_annotate_expr_type(index_expr, Some(index_hint), state, fn_signature)
                        .unwrap()
                }
                ast::IndexExpr::Static(_) => ast_types::DataType::U32,
            };
            if !is_index_type(&index_type) {
                panic!("Cannot index list with type '{index_type}'");
            }

            // Only `Vec<T>` and `[T; n]` can be indexed, so if type is e.g. `MemPointer(Vec<T>)`, then the
            // type of `a` need to be forced to `Vec<T>`.
            // TODO: It's possible that the type of `list_identifier` needs to be forced to. But to
            // do that, this function probably needs the expression, and not just the identifier.
            let (maybe_list_type, mutable, rewrite_expr) =
                annotate_identifier_type(list_identifier, None, state, fn_signature);
            assert!(
                rewrite_expr.is_none(),
                "Cannot rewrite expression outside of atomic variable expression, like `a`."
            );
            let mut forced_sequence_type = maybe_list_type.clone();
            let element_type = loop {
                if let ast_types::DataType::List(elem_ty, _) = &forced_sequence_type {
                    break elem_ty;
                } else if let ast_types::DataType::Array(array_type) = &forced_sequence_type {
                    //Ensure that this value is not out-of-bounds, *if* this can be checked.
                    let statically_known_index = match index_expr.as_ref() {
                        ast::IndexExpr::Dynamic(_index_expr) => None,
                        ast::IndexExpr::Static(index) => Some(index),
                    };
                    if let Some(x) = statically_known_index {
                        assert!(
                            *x < array_type.length,
                            "Index for array out-of-range. Index was {x}, length is {}",
                            array_type.length
                        )
                    }

                    break &array_type.element_type;
                } else if let ast_types::DataType::Boxed(inner_type) = forced_sequence_type {
                    forced_sequence_type = *inner_type.to_owned();
                } else {
                    panic!("Cannot index into {list_identifier} of type {maybe_list_type}");
                }
            };

            // If list_identifier is a MemPointer, and the element type is not copyable,
            // then the element type stays a MemPointer.
            let element_type = if let ast_types::DataType::Boxed(_) = maybe_list_type {
                if element_type.is_copy() {
                    *element_type.to_owned()
                } else {
                    ast_types::DataType::Boxed(element_type.to_owned())
                }
            } else {
                *element_type.to_owned()
            };

            list_identifier.force_type(&forced_sequence_type);
            *known_type = Typing::KnownType(element_type.clone());

            (element_type, mutable, None)
        }

        // x.foo
        ast::Identifier::Field(ident, field_id, annot) => {
            let (receiver_type, mutable, rewrite_expr) =
                annotate_identifier_type(ident, None, state, fn_signature);
            assert!(
                rewrite_expr.is_none(),
                "Cannot rewrite expression outside of atomic variable expression"
            );

            // Only structs have fields, so receiver_type must be a struct, or a pointer to a struct.
            let field_type = receiver_type.field_access_returned_type(field_id);
            *annot = Typing::KnownType(field_type.clone());

            (field_type, mutable, None)
        }
    }
}

fn get_fn_signature(
    name: &str,
    state: &CheckState,
    type_parameter: &Option<ast_types::DataType>,
    args: &[ast::Expr<Typing>],
    env_fn_signature: &ast::FnSignature,
    output_type_hint: Option<ast_types::DataType>,
) -> ast::FnSignature {
    if let Some(fn_signatures) = state.ftable.get(name) {
        match fn_signatures.len() {
            0 => unreachable!("Function signature list must never be empty."),
            1 => return fn_signatures[0].to_owned(),
            _ => (), // continue
        }

        let types = args.iter().map(|arg| arg.get_type()).collect_vec();
        for sig in fn_signatures.iter().filter(|sig| sig.matches(&types)) {
            let Some(hint) = &output_type_hint else {
                return sig.to_owned();
            };
            if sig.output == *hint {
                return sig.to_owned();
            }
        }

        todo!("\nname: {name}\ntype_parameter: {type_parameter:#?}")
        // <-- We need to inspect types for this case
    }

    if let Some(afunc) = state.composite_types.associated_function_signature(name) {
        return afunc.to_owned();
    }

    // Function from libraries are in scope
    for lib in state.libraries.iter() {
        if let Some(fn_name) = lib.get_function_name(name) {
            return lib.function_name_to_signature(&fn_name, type_parameter.to_owned(), args);
        }
    }

    panic!(
        "Function call in {} – Don't know return type of \"{name}\"! \
        Type parameter: {type_parameter:?}. \
        ftable:\n{}",
        env_fn_signature.name,
        state.ftable.keys().join("\n")
    )
}

fn get_method_signature(
    state: &CheckState,
    method_call: &mut MethodCall<Typing>,
    env_fn_signature: &ast::FnSignature,
) -> ast::FnSignature {
    // Implemented following the description from: https://stackoverflow.com/a/28552082/2574407
    // TODO: Handle automatic dereferencing and referencing of MemPointer types
    let original_receiver_type = method_call.args[0].get_type();
    let receiver = &mut method_call.args[0];
    let mut forced_type = original_receiver_type.clone();
    let mut try_again = true;
    while try_again {
        // if there's a method `bar` where the receiver type (the type of self
        // in the method) matches `forced_type` exactly , use it (a "by value method")
        let dereferenced_type = forced_type.unbox();
        if let Some(comp_type) = state.composite_types.get_by_type(&dereferenced_type) {
            if let Some(method) = comp_type.get_method(&method_call.method_name) {
                method_call.associated_type = Some(dereferenced_type);
                if method.receiver_type() == forced_type {
                    return method.signature.to_owned();
                }

                let auto_boxed_forced_type =
                    ast_types::DataType::Boxed(Box::new(forced_type.clone()));
                if method.receiver_type() == auto_boxed_forced_type {
                    return method.signature.to_owned();
                }
            }
        };

        for lib in state.libraries.iter() {
            if let Some(method_name) = lib.get_method_name(&method_call.method_name, &forced_type) {
                method_call.associated_type = Some(forced_type.clone());
                return lib.method_name_to_signature(
                    &method_name,
                    &forced_type,
                    &method_call.args,
                    state,
                );
            }
        }

        // Keep stripping `Box` until we find a match
        if let ast_types::DataType::Boxed(inner_type) = &forced_type {
            forced_type = *inner_type.to_owned();
            *receiver = ast::Expr::Unary(
                UnaryOp::Deref,
                Box::new(receiver.to_owned()),
                KnownType(forced_type.clone()),
            );
        } else {
            try_again = false;
        }
    }

    let declared_types = state.composite_types.all_composite_type_names();
    let declared_method_names = state.composite_types.all_method_names();
    panic!(
        "Method call in '{}' Don't know what type of value '{}' returns!
         Receiver type was: {original_receiver_type:?}
         \n\nDeclared methods are:\n{declared_method_names}\n\n \
         Declared types are:\n{declared_types}\n\n",
        env_fn_signature.name, method_call.method_name,
    );
}

fn derive_annotate_fn_call_args(
    callees_fn_signature: &ast::FnSignature,
    args: &mut [ast::Expr<Typing>],
    state: &mut CheckState,
) {
    let fn_name = &callees_fn_signature.name;
    assert_eq!(
        callees_fn_signature.args.len(),
        args.len(),
        "Wrong number of arguments in function call to '{}'; expected {} arguments, got {}.",
        fn_name,
        callees_fn_signature.args.len(),
        args.len(),
    );

    // Get list of concrete arguments used in function call
    let concrete_arguments: Vec<ast_types::DataType> = args
        .iter_mut()
        .zip_eq(callees_fn_signature.args.iter())
        .map(|(arg_expr, fn_arg)| match fn_arg {
            ast_types::AbstractArgument::FunctionArgument(abstract_function) => {
                // arg_expr must evaluate to a FnCall here

                let arg_hint = Some(abstract_function.function_type.input_argument.clone());
                derive_annotate_expr_type(arg_expr, arg_hint, state, callees_fn_signature).unwrap()
            }
            ast_types::AbstractArgument::ValueArgument(abstract_value) => {
                let arg_hint = Some(abstract_value.data_type.clone());
                derive_annotate_expr_type(arg_expr, arg_hint, state, callees_fn_signature).unwrap()
            }
        })
        .collect();

    // Compare list of concrete arguments with function signature, i.e. expected arguments
    for (arg_pos, (fn_arg, expr_type)) in callees_fn_signature
        .args
        .iter()
        .zip_eq(concrete_arguments.iter())
        .enumerate()
    {
        match fn_arg {
            ast_types::AbstractArgument::FunctionArgument(ast_types::AbstractFunctionArg {
                function_type,
                ..
            }) => assert_type_equals(
                &ast_types::DataType::Function(Box::new(function_type.to_owned())),
                expr_type,
                "Function argument",
            ),
            ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
                name: arg_name,
                data_type: arg_type,
                ..
            }) => {
                let arg_pos = arg_pos + 1;
                assert_eq!(
                    arg_type, expr_type,
                    "Wrong type of function argument {arg_pos} function call '{arg_name}' in \
                    '{fn_name}'\n \n\
                    expected type \"{arg_type}\", but got type  \"{expr_type}\"\n\n",
                );
            }
        }
    }
}

/// Annotate an expression. This function can return error, if the type information
/// is not yet complete. If it returns an error, later invocations of this function,
/// with specified type hints might pass.
fn derive_annotate_expr_type(
    expr: &mut ast::Expr<Typing>,
    hint: Option<ast_types::DataType>,
    state: &mut CheckState,
    env_fn_signature: &ast::FnSignature,
) -> anyhow::Result<ast_types::DataType> {
    let res = match expr {
        ast::Expr::Panic(_, resolved_type) => {
            if let Some(h) = hint {
                *resolved_type = Typing::KnownType(h.to_owned());
                Ok(h.to_owned())
            } else {
                bail!("Cannot resolve `Never` type without type hint")
            }
        }
        ast::Expr::Lit(ast::ExprLit::Bool(_)) => Ok(ast_types::DataType::Bool),
        ast::Expr::Lit(ast::ExprLit::U32(_)) => Ok(ast_types::DataType::U32),
        ast::Expr::Lit(ast::ExprLit::U64(_)) => Ok(ast_types::DataType::U64),
        ast::Expr::Lit(ast::ExprLit::U128(_)) => Ok(ast_types::DataType::U128),
        ast::Expr::Lit(ast::ExprLit::Bfe(_)) => Ok(ast_types::DataType::Bfe),
        ast::Expr::Lit(ast::ExprLit::Xfe(_)) => Ok(ast_types::DataType::Xfe),
        ast::Expr::Lit(ast::ExprLit::Digest(_)) => Ok(ast_types::DataType::Digest),
        ast::Expr::Lit(ast::ExprLit::GenericNum(n, _t)) => {
            use ast_types::DataType::*;

            match hint.as_ref() {
                Some(&U32) => {
                    let err = format!("Cannot infer number literal {n} as u32");
                    let n: u32 = (*n).try_into().expect(&err);
                    *expr = ast::Expr::Lit(ast::ExprLit::U32(n));
                    Ok(U32)
                }
                Some(&U64) => {
                    *expr =
                        ast::Expr::Lit(ast::ExprLit::U64(TryInto::<u64>::try_into(*n).unwrap()));
                    Ok(U64)
                }
                Some(&U128) => {
                    *expr =
                        ast::Expr::Lit(ast::ExprLit::U128(TryInto::<u128>::try_into(*n).unwrap()));
                    Ok(U128)
                }
                Some(&Bfe) => {
                    assert!(*n <= BFieldElement::MAX as u128);
                    let n = BFieldElement::new(TryInto::<u64>::try_into(*n).unwrap());
                    *expr = ast::Expr::Lit(ast::ExprLit::Bfe(n));
                    Ok(Bfe)
                }
                Some(&Xfe) => {
                    assert!(*n <= BFieldElement::MAX as u128);
                    let n = BFieldElement::new(TryInto::<u64>::try_into(*n).unwrap());
                    let n = XFieldElement::new_const(n);
                    *expr = ast::Expr::Lit(ast::ExprLit::Xfe(n));
                    Ok(Xfe)
                }
                Some(hint) => panic!("GenericNum does not infer as type hint {hint}"),
                None => bail!(
                    "GenericNum does not infer in context with no type hint. \
                    Missing type hint for: {expr}"
                ),
            }
        }

        ast::Expr::Var(identifier) => match identifier.resolved() {
            Some(ty) => Ok(ty.to_owned()),
            None => {
                let (identifier_type, _mutable, new_expr) =
                    annotate_identifier_type(identifier, hint, state, env_fn_signature);
                if let Some(new_expr) = new_expr {
                    *expr = new_expr;
                }

                Ok(identifier_type)
            }
        },

        ast::Expr::Tuple(tuple_exprs) => {
            let no_hint = None;
            let tuple_types = tuple_exprs
                .iter_mut()
                .map(|expr| {
                    derive_annotate_expr_type(expr, no_hint.clone(), state, env_fn_signature)
                })
                .collect_vec();
            if tuple_types.iter().all(|x| x.is_ok()) {
                Ok(ast_types::DataType::Tuple(
                    tuple_types
                        .into_iter()
                        .map(|x| x.unwrap())
                        .collect_vec()
                        .into(),
                ))
            } else {
                bail!("Missing types in tuple")
            }
        }

        ast::Expr::Array(array_expr, array_type) => {
            let resolved_element_type = array_element_type(state, env_fn_signature, array_expr)?;
            let element_type = resolved_element_type
                .expect("Cannot derive type for empty array")
                .to_owned();

            let derived_type = ast_types::DataType::Array(ast_types::ArrayType {
                element_type: Box::new(element_type),
                length: array_expr.len(),
            });
            *array_type = Typing::KnownType(derived_type.clone());
            Ok(derived_type)
        }

        ast::Expr::Struct(struct_expr) => {
            let struct_type =
                if let ast_types::DataType::Struct(struct_type) = &struct_expr.struct_type {
                    struct_type.to_owned()
                } else {
                    panic!("Type in struct declaration must be Struct type");
                };
            let struct_type =
                if let ast_types::StructVariant::NamedFields(named_fields) = &struct_type.variant {
                    named_fields.to_owned()
                } else {
                    panic!("Type in struct declaration must be struct with named fields");
                };

            // Perform type-checking of each field, ensure all fields are defined
            let mut remaining_fields = struct_type.fields.clone();
            for (field_name, expr) in struct_expr.field_names_and_values.iter_mut() {
                let field_id = ast_types::FieldId::NamedField(field_name.to_owned());
                let expected_field_type = struct_expr
                    .struct_type
                    .field_access_returned_type(&field_id);
                let derived_field_type = derive_annotate_expr_type(
                    expr,
                    Some(expected_field_type.clone()),
                    state,
                    env_fn_signature,
                )
                .expect("Must know result type here");
                assert_type_equals(
                    &derived_field_type,
                    &expected_field_type,
                    &format!("Struct initialization of {}", struct_expr.struct_type),
                );

                let m = remaining_fields
                    .iter()
                    .filter(|(name, _)| name == field_name)
                    .collect_vec();
                assert!(m.len().is_one());
                remaining_fields.retain(|(name, _)| name != field_name);
            }

            assert!(
                remaining_fields.is_empty(),
                "Missing declarations of fields {remaining_fields:?} for struct {}",
                struct_expr.struct_type
            );

            // Sort expression such that it matches type declaration, and ends up with the
            // last declared field on top of stack. This generates more efficient code, so
            // we just demand that the programmer does that.
            let original_order = struct_expr.field_names_and_values.clone();
            struct_expr
                .field_names_and_values
                .sort_by_key(|(field_name, _)| {
                    struct_type
                        .fields
                        .iter()
                        .position(|(name, _)| name == field_name)
                        .unwrap_or(usize::MAX)
                });
            assert_eq!(
                original_order, struct_expr.field_names_and_values,
                "{}: Fields in declaration must match that in type definition in order to allow left-to-right evaluation",
                struct_expr.struct_type
            );

            Ok(struct_expr.struct_type.clone())
        }

        ast::Expr::EnumDeclaration(enum_decl) => {
            // 1. Verify that `variant_name` exists in `enum_type`
            let enum_type = enum_decl.enum_type.as_enum_type();
            assert!(
                enum_type.has_variant_of_name(&enum_decl.variant_name),
                "Variant name {} not known for enum type {}",
                enum_decl.variant_name,
                enum_type.name
            );

            let variant_data_type = enum_type.variant_data_type(&enum_decl.variant_name);
            assert!(
                variant_data_type.is_unit(),
                "Declarations of data-carrying enum variants must contain data"
            );

            Ok(enum_decl.enum_type.clone())
        }

        ast::Expr::FnCall(fn_call) => {
            // Attempt to annotate all arguments before getting the function signature
            for arg in fn_call.args.iter_mut() {
                // It's OK if this fails, as a later invocation of the argument expressions
                // might pass.
                let _ = derive_annotate_expr_type(arg, None, state, env_fn_signature);
            }

            let callees_fn_signature = get_fn_signature(
                &fn_call.name,
                state,
                &fn_call.type_parameter,
                &fn_call.args,
                env_fn_signature,
                hint,
            );

            derive_annotate_fn_call_args(&callees_fn_signature, &mut fn_call.args, state);
            fn_call.annot = Typing::KnownType(callees_fn_signature.output.clone());
            fn_call.arg_evaluation_order = callees_fn_signature.arg_evaluation_order;

            Ok(callees_fn_signature.output)
        }

        ast::Expr::MethodCall(method_call) => {
            // Attempt to annotate all arguments before getting the function signature
            for arg in method_call.args.iter_mut() {
                // It's OK if this fails, as a later invocation of the argument expressions
                // might pass.
                let _ = derive_annotate_expr_type(arg, None, state, env_fn_signature);
            }

            let callees_method_signature: ast::FnSignature =
                get_method_signature(state, method_call, env_fn_signature);
            assert!(
                !callees_method_signature.output.is_unit(),
                "Method call {} in expressions cannot return the unit type",
                method_call.method_name
            );

            // We don't need to check receiver type here since that is done by
            // `derive_annotate_fn_call_args` below.

            derive_annotate_fn_call_args(&callees_method_signature, &mut method_call.args, state);

            method_call.annot = Typing::KnownType(callees_method_signature.output.clone());

            Ok(callees_method_signature.output)
        }

        ast::Expr::Unary(unaryop, rhs_expr, unaryop_type) => {
            use ast::UnaryOp::*;
            let rhs_type = derive_annotate_expr_type(rhs_expr, hint, state, env_fn_signature)?;
            match unaryop {
                Neg => {
                    assert!(
                        is_negatable_type(&rhs_type),
                        "Cannot negate type '{rhs_type}'",
                    );
                    *unaryop_type = Typing::KnownType(rhs_type.clone());
                    Ok(rhs_type)
                }
                Not => {
                    assert!(
                        type_compatible_with_not(&rhs_type),
                        "Cannot negate type '{rhs_type}'",
                    );
                    *unaryop_type = Typing::KnownType(rhs_type.clone());
                    Ok(rhs_type)
                }
                Deref => {
                    match rhs_type {
                        ast_types::DataType::Boxed(inner_inner_type) => {
                            *unaryop_type = Typing::KnownType(*inner_inner_type.clone());
                            Ok(*inner_inner_type)
                        },
                        _ =>  panic!("Cannot dereference type of `{rhs_type}` as this expression has:\n{rhs_expr:#?}")
                    }
                }
                Ref(_mutable) => {
                    let resulting_type = match rhs_type {
                        ast_types::DataType::Boxed(_) => rhs_type,
                        _ => ast_types::DataType::Boxed(Box::new(rhs_type)),
                    };
                    *unaryop_type = Typing::KnownType(resulting_type.clone());
                    Ok(resulting_type)
                },
            }
        }

        ast::Expr::Binop(lhs_expr, binop, rhs_expr, binop_type) => {
            use ast::BinOp::*;
            match binop {
                // Overloaded for all arithmetic types.
                Add => {
                    let maybe_lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint.clone(), state, env_fn_signature);
                    let rhs_hint = match &hint {
                        Some(hint) => Some(hint.to_owned()),
                        None => match &maybe_lhs_type {
                            Ok(ty) => Some(ty.to_owned()),
                            Err(_) => None,
                        },
                    };
                    let rhs_type =
                        derive_annotate_expr_type(rhs_expr, rhs_hint, state, env_fn_signature)?;
                    let lhs_type = match maybe_lhs_type {
                        Ok(ty) => ty,
                        Err(_) => derive_annotate_expr_type(
                            lhs_expr,
                            Some(rhs_type.clone()),
                            state,
                            env_fn_signature,
                        )?,
                    };

                    // We are allowed to add an XFieldElement with a BFieldElement, but we
                    // don't support the mirrored expression.
                    if lhs_type == ast_types::DataType::Xfe && rhs_type == ast_types::DataType::Bfe
                    {
                        *binop_type = Typing::KnownType(ast_types::DataType::Xfe);
                        Ok(ast_types::DataType::Xfe)
                    } else {
                        assert_type_equals(&lhs_type, &rhs_type, "mul-expr");
                        assert!(
                            is_arithmetic_type(&lhs_type),
                            "Cannot multiply non-arithmetic type '{lhs_type}'"
                        );
                        *binop_type = Typing::KnownType(lhs_type.clone());
                        Ok(lhs_type)
                    }
                }

                // Restricted to bool only.
                And => {
                    let bool_hint = Some(ast_types::DataType::Bool);
                    let lhs_type = derive_annotate_expr_type(
                        lhs_expr,
                        bool_hint.clone(),
                        state,
                        env_fn_signature,
                    )?;
                    let rhs_type =
                        derive_annotate_expr_type(rhs_expr, bool_hint, state, env_fn_signature)?;

                    assert_type_equals(&lhs_type, &ast_types::DataType::Bool, "and-expr lhs");
                    assert_type_equals(&rhs_type, &ast_types::DataType::Bool, "and-expr rhs");
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    Ok(ast_types::DataType::Bool)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitAnd => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-and-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-and type '{lhs_type}' (not u32-based)",
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitXor => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-xor-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-xor type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                BitOr => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "bitwise-or-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot bitwise-or type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Overloaded for all arithmetic types.
                Div => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "div-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot divide non-arithmetic type '{lhs_type}'"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Overloaded for all primitive types.
                Eq => {
                    // Cannot provide parent `hint` (since it's Bool)
                    let no_hint = None;
                    let maybe_lhs_type =
                        derive_annotate_expr_type(lhs_expr, no_hint, state, env_fn_signature);
                    let rhs_hint = match maybe_lhs_type.as_ref() {
                        Ok(ty) => Some(ty.to_owned()),
                        Err(_) => None,
                    };
                    let rhs_type =
                        derive_annotate_expr_type(rhs_expr, rhs_hint, state, env_fn_signature)?;
                    let lhs_type = match maybe_lhs_type.as_ref() {
                        Ok(ty) => ty.to_owned(),
                        Err(_) => derive_annotate_expr_type(
                            lhs_expr,
                            Some(rhs_type.clone()),
                            state,
                            env_fn_signature,
                        )?,
                    };

                    assert_type_equals(&lhs_type, &rhs_type, "eq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare non-primitive type '{lhs_type}' for equality"
                    );
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    Ok(ast_types::DataType::Bool)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Lt | Gt => {
                    // FIXME: Cannot provide parent `hint` (since it's Bool)
                    let no_hint = None;
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, no_hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "lt-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot compare type '{lhs_type}' with less-than (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    Ok(ast_types::DataType::Bool)
                }

                // Overloaded for all primitive types.
                Mul => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    // We are allowed to multiply an XFieldElement with a BFieldElement, but we
                    // don't currently support the mirrored expression.
                    if lhs_type == ast_types::DataType::Xfe && rhs_type == ast_types::DataType::Bfe
                    {
                        *binop_type = Typing::KnownType(ast_types::DataType::Xfe);
                        Ok(ast_types::DataType::Xfe)
                    } else {
                        assert_type_equals(&lhs_type, &rhs_type, "mul-expr");
                        assert!(
                            is_arithmetic_type(&lhs_type),
                            "Cannot multiply non-arithmetic type '{lhs_type}'"
                        );
                        *binop_type = Typing::KnownType(lhs_type.clone());
                        Ok(lhs_type)
                    }
                }

                // Overloaded for all primitive types.
                Neq => {
                    // FIXME: Cannot provide parent `hint` (since it's Bool)
                    let no_hint = None;
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, no_hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "neq-expr");
                    assert!(
                        is_primitive_type(&lhs_type),
                        "Cannot compare type '{lhs_type}' with not-equal (not primitive)"
                    );
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    Ok(ast_types::DataType::Bool)
                }

                // Restricted to bool only.
                Or => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint.clone(), state, env_fn_signature)?;
                    let rhs_type =
                        derive_annotate_expr_type(rhs_expr, hint, state, env_fn_signature)?;

                    assert_type_equals(&lhs_type, &ast_types::DataType::Bool, "or-expr lhs");
                    assert_type_equals(&rhs_type, &ast_types::DataType::Bool, "or-expr rhs");
                    *binop_type = Typing::KnownType(ast_types::DataType::Bool);
                    Ok(ast_types::DataType::Bool)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Rem => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint.clone(), state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "rem-expr");
                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot find remainder for type '{lhs_type}' (not u32-based)"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shl => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;

                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-left for type '{lhs_type}' (not u32-based)"
                    );

                    let rhs_hint = Some(ast_types::DataType::U32);
                    let rhs_type =
                        derive_annotate_expr_type(rhs_expr, rhs_hint, state, env_fn_signature)?;

                    assert_type_equals(&rhs_type, &ast_types::DataType::U32, "shl-rhs-expr");
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Restricted to U32-based types. (Triton VM limitation)
                Shr => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;

                    assert!(
                        is_u32_based_type(&lhs_type),
                        "Cannot shift-right for type '{lhs_type}' (not u32-based)"
                    );

                    let rhs_hint = Some(ast_types::DataType::U32);
                    let rhs_type =
                        derive_annotate_expr_type(rhs_expr, rhs_hint, state, env_fn_signature)?;

                    assert_type_equals(&rhs_type, &ast_types::DataType::U32, "shr-rhs-expr");
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }

                // Overloaded for all arithmetic types.
                Sub => {
                    let lhs_type =
                        derive_annotate_expr_type(lhs_expr, hint, state, env_fn_signature)?;
                    let rhs_type = derive_annotate_expr_type(
                        rhs_expr,
                        Some(lhs_type.clone()),
                        state,
                        env_fn_signature,
                    )?;

                    assert_type_equals(&lhs_type, &rhs_type, "sub-expr");
                    assert!(
                        is_arithmetic_type(&lhs_type),
                        "Cannot subtract non-arithmetic type '{lhs_type}'"
                    );
                    *binop_type = Typing::KnownType(lhs_type.clone());
                    Ok(lhs_type)
                }
            }
        }

        ast::Expr::If(expr_if) => derive_annotate_expr_if(hint, state, env_fn_signature, expr_if),
        ast::Expr::ReturningBlock(ret_block) => {
            derive_annotate_returning_block_expr(ret_block, hint, state, env_fn_signature)
        }

        ast::Expr::Cast(expr, to_type) => {
            let from_type = derive_annotate_expr_type(expr, None, state, env_fn_signature)?;
            let valid_cast = is_u32_based_type(&from_type) && is_u32_based_type(to_type)
                || from_type == ast_types::DataType::Bool && is_arithmetic_type(to_type);

            assert!(valid_cast, "Cannot cast from {from_type} to {to_type}");

            Ok(to_type.to_owned())
        }
        ast::Expr::Match(match_expr) => {
            derive_annotate_match_expression(match_expr, state, env_fn_signature, hint)
        }
        ast::Expr::MemoryLocation(ast::MemPointerExpression {
            ref mut mem_pointer_address,
            mem_pointer_declared_type,
            resolved_type,
        }) => {
            let expected_address_type = ast_types::DataType::Bfe;
            let address_type = derive_annotate_expr_type(
                mem_pointer_address,
                Some(expected_address_type.clone()),
                state,
                env_fn_signature,
            )?;
            assert_eq!(expected_address_type, address_type);
            let read_item_type =
                ast_types::DataType::Boxed(Box::new(mem_pointer_declared_type.to_owned()));
            *resolved_type = Typing::KnownType(read_item_type.clone());
            Ok(read_item_type)
        }
    };

    res
}

fn derive_annotate_expr_if(
    hint: Option<ast_types::DataType>,
    state: &mut CheckState,
    env_fn_signature: &ast::FnSignature,
    expr_if: &mut ast::ExprIf<Typing>,
) -> anyhow::Result<ast_types::DataType> {
    let ast::ExprIf {
        condition,
        then_branch,
        else_branch,
    } = expr_if;
    let condition_hint = ast_types::DataType::Bool;
    let condition_type = derive_annotate_expr_type(
        condition,
        Some(condition_hint.clone()),
        state,
        env_fn_signature,
    )?;
    assert_type_equals(&condition_type, &condition_hint, "if-condition-expr");

    let mut resolved_return_type = hint.clone();
    let mut branches = [then_branch.as_mut(), else_branch.as_mut()];

    // Loop over all branches until we have resolved one of them
    for branch in branches.iter_mut() {
        if let Ok(resolved_type) =
            derive_annotate_returning_block_expr(branch, hint.clone(), state, env_fn_signature)
        {
            resolved_return_type = Some(resolved_type);
            break;
        };
    }

    let Some(resolved_return_type) = resolved_return_type else {
        panic!("Could not resolve return type for any branch of if-then-else expression");
    };

    // Then check that all branches return the same type
    for branch in branches.iter_mut() {
        let branch_type = derive_annotate_returning_block_expr(
            branch,
            Some(resolved_return_type.clone()),
            state,
            env_fn_signature,
        )?;
        assert_type_equals(&resolved_return_type, &branch_type, "if-then-else-expr");
    }

    Ok(resolved_return_type)
}

/// Derive return type and annotate a `match` expression
/// let a: u32 = match b {
///    Some(val) => { 42 },
///    None => { 0 },
/// };
fn derive_annotate_match_expression(
    match_expr: &mut ast::MatchExpr<Typing>,
    state: &mut CheckState<'_>,
    env_fn_signature: &ast::FnSignature,
    hint: Option<ast_types::DataType>,
) -> Result<ast_types::DataType, anyhow::Error> {
    let match_expression_type = derive_annotate_expr_type(
        &mut match_expr.match_expression,
        None,
        state,
        env_fn_signature,
    )
    .unwrap();
    let (enum_type, is_boxed) = match match_expression_type {
        ast_types::DataType::Enum(enum_type) => (enum_type, false),
        ast_types::DataType::Boxed(inner) => match *inner.to_owned() {
            ast_types::DataType::Enum(enum_type) => (enum_type, true),
            other => panic!(
                "`match` statements are only supported on enum types. \
                For now. Got {other}"
            ),
        },
        _ => panic!(
            "`match` statements are only supported on enum types. \
            For now. Got {match_expression_type}"
        ),
    };

    // Loop over all arms until *one* of them has a known return type
    let mut resolved_return_type = hint;
    for arm in match_expr.arms.iter_mut() {
        if let Ok(resolved) = resolve_match_arm_return_type(
            arm,
            resolved_return_type.clone(),
            state,
            enum_type.as_ref(),
            is_boxed,
            env_fn_signature,
        ) {
            resolved_return_type = Some(resolved);
            break;
        };
    }
    let Some(resolved_return_type) = resolved_return_type else {
        panic!(
            "Could not resolve return type for any arm of match expression for {}",
            enum_type.name
        )
    };

    // Loop over all arms to verify that they agree on the return type
    let mut variants_encountered: HashSet<String> = HashSet::default();
    let arm_count = match_expr.arms.len();
    let mut contains_catch_all_arm = false;
    for (i, arm) in match_expr.arms.iter_mut().enumerate() {
        let arm_ret_type = resolve_match_arm_return_type(
            arm,
            Some(resolved_return_type.clone()),
            state,
            enum_type.as_ref(),
            is_boxed,
            env_fn_signature,
        )?;
        assert_type_equals(
            &resolved_return_type,
            &arm_ret_type,
            format!("Return type for match-arm {i}"),
        );

        match &arm.match_condition {
            ast::MatchCondition::CatchAll => {
                assert_eq!(
                    i,
                    arm_count - 1,
                    "When using catch_all in match statement, catch_all must be used in last match arm. \
                            Match expression was for type {}",
                    enum_type.name
                );
                contains_catch_all_arm = true;
            }
            ast::MatchCondition::EnumVariant(ast::EnumVariantSelector {
                type_name,
                variant_name,
                data_bindings,
            }) => {
                assert!(
                    variants_encountered.insert(variant_name.clone()),
                    "Repeated variant name {} in match statement on {} encounter",
                    variant_name,
                    enum_type.name
                );

                match type_name {
                    Some(enum_type_name) => {
                        assert_eq!(
                            &enum_type.name,
                            enum_type_name,
                            "Match conditions on type {} must all be of same type. Got bad type: {enum_type_name}", enum_type.name);
                    }
                    None => {
                        assert!(enum_type.is_prelude, "Only enums specified in prelude may use only the variant name in a match arm");
                    }
                };

                let variant_data_tuple = enum_type.variant_data_type(variant_name).as_tuple_type();
                assert!(
                    data_bindings.is_empty()
                        || variant_data_tuple.element_count() == data_bindings.len(),
                    "Number of bindings must match number of elements in variant data tuple"
                );
                assert!(
                    data_bindings.iter().map(|x| &x.name).all_unique(),
                    "Name repetition in pattern matching not allowed"
                );
            }
        }
    }

    assert!(
        variants_encountered.len() == enum_type.variants.len() || contains_catch_all_arm,
        "All cases must be covered for match-expression for {}. Missing variants: {}.",
        enum_type.name,
        enum_type
            .variants
            .iter()
            .map(|x| &x.0)
            .filter(|x| !variants_encountered.contains(*x))
            .join(",")
    );

    Ok(resolved_return_type)
}

/// Resolve the return type of a match arm. Does not mutate state.
fn resolve_match_arm_return_type(
    arm: &mut ast::MatchExprArm<Typing>,
    hint: Option<ast_types::DataType>,
    state: &mut CheckState,
    enum_type: &ast_types::EnumType,
    is_boxed: bool,
    env_fn_signature: &ast::FnSignature,
) -> anyhow::Result<ast_types::DataType> {
    match &arm.match_condition {
        ast::MatchCondition::EnumVariant(enum_variant_selector) => {
            let variant_data_tuple = enum_type
                .variant_data_type(&enum_variant_selector.variant_name)
                .as_tuple_type();
            enum_variant_selector
                .data_bindings
                .iter()
                .enumerate()
                .for_each(|(i, x)| {
                    let new_binding_type = if is_boxed {
                        ast_types::DataType::Boxed(Box::new(
                            variant_data_tuple.fields[i].to_owned(),
                        ))
                    } else {
                        variant_data_tuple.fields[i].to_owned()
                    };

                    state.vtable.insert(
                        x.name.to_owned(),
                        DataTypeAndMutability::new(&new_binding_type, x.mutable),
                    );
                });
            let ret =
                derive_annotate_returning_block_expr(&mut arm.body, hint, state, env_fn_signature);

            // Remove bindings set in match-arm after body's type check
            enum_variant_selector.data_bindings.iter().for_each(|x| {
                state.vtable.remove(&x.name);
            });

            ret
        }
        ast::MatchCondition::CatchAll => {
            derive_annotate_returning_block_expr(&mut arm.body, hint, state, env_fn_signature)
        }
    }
}

fn array_element_type(
    state: &mut CheckState,
    env_fn_signature: &ast::FnSignature,
    array_expr: &mut ast::ArrayExpression<Typing>,
) -> anyhow::Result<Option<ast_types::DataType>> {
    let mut resolved_element_type = None;
    match array_expr {
        ast::ArrayExpression::ElementsSpecified(elem_expressions) => {
            for element_expression in elem_expressions.iter_mut() {
                let expr_type = derive_annotate_expr_type(
                    element_expression,
                    resolved_element_type.clone(),
                    state,
                    env_fn_signature,
                )?;
                if let Some(hint) = &resolved_element_type {
                    assert_eq!(
                        *hint, expr_type,
                        "All expressions in array declaration must evaluate to the same type. \
                        Got {hint} and {expr_type}.",
                    );
                }
                resolved_element_type = Some(expr_type);
            }
        }
        ast::ArrayExpression::Repeat { element, .. } => {
            let expr_type = derive_annotate_expr_type(element, None, state, env_fn_signature)?;
            resolved_element_type = Some(expr_type);
        }
    };

    Ok(resolved_element_type)
}

fn derive_annotate_returning_block_expr(
    ret_block: &mut ast::ReturningBlock<Typing>,
    hint: Option<ast_types::DataType>,
    state: &mut CheckState,
    env_fn_signature: &ast::FnSignature,
) -> anyhow::Result<ast_types::DataType> {
    let vtable_before = state.vtable.clone();
    ret_block
        .stmts
        .iter_mut()
        .for_each(|stmt| annotate_stmt(stmt, state, env_fn_signature));
    let resolved_return_type =
        derive_annotate_expr_type(&mut ret_block.return_expr, hint, state, env_fn_signature);
    state.vtable = vtable_before.clone();

    resolved_return_type
}

/// A type that can be used as address in `read_mem` and `write_mem` calls.
///
/// Since memory addresses are essentially `BFieldElement`s, only types
/// that are subsets of `BFE`s can be used. The only such type is `U32`.
pub(crate) fn is_index_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | Bfe)
}

/// A type for which basic arithmetic operators can be overloaded.
///
/// Note that not all operators work for all arithmetic types.
///
/// E.g. the bitwise operators only work for `is_u32_based_type()`.
pub(crate) fn is_arithmetic_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | U64 | U128 | Bfe | Xfe)
}

/// A type from which expressions such as `-value` can be formed
pub(crate) fn is_negatable_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, Bfe | Xfe)
}

/// A type from which expressions such as `!value` can be formed
pub(crate) fn type_compatible_with_not(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, Bool | U32 | U64)
}

/// A type that is implemented in terms of `U32` values.
///
/// E.g. `U32` and `U64`.
pub(crate) fn is_u32_based_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, U32 | U64 | U128)
}

/// A non-composite fixed-length type.
pub(crate) fn is_primitive_type(data_type: &ast_types::DataType) -> bool {
    use ast_types::DataType::*;
    matches!(data_type, Bool | U32 | U64 | U128 | Bfe | Xfe | Digest)
}
