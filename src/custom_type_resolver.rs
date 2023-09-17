use std::collections::HashMap;

use crate::{
    ast::*,
    ast_types::{self, AbstractArgument, AbstractValueArg, DataType, StructType},
    libraries,
    type_checker::Typing,
};

impl ReturningBlock<Typing> {
    fn resolve_types(&mut self, declared_structs: &HashMap<String, ast_types::StructType>) {
        let ReturningBlock { stmts, return_expr } = self;
        stmts
            .iter_mut()
            .for_each(|x| x.resolve_types(declared_structs));
        return_expr.resolve_types(declared_structs);
    }
}

impl Expr<Typing> {
    fn resolve_types(&mut self, declared_structs: &HashMap<String, ast_types::StructType>) {
        match self {
            Expr::MethodCall(MethodCall {
                method_name: _,
                args,
                annot: _,
            }) => args
                .iter_mut()
                .for_each(|arg_expr| arg_expr.resolve_types(declared_structs)),
            Expr::Lit(lit) => match lit {
                ExprLit::MemPointer(MemPointerLiteral {
                    mem_pointer_address: _,
                    mem_pointer_declared_type,
                    resolved_type: _,
                }) => mem_pointer_declared_type.resolve_types(declared_structs),
                _ => (),
            },
            Expr::Tuple(exprs) => exprs
                .iter_mut()
                .for_each(|x| x.resolve_types(declared_structs)),
            Expr::FnCall(FnCall {
                name: _,
                args,
                type_parameter,
                arg_evaluation_order: _,
                annot: _,
            }) => {
                args.iter_mut()
                    .for_each(|x| x.resolve_types(declared_structs));
                type_parameter
                    .as_mut()
                    .map(|typa| typa.resolve_types(declared_structs));
            }
            Expr::Unary(_, ref mut expr, _) => expr.resolve_types(declared_structs),
            Expr::If(ExprIf {
                ref mut condition,
                ref mut then_branch,
                ref mut else_branch,
            }) => {
                condition.resolve_types(declared_structs);
                then_branch.resolve_types(declared_structs);
                else_branch.resolve_types(declared_structs);
            }
            Expr::Cast(_, _) => todo!(),
            Expr::ReturningBlock(_) => todo!(),
            Expr::Var(_) => todo!(),
            Expr::Binop(ref mut lhs, _, ref mut rhs, _) => {
                lhs.resolve_types(declared_structs);
                rhs.resolve_types(declared_structs);
            }
        }
    }
}

impl Stmt<Typing> {
    fn resolve_types(&mut self, declared_structs: &HashMap<String, ast_types::StructType>) {
        match self {
            Stmt::Let(LetStmt {
                var_name,
                mutable,
                data_type,
                expr,
            }) => {
                data_type.resolve_types(declared_structs);
                todo!()
            }
            Stmt::Assign(_) => todo!(),
            Stmt::Return(_) => todo!(),
            Stmt::FnCall(_) => todo!(),
            Stmt::MethodCall(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::If(_) => todo!(),
            Stmt::Block(_) => todo!(),
            Stmt::Assert(_) => todo!(),
            Stmt::FnDeclaration(_) => todo!(),
        }
    }
}

pub fn resolve_types(
    function: &mut Fn<Typing>,
    declared_structs: &HashMap<String, ast_types::StructType>,
    declared_methods: &mut [Method<Typing>],
    associated_functions: &mut HashMap<String, HashMap<String, Fn<Typing>>>,
    libraries: &[Box<dyn libraries::Library>],
) {
    function.signature.resolve_types(declared_structs);
    function
        .body
        .iter_mut()
        .for_each(|x| x.resolve_types(declared_structs));
}

impl DataType {
    /// Returns true iff any of the contained types have to be resolved through types associated with the program
    pub fn is_unresolved(&self) -> bool {
        match self {
            DataType::Unresolved(_) => true,
            DataType::MemPointer(inner) => inner.is_unresolved(),
            DataType::Tuple(inners) => inners.into_iter().any(|inner| inner.is_unresolved()),
            DataType::List(element, _) => element.is_unresolved(),
            DataType::Struct(struct_type) => struct_type
                .field_types()
                .any(|field_type| field_type.is_unresolved()),
            DataType::Function(function_type) => {
                function_type.input_argument.is_unresolved() || function_type.output.is_unresolved()
            }
            _ => false,
        }
    }

    pub fn resolve_types(&mut self, declared_structs: &HashMap<String, StructType>) {
        // TODO: Should this also mutate the structs in `declared_structs`? Currently
        // it only mutates `self` to the resolved type.
        match self {
            DataType::Unresolved(unresolved_type) => {
                let mut outer_resolved = declared_structs
                    .get(unresolved_type)
                    .unwrap_or_else(|| {
                        panic!("Failed to resolve type {unresolved_type}. Does not know this type.")
                    })
                    .to_owned();
                outer_resolved
                    .field_types_mut()
                    .for_each(|x| x.resolve_types(declared_structs));
                *self = DataType::Struct(outer_resolved);
            }
            DataType::List(inner, list_type) => {
                inner.resolve_types(declared_structs);
            }
            DataType::Tuple(inners) => inners
                .into_iter()
                .for_each(|x| x.resolve_types(declared_structs)),
            DataType::Function(function_type) => {
                function_type.input_argument.resolve_types(declared_structs);
                function_type.output.resolve_types(declared_structs);
            }
            DataType::MemPointer(inner) => {
                inner.resolve_types(declared_structs);
            }
            DataType::Struct(struct_type) => {
                struct_type
                    .field_types_mut()
                    .for_each(|x| x.resolve_types(declared_structs));
            }
            _ => (),
        }
    }
}

impl FnSignature {
    pub fn resolve_types(&mut self, declared_structs: &HashMap<String, StructType>) {
        self.output.resolve_types(declared_structs);
        for input in self.args.iter_mut() {
            match input {
                AbstractArgument::FunctionArgument(fn_arg) => (),
                AbstractArgument::ValueArgument(AbstractValueArg {
                    name,
                    data_type,
                    mutable,
                }) => data_type.resolve_types(declared_structs),
            }
        }
    }
}
