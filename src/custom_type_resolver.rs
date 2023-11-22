use std::collections::HashMap;

use crate::{
    ast::*,
    ast_types::{self, AbstractArgument, AbstractValueArg, CustomTypeOil, DataType},
    type_checker::Typing,
};

pub trait CustomTypeResolution {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    );
}

impl CustomTypeResolution for ReturningBlock<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        let ReturningBlock { stmts, return_expr } = self;
        stmts
            .iter_mut()
            .for_each(|x| x.resolve_custom_types(declared_structs));
        return_expr.resolve_custom_types(declared_structs);
    }
}

impl CustomTypeResolution for Expr<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        match self {
            Expr::MethodCall(MethodCall {
                method_name: _,
                args,
                annot: _,
            }) => args
                .iter_mut()
                .for_each(|arg_expr| arg_expr.resolve_custom_types(declared_structs)),
            Expr::Lit(lit) => {
                if let ExprLit::MemPointer(MemPointerLiteral {
                    mem_pointer_address: _,
                    mem_pointer_declared_type,
                    resolved_type: _,
                }) = lit
                {
                    mem_pointer_declared_type.resolve_custom_types(declared_structs)
                }
            }
            Expr::Tuple(exprs) => exprs
                .iter_mut()
                .for_each(|x| x.resolve_custom_types(declared_structs)),
            Expr::Array(array_expr, _) => match array_expr {
                ArrayExpression::ElementsSpecified(elements_exprs) => elements_exprs
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs)),
            },
            Expr::FnCall(FnCall {
                name: _,
                args,
                type_parameter,
                arg_evaluation_order: _,
                annot: _,
            }) => {
                args.iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
                if let Some(typa) = type_parameter.as_mut() {
                    typa.resolve_custom_types(declared_structs)
                }
            }
            Expr::Unary(_, ref mut expr, _) => expr.resolve_custom_types(declared_structs),
            Expr::If(ExprIf {
                ref mut condition,
                ref mut then_branch,
                ref mut else_branch,
            }) => {
                condition.resolve_custom_types(declared_structs);
                then_branch.resolve_custom_types(declared_structs);
                else_branch.resolve_custom_types(declared_structs);
            }
            Expr::Cast(expr, target_type) => {
                expr.resolve_custom_types(declared_structs);
                target_type.resolve_custom_types(declared_structs);
            }
            Expr::ReturningBlock(ret_block) => ret_block.resolve_custom_types(declared_structs),
            Expr::Var(ident) => ident.resolve_custom_types(declared_structs),
            Expr::Binop(ref mut lhs, _, ref mut rhs, _) => {
                lhs.resolve_custom_types(declared_structs);
                rhs.resolve_custom_types(declared_structs);
            }
            Expr::Struct(struct_expr) => {
                struct_expr
                    .struct_type
                    .resolve_custom_types(declared_structs);
                for (_field_name, value) in struct_expr.field_names_and_values.iter_mut() {
                    value.resolve_custom_types(declared_structs);
                }
            }
            Expr::EnumDeclaration(enum_decl) => {
                enum_decl.enum_type.resolve_custom_types(declared_structs);
            }
        }
    }
}

impl CustomTypeResolution for IndexExpr<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        match self {
            IndexExpr::Dynamic(index_expr) => index_expr.resolve_custom_types(declared_structs),
            IndexExpr::Static(_) => (),
        }
    }
}

impl CustomTypeResolution for Identifier<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        match self {
            Identifier::String(_, _) => (),
            Identifier::Index(inner_id, index_expr, _) => {
                inner_id.resolve_custom_types(declared_structs);
                index_expr.resolve_custom_types(declared_structs);
            }
            Identifier::Field(inner_id, _, _) => inner_id.resolve_custom_types(declared_structs),
        }
    }
}

impl CustomTypeResolution for Stmt<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        match self {
            Stmt::Let(LetStmt {
                var_name: _,
                mutable: _,
                data_type,
                expr,
            }) => {
                data_type.resolve_custom_types(declared_structs);
                expr.resolve_custom_types(declared_structs);
            }
            Stmt::Assign(AssignStmt { identifier, expr }) => {
                expr.resolve_custom_types(declared_structs);
                identifier.resolve_custom_types(declared_structs);
            }
            Stmt::Return(maybe_expr) => {
                if let Some(x) = maybe_expr.as_mut() {
                    x.resolve_custom_types(declared_structs)
                }
            }
            Stmt::FnCall(FnCall {
                name: _,
                args,
                type_parameter,
                arg_evaluation_order: _,
                annot: _,
            }) => {
                args.iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
                if let Some(typa) = type_parameter.as_mut() {
                    typa.resolve_custom_types(declared_structs)
                }
            }
            Stmt::MethodCall(MethodCall {
                method_name: _,
                args,
                annot: _,
            }) => {
                args.iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Stmt::While(WhileStmt { condition, block }) => {
                condition.resolve_custom_types(declared_structs);
                block
                    .stmts
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Stmt::If(IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                condition.resolve_custom_types(declared_structs);
                then_branch
                    .stmts
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
                else_branch
                    .stmts
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Stmt::Block(block_stmt) => {
                block_stmt.resolve_custom_types(declared_structs);
            }
            Stmt::Assert(AssertStmt { expression }) => {
                expression.resolve_custom_types(declared_structs)
            }
            Stmt::FnDeclaration(Fn { signature, body }) => {
                signature.resolve_custom_types(declared_structs);
                body.iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Stmt::Match(MatchStmt {
                match_expression,
                arms,
            }) => {
                match_expression.resolve_custom_types(declared_structs);
                arms.iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
        }
    }
}

pub fn resolve_custom_types(
    function: &mut Fn<Typing>,
    custom_types: &mut HashMap<String, ast_types::CustomTypeOil>,
    declared_methods: &mut [Method<Typing>],
    associated_functions: &mut HashMap<String, HashMap<String, Fn<Typing>>>,
) {
    // Resolve any nested types
    let mut custom_types_copy = custom_types.clone();
    while custom_types_copy.iter().any(|x| x.1.is_unresolved()) {
        custom_types.values_mut().for_each(|x| {
            x.field_or_variant_types_mut()
                .for_each(|x| x.resolve_custom_types(&custom_types_copy));
        });
        custom_types_copy = custom_types.to_owned();
    }

    function.resolve_custom_types(custom_types);

    declared_methods
        .iter_mut()
        .for_each(|x| x.resolve_custom_types(custom_types));

    associated_functions.values_mut().for_each(|x| {
        x.values_mut()
            .for_each(|x| x.resolve_custom_types(custom_types))
    });
}

impl CustomTypeResolution for MatchArm<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        self.body.resolve_custom_types(declared_structs);
    }
}

impl CustomTypeResolution for BlockStmt<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        self.stmts
            .iter_mut()
            .for_each(|x| x.resolve_custom_types(declared_structs));
    }
}

impl CustomTypeResolution for Fn<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        self.signature.resolve_custom_types(declared_structs);
        self.body
            .iter_mut()
            .for_each(|x| x.resolve_custom_types(declared_structs));
    }
}

impl CustomTypeResolution for Method<Typing> {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        self.signature.resolve_custom_types(declared_structs);
        self.body
            .iter_mut()
            .for_each(|x| x.resolve_custom_types(declared_structs));
    }
}

impl DataType {
    /// Returns true iff any of the contained types have to be resolved through types associated with the program
    pub fn is_unresolved(&self) -> bool {
        match self {
            DataType::Unresolved(_) => true,
            DataType::Boxed(inner) => inner.is_unresolved(),
            DataType::Tuple(inners) => inners.into_iter().any(|inner| inner.is_unresolved()),
            DataType::List(element, _) => element.is_unresolved(),
            DataType::Struct(struct_type) => struct_type
                .field_types()
                .any(|field_type| field_type.is_unresolved()),
            DataType::Function(function_type) => {
                function_type.input_argument.is_unresolved() || function_type.output.is_unresolved()
            }
            DataType::Enum(enum_type) => enum_type
                .variants
                .iter()
                .any(|(_name, dtype)| dtype.is_unresolved()),
            DataType::Array(array_type) => array_type.element_type.is_unresolved(),
            DataType::Reference(inner_type) => inner_type.is_unresolved(),
            DataType::Bool => false,
            DataType::U32 => false,
            DataType::U64 => false,
            DataType::U128 => false,
            DataType::BFE => false,
            DataType::XFE => false,
            DataType::Digest => false,
            DataType::VoidPointer => false,
            DataType::Result(ok) => ok.is_unresolved(),
        }
    }
}

impl CustomTypeOil {
    pub fn is_unresolved(&self) -> bool {
        self.field_or_variant_types().any(|x| x.is_unresolved())
    }
}

impl CustomTypeResolution for DataType {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        // TODO: Should this also mutate the structs in `declared_structs`? Currently
        // it only mutates `self` to the resolved type.
        use DataType::*;
        match self {
            Unresolved(unresolved_type) => {
                let mut outer_resolved = declared_structs
                    .get(unresolved_type)
                    .unwrap_or_else(|| {
                        panic!("Failed to resolve type {unresolved_type}. Does not know this type.")
                    })
                    .to_owned();
                outer_resolved
                    .field_or_variant_types_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
                match outer_resolved {
                    ast_types::CustomTypeOil::Struct(resolved_struct_type) => {
                        *self = DataType::Struct(resolved_struct_type);
                    }
                    ast_types::CustomTypeOil::Enum(resolved_enum_type) => {
                        *self = DataType::Enum(Box::new(resolved_enum_type));
                    }
                }
            }
            List(inner, _list_type) => {
                inner.resolve_custom_types(declared_structs);
            }
            Tuple(inners) => {
                inners
                    .fields
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Function(function_type) => {
                function_type
                    .input_argument
                    .resolve_custom_types(declared_structs);
                function_type.output.resolve_custom_types(declared_structs);
            }
            Boxed(inner) => {
                inner.resolve_custom_types(declared_structs);
            }
            Struct(struct_type) => {
                struct_type
                    .field_types_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Enum(enum_type) => {
                enum_type
                    .variant_types_mut()
                    .for_each(|x| x.resolve_custom_types(declared_structs));
            }
            Array(array_type) => {
                array_type
                    .element_type
                    .as_mut()
                    .resolve_custom_types(declared_structs);
            }
            Reference(inner_type) => {
                inner_type.resolve_custom_types(declared_structs);
            }
            Result(ok_type) => ok_type.resolve_custom_types(declared_structs),

            Bool => (),
            U32 => (),
            U64 => (),
            U128 => (),
            BFE => (),
            XFE => (),
            Digest => (),
            VoidPointer => (),
        }
    }
}

impl CustomTypeResolution for FnSignature {
    fn resolve_custom_types(
        &mut self,
        declared_structs: &HashMap<String, ast_types::CustomTypeOil>,
    ) {
        self.output.resolve_custom_types(declared_structs);
        for input in self.args.iter_mut() {
            match input {
                AbstractArgument::FunctionArgument(_fn_arg) => (),
                AbstractArgument::ValueArgument(AbstractValueArg {
                    name: _,
                    data_type,
                    mutable: _,
                }) => data_type.resolve_custom_types(declared_structs),
            }
        }
    }
}
