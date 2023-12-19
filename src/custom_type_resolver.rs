use crate::{
    ast::*,
    ast_types::{self, AbstractArgument, AbstractValueArg, CustomTypeOil, DataType, Tuple},
    composite_types::CompositeTypes,
    type_checker::Typing,
};

pub(crate) trait CustomTypeResolution {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes);
}

impl CustomTypeResolution for ReturningBlock<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        let ReturningBlock { stmts, return_expr } = self;
        stmts
            .iter_mut()
            .for_each(|x| x.resolve_custom_types(composite_types));
        return_expr.resolve_custom_types(composite_types);
    }
}

impl CustomTypeResolution for Expr<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        match self {
            Expr::MethodCall(MethodCall {
                method_name: _,
                args,
                annot: _,
                associated_type: _,
            }) => args
                .iter_mut()
                .for_each(|arg_expr| arg_expr.resolve_custom_types(composite_types)),
            Expr::Lit(lit) => {
                if let ExprLit::MemPointer(MemPointerLiteral {
                    mem_pointer_address: _,
                    mem_pointer_declared_type,
                    resolved_type: _,
                }) = lit
                {
                    mem_pointer_declared_type.resolve_custom_types(composite_types)
                }
            }
            Expr::Tuple(exprs) => exprs
                .iter_mut()
                .for_each(|x| x.resolve_custom_types(composite_types)),
            Expr::Array(array_expr, _) => match array_expr {
                ArrayExpression::ElementsSpecified(elements_exprs) => elements_exprs
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types)),
            },
            Expr::FnCall(FnCall {
                name: _,
                args,
                type_parameter,
                arg_evaluation_order: _,
                annot: _,
            }) => {
                args.iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
                if let Some(typa) = type_parameter.as_mut() {
                    typa.resolve_custom_types(composite_types)
                }
            }
            Expr::Unary(_, ref mut expr, _) => expr.resolve_custom_types(composite_types),
            Expr::If(ExprIf {
                ref mut condition,
                ref mut then_branch,
                ref mut else_branch,
            }) => {
                condition.resolve_custom_types(composite_types);
                then_branch.resolve_custom_types(composite_types);
                else_branch.resolve_custom_types(composite_types);
            }
            Expr::Cast(expr, target_type) => {
                expr.resolve_custom_types(composite_types);
                target_type.resolve_custom_types(composite_types);
            }
            Expr::ReturningBlock(ret_block) => ret_block.resolve_custom_types(composite_types),
            Expr::Var(ident) => ident.resolve_custom_types(composite_types),
            Expr::Binop(ref mut lhs, _, ref mut rhs, _) => {
                lhs.resolve_custom_types(composite_types);
                rhs.resolve_custom_types(composite_types);
            }
            Expr::Struct(struct_expr) => {
                struct_expr
                    .struct_type
                    .resolve_custom_types(composite_types);
                for (_field_name, value) in struct_expr.field_names_and_values.iter_mut() {
                    value.resolve_custom_types(composite_types);
                }
            }
            Expr::EnumDeclaration(enum_decl) => {
                enum_decl.enum_type.resolve_custom_types(composite_types);
            }
        }
    }
}

impl CustomTypeResolution for IndexExpr<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        match self {
            IndexExpr::Dynamic(index_expr) => index_expr.resolve_custom_types(composite_types),
            IndexExpr::Static(_) => (),
        }
    }
}

impl CustomTypeResolution for Identifier<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        match self {
            Identifier::String(_, _) => (),
            Identifier::Index(inner_id, index_expr, _) => {
                inner_id.resolve_custom_types(composite_types);
                index_expr.resolve_custom_types(composite_types);
            }
            Identifier::Field(inner_id, _, _) => inner_id.resolve_custom_types(composite_types),
        }
    }
}

impl CustomTypeResolution for Stmt<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        match self {
            Stmt::Let(LetStmt {
                var_name: _,
                mutable: _,
                data_type,
                expr,
            }) => {
                data_type.resolve_custom_types(composite_types);
                expr.resolve_custom_types(composite_types);
            }
            Stmt::Assign(AssignStmt { identifier, expr }) => {
                expr.resolve_custom_types(composite_types);
                identifier.resolve_custom_types(composite_types);
            }
            Stmt::Return(maybe_expr) => {
                if let Some(x) = maybe_expr.as_mut() {
                    x.resolve_custom_types(composite_types)
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
                    .for_each(|x| x.resolve_custom_types(composite_types));
                if let Some(typa) = type_parameter.as_mut() {
                    typa.resolve_custom_types(composite_types)
                }
            }
            Stmt::MethodCall(MethodCall {
                method_name: _,
                args,
                annot: _,
                associated_type: _,
            }) => {
                args.iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
            Stmt::While(WhileStmt { condition, block }) => {
                condition.resolve_custom_types(composite_types);
                block
                    .stmts
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
            Stmt::If(IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                condition.resolve_custom_types(composite_types);
                then_branch
                    .stmts
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
                else_branch
                    .stmts
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
            Stmt::Block(block_stmt) => {
                block_stmt.resolve_custom_types(composite_types);
            }
            Stmt::Assert(AssertStmt { expression }) => {
                expression.resolve_custom_types(composite_types)
            }
            Stmt::FnDeclaration(Fn { signature, body }) => {
                signature.resolve_custom_types(composite_types);
                match body {
                    RoutineBody::Ast(stmts) => {
                        stmts
                            .iter_mut()
                            .for_each(|x| x.resolve_custom_types(composite_types));
                    }
                    RoutineBody::Instructions(_instrs) => (),
                }
            }
            Stmt::Match(MatchStmt {
                match_expression,
                arms,
            }) => {
                match_expression.resolve_custom_types(composite_types);
                arms.iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
        }
    }
}

pub(crate) fn resolve_custom_types(function: &mut Fn<Typing>, custom_types: &mut CompositeTypes) {
    custom_types.resolve_nested();

    function.resolve_custom_types(custom_types);
}

impl CustomTypeResolution for MatchArm<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        self.body.resolve_custom_types(composite_types);
    }
}

impl CustomTypeResolution for BlockStmt<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        self.stmts
            .iter_mut()
            .for_each(|x| x.resolve_custom_types(composite_types));
    }
}

impl CustomTypeResolution for Fn<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        self.signature.resolve_custom_types(composite_types);
        match &mut self.body {
            RoutineBody::Ast(stmts) => stmts
                .iter_mut()
                .for_each(|x| x.resolve_custom_types(composite_types)),
            RoutineBody::Instructions(_instrs) => (),
        }
    }
}

impl CustomTypeResolution for Method<Typing> {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        self.signature.resolve_custom_types(composite_types);
        match &mut self.body {
            RoutineBody::Ast(stmts) => stmts
                .iter_mut()
                .for_each(|x| x.resolve_custom_types(composite_types)),
            RoutineBody::Instructions(_instrs) => (),
        }
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
            DataType::Bfe => false,
            DataType::Xfe => false,
            DataType::Digest => false,
            DataType::VoidPointer => false,
        }
    }
}

impl Tuple {
    pub fn is_unresolved(&self) -> bool {
        self.fields.iter().any(|x| x.is_unresolved())
    }
}

impl CustomTypeOil {
    pub fn is_unresolved(&self) -> bool {
        self.field_or_variant_types().any(|x| x.is_unresolved())
    }
}

impl CustomTypeResolution for DataType {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        use DataType::*;
        match self {
            Unresolved(unresolved_type) => {
                let resolved = composite_types
                    .get_unique_by_name(unresolved_type)
                    .composite_type;
                match resolved {
                    ast_types::CustomTypeOil::Struct(resolved_struct_type) => {
                        *self = DataType::Struct(resolved_struct_type);
                    }
                    ast_types::CustomTypeOil::Enum(resolved_enum_type) => {
                        *self = DataType::Enum(Box::new(resolved_enum_type));
                    }
                }
            }
            List(inner, _list_type) => {
                inner.resolve_custom_types(composite_types);
            }
            Tuple(inners) => {
                inners
                    .fields
                    .iter_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
            Function(function_type) => {
                function_type
                    .input_argument
                    .resolve_custom_types(composite_types);
                function_type.output.resolve_custom_types(composite_types);
            }
            Boxed(inner) => {
                inner.resolve_custom_types(composite_types);
            }
            Struct(struct_type) => {
                struct_type
                    .field_types_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
            Enum(enum_type) => {
                enum_type
                    .variant_types_mut()
                    .for_each(|x| x.resolve_custom_types(composite_types));
            }
            Array(array_type) => {
                array_type
                    .element_type
                    .as_mut()
                    .resolve_custom_types(composite_types);
            }
            Reference(inner_type) => {
                inner_type.resolve_custom_types(composite_types);
            }

            Bool => (),
            U32 => (),
            U64 => (),
            U128 => (),
            Bfe => (),
            Xfe => (),
            Digest => (),
            VoidPointer => (),
        }
    }
}

impl CustomTypeResolution for FnSignature {
    fn resolve_custom_types(&mut self, composite_types: &CompositeTypes) {
        self.output.resolve_custom_types(composite_types);
        for input in self.args.iter_mut() {
            match input {
                AbstractArgument::FunctionArgument(_fn_arg) => (),
                AbstractArgument::ValueArgument(AbstractValueArg {
                    name: _,
                    data_type,
                    mutable: _,
                }) => data_type.resolve_custom_types(composite_types),
            }
        }
    }
}
