use itertools::Itertools;
use std::fmt::Display;
use tasm_lib::traits::basic_snippet::BasicSnippet;
use tasm_lib::triton_vm::prelude::*;

use crate::ast_types;
use crate::ast_types::AbstractArgument;
use crate::ast_types::DataType;
use crate::ast_types::FieldId;
use crate::type_checker::Typing;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum RoutineBody<T> {
    Ast(Vec<Stmt<T>>),
    Instructions(Vec<LabelledInstruction>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Method<T> {
    pub(crate) signature: FnSignature,
    pub(crate) body: RoutineBody<T>,
}

impl<T> Display for Method<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.signature.name)
    }
}

impl<T: Clone> Method<T> {
    /// Convert a method to a function data type with a specified name. Consumes the
    /// method.
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn as_ast_function(self, new_name: &str) -> Fn<T> {
        let mut fn_signature = self.signature;
        fn_signature.name = new_name.to_owned();
        Fn {
            signature: fn_signature,
            body: self.body,
        }
    }
}

impl<T> Method<T> {
    pub(crate) fn receiver_type(&self) -> crate::ast_types::DataType {
        match &self.signature.args[0] {
            AbstractArgument::FunctionArgument(_) => {
                panic!("Method cannot take function as 1st argument")
            }
            AbstractArgument::ValueArgument(crate::ast_types::AbstractValueArg {
                name: _,
                data_type,
                mutable: _,
            }) => data_type.to_owned(),
        }
    }

    /// Return a label uniquely identifying a method
    pub(crate) fn get_tasm_label(&self) -> String {
        let receiver_type = self.receiver_type().label_friendly_name();
        let method_name = self.signature.name.to_owned();
        format!("method_{receiver_type}_{method_name}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Fn<T> {
    pub(crate) signature: FnSignature,
    pub(crate) body: RoutineBody<T>,
}

impl<T> Fn<T> {
    pub(crate) fn get_tasm_label(&self) -> String {
        self.signature.name.replace("::", "_assoc_function___of___")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct FnSignature {
    pub(crate) name: String,
    pub(crate) args: Vec<AbstractArgument>,
    pub(crate) output: DataType,
    pub(crate) arg_evaluation_order: ArgEvaluationOrder,
}

impl FnSignature {
    /// Return the number of words that the function's input arguments take up on the stack
    pub(crate) fn input_arguments_stack_size(&self) -> usize {
        self.args.iter().map(|arg| arg.stack_size()).sum()
    }

    /// Convert snippet implementing `BasicSnippet` from `tasm-lib` into a function signature.
    pub(crate) fn from_basic_snippet(
        snippet: Box<dyn BasicSnippet>,
        list_type: ast_types::list_type::ListType,
    ) -> Self {
        let name = snippet.entrypoint();
        let mut args: Vec<ast_types::AbstractArgument> = vec![];
        for (ty, name) in snippet.inputs().into_iter() {
            let fn_arg = ast_types::AbstractValueArg {
                name,
                data_type: ast_types::DataType::from_tasm_lib_datatype(ty, list_type),
                mutable: true,
            };
            args.push(ast_types::AbstractArgument::ValueArgument(fn_arg));
        }

        let mut output_types: Vec<ast_types::DataType> = vec![];
        for (ty, _name) in snippet.outputs() {
            output_types.push(ast_types::DataType::from_tasm_lib_datatype(ty, list_type));
        }

        let output = match output_types.len() {
            1 => output_types[0].clone(),
            0 => ast_types::DataType::Tuple(vec![].into()),
            _ => ast_types::DataType::Tuple(output_types.into()),
        };

        Self {
            name,
            args,
            output,
            arg_evaluation_order: Default::default(),
        }
    }

    /// Returns a boolean indicating if the function signature matches a list of input types
    pub(crate) fn matches(&self, types: &[DataType]) -> bool {
        if self.args.len() != types.len() {
            return false;
        }

        for (arg, dtype) in self.args.iter().zip_eq(types.iter()) {
            match arg {
                AbstractArgument::FunctionArgument(fun_arg) => {
                    let DataType::Function(fun) = dtype else {
                        return false;
                    };
                    if fun_arg.function_type != **fun {
                        return false;
                    }
                }
                AbstractArgument::ValueArgument(val_arg) => {
                    if val_arg.data_type != *dtype {
                        return false;
                    }
                }
            }
        }

        true
    }
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ArgEvaluationOrder {
    #[default]
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Stmt<T> {
    Let(LetStmt<T>),
    TupleDestructuring(TupleDestructStmt<T>),
    Assign(AssignStmt<T>),
    Return(Option<Expr<T>>),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    While(WhileStmt<T>),
    If(IfStmt<T>),
    Block(BlockStmt<T>),
    Assert(AssertStmt<T>),
    Panic(PanicMacro),
    FnDeclaration(Fn<T>),
    Match(MatchStmt<T>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MatchStmt<T> {
    pub(crate) match_expression: Expr<T>,
    pub(crate) arms: Vec<MatchStmtArm<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MatchStmtArm<T> {
    pub(crate) match_condition: MatchCondition,
    pub(crate) body: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum MatchCondition {
    CatchAll,
    EnumVariant(EnumVariantSelector),
}

impl Display for MatchCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MatchCondition::CatchAll => String::from("_"),
                MatchCondition::EnumVariant(inner) => inner.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct TupleDestructStmt<T> {
    pub bindings: Vec<PatternMatchedBinding>,
    pub ident: Identifier<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct EnumVariantSelector {
    // `Bar` in `Bar::Foo(baz)`
    pub(crate) type_name: Option<String>,

    // `Foo`
    pub(crate) variant_name: String,

    // `baz`
    pub(crate) data_bindings: Vec<PatternMatchedBinding>,
}

impl Display for EnumVariantSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data_bindings.len() {
            0 => write!(f, "{}", self.variant_name),
            _ => write!(
                f,
                "{}({})",
                self.variant_name,
                self.data_bindings.iter().join(",")
            ),
        }
    }
}

impl MatchCondition {
    pub(crate) fn data_type_of_bindings(
        &self,
        enum_type: &ast_types::EnumType,
    ) -> ast_types::Tuple {
        match self {
            Self::CatchAll => ast_types::Tuple::unit(),
            Self::EnumVariant(selector) => selector.get_bindings_type(enum_type),
        }
    }

    pub(crate) fn data_binding_declarations(&self) -> Vec<PatternMatchedBinding> {
        match self {
            Self::CatchAll => vec![],
            Self::EnumVariant(selector) => selector.data_bindings.to_owned(),
        }
    }

    pub(crate) fn declarations_and_their_types(
        &self,
        enum_type: &ast_types::EnumType,
    ) -> impl Iterator<Item = (PatternMatchedBinding, ast_types::DataType)> {
        let tuple_type = self.data_type_of_bindings(enum_type);
        let data_bindings = self.data_binding_declarations();

        data_bindings.into_iter().zip_eq(tuple_type.fields)
    }

    /// Return the label for the subroutine that creates the bindings defined by this match-condition.
    pub(crate) fn label_for_binding_subroutine(&self, boxed: bool) -> String {
        match self {
            Self::CatchAll => "__catch_all_bindings_subroutine".to_owned(),
            Self::EnumVariant(selector) => selector.label_for_binding_subroutine(boxed),
        }
    }
}

impl EnumVariantSelector {
    /// Return the label for the subroutine that creates the bindings defined in this
    /// match-arm.
    pub(crate) fn label_for_binding_subroutine(&self, boxed: bool) -> String {
        let boxed_qualifier = if boxed { "boxed" } else { "stack" };
        format!(
            "{}_{}_bind_{}_{boxed_qualifier}",
            self.type_name.clone().unwrap_or_default(),
            self.variant_name,
            self.data_bindings.len()
        )
    }

    /// Return the composite type of this match-arm binding
    pub(crate) fn get_bindings_type(&self, enum_type: &ast_types::EnumType) -> ast_types::Tuple {
        if self.data_bindings.is_empty() {
            ast_types::Tuple::unit()
        } else {
            enum_type
                .variant_data_type(&self.variant_name)
                .as_tuple_type()
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct PatternMatchedBinding {
    pub(crate) name: String,
    pub(crate) mutable: bool,
    // Add `ref` here also?
}

impl Display for PatternMatchedBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "mut {}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct AssertStmt<T> {
    pub(crate) expression: Expr<T>,
    // pub(crate) decription: Option<String>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct PanicMacro;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct WhileStmt<T> {
    pub(crate) condition: Expr<T>,
    pub(crate) block: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct IfStmt<T> {
    pub(crate) condition: Expr<T>,
    pub(crate) then_branch: BlockStmt<T>,
    pub(crate) else_branch: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct BlockStmt<T> {
    pub(crate) stmts: Vec<Stmt<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ExprLit<T> {
    Bool(bool),
    U32(u32),
    U64(u64),
    U128(u128),
    Bfe(BFieldElement),
    Xfe(XFieldElement),
    Digest(Digest),
    GenericNum(u128, T),
}

impl<T> ExprLit<T> {
    pub(crate) fn label_friendly_name(&self) -> String {
        match self {
            ExprLit::Bool(b) => b.to_string(),
            ExprLit::U32(u32) => u32.to_string(),
            ExprLit::U64(val) => val.to_string(),
            ExprLit::U128(val) => val.to_string(),
            ExprLit::Bfe(val) => val.to_string(),
            ExprLit::Xfe(val) => val.to_string(),
            ExprLit::Digest(val) => val.to_string(),
            ExprLit::GenericNum(val, _) => val.to_string(),
        }
    }
}

impl<T> Display for ExprLit<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            ExprLit::Bool(b) => b.to_string(),
            ExprLit::U32(u32) => u32.to_string(),
            ExprLit::U64(val) => val.to_string(),
            ExprLit::U128(val) => val.to_string(),
            ExprLit::Bfe(val) => val.to_string(),
            ExprLit::Xfe(val) => val.to_string(),
            ExprLit::Digest(val) => val.to_string(),
            ExprLit::GenericNum(val, _) => val.to_string(),
        };
        write!(f, "{output}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MemPointerExpression<T> {
    /// Where in memory does the struct start?
    pub(crate) mem_pointer_address: Box<Expr<T>>,

    /// What type was used in the declaration of the memory pointer?
    pub(crate) mem_pointer_declared_type: DataType,

    /// Resolved type for binding
    pub(crate) resolved_type: T,
}

impl<T> Display for MemPointerExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.mem_pointer_address)
    }
}

impl<T> BFieldCodec for ExprLit<T> {
    type Error = anyhow::Error;

    fn decode(_sequence: &[BFieldElement]) -> anyhow::Result<Box<Self>> {
        todo!()
    }

    fn encode(&self) -> Vec<BFieldElement> {
        match self {
            ExprLit::Bool(value) => vec![BFieldElement::new(*value as u64)],
            ExprLit::U32(value) => value.encode(),
            ExprLit::U64(value) => value.encode(),
            ExprLit::U128(val) => val.encode(),
            ExprLit::Bfe(value) => value.encode(),
            ExprLit::Xfe(value) => value.encode(),
            ExprLit::Digest(value) => value.encode(),
            ExprLit::GenericNum(_, _) => todo!(),
        }
    }

    fn static_length() -> Option<usize> {
        None
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum BinOp {
    Add,
    And,
    BitAnd,
    BitXor,
    BitOr,
    Div,
    Eq,
    Lt,
    Gt,
    Mul,
    Neq,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum UnaryOp {
    Neg,
    Not,
    Deref,
    Ref(bool),
}

impl UnaryOp {
    pub(crate) fn label_friendly_name(&self) -> String {
        match self {
            UnaryOp::Neg => "negative".to_owned(),
            UnaryOp::Not => "not".to_owned(),
            UnaryOp::Deref => "deref".to_owned(),
            UnaryOp::Ref(mutable) => format!("ret_L{mutable}R"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Expr<T> {
    Lit(ExprLit<T>),
    EnumDeclaration(EnumDeclaration),
    Var(Identifier<T>),
    Tuple(Vec<Expr<T>>),
    Array(ArrayExpression<T>, T),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    Binop(Box<Expr<T>>, BinOp, Box<Expr<T>>, T),
    Unary(UnaryOp, Box<Expr<T>>, T),
    If(ExprIf<T>),
    Match(MatchExpr<T>),
    Cast(Box<Expr<T>>, DataType),
    ReturningBlock(Box<ReturningBlock<T>>),
    Struct(StructExpr<T>),
    Panic(PanicMacro, T),
    MemoryLocation(MemPointerExpression<T>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MatchExpr<T> {
    pub(crate) match_expression: Box<Expr<T>>,
    pub(crate) arms: Vec<MatchExprArm<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MatchExprArm<T> {
    pub(crate) match_condition: MatchCondition,
    pub(crate) body: ReturningBlock<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ArrayExpression<T> {
    ElementsSpecified(Vec<Expr<T>>),
    Repeat {
        element: Box<Expr<T>>,
        length: usize,
    },
}

impl<T> Display for ArrayExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ArrayExpression::ElementsSpecified(elements) => {
                format!("[{}]", elements.iter().join(","))
            }
            ArrayExpression::Repeat { element, length } => format!("[{element}; {length}]"),
        };
        write!(f, "{str}")
    }
}

impl<T> ArrayExpression<T> {
    pub(crate) fn len(&self) -> usize {
        match &self {
            ArrayExpression::ElementsSpecified(elements) => elements.len(),
            ArrayExpression::Repeat { length, .. } => *length,
        }
    }
}

impl<T> Expr<T> {
    pub(crate) fn label_friendly_name(&self) -> String {
        match self {
            Expr::Lit(lit) => lit.label_friendly_name(),
            Expr::Var(var) => var.label_friendly_name(),
            Expr::Tuple(vals) => format!(
                "tuple__L{}R__",
                vals.iter().map(|x| x.label_friendly_name()).join("_")
            ),
            Expr::Array(array_expr, _) => format!("array{}", array_expr.len(),),
            Expr::FnCall(_) => "fn_call".to_owned(),
            Expr::MethodCall(_) => "method_call_method_name".to_owned(),
            Expr::Binop(_, binop, _, _) => format!("binop_{binop:?}"),
            Expr::Unary(unaryop, _, _) => format!("unaryop_{}", unaryop.label_friendly_name()),
            Expr::If(_) => "if_else".to_owned(),
            Expr::Cast(_, dt) => format!("cast_{}", dt.label_friendly_name()),
            Expr::ReturningBlock(_) => "returning_block".to_owned(),
            Expr::Struct(struct_expr) => {
                format!("struct_expr_{}", struct_expr.label_friendly_name())
            }
            Expr::EnumDeclaration(enum_init) => {
                format!("enum_init_{}", enum_init.label_friendly_name())
            }
            Expr::Match(match_expr) => format!(
                "match_expr_{}",
                match_expr.match_expression.label_friendly_name()
            ),
            Expr::Panic(_, _) => "panic_macro".to_owned(),
            Expr::MemoryLocation(val) => {
                format!("MP_L{}R", val.mem_pointer_address.label_friendly_name())
            }
        }
    }
}

// Used for making nice value identifiers whose position you might be
// able to guess in the source code.
impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expr::Lit(lit) => lit.to_string(),
            Expr::Var(id) => id.to_string(),
            Expr::Tuple(inner) => format!("({})", inner.iter().join(",")),
            Expr::Array(array_type, _) => array_type.to_string(),
            Expr::FnCall(_) => "fn_call".to_owned(),
            Expr::MethodCall(MethodCall {
                method_name,
                args,
                annot: _,
                associated_type: _,
            }) => format!("{}.{method_name}", args[0]),
            Expr::Binop(_, binop, _, _) => format!("binop_{binop:?}"),
            Expr::If(_) => "if_else".to_owned(),
            Expr::Cast(_, dt) => format!("cast_{dt}"),
            Expr::Unary(unaryop, _, _) => format!("unaryop_{unaryop:?}"),
            Expr::ReturningBlock(_) => "returning_block".to_owned(),
            Expr::Struct(struct_expr) => {
                format!(
                    "struct_expression_for_{}",
                    struct_expr.struct_type.label_friendly_name()
                )
            }
            Expr::EnumDeclaration(enum_decl) => {
                format!(
                    "enum_declaration_for_{}",
                    enum_decl.enum_type.label_friendly_name()
                )
            }
            Expr::Match(match_expr) => format!("match expression: {}", match_expr.match_expression),
            Expr::Panic(_, _) => "panic".to_owned(),
            Expr::MemoryLocation(val) => format!("*{}", val),
        };

        write!(f, "{str}")
    }
}

/// Represents an enum variant without data. `let a = Foo::Bar;`
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct EnumDeclaration {
    // Needs to be `DataType` since we populate it with `Unresolved` in grafter
    pub(crate) enum_type: ast_types::DataType,
    pub(crate) variant_name: String,
}

impl EnumDeclaration {
    pub(crate) fn label_friendly_name(&self) -> String {
        format!(
            "enum_{}__{}",
            self.enum_type.label_friendly_name(),
            self.variant_name,
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct StructExpr<T> {
    pub(crate) struct_type: DataType,
    pub(crate) field_names_and_values: Vec<(String, Expr<T>)>,
}

impl<T> StructExpr<T> {
    pub(crate) fn label_friendly_name(&self) -> String {
        format!(
            "struct_{}__{}",
            self.struct_type.label_friendly_name(),
            self.field_names_and_values
                .iter()
                .map(|(n, v)| format!("n_{n}_v_{v}"))
                .join("__")
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ExprIf<T> {
    pub(crate) condition: Box<Expr<T>>,
    pub(crate) then_branch: Box<ReturningBlock<T>>,
    pub(crate) else_branch: Box<ReturningBlock<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ReturningBlock<T> {
    pub(crate) stmts: Vec<Stmt<T>>,
    pub(crate) return_expr: Expr<T>,
}

impl<T> Display for ReturningBlock<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "...\n{}", self.return_expr)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Identifier<T> {
    String(String, T),                               // x
    Index(Box<Identifier<T>>, Box<IndexExpr<T>>, T), // x[0]
    Field(Box<Identifier<T>>, FieldId, T),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum IndexExpr<T> {
    Dynamic(Expr<T>),
    Static(usize),
}

impl<T> Display for IndexExpr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            IndexExpr::Dynamic(expr) => format!("{expr}"),
            IndexExpr::Static(index) => index.to_string(),
        };
        write!(f, "{output}")
    }
}

impl<T> Display for Identifier<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Identifier::String(name, _) => name.to_string(),
            Identifier::Index(inner, index, _) => format!("{inner}[{index}]"),
            Identifier::Field(inner, field_name, _) => format!("{inner}.{field_name}"),
        };
        write!(f, "{output}")
    }
}

impl Identifier<Typing> {
    pub(crate) fn force_type(&mut self, forced_type: &DataType) {
        let forced_type = forced_type.to_owned();
        eprintln!("Forcing {self} to {forced_type}");
        match self {
            Identifier::String(_, t) => *t = crate::type_checker::Typing::KnownType(forced_type),
            Identifier::Index(_, _, t) => *t = crate::type_checker::Typing::KnownType(forced_type),
            Identifier::Field(_, _, t) => *t = crate::type_checker::Typing::KnownType(forced_type),
        }
    }

    pub(crate) fn resolved(&self) -> Option<DataType> {
        let t = match self {
            Identifier::String(_, t) => t,
            Identifier::Index(_, _, t) => t,
            Identifier::Field(_, _, t) => t,
        };
        match t {
            Typing::UnknownType => None,
            Typing::KnownType(resolved_type) => Some(resolved_type.to_owned()),
        }
    }
}

impl<T> Identifier<T> {
    pub(crate) fn binding_name(&self) -> String {
        match self {
            Identifier::String(name, _) => name.to_owned(),
            Identifier::Index(inner_id, _, _) => inner_id.binding_name(),
            Identifier::Field(inner_id, _, _) => inner_id.binding_name(),
        }
    }

    pub(crate) fn label_friendly_name(&self) -> String {
        match self {
            Identifier::String(name, _) => name.to_string(),
            Identifier::Index(inner_id, l_index_expr, _) => {
                format!("{}_{l_index_expr}", inner_id.label_friendly_name(),)
            }
            Identifier::Field(inner_id, field_name, _) => {
                format!("{}___{field_name}", inner_id.label_friendly_name())
            }
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct AssignStmt<T> {
    pub(crate) identifier: Identifier<T>,
    pub(crate) expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct LetStmt<T> {
    pub(crate) var_name: String,
    pub(crate) mutable: bool,
    pub(crate) data_type: DataType,
    pub(crate) expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct FnCall<T> {
    pub(crate) name: String,
    pub(crate) args: Vec<Expr<T>>,
    pub(crate) type_parameter: Option<DataType>,
    pub(crate) arg_evaluation_order: ArgEvaluationOrder,
    pub(crate) annot: T,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MethodCall<T> {
    pub(crate) method_name: String,
    pub(crate) args: Vec<Expr<T>>,
    pub(crate) annot: T,

    /// To type does this method belong? Not the same
    /// as receiver type, since receiver type can be
    /// `&self` or `Box<Self>`.
    pub(crate) associated_type: Option<DataType>,
}
