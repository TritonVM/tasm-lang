use itertools::Itertools;
use std::fmt::Display;
use triton_vm::{instruction::LabelledInstruction, Digest};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::{
    ast_types::{self, AbstractArgument, DataType, FieldId},
    type_checker::Typing,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum RoutineBody<T> {
    Ast(Vec<Stmt<T>>),
    Instructions(Vec<LabelledInstruction>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Method<T> {
    pub signature: FnSignature,
    pub body: RoutineBody<T>,
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
    pub fn as_ast_function(self, new_name: &str) -> Fn<T> {
        let mut fn_signature = self.signature;
        fn_signature.name = new_name.to_owned();
        Fn {
            signature: fn_signature,
            body: self.body,
        }
    }
}

impl<T> Method<T> {
    pub fn receiver_type(&self) -> crate::ast_types::DataType {
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
    pub fn get_tasm_label(&self) -> String {
        let receiver_type = self.receiver_type().label_friendly_name();
        let method_name = self.signature.name.to_owned();
        format!("method_{receiver_type}_{method_name}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Fn<T> {
    pub signature: FnSignature,
    // TODO: Should probably be a BlockStmt<T> instead of Vec<Stmt>
    pub body: RoutineBody<T>,
}

impl<T> Fn<T> {
    pub fn get_tasm_label(&self) -> String {
        self.signature.name.replace("::", "_assoc_funciton___of___")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct FnSignature {
    pub name: String,
    pub args: Vec<AbstractArgument>,
    pub output: DataType,
    pub arg_evaluation_order: ArgEvaluationOrder,
}

impl FnSignature {
    pub(crate) fn input_arguments_stack_size(&self) -> usize {
        let mut input_args_stack_size = 0;
        for arg in self.args.iter() {
            input_args_stack_size += match arg {
                AbstractArgument::FunctionArgument(_) => 0,
                AbstractArgument::ValueArgument(val_arg) => val_arg.data_type.stack_size(),
            }
        }

        input_args_stack_size
    }

    pub fn matches(&self, types: &[ast_types::DataType]) -> bool {
        if self.args.len() != types.len() {
            return false;
        }

        for (arg, dtype) in self.args.iter().zip_eq(types.iter()) {
            match arg {
                AbstractArgument::FunctionArgument(fun_arg) => {
                    let ast_types::DataType::Function(fun) = dtype else {
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
    Assign(AssignStmt<T>),
    Return(Option<Expr<T>>),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    While(WhileStmt<T>),
    If(IfStmt<T>),
    Block(BlockStmt<T>),
    Assert(AssertStmt<T>),
    FnDeclaration(Fn<T>),
    Match(MatchStmt<T>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MatchStmt<T> {
    pub match_expression: Expr<T>,
    pub arms: Vec<MatchArm<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MatchArm<T> {
    pub match_condition: MatchCondition,
    pub body: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum MatchCondition {
    CatchAll,
    EnumVariant(EnumVariantSelector),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct EnumVariantSelector {
    // `Bar` in `Bar::Foo(baz)`
    pub enum_name: String,

    // `Foo`
    pub variant_name: String,

    // `baz`
    pub data_bindings: Vec<PatternMatchedBinding>,
}

impl EnumVariantSelector {
    /// Return the label for the subroutine that creates the bindings defined in this
    /// match-arm.
    pub(crate) fn label_for_binding_subroutine(&self, boxed: bool) -> String {
        let boxed_qualifier = if boxed { "boxed" } else { "stack" };
        format!(
            "{}_{}_bind_{}_{boxed_qualifier}",
            self.enum_name,
            self.variant_name,
            self.data_bindings.len()
        )
    }

    /// Return the composite type of this match-arm binding
    pub(crate) fn get_bindings_type(&self, data_type: &ast_types::Tuple) -> ast_types::Tuple {
        if self.data_bindings.is_empty() {
            ast_types::Tuple::unit()
        } else {
            data_type.to_owned()
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct PatternMatchedBinding {
    pub name: String,
    pub mutable: bool,
    // Add `ref` here also?
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct AssertStmt<T> {
    pub expression: Expr<T>,
    // pub decription: Option<String>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct WhileStmt<T> {
    pub condition: Expr<T>,
    pub block: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct IfStmt<T> {
    pub condition: Expr<T>,
    pub then_branch: BlockStmt<T>,
    pub else_branch: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct BlockStmt<T> {
    pub stmts: Vec<Stmt<T>>,
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
    MemPointer(MemPointerLiteral<T>),
    GenericNum(u128, T),
}

impl<T> ExprLit<T> {
    pub fn label_friendly_name(&self) -> String {
        match self {
            ExprLit::Bool(b) => b.to_string(),
            ExprLit::U32(u32) => u32.to_string(),
            ExprLit::U64(val) => val.to_string(),
            ExprLit::U128(val) => val.to_string(),
            ExprLit::Bfe(val) => val.to_string(),
            ExprLit::Xfe(val) => val.to_string(),
            ExprLit::Digest(val) => val.to_string(),
            ExprLit::MemPointer(val) => format!("MP_L{}R", val.mem_pointer_address),
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
            ExprLit::MemPointer(val) => format!("*{}", val),
            ExprLit::GenericNum(val, _) => val.to_string(),
        };
        write!(f, "{output}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MemPointerLiteral<T> {
    /// Where in memory does the struct start?
    pub mem_pointer_address: BFieldElement,

    /// What type was used in the declaration of the memory pointer?
    pub mem_pointer_declared_type: DataType,

    // Resolved type for binding
    pub resolved_type: T,
}

impl<T> Display for MemPointerLiteral<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.mem_pointer_address)
    }
}

impl<T> BFieldCodec for ExprLit<T> {
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
            ExprLit::MemPointer(_) => todo!(),
        }
    }

    fn static_length() -> Option<usize> {
        None
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BinOp {
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
pub enum UnaryOp {
    Neg,
    Not,
    Deref,
    Ref(bool),
}

impl UnaryOp {
    pub fn label_friendly_name(&self) -> String {
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
    Cast(Box<Expr<T>>, DataType),
    ReturningBlock(Box<ReturningBlock<T>>),
    Struct(StructExpr<T>),
    // Index(Box<Expr<T>>, Box<Expr<T>>), // a_expr[i_expr]    (a + 5)[3]
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ArrayExpression<T> {
    ElementsSpecified(Vec<Expr<T>>),
}

impl<T> Display for ArrayExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ArrayExpression::ElementsSpecified(elements) => {
                format!("[{}]", elements.iter().join(","))
            }
        };
        write!(f, "{str}")
    }
}

impl<T> ArrayExpression<T> {
    pub fn len(&self) -> usize {
        match &self {
            ArrayExpression::ElementsSpecified(elements) => elements.len(),
        }
    }
}

impl<T> Expr<T> {
    pub fn label_friendly_name(&self) -> String {
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
        };

        write!(f, "{str}")
    }
}

/// Represents an enum variant without data. `let a = Foo::Bar;`
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct EnumDeclaration {
    // Needs to be `DataType` since we populate it with `Unresolved` in grafter
    pub enum_type: ast_types::DataType,
    pub variant_name: String,
}

impl EnumDeclaration {
    pub fn label_friendly_name(&self) -> String {
        format!(
            "enum_{}__{}",
            self.enum_type.label_friendly_name(),
            self.variant_name,
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct StructExpr<T> {
    pub struct_type: DataType,
    pub field_names_and_values: Vec<(String, Expr<T>)>,
}

impl<T> StructExpr<T> {
    pub fn label_friendly_name(&self) -> String {
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
    pub condition: Box<Expr<T>>,
    pub then_branch: Box<ReturningBlock<T>>,
    pub else_branch: Box<ReturningBlock<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ReturningBlock<T> {
    pub stmts: Vec<Stmt<T>>,
    pub return_expr: Expr<T>,
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
    pub fn force_type(&mut self, forced_type: &DataType) {
        let forced_type = forced_type.to_owned();
        eprintln!("Forcing {self} to {forced_type}");
        match self {
            Identifier::String(_, t) => *t = crate::type_checker::Typing::KnownType(forced_type),
            Identifier::Index(_, _, t) => *t = crate::type_checker::Typing::KnownType(forced_type),
            Identifier::Field(_, _, t) => *t = crate::type_checker::Typing::KnownType(forced_type),
        }
    }

    pub fn resolved(&self) -> Option<DataType> {
        let t = match self {
            Identifier::String(_, t) => t,
            Identifier::Index(_, _, t) => t,
            Identifier::Field(_, _, t) => t,
        };
        // matches!(t, Typing::KnownType(_))
        match t {
            Typing::UnknownType => None,
            Typing::KnownType(resolved_type) => Some(resolved_type.to_owned()),
        }
    }
}

impl<T> Identifier<T> {
    pub fn binding_name(&self) -> String {
        match self {
            Identifier::String(name, _) => name.to_owned(),
            Identifier::Index(inner_id, _, _) => inner_id.binding_name(),
            Identifier::Field(inner_id, _, _) => inner_id.binding_name(),
        }
    }

    pub fn label_friendly_name(&self) -> String {
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
    pub identifier: Identifier<T>,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct LetStmt<T> {
    pub var_name: String,
    pub mutable: bool,
    pub data_type: DataType,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct FnCall<T> {
    pub name: String,
    pub args: Vec<Expr<T>>,
    pub type_parameter: Option<DataType>,
    pub arg_evaluation_order: ArgEvaluationOrder,
    pub annot: T,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MethodCall<T> {
    pub method_name: String,
    pub args: Vec<Expr<T>>,
    pub annot: T,

    /// To type does this method belong? Not the same
    /// same as receiver type, since receiver type can be
    /// `&self` or `Box<Self>`.
    pub associated_type: Option<DataType>,
}
