use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;

use anyhow::bail;
use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnSignature {
    pub name: String,
    pub args: Vec<FnArg>,
    pub output: DataType,
    pub arg_evaluation_order: ArgEvaluationOrder,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub enum ArgEvaluationOrder {
    #[default]
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Fn<T> {
    pub fn_signature: FnSignature,
    pub body: Vec<Stmt<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnArg {
    pub name: String,
    pub data_type: DataType,
    pub mutable: bool,
}

impl Display for FnArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt<T> {
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
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AssertStmt<T> {
    pub expression: Expr<T>,
    // pub decription: Option<String>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WhileStmt<T> {
    pub condition: Expr<T>,
    pub block: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IfStmt<T> {
    pub condition: Expr<T>,
    pub then_branch: BlockStmt<T>,
    pub else_branch: BlockStmt<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BlockStmt<T> {
    pub stmts: Vec<Stmt<T>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ExprLit<T> {
    Bool(bool),
    U32(u32),
    U64(u64),
    BFE(BFieldElement),
    XFE(XFieldElement),
    Digest(Digest),
    GenericNum(u64, T),
}

// FIXME: Use u64::to_sequence() after upgrading twenty-first
impl<T> Hashable for ExprLit<T> {
    fn to_sequence(&self) -> Vec<BFieldElement> {
        match self {
            ExprLit::Bool(value) => vec![BFieldElement::new(*value as u64)],
            ExprLit::U32(value) => value.to_sequence(),
            ExprLit::U64(value) => vec![
                BFieldElement::new(value & 0xffff_ffff),
                BFieldElement::new(value >> 32),
            ],
            ExprLit::BFE(value) => value.to_sequence(),
            ExprLit::XFE(value) => value.to_sequence(),
            ExprLit::Digest(value) => value.to_sequence(),
            ExprLit::GenericNum(_, _) => todo!(),
        }
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
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr<T> {
    Lit(ExprLit<T>),
    Var(Identifier<T>),
    Tuple(Vec<Expr<T>>),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    Binop(Box<Expr<T>>, BinOp, Box<Expr<T>>, T),
    Unary(UnaryOp, Box<Expr<T>>, T),
    If(ExprIf<T>),
    Cast(Box<Expr<T>>, DataType),
    // Index(Box<Expr<T>>, Box<Expr<T>>), // a_expr[i_expr]    (a + 5)[3]
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

// Used for making nice value identifiers whose position you might be
// able to guess in the source code.
impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expr::Lit(_lit) => "lit".to_owned(),
            Expr::Var(_) => "var_copy".to_owned(),
            Expr::Tuple(_) => "tuple".to_owned(),
            Expr::FnCall(_) => "fn_call".to_owned(),
            Expr::MethodCall(_) => "method_call.method_name".to_owned(),
            Expr::Binop(_, binop, _, _) => format!("binop_{binop:?}"),
            Expr::If(_) => "if_else".to_owned(),
            Expr::Cast(_, dt) => format!("cast_{dt}"),
            Expr::Unary(unaryop, _, _) => format!("unaryop_{unaryop:?}"),
        };

        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprIf<T> {
    pub condition: Box<Expr<T>>,
    pub then_branch: Box<Expr<T>>,
    pub else_branch: Box<Expr<T>>,
}

pub struct SymTable(HashMap<String, (u8, DataType)>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    U128,
    BFE,
    XFE,
    Digest,
    List(Box<DataType>),
    Tuple(Vec<DataType>),
}

impl DataType {
    /// Return the element type for lists
    pub fn type_parameter(&self) -> Option<DataType> {
        match self {
            DataType::List(element_type) => Some(*element_type.to_owned()),
            _ => None,
        }
    }

    pub fn unit() -> Self {
        Self::Tuple(vec![])
    }

    pub fn size_of(&self) -> usize {
        match self {
            Self::Bool => 1,
            Self::U32 => 1,
            Self::U64 => 2,
            Self::U128 => 4,
            Self::BFE => 1,
            Self::XFE => 3,
            Self::Digest => 5,
            Self::List(_list_type) => 1,
            Self::Tuple(tuple_type) => tuple_type.iter().map(Self::size_of).sum(),
        }
    }
}

impl TryFrom<DataType> for tasm_lib::snippet::DataType {
    type Error = String;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        match value {
            DataType::Bool => Ok(tasm_lib::snippet::DataType::Bool),
            DataType::U32 => Ok(tasm_lib::snippet::DataType::U32),
            DataType::U64 => Ok(tasm_lib::snippet::DataType::U64),
            DataType::U128 => Ok(tasm_lib::snippet::DataType::U128),
            DataType::BFE => Ok(tasm_lib::snippet::DataType::BFE),
            DataType::XFE => Ok(tasm_lib::snippet::DataType::XFE),
            DataType::Digest => Ok(tasm_lib::snippet::DataType::Digest),
            DataType::List(elem_type) => {
                let element_type = (*elem_type).try_into();
                let element_type = match element_type {
                    Ok(e) => e,
                    Err(err) => return Err(format!("Failed to convert element type of list: {err}")),
                };
                Ok(tasm_lib::snippet::DataType::List(Box::new(element_type)))
            },
            DataType::Tuple(_) => Err("Tuple cannot be converted to a tasm_lib type. Try converting its individual elements".to_string()),
        }
    }
}

impl From<tasm_lib::snippet::DataType> for DataType {
    fn from(value: tasm_lib::snippet::DataType) -> Self {
        match value {
            tasm_lib::snippet::DataType::Bool => DataType::Bool,
            tasm_lib::snippet::DataType::U32 => DataType::U32,
            tasm_lib::snippet::DataType::U64 => DataType::U64,
            tasm_lib::snippet::DataType::U128 => DataType::U128,
            tasm_lib::snippet::DataType::BFE => DataType::BFE,
            tasm_lib::snippet::DataType::XFE => DataType::XFE,
            tasm_lib::snippet::DataType::Digest => DataType::Digest,
            tasm_lib::snippet::DataType::List(elem_type_snip) => {
                let element_type: DataType = (*elem_type_snip).into();
                DataType::List(Box::new(element_type))
            }
        }
    }
}

impl FromStr for DataType {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(DataType::Bool),
            "u32" => Ok(DataType::U32),

            // `usize` is just an alias for `u32` in this compiler
            "usize" => Ok(DataType::U32),
            "u64" => Ok(DataType::U64),
            "BFieldElement" => Ok(DataType::BFE),
            "XFieldElement" => Ok(DataType::XFE),
            "Digest" => Ok(DataType::Digest),
            ty => bail!("Unsupported type {}", ty),
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
        write!(
            f,
            "{}",
            match self {
                Bool => "bool".to_string(),
                U32 => "u32".to_string(),
                U64 => "u64".to_string(),
                U128 => "u128".to_string(),
                BFE => "BField".to_string(),
                XFE => "XField".to_string(),
                Digest => "Digest".to_string(),
                List(ty) => format!("List({ty})"),
                Tuple(tys) => tys.iter().map(|ty| format!("{ty}")).join(" "),
            }
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Identifier<T> {
    String(String, T),                           // x
    TupleIndex(Box<Identifier<T>>, usize),       // x.0
    ListIndex(Box<Identifier<T>>, Box<Expr<T>>), // x[0]
}

impl<T: core::fmt::Debug> Display for Identifier<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Identifier::String(name, _) => name.to_string(),
                Identifier::TupleIndex(ident, t_index) => format!("{ident}.{t_index}"),
                Identifier::ListIndex(ident, l_index_expr) => format!("{ident}[{l_index_expr:?}]"),
            }
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AssignStmt<T> {
    pub identifier: Identifier<T>,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LetStmt<T> {
    pub var_name: String,
    pub mutable: bool,
    pub data_type: DataType,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnCall<T> {
    pub name: String,
    pub args: Vec<Expr<T>>,
    pub type_parameter: Option<DataType>,
    pub arg_evaluation_order: ArgEvaluationOrder,
    pub annot: T,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MethodCall<T> {
    pub method_name: String,
    pub args: Vec<Expr<T>>,
    pub annot: T,
}
