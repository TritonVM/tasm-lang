use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;

use anyhow::bail;
use itertools::Itertools;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::x_field_element::XFieldElement;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnSignature {
    pub name: String,
    pub args: Vec<FnArg>,
    pub output: DataType,
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
pub enum ExprLit {
    Bool(bool),
    U32(u32),
    U64(u64),
    BFE(BFieldElement),
    XFE(XFieldElement),
    Digest(Digest),
}

impl ExprLit {
    pub fn get_type(&self) -> DataType {
        match self {
            ExprLit::Bool(_) => DataType::Bool,
            ExprLit::U32(_) => DataType::U32,
            ExprLit::U64(_) => DataType::U64,
            ExprLit::BFE(_) => DataType::BFE,
            ExprLit::XFE(_) => DataType::XFE,
            ExprLit::Digest(_) => DataType::Digest,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BinOp {
    Add,
    And,
    BitAnd,
    BitXor,
    Div,
    Eq,
    Lt,
    Mul,
    Neq,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr<T> {
    Lit(ExprLit, T),
    Var(Identifier<T>), // x[i]
    // Index(Box<Expr<T>>, Box<Expr<T>>), // a_expr[i_expr]    (a + 5)[3]
    FlatList(Vec<Expr<T>>),
    FnCall(FnCall<T>),
    MethodCall(MethodCall<T>),
    Binop(Box<Expr<T>>, BinOp, Box<Expr<T>>, T),
    If(ExprIf<T>),
    Cast(Box<Expr<T>>, DataType),
    // TODO: Overloaded arithmetic operators
    // TODO: VM-specific intrinsics (hash, absorb, squeeze, etc.)
}

impl Expr<Typing> {
    pub fn get_type(&self) -> DataType {
        match self {
            Expr::Lit(_, t) => t.get_type(),
            Expr::Var(id) => id.get_type(),
            Expr::FlatList(t_list) => {
                let types = t_list.iter().map(|elem| elem.get_type()).collect_vec();
                DataType::FlatList(types)
            }
            Expr::FnCall(fnc) => fnc.get_type(),
            Expr::MethodCall(mtc) => mtc.get_type(),
            Expr::Binop(_, _, _, t) => t.get_type(),
            Expr::If(if_expr) => if_expr.get_type(),
            Expr::Cast(_expr, t) => t.to_owned(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprIf<T> {
    pub condition: Box<Expr<T>>,
    pub then_branch: Box<Expr<T>>,
    pub else_branch: Box<Expr<T>>,
}

impl ExprIf<Typing> {
    pub fn get_type(&self) -> DataType {
        // The type check should have verified that the then branch and else branch
        // returns the same type. So we can just pick one of them.
        self.then_branch.get_type()
    }
}

pub struct SymTable(HashMap<String, (u8, DataType)>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    BFE,
    XFE,
    Digest,
    List(Box<DataType>),
    FlatList(Vec<DataType>),
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
        Self::FlatList(vec![])
    }
}

impl TryFrom<DataType> for tasm_lib::snippet::DataType {
    type Error = String;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        match value {
            DataType::Bool => Ok(tasm_lib::snippet::DataType::Bool),
            DataType::U32 => Ok(tasm_lib::snippet::DataType::U32),
            DataType::U64 => Ok(tasm_lib::snippet::DataType::U64),
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
            DataType::FlatList(_) => Err("FlatList cannot be converted to a tasm_lib type. Try converting its individual elements".to_string()),
        }
    }
}

impl From<tasm_lib::snippet::DataType> for DataType {
    fn from(value: tasm_lib::snippet::DataType) -> Self {
        match value {
            tasm_lib::snippet::DataType::Bool => DataType::Bool,
            tasm_lib::snippet::DataType::U32 => DataType::U32,
            tasm_lib::snippet::DataType::U64 => DataType::U64,
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
                BFE => "BField".to_string(),
                XFE => "XField".to_string(),
                Digest => "Digest".to_string(),
                List(ty) => format!("List({ty})"),
                FlatList(tys) => tys.iter().map(|ty| format!("{ty}")).join(" "),
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

impl Identifier<Typing> {
    /// Return the type for an identifier. Must be run after completed type annotation.
    pub fn get_type(&self) -> DataType {
        let t = match self {
            Identifier::String(_, t) => t.to_owned(),
            Identifier::TupleIndex(id, idx) => {
                let rec = id.get_type();
                match rec {
                    DataType::FlatList(list) => Typing::KnownType(list[*idx].clone()),
                    dt => panic!("Internal type error. Expected FlatList got: {dt:?}"),
                }
            }
            Identifier::ListIndex(id, _) => {
                let rec = id.get_type();
                match rec {
                    DataType::List(element_type) => Typing::KnownType(*element_type),
                    dt => panic!("Internal type error. Expected List got: {dt:?}"),
                }
            }
        };

        t.get_type()
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
    pub data_type: DataType,
    pub expr: Expr<T>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnCall<T> {
    pub name: String,
    pub args: Vec<Expr<T>>,
    pub type_parameter: Option<DataType>,
    pub annot: T,
}

impl FnCall<Typing> {
    pub fn get_type(&self) -> DataType {
        self.annot.get_type()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MethodCall<T> {
    pub method_name: String,
    pub args: Vec<Expr<T>>,
    pub annot: T,
}

impl MethodCall<Typing> {
    pub fn get_type(&self) -> DataType {
        self.annot.get_type()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Typing {
    UnknownType,
    KnownType(DataType),
}

impl Default for Typing {
    fn default() -> Self {
        Typing::UnknownType
    }
}

impl Typing {
    pub fn get_type(&self) -> DataType {
        match self {
            Typing::UnknownType => panic!("Cannot unpack type before complete type annotation."),
            Typing::KnownType(data_type) => data_type.clone(),
        }
    }
}
