use std::fmt::Display;

use super::DataType;
use super::FunctionType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AbstractArgument {
    FunctionArgument(AbstractFunctionArg),
    ValueArgument(AbstractValueArg),
}

impl Display for AbstractArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AbstractArgument::ValueArgument(val_arg) => {
                    format!("{}: {}", val_arg.name, val_arg.data_type)
                }
                AbstractArgument::FunctionArgument(fun_arg) => {
                    format!("fn ({}): {}", fun_arg.abstract_name, fun_arg.function_type)
                }
            }
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AbstractFunctionArg {
    pub abstract_name: String,
    pub function_type: FunctionType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AbstractValueArg {
    pub name: String,
    pub data_type: DataType,
    pub mutable: bool,
}

impl Display for AbstractValueArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}
