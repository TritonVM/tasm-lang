use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use super::DataType;
use super::FunctionType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum AbstractArgument {
    FunctionArgument(AbstractFunctionArg),
    ValueArgument(AbstractValueArg),
}

impl AbstractArgument {
    pub(crate) fn stack_size(&self) -> usize {
        match self {
            Self::FunctionArgument(_) => 0,
            Self::ValueArgument(arg) => arg.data_type.stack_size(),
        }
    }
}

impl Display for AbstractArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::FunctionArgument(arg) => write!(f, "{arg}"),
            Self::ValueArgument(arg) => write!(f, "{arg}"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct AbstractFunctionArg {
    pub(crate) abstract_name: String,
    pub(crate) function_type: FunctionType,
}

impl Display for AbstractFunctionArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "fn ({}): {}", self.abstract_name, self.function_type)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct AbstractValueArg {
    pub(crate) name: String,
    pub(crate) data_type: DataType,
    pub(crate) mutable: bool,
}

impl Display for AbstractValueArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}
