use std::fmt::Display;

use super::DataType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct FunctionType {
    pub(crate) input_argument: DataType,
    pub(crate) output: DataType,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.input_argument, self.output)
    }
}
