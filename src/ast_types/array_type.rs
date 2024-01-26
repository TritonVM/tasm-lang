use super::DataType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ArrayType {
    pub(crate) element_type: Box<DataType>,
    pub(crate) length: usize,
}

impl ArrayType {
    pub(crate) fn size_in_memory(&self) -> usize {
        self.element_type.stack_size() * self.length
    }
}

impl From<&ArrayType> for DataType {
    fn from(array_type: &ArrayType) -> Self {
        DataType::Array(array_type.to_owned())
    }
}
