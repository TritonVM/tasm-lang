use super::DataType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ArrayType {
    pub element_type: Box<DataType>,
    pub length: usize,
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
