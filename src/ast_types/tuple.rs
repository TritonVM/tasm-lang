use itertools::Itertools;
use triton_vm::triton_asm;

use crate::ast::FnSignature;
use crate::libraries::LibraryFunction;

use super::AbstractArgument;
use super::AbstractValueArg;
use super::DataType;
use super::FieldId;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Tuple {
    pub(crate) fields: Vec<DataType>,
}

impl From<Tuple> for DataType {
    fn from(value: Tuple) -> Self {
        DataType::Tuple(value)
    }
}

impl From<Vec<DataType>> for Tuple {
    fn from(fields: Vec<DataType>) -> Self {
        Self { fields }
    }
}

impl IntoIterator for Tuple {
    type Item = DataType;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}

impl<'a> IntoIterator for &'a Tuple {
    type Item = &'a DataType;
    type IntoIter = std::slice::Iter<'a, DataType>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter()
    }
}

impl<'a> IntoIterator for &'a mut Tuple {
    type Item = &'a mut DataType;
    type IntoIter = std::slice::IterMut<'a, DataType>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter_mut()
    }
}

impl std::ops::Index<usize> for Tuple {
    type Output = DataType;

    fn index(&self, index: usize) -> &Self::Output {
        &self.fields[index]
    }
}

impl Tuple {
    pub(crate) fn element_count(&self) -> usize {
        self.fields.len()
    }

    pub(crate) fn unit() -> Self {
        Self { fields: vec![] }
    }

    pub(crate) fn is_unit(&self) -> bool {
        self.fields.is_empty()
    }

    pub(crate) fn stack_size(&self) -> usize {
        self.into_iter().map(|x| x.stack_size()).sum()
    }

    pub(crate) fn is_copy(&self) -> bool {
        self.fields.iter().all(|x| x.is_copy())
    }

    pub(crate) fn label_friendly_name(&self) -> String {
        format!(
            "tuple_L{}R",
            self.into_iter().map(|x| x.label_friendly_name()).join("_")
        )
    }

    pub(crate) fn field_ids_and_types_reversed<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (FieldId, &'a DataType)> + 'a> {
        Box::new(
            self.fields
                .iter()
                .enumerate()
                .rev()
                .map(|(tuple_idx, element_type)| (tuple_idx.into(), element_type)),
        )
    }

    pub(crate) fn constructor(
        &self,
        tuple_struct_name: &str,
        tuple_struct_type: DataType,
    ) -> LibraryFunction {
        let args = self
            .fields
            .iter()
            .enumerate()
            .map(|(i, x)| {
                AbstractArgument::ValueArgument(AbstractValueArg {
                    name: format!("tuple_elem_{i}"),
                    data_type: x.to_owned(),
                    mutable: false,
                })
            })
            .collect_vec();
        let signature = FnSignature {
            name: tuple_struct_name.to_owned(),
            args,
            output: tuple_struct_type.to_owned(),
            arg_evaluation_order: Default::default(),
        };

        // Function body of the tuple-struct constructor is empty, since
        // the construction simply corresponds to the evaluation of arguments
        // from left-to-rigth, as this will leave the last element of the
        // tuple on top of the stack.
        let body = triton_asm!();
        LibraryFunction { signature, body }
    }
}
