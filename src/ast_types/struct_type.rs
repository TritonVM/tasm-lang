use std::fmt::Display;

use itertools::Itertools;

use super::DataType;
use super::FieldId;
use super::Tuple;
use crate::libraries::LibraryFunction;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct NamedFieldsStruct {
    pub(crate) fields: Vec<(String, DataType)>,
}

impl Display for NamedFieldsStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.fields
                .iter()
                .map(|(k, v)| format!("{k} => {v}"))
                .join(",")
        )
    }
}

impl NamedFieldsStruct {
    pub(crate) fn new(fields: Vec<(String, DataType)>) -> Self {
        Self { fields }
    }

    pub(crate) fn stack_size(&self) -> usize {
        self.fields
            .iter()
            .map(|(_, field_type)| field_type.stack_size())
            .sum()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum StructVariant {
    TupleStruct(Tuple),
    NamedFields(NamedFieldsStruct),
}

impl StructVariant {
    pub(crate) fn named_fields(fields: Vec<(String, DataType)>) -> Self {
        Self::NamedFields(NamedFieldsStruct::new(fields))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct StructType {
    pub(crate) name: String,
    pub(crate) is_copy: bool,
    pub(crate) variant: StructVariant,
}

impl From<StructType> for DataType {
    fn from(value: StructType) -> Self {
        DataType::Struct(value)
    }
}

impl From<&StructType> for DataType {
    fn from(value: &StructType) -> Self {
        value.to_owned().into()
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl StructType {
    /// Only named tuples, i.e. tuple structs should use this constructor.
    pub(crate) fn constructor(&self) -> LibraryFunction {
        let tuple = if let StructVariant::TupleStruct(tuple) = &self.variant {
            tuple
        } else {
            panic!("Only tuple structs have constructor functions. Attempted to get constructor for struct {}", self.name);
        };
        tuple.constructor(&self.name, DataType::Struct(self.to_owned()))
    }

    pub(crate) fn get_field_type(&self, field_id: &FieldId) -> DataType {
        let res = match &self.variant {
            StructVariant::TupleStruct(ts) => match field_id {
                FieldId::NamedField(_) => {
                    panic!("Cannot access tuple struct with named field '{field_id}'.")
                }

                FieldId::UnnamedField(tuple_index) => {
                    ts.fields.get(*tuple_index).map(|x| x.to_owned())
                }
            },
            StructVariant::NamedFields(nfs) => match field_id {
                FieldId::NamedField(field_name) => nfs
                    .fields
                    .iter()
                    .find(|&field| field.0 == *field_name)
                    .map(|x| x.1.to_owned()),
                FieldId::UnnamedField(tuple_index) => panic!("Cannot access struct with named fields with a tuple index. Got tuple index '{tuple_index}'.")
            },
        };

        match res {
            Some(dtype) => dtype,
            None => {
                let field_names = match &self.variant {
                    StructVariant::TupleStruct(tuple) => format!("0..{}", tuple.element_count()),
                    StructVariant::NamedFields(named_fields_struct) => {
                        format!("{named_fields_struct}")
                    }
                };
                panic!("Struct {self} has no field '{field_id}'. Known fields: {field_names}");
            }
        }
    }

    pub(crate) fn field_count(&self) -> usize {
        match &self.variant {
            StructVariant::TupleStruct(tuple) => tuple.element_count(),
            StructVariant::NamedFields(nfs) => nfs.fields.len(),
        }
    }

    pub(crate) fn field_types<'a>(&'a self) -> Box<dyn Iterator<Item = &'a DataType> + 'a> {
        match &self.variant {
            StructVariant::TupleStruct(ts) => Box::new(ts.fields.iter()),
            StructVariant::NamedFields(nfs) => {
                Box::new(nfs.fields.iter().map(|(_name, dtype)| dtype))
            }
        }
    }

    pub(crate) fn field_types_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut DataType> + 'a> {
        match &mut self.variant {
            StructVariant::TupleStruct(ts) => Box::new(ts.fields.iter_mut()),
            StructVariant::NamedFields(nfs) => {
                Box::new(nfs.fields.iter_mut().map(|(_name, dtype)| dtype))
            }
        }
    }

    pub(crate) fn field_ids_and_types<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (FieldId, &'a DataType)> + 'a> {
        match &self.variant {
            StructVariant::TupleStruct(ts) => Box::new(
                ts.fields
                    .iter()
                    .enumerate()
                    .map(|(tuple_idx, element_type)| (tuple_idx.into(), element_type)),
            ),
            StructVariant::NamedFields(nfs) => Box::new(
                nfs.fields
                    .iter()
                    .map(|(field_name, element_type)| (field_name.into(), element_type)),
            ),
        }
    }

    /// Iterate over all fields in a type in reverse order. Needed since the
    /// "natural" order of fields is flipped whether the struct lives on
    /// stack or in memory.
    pub(crate) fn field_ids_and_types_reversed<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (FieldId, &'a DataType)> + 'a> {
        match &self.variant {
            StructVariant::TupleStruct(ts) => ts.field_ids_and_types_reversed(),
            StructVariant::NamedFields(nfs) => Box::new(
                nfs.fields
                    .iter()
                    .rev()
                    .map(|(field_name, element_type)| (field_name.into(), element_type)),
            ),
        }
    }

    pub(crate) fn label_friendly_name(&self) -> String {
        self.name.to_owned()
    }
}
