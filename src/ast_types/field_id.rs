use std::fmt::Display;

use anyhow::bail;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum FieldId {
    NamedField(String),
    UnnamedField(usize),
}

impl From<&str> for FieldId {
    fn from(value: &str) -> Self {
        Self::NamedField(value.to_owned())
    }
}

impl From<&String> for FieldId {
    fn from(value: &String) -> Self {
        Self::NamedField(value.to_owned())
    }
}

impl From<String> for FieldId {
    fn from(value: String) -> Self {
        Self::NamedField(value)
    }
}

impl From<&usize> for FieldId {
    fn from(value: &usize) -> Self {
        Self::UnnamedField(*value)
    }
}

impl From<usize> for FieldId {
    fn from(value: usize) -> Self {
        Self::UnnamedField(value)
    }
}

impl From<u32> for FieldId {
    fn from(value: u32) -> Self {
        Self::UnnamedField(value as usize)
    }
}

impl TryFrom<&FieldId> for usize {
    type Error = anyhow::Error;

    fn try_from(value: &FieldId) -> Result<Self, Self::Error> {
        match value {
            FieldId::NamedField(_) => bail!("Cannot convert named field to usize"),
            FieldId::UnnamedField(tuple_index) => Ok(*tuple_index),
        }
    }
}

impl Display for FieldId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            FieldId::NamedField(name) => name.to_owned(),
            FieldId::UnnamedField(tuple_index) => tuple_index.to_string(),
        };
        write!(f, "{output}")
    }
}
