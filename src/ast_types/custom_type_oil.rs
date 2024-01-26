use anyhow::bail;

use super::DataType;
use super::EnumType;
use super::StructType;

/// Helper-type used during parsing to handle all
/// custom-types.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum CustomTypeOil {
    Struct(StructType),
    Enum(EnumType),
}

impl From<EnumType> for CustomTypeOil {
    fn from(value: EnumType) -> Self {
        CustomTypeOil::Enum(value)
    }
}

impl From<&EnumType> for CustomTypeOil {
    fn from(value: &EnumType) -> Self {
        CustomTypeOil::Enum(value.to_owned())
    }
}

impl TryFrom<&CustomTypeOil> for EnumType {
    type Error = anyhow::Error;

    fn try_from(value: &CustomTypeOil) -> Result<Self, Self::Error> {
        match value {
            CustomTypeOil::Struct(s) => bail!("Expected enum but found struct {s}"),
            CustomTypeOil::Enum(e) => Ok(e.to_owned()),
        }
    }
}

impl TryFrom<DataType> for CustomTypeOil {
    type Error = anyhow::Error;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        match value {
            DataType::Enum(enumt) => Ok(CustomTypeOil::Enum(*enumt.to_owned())),
            DataType::Struct(structt) => Ok(CustomTypeOil::Struct(structt.to_owned())),
            _ => bail!("Cannot convert {value} to specified type"),
        }
    }
}

impl TryFrom<CustomTypeOil> for EnumType {
    type Error = anyhow::Error;

    fn try_from(value: CustomTypeOil) -> Result<Self, Self::Error> {
        match value {
            CustomTypeOil::Struct(s) => bail!("Expected enum but found struct {s}"),
            CustomTypeOil::Enum(e) => Ok(e),
        }
    }
}

impl From<StructType> for CustomTypeOil {
    fn from(value: StructType) -> Self {
        CustomTypeOil::Struct(value)
    }
}

impl From<CustomTypeOil> for DataType {
    fn from(value: CustomTypeOil) -> Self {
        match value {
            CustomTypeOil::Struct(s) => DataType::Struct(s),
            CustomTypeOil::Enum(e) => DataType::Enum(Box::new(e)),
        }
    }
}

impl CustomTypeOil {
    pub(crate) fn name(&self) -> &str {
        match self {
            CustomTypeOil::Struct(s) => &s.name,
            CustomTypeOil::Enum(e) => &e.name,
        }
    }

    pub(crate) fn is_prelude(&self) -> bool {
        match self {
            CustomTypeOil::Struct(_) => false,
            CustomTypeOil::Enum(e) => e.is_prelude,
        }
    }

    pub(crate) fn field_or_variant_types_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut DataType> + 'a> {
        match self {
            CustomTypeOil::Struct(struct_type) => struct_type.field_types_mut(),
            CustomTypeOil::Enum(enum_type) => enum_type.variant_types_mut(),
        }
    }

    pub(crate) fn field_or_variant_types<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = &'a DataType> + 'a> {
        match self {
            CustomTypeOil::Struct(struct_type) => struct_type.field_types(),
            CustomTypeOil::Enum(enum_type) => enum_type.variant_types(),
        }
    }
}
