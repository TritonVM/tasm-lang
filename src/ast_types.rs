use std::collections::HashMap;
use std::fmt::Display;
use std::str::FromStr;

use anyhow::bail;
use itertools::Itertools;
use regex::Regex;
use tasm_lib::triton_vm::table::{NUM_BASE_COLUMNS, NUM_EXT_COLUMNS, NUM_QUOTIENT_SEGMENTS};

use crate::ast::FnSignature;
use crate::libraries::polynomial::PolynomialCoefficientType;

pub(crate) use self::abstract_argument::*;
pub(crate) use self::array_type::ArrayType;
pub(crate) use self::custom_type_oil::CustomTypeOil;
pub(crate) use self::enum_type::EnumType;
pub(crate) use self::field_id::FieldId;
pub(crate) use self::function_type::FunctionType;
pub(crate) use self::struct_type::*;
pub(crate) use self::tuple::Tuple;

pub(crate) mod abstract_argument;
pub(crate) mod array_type;
pub(crate) mod custom_type_oil;
pub(crate) mod enum_type;
pub(crate) mod field_id;
pub(crate) mod function_type;
pub(crate) mod struct_type;
pub(crate) mod tuple;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum DataType {
    Bool,
    U32,
    U64,
    U128,
    Bfe,
    Xfe,
    Digest,
    List(Box<DataType>),
    Tuple(Tuple),
    Array(ArrayType),
    Struct(StructType),
    Enum(Box<EnumType>),
    VoidPointer,
    Function(Box<FunctionType>),
    Boxed(Box<DataType>),
    Unresolved(String),
}

impl FromStr for DataType {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(DataType::Bool),
            "u32" => Ok(DataType::U32),

            // `usize` is just an alias for `u32` in this compiler
            "usize" => Ok(DataType::U32),
            "u64" => Ok(DataType::U64),
            "u128" => Ok(DataType::U128),
            "BFieldElement" => Ok(DataType::Bfe),
            "XFieldElement" => Ok(DataType::Xfe),
            "Digest" => Ok(DataType::Digest),
            ty => bail!("Unsupported type {}", ty),
        }
    }
}

impl DataType {
    pub(crate) fn try_from_string(type_str: &str) -> Result<Self, ()> {
        let box_regex = Regex::new(r"Box<(?<inner>.+)>").unwrap();
        if let Some(caps) = box_regex.captures(type_str) {
            let inner_parsed = Self::try_from_string(&caps["inner"])?;
            return Ok(DataType::Boxed(Box::new(inner_parsed)));
        }

        let vec_regex = Regex::new(r"Vec<(?<inner>.+)>").unwrap();
        if let Some(caps) = vec_regex.captures(type_str) {
            let inner_parsed = Self::try_from_string(&caps["inner"])?;
            return Ok(DataType::List(Box::new(inner_parsed)));
        }

        let array_regex = Regex::new(r"\[(?<inner>.+); (?<array_size>.+)\]").unwrap();
        if let Some(caps) = array_regex.captures(type_str) {
            let known_constants: HashMap<&str, usize> =
                [("NUM_QUOTIENT_SEGMENTS", 4)].into_iter().collect();
            let inner_parsed = Self::try_from_string(&caps["inner"])?;
            let array_size_indication = &caps["array_size"];
            let parsed_size = if known_constants.contains_key(&array_size_indication) {
                known_constants[&array_size_indication]
            } else {
                usize::from_str(array_size_indication).unwrap_or_else(|_| {
                    panic!("Could not parse {array_size_indication} as array size")
                })
            };
            return Ok(DataType::Array(ArrayType {
                element_type: Box::new(inner_parsed),
                length: parsed_size,
            }));
        }

        match type_str {
            "bool" => Ok(DataType::Bool),
            "u32" => Ok(DataType::U32),

            // `usize` is just an alias for `u32` in this compiler
            "usize" => Ok(DataType::U32),
            "u64" => Ok(DataType::U64),
            "u128" => Ok(DataType::U128),
            "BFieldElement" => Ok(DataType::Bfe),
            "XFieldElement" => Ok(DataType::Xfe),
            "Digest" => Ok(DataType::Digest),

            "AuthenticationStructure" => Ok(DataType::List(Box::new(DataType::Digest))),
            "FriResponse" => Ok(DataType::Unresolved(type_str.to_owned())),
            "BaseRow<XFieldElement>" => Ok(DataType::Array(ArrayType {
                element_type: Box::new(DataType::Xfe),
                length: NUM_BASE_COLUMNS,
            })),
            "BaseRow<BFieldElement>" => Ok(DataType::Array(ArrayType {
                element_type: Box::new(DataType::Bfe),
                length: NUM_BASE_COLUMNS,
            })),
            "ExtensionRow" => Ok(DataType::Array(ArrayType {
                element_type: Box::new(DataType::Xfe),
                length: NUM_EXT_COLUMNS,
            })),
            "QuotientSegments" => Ok(DataType::Array(ArrayType {
                element_type: Box::new(DataType::Xfe),
                length: NUM_QUOTIENT_SEGMENTS,
            })),
            _ => todo!("{type_str}"),
        }
    }

    /// Return true if it's OK to copy this type including its underlying
    /// data, false if it's expensive to copy.
    pub(crate) fn is_copy(&self) -> bool {
        match self {
            DataType::Bool => true,
            DataType::U32 => true,
            DataType::U64 => true,
            DataType::U128 => true,
            DataType::Bfe => true,
            DataType::Xfe => true,
            DataType::Digest => true,
            DataType::Tuple(tuple) => tuple.is_copy(),
            DataType::Array(array_type) => array_type.element_type.is_copy(),
            DataType::List(_) => false,
            DataType::Polynomial(_) => false,
            DataType::VoidPointer => false,
            DataType::Function(_) => false,
            DataType::Struct(struct_type) => struct_type.is_copy,
            DataType::Enum(enum_type) => enum_type.is_copy,
            DataType::Unresolved(_) => false,
            DataType::Boxed(_) => false,
        }
    }

    /// Return true if this type only has a pointer into memory on the stack
    pub(crate) fn is_pointer(&self) -> bool {
        matches!(
            self,
            DataType::List(_) | DataType::Array(_) | DataType::VoidPointer | DataType::Boxed(_)
        )
    }

    /// Use this if the type is used to make labels in the TASM code
    pub(crate) fn label_friendly_name(&self) -> String {
        use DataType::*;
        match self {
            Bool => "bool".to_string(),
            U32 => "u32".to_string(),
            U64 => "u64".to_string(),
            U128 => "u128".to_string(),
            Bfe => "BField".to_string(),
            Xfe => "XField".to_string(),
            Digest => "Digest".to_string(),
            List(ty) => format!("Vec_L{}R", ty.label_friendly_name()),
            Polynomial(ty) => format!("Polynomial_L{}R", ty.label_friendly_name()),
            Array(array_type) => format!(
                "array{}_of_L{}R",
                array_type.length,
                array_type.element_type.label_friendly_name()
            ),
            Tuple(tys) => tys.label_friendly_name(),
            VoidPointer => "void_pointer".to_string(),
            Function(fn_type) => {
                let input = fn_type.input_argument.label_friendly_name();
                let output = fn_type.output.label_friendly_name();
                format!("function_from_L{}R__to_L{}R", input, output)
            }
            Struct(struct_type) => struct_type.label_friendly_name(),
            Enum(enum_type) => enum_type.label_friendly_name(),
            Unresolved(name) => name.to_string(),
            Boxed(ty) => format!("boxed_L{}R", ty.label_friendly_name()),
        }
    }

    /// What type is returned when type is accessed with a field of name `field_name`?
    pub(crate) fn field_access_returned_type(&self, field_id: &FieldId) -> Self {
        match &self {
            DataType::Boxed(inner_type) => match &**inner_type {
                DataType::Struct(struct_type) => {
                    let field_type = struct_type.get_field_type(field_id);
                    if field_type.is_copy() {
                        field_type
                    } else {
                        DataType::Boxed(Box::new(field_type))
                    }
                }
                DataType::Tuple(tuple) => {
                    let tuple_index: usize = field_id
                        .try_into()
                        .expect("Tuple must be accessed with a tuple index");
                    let field_type = tuple.fields[tuple_index].clone();
                    if field_type.is_copy() {
                        field_type
                    } else {
                        DataType::Boxed(Box::new(field_type))
                    }
                }
                _ => panic!(
                    "Field getter can only operate on type of struct or pointer to struct. \
                    Attempted to access field `{field_id}` on type `{self}`"
                ),
            },
            DataType::Struct(struct_type) => struct_type.get_field_type(field_id),
            DataType::Tuple(tuple) => {
                let tuple_index: usize = field_id
                    .try_into()
                    .expect("Tuple must be accessed with a tuple index");
                tuple.fields[tuple_index].clone()
            }
            _ => panic!(
                "Field getter can only operate on type of struct or pointer to struct. \
                Attempted to access field `{field_id}` on type `{self}`"
            ),
        }
    }

    // Notice that this implementation must match that derived by `BFieldCodec`
    pub(crate) fn bfield_codec_static_length(&self) -> Option<usize> {
        match self {
            DataType::Bool => Some(1),
            DataType::U32 => Some(1),
            DataType::U64 => Some(2),
            DataType::U128 => Some(4),
            DataType::Bfe => Some(1),
            DataType::Xfe => Some(3),
            DataType::Digest => Some(5),
            DataType::Array(array_type) => {
                Some(array_type.element_type.bfield_codec_static_length()? * array_type.length)
            }
            DataType::Tuple(inner_types) => inner_types
                .into_iter()
                .map(|x| x.bfield_codec_static_length())
                .try_fold(0, |acc: usize, x| x.map(|x| x + acc)),
            DataType::List(_) => None,
            DataType::Polynomial(_) => None,
            DataType::Struct(struct_type) => struct_type
                .field_types()
                .try_fold(0, |acc: usize, field_type| {
                    field_type.bfield_codec_static_length().map(|v| acc + v)
                }),
            DataType::Enum(enum_type) => enum_type.bfield_codec_static_length(),
            DataType::Boxed(inner_type) => inner_type.bfield_codec_static_length(),
            DataType::VoidPointer => panic!(),
            DataType::Function(_) => panic!(),
            DataType::Unresolved(_) => panic!(),
        }
    }

    pub(crate) fn unit() -> Self {
        Self::Tuple(vec![].into())
    }

    pub(crate) fn is_unit(&self) -> bool {
        *self == Self::unit()
    }

    pub(crate) fn stack_size(&self) -> usize {
        match self {
            Self::Bool => 1,
            Self::U32 => 1,
            Self::U64 => 2,
            Self::U128 => 4,
            Self::Bfe => 1,
            Self::Xfe => 3,
            Self::Digest => 5,
            Self::List(_) => 1,
            Self::Polynomial(_) => 1,
            Self::Array(_) => 1,
            Self::Tuple(tuple_type) => tuple_type.stack_size(),
            Self::VoidPointer => 1,
            Self::Unresolved(name) => panic!("cannot get size of unresolved type {name}"),
            Self::Struct(inner_type) => inner_type.stack_size(),
            Self::Enum(enum_type) => enum_type.stack_size(),
            Self::Boxed(_) => 1,
            Self::Function(_) => 0, // Exists as instructions only
        }
    }

    pub(crate) fn unbox(&self) -> DataType {
        match self {
            DataType::Boxed(inner) => inner.unbox(),
            dtype => dtype.to_owned(),
        }
    }

    pub(crate) fn as_enum_type(&self) -> EnumType {
        match self {
            DataType::Enum(enum_type) => *enum_type.to_owned(),
            _ => panic!("Expected enum type. Got: {self}"),
        }
    }

    pub(crate) fn as_tuple_type(&self) -> Tuple {
        match self {
            DataType::Tuple(tuple_type) => tuple_type.to_owned(),
            other => Tuple {
                fields: vec![other.to_owned()],
            },
        }
    }

    fn tasm_lib_struct_to_lang_struct(struct_type: tasm_lib::data_type::StructType) -> Self {
        let tasm_lib::data_type::StructType { name, fields } = struct_type;
        let fields = fields
            .into_iter()
            .map(|(name, dtype)| (name, dtype.try_into().unwrap()))
            .collect();

        let named_fields_struct = NamedFieldsStruct { fields };
        let variant = StructVariant::NamedFields(named_fields_struct);
        let new_struct_type = StructType {
            name,
            is_copy: false,
            variant,
        };

        Self::Struct(new_struct_type)
    }
}

impl TryFrom<tasm_lib::data_type::DataType> for DataType {
    type Error = String;

    fn try_from(tasm_lib_type: tasm_lib::data_type::DataType) -> Result<Self, Self::Error> {
        use DataType::*;
        let resolved = match tasm_lib_type {
            tasm_lib::data_type::DataType::Bool => Bool,
            tasm_lib::data_type::DataType::U32 => U32,
            tasm_lib::data_type::DataType::U64 => U64,
            tasm_lib::data_type::DataType::U128 => U128,
            tasm_lib::data_type::DataType::Bfe => Bfe,
            tasm_lib::data_type::DataType::Xfe => Xfe,
            tasm_lib::data_type::DataType::Digest => Digest,
            tasm_lib::data_type::DataType::List(elem_type) => {
                let element_type = (*elem_type).try_into()?;
                List(Box::new(element_type))
            }
            tasm_lib::data_type::DataType::Array(array_type) => {
                let element_type = array_type.element_type.try_into()?;
                Array(ArrayType {
                    element_type: Box::new(element_type),
                    length: array_type.length,
                })
            }
            tasm_lib::data_type::DataType::Tuple(tasm_types) => {
                let mut element_types: Vec<DataType> = vec![];
                for tasm_type in tasm_types.into_iter() {
                    element_types.push(tasm_type.try_into()?);
                }

                Tuple(element_types.into())
            }
            tasm_lib::data_type::DataType::VoidPointer => VoidPointer,
            tasm_lib::data_type::DataType::StructRef(struct_type) => {
                Boxed(Box::new(Self::tasm_lib_struct_to_lang_struct(struct_type)))
            }
        };

        Ok(resolved)
    }
}

impl TryFrom<DataType> for tasm_lib::data_type::DataType {
    type Error = String;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        fn try_from_boxed(value: DataType) -> Result<tasm_lib::data_type::DataType, String> {
            match value {
                // A Boxed list is just a list
                DataType::List(_) => value.try_into(),
                DataType::Unresolved(_) => todo!(),
                DataType::Struct(struct_type) => Ok(tasm_lib::data_type::DataType::StructRef(
                    struct_type.try_into()?,
                )),
                _ => Ok(tasm_lib::data_type::DataType::VoidPointer),
            }
        }

        match value {
            DataType::Bool => Ok(tasm_lib::data_type::DataType::Bool),
            DataType::U32 => Ok(tasm_lib::data_type::DataType::U32),
            DataType::U64 => Ok(tasm_lib::data_type::DataType::U64),
            DataType::U128 => Ok(tasm_lib::data_type::DataType::U128),
            DataType::Bfe => Ok(tasm_lib::data_type::DataType::Bfe),
            DataType::Xfe => Ok(tasm_lib::data_type::DataType::Xfe),
            DataType::Digest => Ok(tasm_lib::data_type::DataType::Digest),
            DataType::List(elem_type) => {
                let element_type = (*elem_type).try_into();
                let element_type = match element_type {
                    Ok(e) => e,
                    Err(err) => {
                        return Err(format!("Failed to convert element type of list: {err}"));
                    }
                };
                Ok(tasm_lib::data_type::DataType::List(Box::new(element_type)))
            }
            DataType::Polynomial(_) => {
                Err("Polynomial types not yet supported by tasm-lib".to_owned())
            }
            DataType::Array(_array_type) => {
                Err("Array types not yet supported by tasm-lib".to_owned())
            }
            DataType::Tuple(tuple_elements) => {
                let tuple_elements = tuple_elements
                    .into_iter()
                    .map(|x| x.try_into().unwrap())
                    .collect_vec();
                Ok(tasm_lib::data_type::DataType::Tuple(tuple_elements))
            }
            DataType::VoidPointer => Ok(tasm_lib::data_type::DataType::VoidPointer),
            DataType::Function(_) => todo!(),
            DataType::Struct(_) => todo!(),
            DataType::Enum(_) => todo!(),
            DataType::Unresolved(name) => Err(format!(
                "Cannot convert unresolved type {name} to tasm-lib type"
            )),
            DataType::Boxed(value) => try_from_boxed(*value),
        }
    }
}

impl TryFrom<StructType> for tasm_lib::data_type::StructType {
    type Error = String;

    fn try_from(value: StructType) -> Result<Self, Self::Error> {
        let StructVariant::NamedFields(NamedFieldsStruct { fields }) = value.variant else {
            return Err("Structs in `tasm-lib` must have named fields.".to_owned());
        };

        let fields: Result<Vec<_>, _> = fields
            .into_iter()
            .map(|(name, dtype)| (name, dtype.try_into()))
            .map(|(name, result)| result.map(|dtype| (name, dtype)))
            .collect();
        let name = value.name;
        let tasm_lib_struct = tasm_lib::data_type::StructType {
            name,
            fields: fields?,
        };

        Ok(tasm_lib_struct)
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
        let str = match self {
            Bool => "bool".to_string(),
            U32 => "u32".to_string(),
            U64 => "u64".to_string(),
            U128 => "u128".to_string(),
            Bfe => "BFieldElement".to_string(),
            Xfe => "XFieldElement".to_string(),
            Digest => "Digest".to_string(),
            List(ty) => format!("Vec<{ty}>"),
            Polynomial(ty) => format!("Polynomial<{ty}>"),
            Array(array_type) => format!("[{}; {}]", array_type.element_type, array_type.length),
            Tuple(tys) => format!("({})", tys.into_iter().join(", ")),
            VoidPointer => "void pointer".to_string(),
            Function(fn_type) => {
                let input = fn_type.input_argument.to_string();
                let output = fn_type.output.to_string();
                format!("Function: {input} -> {output}")
            }
            Struct(struct_type) => struct_type.name.to_owned(),
            Enum(enum_type) => match &enum_type.type_parameter {
                Some(type_param) => format!("{}<{}>", enum_type.name, type_param),
                None => enum_type.name.to_owned(),
            },
            Unresolved(name) => name.to_string(),
            Boxed(ty) => format!("Boxed<{ty}>"),
        };
        write!(f, "{str}")
    }
}

impl From<FnSignature> for DataType {
    fn from(value: FnSignature) -> Self {
        let mut input_args = vec![];

        for inp in value.args.iter() {
            let input = match inp {
                AbstractArgument::FunctionArgument(_) => todo!(),
                AbstractArgument::ValueArgument(abs_val) => abs_val.data_type.to_owned(),
            };
            input_args.push(input);
        }

        DataType::Function(Box::new(FunctionType {
            input_argument: match input_args.len() {
                1 => input_args[0].to_owned(),
                _ => DataType::Tuple(input_args.into()),
            },
            output: value.output.to_owned(),
        }))
    }
}
