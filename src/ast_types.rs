use std::fmt::Display;
use std::str::FromStr;

use anyhow::bail;
use itertools::Itertools;

use crate::ast::FnSignature;

pub use self::abstract_argument::*;
pub use self::array_type::ArrayType;
pub use self::custom_type_oil::CustomTypeOil;
pub use self::enum_type::EnumType;
pub use self::field_id::FieldId;
pub use self::function_type::FunctionType;
pub use self::list_type::ListType;
pub use self::struct_type::*;
pub use self::tuple::Tuple;

pub mod abstract_argument;
pub mod array_type;
pub mod custom_type_oil;
pub mod enum_type;
pub mod field_id;
pub mod function_type;
pub mod list_type;
pub mod struct_type;
pub mod tuple;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U32,
    U64,
    U128,
    Bfe,
    Xfe,
    Digest,
    List(Box<DataType>, ListType),
    Tuple(Tuple),
    Array(ArrayType),
    Struct(StructType),
    Enum(Box<EnumType>),
    VoidPointer,
    Function(Box<FunctionType>),
    Boxed(Box<DataType>),
    Unresolved(String),
}

impl DataType {
    pub fn get_tuple_elements(&self) -> Tuple {
        match self {
            DataType::Tuple(tuple) => tuple.to_owned(),
            DataType::Struct(StructType {
                name: _,
                is_copy: _,
                variant,
            }) => match variant {
                StructVariant::TupleStruct(ts) => ts.to_owned(),
                StructVariant::NamedFields(_) => {
                    panic!("Type is not unnamed or named tuple. Type was: {self}")
                }
            },
            DataType::Boxed(inner) => inner.get_tuple_elements(),
            _ => panic!("Type is not unnamed or named tuple. Type was: {self}"),
        }
    }

    /// Return true if it's OK to copy this type including its underlying
    /// data, false if it's expensive to copy.
    pub fn is_copy(&self) -> bool {
        match self {
            DataType::Bool => true,
            DataType::U32 => true,
            DataType::U64 => true,
            DataType::U128 => true,
            DataType::Bfe => true,
            DataType::Xfe => true,
            DataType::Digest => true,
            DataType::Tuple(_) => true,
            DataType::Array(array_type) => array_type.element_type.is_copy(),
            DataType::List(_, _) => false,
            DataType::VoidPointer => false,
            DataType::Function(_) => false,
            DataType::Struct(struct_type) => struct_type.is_copy,
            DataType::Enum(enum_type) => enum_type.is_copy,
            DataType::Unresolved(_) => false,
            DataType::Boxed(_) => false,
        }
    }

    /// Use this if the type is used to make labels in the TASM code
    pub fn label_friendly_name(&self) -> String {
        use DataType::*;
        match self {
            Bool => "bool".to_string(),
            U32 => "u32".to_string(),
            U64 => "u64".to_string(),
            U128 => "u128".to_string(),
            Bfe => "BField".to_string(),
            Xfe => "XField".to_string(),
            Digest => "Digest".to_string(),
            List(ty, _list_type) => format!("Vec_L{}R", ty.label_friendly_name()),
            Array(_array_type) => format!(
                "array{}_of_L{}R",
                _array_type.length,
                _array_type.element_type.label_friendly_name()
            ),
            Tuple(tys) => format!(
                "tuple_L{}R",
                tys.into_iter().map(|x| x.label_friendly_name()).join("_")
            ),
            VoidPointer => "void_pointer".to_string(),
            Function(fn_type) => {
                let input = fn_type.input_argument.label_friendly_name();
                let output = fn_type.output.label_friendly_name();
                format!("function_from_L{}R__to_L{}R", input, output)
            }
            Struct(struct_type) => struct_type.name.to_owned(),
            Enum(enum_type) => enum_type.label_friendly_name(),
            Unresolved(name) => name.to_string(),
            Boxed(ty) => format!("boxed_L{}R", ty.label_friendly_name()),
        }
    }

    pub fn from_tasm_lib_datatype(
        tasm_lib_type: tasm_lib::data_type::DataType,
        list_type: ListType,
    ) -> Self {
        use DataType::*;
        match tasm_lib_type {
            tasm_lib::data_type::DataType::Bool => Bool,
            tasm_lib::data_type::DataType::U32 => U32,
            tasm_lib::data_type::DataType::U64 => U64,
            tasm_lib::data_type::DataType::U128 => U128,
            tasm_lib::data_type::DataType::Bfe => Bfe,
            tasm_lib::data_type::DataType::Xfe => Xfe,
            tasm_lib::data_type::DataType::Digest => Digest,
            tasm_lib::data_type::DataType::List(elem_type) => {
                let element_type = Self::from_tasm_lib_datatype(*elem_type, list_type);
                List(Box::new(element_type), list_type)
            }
            tasm_lib::data_type::DataType::Tuple(tasm_types) => {
                let element_types: Vec<DataType> = tasm_types
                    .into_iter()
                    .map(|t| Self::from_tasm_lib_datatype(t, list_type))
                    .collect();
                Tuple(element_types.into())
            }
            tasm_lib::data_type::DataType::VoidPointer => VoidPointer,
        }
    }

    /// What type is returned when type is accessed with a field of name `field_name`?
    pub fn field_access_returned_type(&self, field_id: &FieldId) -> Self {
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
                    let tuple_index: usize = field_id.try_into().expect("Tuple must be accessed with a tuple index");
                    let field_type = tuple.fields[tuple_index].clone();
                    if field_type.is_copy() {
                        field_type
                    } else {
                        DataType::Boxed(Box::new(field_type))
                    }
                },
                _ => panic!("Field getter can only operate on type of struct or pointer to struct. Attempted to access field `{field_id}` on type `{self}`")
            },
            DataType::Struct(struct_type) => {
                struct_type.get_field_type(field_id)
            },
            DataType::Tuple(tuple) => {
                let tuple_index: usize = field_id.try_into().expect("Tuple must be accessed with a tuple index");
                tuple.fields[tuple_index].clone()
            },
            _ => panic!("Field getter can only operate on type of struct or pointer to struct. Attempted to access field `{field_id}` on type `{self}`")
            }
    }

    // TODO: Consider getting rid of this method
    /// Return the element type for lists
    pub fn type_parameter(&self) -> Option<DataType> {
        match self {
            DataType::List(element_type, _) => Some(*element_type.to_owned()),
            // TODO: Is this the right solution, or do we perhaps need to resolve
            // other types for our field access operators? I'm leaning towards the
            // latter ... but what if the are more field operators following each
            // other? Then the 2nd layer is a `struct` which should then probably
            // be `MemPointer(Struct)`. I think we need to resolve our problems
            // as best we can in the type resolver for expressions!
            // Can we do automatic, or selective dereferencing when meating a `.`
            // operator?
            // DataType::MemPointer(inner_type) => inner_type.type_parameter(),
            _ => None,
        }
    }

    // Notice that this implementation must match that derived by `BFieldCodec`
    pub fn bfield_codec_static_length(&self) -> Option<usize> {
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
            DataType::List(_, _) => None,
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

    pub fn unit() -> Self {
        Self::Tuple(vec![].into())
    }

    pub fn is_unit(&self) -> bool {
        *self == Self::unit()
    }

    pub fn stack_size(&self) -> usize {
        match self {
            Self::Bool => 1,
            Self::U32 => 1,
            Self::U64 => 2,
            Self::U128 => 4,
            Self::Bfe => 1,
            Self::Xfe => 3,
            Self::Digest => 5,
            Self::List(_list_type, _) => 1,
            Self::Array(_) => 1,
            Self::Tuple(tuple_type) => tuple_type.stack_size(),
            Self::VoidPointer => 1,
            Self::Function(_) => panic!(),
            Self::Unresolved(name) => panic!("cannot get size of unresolved type {name}"),
            Self::Struct(inner_type) => inner_type.stack_size(),
            Self::Enum(enum_type) => enum_type.stack_size(),
            Self::Boxed(_inner) => 1,
        }
    }

    pub fn unbox(&self) -> DataType {
        match self {
            DataType::Boxed(inner) => inner.unbox(),
            dtype => dtype.to_owned(),
        }
    }

    pub fn is_boxed(&self) -> bool {
        matches!(self, DataType::Boxed(_))
    }

    pub fn as_enum_type(&self) -> EnumType {
        match self {
            DataType::Enum(enum_type) => *enum_type.to_owned(),
            _ => panic!("Expected enum type. Got: {self}"),
        }
    }

    pub fn as_tuple_type(&self) -> Tuple {
        match self {
            DataType::Tuple(tuple_type) => tuple_type.to_owned(),
            other => Tuple {
                fields: vec![other.to_owned()],
            },
        }
    }
}

impl TryFrom<DataType> for tasm_lib::data_type::DataType {
    type Error = String;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        match value {
            DataType::Bool => Ok(tasm_lib::data_type::DataType::Bool),
            DataType::U32 => Ok(tasm_lib::data_type::DataType::U32),
            DataType::U64 => Ok(tasm_lib::data_type::DataType::U64),
            DataType::U128 => Ok(tasm_lib::data_type::DataType::U128),
            DataType::Bfe => Ok(tasm_lib::data_type::DataType::Bfe),
            DataType::Xfe => Ok(tasm_lib::data_type::DataType::Xfe),
            DataType::Digest => Ok(tasm_lib::data_type::DataType::Digest),
            DataType::List(elem_type, _) => {
                let element_type = (*elem_type).try_into();
                let element_type = match element_type {
                    Ok(e) => e,
                    Err(err) => {
                        return Err(format!("Failed to convert element type of list: {err}"))
                    }
                };
                Ok(tasm_lib::data_type::DataType::List(Box::new(element_type)))
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
            DataType::Boxed(value) => match *value {
                // A Boxed list is just a list
                // TODO: Default to `Unsafe` list here??
                DataType::List(_, ListType::Unsafe) => (*value).try_into(),
                DataType::Unresolved(_) => todo!(),
                _ => Ok(tasm_lib::data_type::DataType::VoidPointer),
            },
        }
    }
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
            List(ty, _list_type) => format!("Vec<{ty}>"),
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
        write!(f, "{str}",)
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
