use itertools::Itertools;

use super::DataType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EnumType {
    pub name: String,
    pub is_copy: bool,
    pub variants: Vec<(String, DataType)>,
    pub is_prelude: bool,

    // Use `type_parameter` to create differentiate between function labels
    // for e.g. `Result<T>` and `Result<S>` types.
    pub type_parameter: Option<DataType>,
}

impl From<&EnumType> for DataType {
    fn from(value: &EnumType) -> Self {
        Self::Enum(Box::new(value.to_owned()))
    }
}

impl From<EnumType> for DataType {
    fn from(value: EnumType) -> Self {
        Self::Enum(Box::new(value))
    }
}

impl EnumType {
    /// Return an iterator over mutable references to the type's nested datatypes
    pub(crate) fn variant_types_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = &'a mut DataType> + 'a> {
        Box::new(self.variants.iter_mut().map(|x| &mut x.1))
    }

    pub(crate) fn variant_types<'a>(&'a self) -> Box<dyn Iterator<Item = &'a DataType> + 'a> {
        Box::new(self.variants.iter().map(|x| &x.1))
    }

    pub(crate) fn has_variant_of_name(&self, variant_name: &str) -> bool {
        self.variants.iter().any(|x| x.0 == variant_name)
    }

    pub(crate) fn variant_data_type(&self, variant_name: &str) -> DataType {
        for type_variant in self.variants.iter() {
            if type_variant.0 == variant_name {
                return type_variant.1.clone();
            }
        }

        panic!(
            "variant name \"{variant_name}\" is not defined for enum {}",
            self.name
        );
    }

    /// Return the "discriminant" of an enum variant, an integer showing
    /// what variant the enum type takes.
    pub(crate) fn variant_discriminant(&self, variant_name: &str) -> usize {
        self.variants
            .iter()
            .find_position(|x| x.0 == variant_name)
            .unwrap_or_else(|| {
                panic!(
                    "Could not find variant {variant_name} in enum {}",
                    self.name,
                )
            })
            .0
    }

    /// Returns the stack size that this enum type always occupies, assuming
    /// it's on the stack, and not boxed.
    pub(crate) fn stack_size(&self) -> usize {
        self.variants
            .iter()
            .max_by_key(|x| x.1.stack_size())
            .map(|x| x.1.stack_size() + 1)
            .unwrap_or_default()
    }

    /// Return the words of padding used for a specific variant in this enum
    pub(crate) fn padding_size(&self, variant_name: &str) -> usize {
        self.stack_size() - self.variant_data_type(variant_name).stack_size() - 1
    }

    /// Decompose the type of a variant into its three consituent parts:
    /// data, padding, discriminant. Must match layout defined by constructor
    /// which is:
    /// stack: _ [data] [padding] discriminator
    pub fn decompose_variant(&self, variant_name: &str) -> Vec<DataType> {
        [
            self.variant_data_type(variant_name).as_tuple_type().fields,
            vec![DataType::Tuple(
                vec![DataType::Bfe; self.padding_size(variant_name)].into(),
            )],
            vec![DataType::Bfe],
        ]
        .concat()
    }

    /// Use this if the type is used to make labels in the TASM code
    pub fn label_friendly_name(&self) -> String {
        match self.type_parameter.as_ref() {
            // Use type parameter here to differentiate between
            // methods for `Result<BFE>` and `Result<XFE>`.
            Some(type_param) => {
                format!("{}___{}", self.name, type_param.label_friendly_name())
            }
            None => self.name.to_owned(),
        }
    }
}
