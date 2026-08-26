use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use tasm_lib::triton_vm::prelude::*;

use crate::ast;
use crate::ast_types;
use crate::ast_types::CustomTypeOil;
use crate::ast_types::DataType;
use crate::ast_types::EnumType;
use crate::type_checker;
use crate::type_checker::GetType;
use crate::type_checker::Typing;

/// A type definition, its methods, and its association functions
#[derive(Debug, Clone)]
pub(crate) struct TypeContext {
    pub(crate) composite_type: ast_types::CustomTypeOil,
    pub(crate) methods: Vec<ast::Method<Typing>>,
    pub(crate) associated_functions: Vec<ast::Fn<Typing>>,
}

impl From<CustomTypeOil> for TypeContext {
    fn from(value: CustomTypeOil) -> Self {
        Self {
            composite_type: value,
            methods: Default::default(),
            associated_functions: Default::default(),
        }
    }
}

impl From<TypeContext> for DataType {
    fn from(value: TypeContext) -> DataType {
        value.composite_type.into()
    }
}

impl TypeContext {
    /// Add a method to this type. Panics if method name already there.
    pub(crate) fn add_method(&mut self, new_method: ast::Method<Typing>) {
        assert!(
            !self
                .methods
                .iter()
                .any(|m| m.signature.name == new_method.signature.name),
            "Duplicate method with name {} for type {}",
            new_method.signature.name,
            self.composite_type.name()
        );
        self.methods.push(new_method);
    }

    /// Get a method identified by name
    pub(crate) fn get_method(&self, method_name: &str) -> Option<&ast::Method<Typing>> {
        self.methods
            .iter()
            .find(|x| x.signature.name == method_name)
    }

    /// Add an associated function to this type. Panics if function name is already there.
    pub(crate) fn add_associated_function(&mut self, new_fun: ast::Fn<Typing>) {
        assert!(
            !self
                .methods
                .iter()
                .any(|m| m.signature.name == new_fun.signature.name),
            "Duplicate associated function with name {} for type {}",
            new_fun.signature.name,
            self.composite_type.name()
        );
        self.associated_functions.push(new_fun)
    }

    /// Get an associated function identifier by name
    pub(crate) fn get_associated_function(&self, fname: &str) -> Option<&ast::Fn<Typing>> {
        self.associated_functions
            .iter()
            .find(|x| x.signature.name == fname)
    }
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.composite_type == other.composite_type
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct CompositeTypes {
    /// Map from type name to indices into `composite_types` list
    by_name: HashMap<String, Vec<usize>>,

    // Map from data type to index into `composite_types` list
    by_type: HashMap<ast_types::DataType, usize>,

    // A list of all custom types that have been added to the current compilation instance
    composite_types: Vec<TypeContext>,
}

impl IntoIterator for CompositeTypes {
    type Item = TypeContext;
    type IntoIter = std::vec::IntoIter<TypeContext>;

    fn into_iter(self) -> Self::IntoIter {
        self.composite_types.into_iter()
    }
}

impl CompositeTypes {
    /// Add a composite type to the collection. Does nothing if it's already included.
    pub(crate) fn add_type_context_if_new(&mut self, tyctx: TypeContext) {
        self.idempotent_add(tyctx.composite_type.name().to_owned(), tyctx);
    }

    /// Add a composite type to the collection. Panics if it's already included.
    pub(crate) fn add_custom_type(&mut self, dtype: CustomTypeOil) {
        let name = dtype.name().to_owned();
        let tyctx: TypeContext = dtype.into();
        self.unique_add(name, tyctx);
    }

    /// Add a composite type to the collection. Does nothing if the type is already
    /// included.
    fn idempotent_add(&mut self, type_name: String, tyctx: TypeContext) {
        let mut do_nothing = false;
        let type_count = self.composite_types.len();
        self.by_name
            .entry(type_name.to_owned())
            .and_modify(|types_w_same_name| {
                // If all existing types with this name are different, insert
                // the new type.
                if types_w_same_name
                    .iter()
                    .all(|x| self.composite_types[*x] != tyctx)
                {
                    types_w_same_name.push(type_count);
                } else {
                    do_nothing = true;
                }
            })
            .or_insert(vec![type_count]);

        if do_nothing {
            return;
        }

        self.by_type
            .insert(tyctx.composite_type.clone().into(), type_count);
        self.composite_types.push(tyctx);
    }

    /// Add a composite type to the collection. Panics if it's already included.
    fn unique_add(&mut self, type_name: String, tyctx: TypeContext) {
        let type_count = self.composite_types.len();
        self.by_name
            .entry(type_name.to_owned())
            .and_modify(|types_w_same_name| {
                // Assert that this type has not been seen before
                assert!(
                    types_w_same_name
                        .iter()
                        .all(|x| self.composite_types[*x] != tyctx),
                    "Attempted to insert repeated composite type with name {type_name}",
                );
                types_w_same_name.push(type_count);
            })
            .or_insert(vec![type_count]);

        self.by_type
            .insert(tyctx.composite_type.clone().into(), type_count);
        self.composite_types.push(tyctx);
    }

    /// Return a type context that must be uniquely identified by its name.
    /// Otherwise this function panics.
    pub(crate) fn get_unique_by_name(&self, type_name: &str) -> TypeContext {
        let Some(indices) = self.by_name.get(type_name) else {
            panic!(
                "Did not find composite type with name {type_name}. Known types are:\n{}",
                self.by_name.keys().join(", ")
            );
        };

        assert!(
            indices.len().is_one(),
            "Type \"{type_name}\" was defined more than once."
        );
        self.composite_types[indices[0]].clone()
    }

    /// Return a type context that is uniquely identified by its data type.
    pub(crate) fn get_by_type(&self, composite_type: &ast_types::DataType) -> Option<&TypeContext> {
        let index = self.by_type.get(composite_type);
        index.map(|index| &self.composite_types[*index])
    }

    /// Return a mutable type context that is uniquely identified by its data type.
    pub(crate) fn get_mut_by_type(
        &mut self,
        composite_type: &ast_types::DataType,
    ) -> Option<&mut TypeContext> {
        let index = *self.by_type.get(composite_type)?;
        Some(&mut self.composite_types[index])
    }

    pub(crate) fn get_method(
        &self,
        method_call: &ast::MethodCall<type_checker::Typing>,
    ) -> Option<ast::Method<Typing>> {
        self.get_by_type(method_call.associated_type.as_ref().unwrap())
            .map(|tyctx| tyctx.get_method(&method_call.method_name).unwrap())
            .cloned()
    }

    /********** Code Generation **********/
    /// Return the instructions for the constructor of the matching type, otherwise
    /// returns None.
    pub(crate) fn constructor_code(
        &self,
        fn_call: &ast::FnCall<Typing>,
    ) -> Option<Vec<LabelledInstruction>> {
        let match_type = self.constructor_match(fn_call)?;

        match match_type {
            (CustomTypeOil::Enum(e), Some(variant_name)) => {
                Some(e.variant_tuple_constructor(&variant_name).body)
            }
            (CustomTypeOil::Struct(s), None) => Some(s.constructor().body),
            (CustomTypeOil::Enum(_), None) => unreachable!(),
            (CustomTypeOil::Struct(_), Some(_)) => unreachable!(),
        }
    }

    /********** Shared Methods **********/
    pub(crate) fn get_associated_function(&self, name: &str) -> Option<ast::Fn<Typing>> {
        // Associated functions be called with `<Type>::<function_name>`, where `function_name`
        // must be lower-cased.
        let split_name = name.split("::").collect_vec();
        if !(split_name.len() > 1 && split_name[1].chars().next().unwrap().is_lowercase()) {
            return None;
        }

        let type_name = split_name[0];
        let fname = split_name[1];

        let indices = self.by_name.get(type_name)?;
        assert!(
            indices.len().is_one(),
            "Multiple composite types with name {type_name} found",
        );
        let ty_ctx = &self.composite_types[indices[0]];

        ty_ctx.get_associated_function(fname).map(|x| x.to_owned())
    }

    /// Return all enums that are included in `prelude`, meaning that
    /// the programmer only has to specify the variant name, not the type.
    /// E.g.: `Ok(5)` instead of `Return::Ok(5)`.
    fn preludes(&self) -> Vec<EnumType> {
        let mut ret = vec![];
        for dtype in self.composite_types.iter() {
            if dtype.composite_type.is_prelude() {
                ret.push((&dtype.composite_type).try_into().unwrap());
            }
        }

        ret
    }

    /// Return the composite type for which the input function call is a constructor.
    /// May only be run after full type annotation, as the output type might be needed
    /// to find the correct constructor.
    /// `Foo::A(100)` will match
    /// enum Foo {
    ///     A(u32),
    /// }
    fn constructor_match(
        &self,
        ast::FnCall {
            name,
            args,
            type_parameter: _,
            arg_evaluation_order: _,
            annot,
            ..
        }: &ast::FnCall<type_checker::Typing>,
    ) -> Option<(ast_types::CustomTypeOil, Option<String>)> {
        let return_type = annot.get_type();
        let split_name = name.split("::").collect_vec();

        // Is this an enum constructor for a type in `prelude`? E.g. `Ok(...)`.
        for prelude in self.preludes() {
            if prelude.has_variant_of_name(split_name[0]) {
                let variant_name = split_name[0];
                if prelude
                    .variant_data_type(variant_name)
                    .as_tuple_type()
                    .into_iter()
                    .zip(args.iter())
                    .all(|(constructor_abstr_arg, actual_arg)| {
                        constructor_abstr_arg == actual_arg.get_type()
                    })
                    && return_type == prelude.clone().into()
                {
                    return Some((prelude.into(), Some(split_name[0].to_owned())));
                }
            }
        }

        // Is this a constructor for a non-prelude enum?
        let type_name = split_name[0];
        if split_name.len() > 1
            && split_name[1].chars().next().unwrap().is_uppercase()
            && self.by_name.contains_key(type_name)
        {
            let candidates = &self.by_name[type_name];
            for candidate in candidates.iter() {
                match &self.composite_types[*candidate].composite_type {
                    ast_types::CustomTypeOil::Struct(_) => {
                        return None;
                    }
                    ast_types::CustomTypeOil::Enum(e) => {
                        if e.has_variant_of_name(split_name[1]) {
                            let variant_name = split_name[1];
                            let variant_data_type = e.variant_data_type(variant_name);
                            if variant_data_type
                                .as_tuple_type()
                                .into_iter()
                                .zip(args.iter())
                                .all(|(constructor_abstr_arg, actual_arg)| {
                                    constructor_abstr_arg == actual_arg.get_type()
                                })
                            {
                                return Some((
                                    self.composite_types[*candidate].composite_type.to_owned(),
                                    Some(variant_name.to_owned()),
                                ));
                            }
                        }
                    }
                }
            }
        }

        // Is this a constructor for a tuple-type?
        if split_name.len().is_one() {
            if let Some(candidates) = self.by_name.get(split_name[0]) {
                for candidate in candidates {
                    match &self.composite_types[*candidate].composite_type {
                        ast_types::CustomTypeOil::Struct(s) => {
                            if s.field_ids_and_types()
                                .zip(args.iter())
                                .all(|((_, aa), ca)| *aa == ca.get_type())
                            {
                                return Some((
                                    self.composite_types[*candidate].composite_type.to_owned(),
                                    None,
                                ));
                            }
                        }
                        ast_types::CustomTypeOil::Enum(_) => (),
                    }
                }
            }
        }

        None
    }
}
