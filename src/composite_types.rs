use itertools::Itertools;

use crate::{
    ast, ast_types,
    type_checker::{self, GetType},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct CompositeTypes {
    custom_types: HashMap<String, Vec<ast_types::CustomTypeOil>>,
    library_types: HashMap<String, Vec<ast_types::CustomTypeOil>>,
}

impl CompositeTypes {
    /// Return the type matching the a constructor call to a defined tuple or enum variant
    fn constructor_match(
        &self,
        ast::FnCall {
            name,
            args,
            type_parameter,
            arg_evaluation_order,
            annot,
        }: &ast::FnCall<type_checker::Typing>,
    ) -> Option<ast_types::CustomTypeOil> {
        fn is_custom_type_constructor(
            ct: &CompositeTypes,
            ast::FnCall {
                name,
                args,
                type_parameter,
                arg_evaluation_order,
                annot,
            }: &ast::FnCall<type_checker::Typing>,
        ) -> Option<ast_types::CustomTypeOil> {
            let split_name = name.split("::").collect_vec();
            if split_name.len() > 1
                && split_name[1].chars().next().unwrap().is_uppercase()
                && ct.custom_types.contains_key(split_name[0])
            {
                // Do the types match?
                let candidates = &ct.custom_types[split_name[0]];
                for candidate in candidates.iter() {
                    match candidate {
                        ast_types::CustomTypeOil::Struct(s) => {
                            if s.field_ids_and_types()
                                .zip(args.iter())
                                .all(|((_, aa), ca)| *aa == ca.get_type())
                            {
                                return Some(candidate.to_owned());
                            }
                        }
                        ast_types::CustomTypeOil::Enum(e) => {
                            if e.field_ids_and_types()
                                .zip(args.iter())
                                .all(|((_, aa), ca)| *aa == ca.get_type())
                            {
                                return Some(candidate.to_owned());
                            }
                        }
                    }
                }
            }

            todo!()
        }
        // Is this a constructor of a custom type?

        todo!()
    }
}
