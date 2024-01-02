use itertools::Itertools;
use num::{One, Zero};
use std::collections::HashMap;
use std::str::FromStr;
use syn::{parse_quote, PathArguments};

use crate::ast;
use crate::ast::ReturningBlock;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::libraries::Library;
use crate::type_checker;

pub type Annotation = type_checker::Typing;

#[derive(Debug)]
pub(crate) struct Graft<'a> {
    pub list_type: ast_types::ListType,
    pub libraries: &'a [Box<dyn Library + 'a>],
    pub(crate) imported_custom_types: CompositeTypes,
}

#[derive(Debug, Clone)]
pub enum CustomTypeRust {
    Struct(syn::ItemStruct),
    Enum(syn::ItemEnum),
}

macro_rules! get_standard_setup {
    ($list_type:expr, $graft_config:ident, $libraries:ident) => {
        let library_config = crate::libraries::LibraryConfig {
            list_type: $list_type,
        };
        let $libraries = crate::libraries::all_libraries(library_config);
        let mut $graft_config = crate::graft::Graft::new($list_type, &$libraries);
    };
}

impl<'a> Graft<'a> {
    pub(crate) fn new(list_type: ast_types::ListType, libraries: &'a [Box<dyn Library>]) -> Self {
        Self {
            list_type,
            libraries,
            imported_custom_types: Default::default(),
        }
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn graft_custom_types_methods_and_associated_functions(
        &mut self,
        structs_and_methods: HashMap<String, (CustomTypeRust, Vec<syn::ImplItemMethod>)>,
    ) -> CompositeTypes {
        fn graft_enum_variants(
            graft_config: &mut Graft,
            variants: Vec<syn::Variant>,
        ) -> Vec<(String, ast_types::DataType)> {
            let mut grafted_variants = vec![];
            for variant in variants {
                let variant_name = variant.ident.to_string();
                let field_type = graft_tuple_struct(graft_config, variant.fields);
                grafted_variants.push((variant_name, ast_types::DataType::Tuple(field_type)));
            }

            grafted_variants
        }

        fn graft_struct_with_named_fields(
            graft_config: &mut Graft,
            struct_name: &str,
            fields: syn::Fields,
        ) -> ast_types::NamedFieldsStruct {
            let mut ast_fields: Vec<(String, ast_types::DataType)> = vec![];
            for field in fields.into_iter() {
                // Ignore fields that are tagged as `tasm_object(ignore)`. These
                // fields simply do not become part of the structure.
                if !field.attrs.len().is_zero()
                    && field.attrs.iter().any(|x| {
                        x.path.segments[0].ident == "tasm_object"
                            && x.tokens.to_string() == "(ignore)"
                    })
                {
                    continue;
                }

                let field_name = field.ident.unwrap().to_string();
                let datatype = graft_config.syn_type_to_ast_type(&field.ty);
                ast_fields.push((field_name, datatype));
            }

            ast_types::NamedFieldsStruct {
                fields: ast_fields,
                name: struct_name.to_owned(),
            }
        }

        fn graft_tuple_struct(graft_config: &mut Graft, fields: syn::Fields) -> ast_types::Tuple {
            let mut ast_fields: Vec<ast_types::DataType> = vec![];
            for field in fields {
                ast_fields.push(graft_config.syn_type_to_ast_type(&field.ty));
            }

            ast_fields.into()
        }

        fn is_copy(attrs: &[syn::Attribute]) -> bool {
            match attrs.len() {
                1 => attrs[0].tokens.to_string().contains("Copy"),
                0 => false,
                _ => panic!("Can only handle one line of attributes for now."),
            }
        }

        let mut composite_types = CompositeTypes::default();

        // Handle composite data structures
        let structs = structs_and_methods
            .clone()
            .into_iter()
            .map(|(_, (syn_struct, _methods))| syn_struct)
            .collect_vec();
        for struct_ in structs {
            match struct_ {
                CustomTypeRust::Enum(enum_item) => {
                    let syn::ItemEnum {
                        attrs,
                        vis: _,
                        enum_token: _,
                        ident,
                        generics: _,
                        brace_token: _,
                        variants,
                    } = enum_item;
                    let name = ident.to_string();

                    let is_copy = is_copy(&attrs);

                    let variants = graft_enum_variants(self, variants.into_iter().collect_vec());
                    let enum_type = ast_types::EnumType {
                        name: name.clone(),
                        is_copy,
                        variants,
                        is_prelude: false,
                        type_parameter: None,
                    };

                    composite_types.add_custom_type(ast_types::CustomTypeOil::Enum(enum_type));
                }
                CustomTypeRust::Struct(struct_item) => {
                    let syn::ItemStruct {
                        attrs,
                        vis: _,
                        struct_token: _,
                        ident,
                        generics: _,
                        fields,
                        semi_token: _,
                    } = struct_item;
                    let name = ident.to_string();

                    let is_copy = is_copy(&attrs);

                    // Rust structs come in three forms: with named fields, tuple structs, and
                    // unit structs. We don't yet support unit structs, so we can assume that
                    // the struct has at least *one* field.
                    let struct_type = match fields.iter().next().unwrap().ident {
                        Some(_) => ast_types::StructVariant::NamedFields(
                            graft_struct_with_named_fields(self, &name, fields),
                        ),
                        None => {
                            ast_types::StructVariant::TupleStruct(graft_tuple_struct(self, fields))
                        }
                    };
                    let struct_type = ast_types::StructType {
                        is_copy,
                        variant: struct_type,
                        name: name.clone(),
                    };

                    composite_types.add_custom_type(ast_types::CustomTypeOil::Struct(struct_type));
                }
            }
        }

        // Handle methods and associated functions
        let struct_names_and_associated_functions = structs_and_methods
            .clone()
            .into_iter()
            .map(|(struct_name, (_syn_struct, methods))| (struct_name, methods))
            .collect_vec();
        for (type_name, assoc_function) in struct_names_and_associated_functions {
            let type_ctx = composite_types.get_mut_unique_by_name(&type_name);
            let as_dt: ast_types::DataType = type_ctx.composite_type.clone().into();
            for syn_method in assoc_function {
                if syn_method.sig.receiver().is_some() {
                    let oil_method = self.graft_method(&syn_method, &as_dt);
                    type_ctx.add_method(oil_method);
                } else {
                    // Associated function is *not* a method, i.e., does not take a `self` argument
                    let new_fun = self.graft_associated_function(&syn_method);
                    type_ctx.add_associated_function(new_fun);
                }
            }
        }

        composite_types
    }

    fn graft_method(
        &mut self,
        method: &syn::ImplItemMethod,
        custom_type: &ast_types::DataType,
    ) -> ast::Method<Annotation> {
        let method_name = method.sig.ident.to_string();
        let receiver = method.sig.receiver().unwrap().to_owned();
        let receiver = if let syn::FnArg::Receiver(receiver) = receiver {
            let syn::Receiver {
                reference,
                mutability,
                ..
            } = receiver;
            let receiver_data_type = match reference {
                Some(_) => {
                    if custom_type.is_copy() {
                        ast_types::DataType::Reference(Box::new(custom_type.to_owned()))
                    } else {
                        ast_types::DataType::Boxed(Box::new(custom_type.to_owned()))
                    }
                }
                None => custom_type.to_owned(),
            };

            ast_types::AbstractValueArg {
                name: "self".to_string(),
                data_type: receiver_data_type,
                mutable: mutability.is_some(),
            }
        } else {
            panic!("Expected receiver as 1st abstract argument to method {method_name}");
        };
        let other_args = method
            .sig
            .inputs
            .iter()
            .skip(1)
            .map(|x| self.graft_fn_arg(x))
            .collect_vec();
        let all_args = [vec![receiver], other_args].concat();
        let all_args = all_args
            .into_iter()
            .map(ast_types::AbstractArgument::ValueArgument)
            .collect_vec();

        let output = self.graft_return_type(&method.sig.output);
        let signature = ast::FnSignature {
            name: method_name,
            args: all_args,
            output,
            arg_evaluation_order: Default::default(),
        };
        let body = method
            .block
            .stmts
            .iter()
            .map(|x| self.graft_stmt(x))
            .collect_vec();

        ast::Method {
            signature,
            body: ast::RoutineBody::Ast(body),
        }
    }

    /// Graft a function declaration inside an `impl` block of a custom-defined
    /// structure. The function does not take a `self` as input argument.
    pub(crate) fn graft_associated_function(
        &mut self,
        input: &syn::ImplItemMethod,
    ) -> ast::Fn<Annotation> {
        let function_name = input.sig.ident.to_string();
        let args = input
            .sig
            .inputs
            .iter()
            .map(|x| self.graft_fn_arg(x))
            .collect_vec();
        let args = args
            .into_iter()
            .map(ast_types::AbstractArgument::ValueArgument)
            .collect_vec();
        let output = self.graft_return_type(&input.sig.output);
        let body = input
            .block
            .stmts
            .iter()
            .map(|x| self.graft_stmt(x))
            .collect_vec();

        ast::Fn {
            signature: ast::FnSignature {
                name: function_name,
                args,
                output,
                arg_evaluation_order: Default::default(),
            },
            body: ast::RoutineBody::Ast(body),
        }
    }

    pub(crate) fn graft_fn_decl(&mut self, input: &syn::ItemFn) -> ast::Fn<Annotation> {
        let function_name = input.sig.ident.to_string();
        let args = input
            .sig
            .inputs
            .iter()
            .map(|x| self.graft_fn_arg(x))
            .collect_vec();
        let args = args
            .into_iter()
            .map(ast_types::AbstractArgument::ValueArgument)
            .collect_vec();
        let output = self.graft_return_type(&input.sig.output);
        let body = input
            .block
            .stmts
            .iter()
            .map(|x| self.graft_stmt(x))
            .collect_vec();

        ast::Fn {
            body: ast::RoutineBody::Ast(body),
            signature: ast::FnSignature {
                name: function_name,
                args,
                output,
                arg_evaluation_order: Default::default(),
            },
        }
    }

    fn rust_type_path_to_data_type(
        &mut self,
        rust_type_path: &syn::TypePath,
    ) -> ast_types::DataType {
        assert_eq!(
            1,
            rust_type_path.path.segments.len(),
            "Length other than one not supported"
        );
        let rust_type_as_string = rust_type_path.path.segments[0].ident.to_string();
        let primitive_type_parse_result = rust_type_as_string.parse::<ast_types::DataType>();

        if let Ok(data_type) = primitive_type_parse_result {
            return data_type;
        }

        // Type is not primitive. Is it a vector?
        // TODO: Below handling of `Vec`, `Box`, and `Result`
        // should be done by the libraries, not here.
        if rust_type_as_string == "Vec" {
            return self.rust_vec_to_data_type(&rust_type_path.path.segments[0].arguments);
        }

        // Handling `Box<T>`
        if rust_type_as_string == "Box" {
            return self.rust_box_to_data_type(&rust_type_path.path.segments[0].arguments);
        }

        if rust_type_as_string == "Result" {
            // I guess, here we need to add the Result<T> type to our list of
            // known types.
            return self.rust_result_type_to_data_type(&rust_type_path.path.segments[0].arguments);
        }

        // We only allow the user to use types that are capitalized
        if rust_type_as_string
            .chars()
            .next()
            .map_or(false, |c| c.is_uppercase())
        {
            // Note that `Unresolved` handles `Self` as well.
            // The `custom_type_resolver` translates `Self` to its
            // associated type.
            ast_types::DataType::Unresolved(rust_type_as_string)
        } else {
            panic!("Does not know type {rust_type_as_string}");
        }
    }

    fn rust_vec_to_data_type(&mut self, path_args: &syn::PathArguments) -> ast_types::DataType {
        match path_args {
            syn::PathArguments::AngleBracketed(ab) => {
                assert_eq!(1, ab.args.len(), "Must be Vec<T> for *one* generic T.");
                match &ab.args[0] {
                    syn::GenericArgument::Type(element_type) => ast_types::DataType::List(
                        Box::new(self.syn_type_to_ast_type(element_type)),
                        self.list_type,
                    ),
                    other => panic!("Unsupported type {other:#?}"),
                }
            }
            other => panic!("Unsupported type {other:#?}"),
        }
    }

    fn rust_box_to_data_type(&mut self, path_args: &syn::PathArguments) -> ast_types::DataType {
        let inner_type = if let syn::PathArguments::AngleBracketed(ab) = &path_args {
            assert_eq!(1, ab.args.len(), "Must be Box<T> for *one* generic T.");
            if let syn::GenericArgument::Type(inner) = &ab.args[0] {
                self.syn_type_to_ast_type(inner)
            } else {
                panic!("Unsupported type parameter for Box<T> {:#?}", ab.args[0])
            }
        } else {
            panic!("Box must be followed by its type parameter `<T>`");
        };

        ast_types::DataType::Boxed(Box::new(inner_type))
    }

    fn rust_result_type_to_data_type(&mut self, path_args: &PathArguments) -> DataType {
        use crate::libraries;
        let PathArguments::AngleBracketed(generics) = path_args else {
            panic!("Unsupported path argument {path_args:#?}");
        };
        assert_eq!(2, generics.args.len(), "`Result` must have two generics");

        let ok_type_arg = &generics.args[0];
        let syn::GenericArgument::Type(ok_type) = ok_type_arg else {
            panic!("Unsupported type {ok_type_arg:#?}");
        };
        let ok_type = self.syn_type_to_ast_type(ok_type);

        let resolved_type = libraries::core::result_type::result_type(ok_type);
        self.imported_custom_types
            .add_type_context_if_new(resolved_type.clone());
        ast_types::DataType::Enum(Box::new(resolved_type.composite_type.try_into().unwrap()))
    }

    pub(crate) fn syn_type_to_ast_type(&mut self, syn_type: &syn::Type) -> ast_types::DataType {
        match syn_type {
            syn::Type::Path(path) => self.rust_type_path_to_data_type(path),
            syn::Type::Tuple(tuple) => {
                let element_types = tuple
                    .elems
                    .iter()
                    .map(|x| self.syn_type_to_ast_type(x))
                    .collect_vec();

                ast_types::DataType::Tuple(element_types.into())
            }
            syn::Type::Reference(syn::TypeReference {
                and_token: _,
                lifetime: _,
                mutability: _,
                elem,
            }) => match *elem.to_owned() {
                syn::Type::Path(type_path) => {
                    let inner_type = self.rust_type_path_to_data_type(&type_path);
                    // Structs that are not copy must be Boxed for reference arguments to work
                    if matches!(inner_type, ast_types::DataType::Struct(_)) && !inner_type.is_copy()
                    {
                        ast_types::DataType::Boxed(Box::new(inner_type))
                    } else {
                        ast_types::DataType::Reference(Box::new(inner_type))
                    }
                }
                _ => todo!(),
            },
            syn::Type::Array(syn::TypeArray {
                bracket_token: _,
                elem,
                semi_token: _,
                len,
            }) => {
                let element_type = self.syn_type_to_ast_type(elem);
                let length = if let syn::Expr::Lit(expr_lit) = len {
                    let grafted_lit = self.graft_lit(&expr_lit.lit);
                    match grafted_lit {
                        ast::ExprLit::U32(length) => length,
                        ast::ExprLit::GenericNum(length, _) => length as u32,
                        other => panic!("Bad type for length indication of array. Got {other:#?}"),
                    }
                } else {
                    panic!("Bad type for length indication of array. Got {len:#?}");
                };

                ast_types::DataType::Array(ast_types::ArrayType {
                    element_type: Box::new(element_type),
                    length: length.try_into().unwrap(),
                })
            }
            other_type => panic!("Unsupported {other_type:#?}"),
        }
    }

    // Extract type and mutability from syn::PatType
    fn pat_type_to_data_type_and_mutability(
        &mut self,
        rust_type_path: &syn::PatType,
    ) -> (ast_types::DataType, bool) {
        let mutable = match *rust_type_path.pat.to_owned() {
            syn::Pat::Ident(syn::PatIdent {
                attrs: _,
                by_ref: _,
                mutability,
                ident: _,
                subpat: _,
            }) => mutability.is_some(),
            other_type => panic!("Unsupported {other_type:#?}"),
        };
        let ast_type = self.syn_type_to_ast_type(rust_type_path.ty.as_ref());

        (ast_type, mutable)
    }

    fn pat_to_name(pat: &syn::Pat) -> String {
        match pat {
            syn::Pat::Ident(ident) => ident.ident.to_string(),
            other => panic!("unsupported: {other:?}"),
        }
    }

    pub(crate) fn path_to_ident(path: &syn::Path) -> String {
        // We just join identifiers with `::` to get the full function name / identifier name
        let identifiers: Vec<String> = path.segments.iter().map(|x| x.ident.to_string()).collect();
        identifiers.join("::")
    }

    fn graft_fn_arg(&mut self, rust_fn_arg: &syn::FnArg) -> ast_types::AbstractValueArg {
        match rust_fn_arg {
            syn::FnArg::Typed(pat_type) => {
                let name = Self::pat_to_name(&pat_type.pat);
                let (data_type, mut mutable) = self.pat_type_to_data_type_and_mutability(pat_type);

                // Sloppy way of handling mutability. But it's what we got for now.
                // we say an fn arg is mutable if it's either declared as such *or*
                // if it is of the `&mut` reference.
                if let syn::Type::Reference(syn::TypeReference {
                    and_token: _,
                    lifetime: _,
                    mutability,
                    elem: _,
                }) = pat_type.ty.as_ref()
                {
                    mutable = mutable || mutability.is_some();
                }

                ast_types::AbstractValueArg {
                    name,
                    data_type,
                    mutable,
                }
            }
            other => panic!("unsupported: {other:?}"),
        }
    }

    fn graft_return_type(&mut self, rust_return_type: &syn::ReturnType) -> ast_types::DataType {
        match rust_return_type {
            syn::ReturnType::Type(_, path) => match path.as_ref() {
                syn::Type::Path(type_path) => self.rust_type_path_to_data_type(type_path),
                syn::Type::Tuple(tuple_type) => {
                    let output_elements = tuple_type
                        .elems
                        .iter()
                        .map(|x| self.syn_type_to_ast_type(x))
                        .collect_vec();

                    ast_types::DataType::Tuple(output_elements.into())
                }
                _ => panic!("unsupported: {path:?}"),
            },
            syn::ReturnType::Default => ast_types::DataType::Tuple(vec![].into()),
        }
    }

    /// Return type argument found in path
    pub(crate) fn path_to_type_parameter(
        &mut self,
        path: &syn::Path,
    ) -> Option<ast_types::DataType> {
        let mut type_parameter: Option<ast_types::DataType> = None;
        for segment in path.segments.iter() {
            if segment.arguments == PathArguments::None {
                continue;
            }
            let PathArguments::AngleBracketed(abgas) = &segment.arguments else {
                panic!("unsupported PathArguments: {path:#?}");
            };
            for generic_arg in abgas.args.iter() {
                let syn::GenericArgument::Type(generic_type) = generic_arg else {
                    panic!("unsupported GenericArgument: {generic_arg:#?}");
                };
                if let syn::Type::Infer(_) = generic_type {
                    continue;
                }
                if type_parameter.is_some() {
                    panic!("only one type parameter supported, got {generic_type:?}");
                }
                type_parameter = Some(self.syn_type_to_ast_type(generic_type));
            }
        }
        type_parameter
    }

    pub(crate) fn graft_call_exp(
        &mut self,
        syn::ExprCall {
            attrs: _,
            func,
            paren_token: _,
            args,
        }: &syn::ExprCall,
    ) -> ast::Expr<Annotation> {
        let (full_name, function_type_parameter) = match func.as_ref() {
            syn::Expr::Path(path) => (
                Graft::path_to_ident(&path.path),
                self.path_to_type_parameter(&path.path),
            ),
            other => panic!("unsupported: {other:?}"),
        };

        // Check if grafting should be handled by a library
        for lib in self.libraries.iter() {
            if let Some(fn_name) = lib.get_graft_function_name(&full_name) {
                return lib
                    .graft_function(self, &fn_name, args, function_type_parameter)
                    .unwrap();
            }
        }

        // Grafting was not handled by a library. Treat function call as a regular
        // function that is hopefully in scope
        let args = args.iter().map(|x| self.graft_expr(x)).collect_vec();
        let annot = Default::default();

        ast::Expr::FnCall(ast::FnCall {
            name: full_name,
            args,
            annot,
            type_parameter: function_type_parameter,
            arg_evaluation_order: Default::default(),
        })
    }

    fn graft_field_expression(
        &mut self,
        syn::ExprField {
            attrs: _,
            base,
            dot_token: _,
            member,
        }: &syn::ExprField,
    ) -> ast::Identifier<Annotation> {
        let base_expression = self.graft_expr(base);
        let base_ident = match base_expression {
            ast::Expr::Var(ident) => ident,
            _ => {
                panic!("Left-hand-side of tuple operator must be a declared variable. Declare more bindings if needed. Failed to parse expression: {base_expression} as an identifier");
            }
        };

        let field_id = match &member {
            syn::Member::Named(field_name) => field_name.to_string().into(),
            syn::Member::Unnamed(tuple_index) => tuple_index.index.into(),
        };

        ast::Identifier::Field(Box::new(base_ident), field_id, Default::default())
    }

    pub(crate) fn graft_method_call(
        &mut self,
        rust_method_call: &syn::ExprMethodCall,
    ) -> ast::Expr<Annotation> {
        for lib in self.libraries.iter() {
            if let Some(method_call) = lib.graft_method(self, rust_method_call) {
                return method_call;
            }
        }

        let last_method_name = rust_method_call.method.to_string();
        let expr = rust_method_call.receiver.as_ref();
        let receiver_expr = self.graft_expr(expr);
        let mut args = vec![receiver_expr];
        args.append(
            &mut rust_method_call
                .args
                .iter()
                .map(|x| self.graft_expr(x))
                .collect_vec(),
        );
        ast::Expr::MethodCall(ast::MethodCall {
            method_name: last_method_name,
            args,
            annot: Default::default(),
            associated_type: Default::default(),
        })
    }

    /// Handle Rust expressions of the type i += 1
    fn graft_binop_eq_expr(
        &mut self,
        left: &syn::Expr,
        op: &syn::BinOp,
        right: &syn::Expr,
    ) -> ast::Expr<Annotation> {
        let left = self.graft_expr(left);
        let ast_binop: ast::BinOp = Self::graft_eq_binop(op);
        let right = self.graft_expr(right);

        ast::Expr::Binop(
            Box::new(left),
            ast_binop,
            Box::new(right),
            Default::default(),
        )
    }

    pub(crate) fn graft_expr(&mut self, rust_exp: &syn::Expr) -> ast::Expr<Annotation> {
        match rust_exp {
            syn::Expr::Binary(bin_expr) => {
                // Handle `<=` and `>=`
                let left = self.graft_expr(&bin_expr.left);
                let right = self.graft_expr(&bin_expr.right);
                if matches!(bin_expr.op, syn::BinOp::Le(_)) {
                    // Rewrite
                    // `lhs <= rhs`
                    // to
                    // `!(lhs > rhs)`
                    let replacement_binop = ast::BinOp::Gt;
                    ast::Expr::Unary(
                        ast::UnaryOp::Not,
                        Box::new(ast::Expr::Binop(
                            Box::new(left),
                            replacement_binop,
                            Box::new(right),
                            Default::default(),
                        )),
                        Default::default(),
                    )
                } else if matches!(bin_expr.op, syn::BinOp::Ge(_)) {
                    // Rewrite
                    // `lhs >= rhs`
                    // to
                    // `!(lhs < rhs)`
                    let replacement_binop = ast::BinOp::Lt;
                    ast::Expr::Unary(
                        ast::UnaryOp::Not,
                        Box::new(ast::Expr::Binop(
                            Box::new(left),
                            replacement_binop,
                            Box::new(right),
                            Default::default(),
                        )),
                        Default::default(),
                    )
                } else {
                    let ast_binop = Self::graft_binop(bin_expr.op);
                    ast::Expr::Binop(
                        Box::new(left),
                        ast_binop,
                        Box::new(right),
                        Default::default(),
                    )
                }
            }
            syn::Expr::Path(path) => {
                let path = &path.path;
                let ident: String = Self::path_to_ident(path);

                // TODO: Maybe not so elegant to handle this here...
                // Should be handled on a different level
                // TODO: Put this into `unsigned` library
                if ident == "u32::MAX" {
                    ast::Expr::Lit(ast::ExprLit::U32(u32::MAX))
                } else if ident == "u32::BITS" {
                    ast::Expr::Lit(ast::ExprLit::U32(u32::BITS))
                } else if ident == "u64::MAX" {
                    ast::Expr::Lit(ast::ExprLit::U64(u64::MAX))
                } else if ident == "u64::BITS" {
                    ast::Expr::Lit(ast::ExprLit::U32(u64::BITS))
                } else if ident == "u128::MAX" {
                    ast::Expr::Lit(ast::ExprLit::U128(u128::MAX))
                } else if ident == "u128::BITS" {
                    ast::Expr::Lit(ast::ExprLit::U32(u128::BITS))
                } else {
                    // if string name contains `::`, and we don't know the type,
                    // we assume this refers to an enum variant, without
                    // contained data: 'Foo::Bar'.
                    let enum_init = ident.split("::").collect_vec();
                    if enum_init.len() > 1 && ast_types::DataType::from_str(enum_init[0]).is_err() {
                        assert_eq!(
                            2,
                            enum_init.len(),
                            "Expected enum initialization to only contain one instance of '::'"
                        );
                        // This is the initialization of a value of an enum type -- without
                        // associated data.
                        ast::Expr::EnumDeclaration({
                            ast::EnumDeclaration {
                                enum_type: ast_types::DataType::Unresolved(enum_init[0].to_owned()),
                                variant_name: enum_init[1].to_owned(),
                            }
                        })
                    } else {
                        // Assume this is a variable
                        ast::Expr::Var(ast::Identifier::String(ident, Default::default()))
                    }
                }
            }
            syn::Expr::Tuple(tuple_expr) => {
                let exprs = tuple_expr
                    .elems
                    .iter()
                    .map(|x| self.graft_expr(x))
                    .collect_vec();
                ast::Expr::Tuple(exprs)
            }
            syn::Expr::Struct(syn::ExprStruct {
                attrs: _,
                path,
                brace_token: _,
                fields,
                dot2_token: _,
                rest: _,
            }) => {
                let mut oil_fields = vec![];
                for field in fields.iter() {
                    let oil_expr = self.graft_expr(&field.expr);
                    let field_name = if let syn::Member::Named(ident) = &field.member {
                        ident.to_string()
                    } else {
                        panic!("Field must be named in struct declaration")
                    };
                    oil_fields.push((field_name, oil_expr));
                }

                // Get name of struct from declaration
                assert!(
                    path.segments.len().is_one(),
                    "Struct declaration must have path segment of length 1 for now"
                );
                let name_of_struct = path.segments[0].ident.to_string();

                ast::Expr::Struct(ast::StructExpr {
                    struct_type: ast_types::DataType::Unresolved(name_of_struct),
                    field_names_and_values: oil_fields,
                })
            }
            syn::Expr::Lit(litexp) => {
                let lit = &litexp.lit;
                ast::Expr::Lit(self.graft_lit(lit))
            }
            syn::Expr::Call(call_exp) => self.graft_call_exp(call_exp),
            syn::Expr::Paren(paren_exp) => self.graft_expr(&paren_exp.expr),
            syn::Expr::If(expr_if) => {
                let condition = self.graft_expr(&expr_if.cond);
                let if_branch = &expr_if.then_branch.stmts;
                let then_branch = match if_branch.last() {
                    Some(syn::Stmt::Expr(last_expr)) => {
                        let then_branch_statements = if_branch[0..if_branch.len() - 1]
                            .iter()
                            .map(|x| self.graft_stmt(x))
                            .collect_vec();
                        let then_branch_last_expr = self.graft_expr(last_expr);
                        ReturningBlock {
                            stmts: then_branch_statements,
                            return_expr: then_branch_last_expr,
                        }
                    }
                    _ => panic!("unsupported: {if_branch:#?}"),
                };

                let else_branch = &expr_if.else_branch.as_ref().unwrap().1;
                let else_branch = match else_branch.as_ref() {
                    syn::Expr::Block(block) => {
                        let else_branch = &block.block.stmts;
                        match else_branch.last() {
                            Some(syn::Stmt::Expr(last_expr)) => {
                                let else_branch_statements = else_branch[0..else_branch.len() - 1]
                                    .iter()
                                    .map(|x| self.graft_stmt(x))
                                    .collect_vec();
                                let else_branch_last_expr = self.graft_expr(last_expr);
                                ReturningBlock {
                                    stmts: else_branch_statements,
                                    return_expr: else_branch_last_expr,
                                }
                            }
                            _ => panic!("unsupported: {if_branch:#?}"),
                        }
                    }
                    other => panic!("unsupported: {other:?}"),
                };

                ast::Expr::If(ast::ExprIf {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
            }
            syn::Expr::MethodCall(method_call_expr) => self.graft_method_call(method_call_expr),
            syn::Expr::Field(field_expr) => ast::Expr::Var(self.graft_field_expression(field_expr)),
            syn::Expr::Index(index_expr) => {
                let expr = self.graft_expr(&index_expr.expr);
                let index = self.graft_expr(&index_expr.index);
                let index = match index {
                    ast::Expr::Lit(lit) => ast::IndexExpr::Static(match lit {
                        ast::ExprLit::GenericNum(val, _) => val.try_into().unwrap(),
                        ast::ExprLit::U32(val) => val.try_into().unwrap(),
                        _ => unreachable!(),
                    }),
                    expr => ast::IndexExpr::Dynamic(expr),
                };

                if let ast::Expr::Var(identifier) = expr {
                    ast::Expr::Var(ast::Identifier::Index(
                        Box::new(identifier),
                        Box::new(index),
                        Default::default(),
                    ))
                } else {
                    panic!("unsupported index expression: {index_expr:#?}");
                }
            }
            syn::Expr::Cast(syn::ExprCast {
                attrs: _attrs,
                expr,
                as_token: _as_token,
                ty,
            }) => {
                let unboxed_ty: syn::Type = *(*ty).to_owned();
                let as_type = self.syn_type_to_ast_type(&unboxed_ty);
                let ast_expr = self.graft_expr(expr);
                ast::Expr::Cast(Box::new(ast_expr), as_type)
            }
            syn::Expr::Unary(syn::ExprUnary { attrs: _, op, expr }) => {
                let inner_expr = self.graft_expr(expr);
                let ast_op = match op {
                    syn::UnOp::Not(_) => ast::UnaryOp::Not,
                    syn::UnOp::Neg(_) => ast::UnaryOp::Neg,
                    syn::UnOp::Deref(_deref) => ast::UnaryOp::Deref,
                };
                ast::Expr::Unary(ast_op, Box::new(inner_expr), Default::default())
            }
            syn::Expr::Reference(syn::ExprReference {
                attrs: _,
                and_token: _,
                raw: _,
                mutability,
                expr,
            }) => {
                let inner_expr = self.graft_expr(expr);
                ast::Expr::Unary(
                    ast::UnaryOp::Ref(mutability.is_some()),
                    Box::new(inner_expr),
                    Default::default(),
                )
            }
            syn::Expr::Block(syn::ExprBlock {
                attrs: _,
                label: _,
                block,
            }) => {
                let (returning_expr, stmts) = block.stmts.split_last().unwrap();

                // Last line must be an expression, cannot be a binding or anything else.
                let return_expr = match returning_expr {
                    syn::Stmt::Local(_) => panic!(),
                    syn::Stmt::Item(_) => panic!(),
                    syn::Stmt::Expr(expr) => self.graft_expr(expr),
                    syn::Stmt::Semi(_, _) => panic!(),
                };

                let stmts = stmts.iter().map(|x| self.graft_stmt(x)).collect();
                ast::Expr::ReturningBlock(Box::new(ast::ReturningBlock { stmts, return_expr }))
            }
            syn::Expr::Array(syn::ExprArray {
                attrs: _,
                bracket_token: _,
                elems,
            }) => {
                let elements = elems.iter().map(|x| self.graft_expr(x));
                ast::Expr::Array(
                    ast::ArrayExpression::ElementsSpecified(elements.collect_vec()),
                    Default::default(),
                )
            }
            syn::Expr::Try(syn::ExprTry {
                attrs: _,
                expr,
                question_token: _,
            }) => {
                // `?` is the same as `unwrap` for this compiler
                ast::Expr::MethodCall(ast::MethodCall {
                    method_name: "unwrap".to_owned(),
                    args: vec![self.graft_expr(expr)],
                    annot: Default::default(),
                    associated_type: Default::default(),
                })
            }
            other => panic!("unsupported: {other:?}"),
        }
    }

    fn graft_lit(&mut self, rust_val: &syn::Lit) -> ast::ExprLit<Annotation> {
        use ast::ExprLit::*;

        match rust_val {
            syn::Lit::Bool(b) => Bool(b.value),
            syn::Lit::Int(int_lit) => {
                let int_lit_str = int_lit.token().to_string();

                // Despite its name `base10_parse` can handle hex. Don't ask me why.
                if let Some(_int_lit_stripped) = int_lit_str.strip_suffix("u32") {
                    if let Ok(int_u32) = int_lit.base10_parse::<u32>() {
                        return ast::ExprLit::U32(int_u32);
                    }
                }

                // `usize` is just an alias for `u32` in this compiler
                if let Some(_int_lit_stripped) = int_lit_str.strip_suffix("usize") {
                    if let Ok(int_u32) = int_lit.base10_parse::<u32>() {
                        return ast::ExprLit::U32(int_u32);
                    }
                }

                if let Some(_int_lit_stripped) = int_lit_str.strip_suffix("u64") {
                    if let Ok(int_u64) = int_lit.base10_parse::<u64>() {
                        return ast::ExprLit::U64(int_u64);
                    }
                }

                if let Some(_int_lit_stripped) = int_lit_str.strip_suffix("u128") {
                    if let Ok(int_u128) = int_lit.base10_parse::<u128>() {
                        return ast::ExprLit::U128(int_u128);
                    }
                }

                if let Ok(int_u128) = int_lit.base10_parse::<u128>() {
                    return ast::ExprLit::GenericNum(int_u128, Default::default());
                }

                panic!("unsupported integer literal: {int_lit_str}");
            }
            other => panic!("unsupported: {other:?}"),
        }
    }

    fn graft_binop(rust_binop: syn::BinOp) -> ast::BinOp {
        match rust_binop {
            syn::BinOp::Add(_) => ast::BinOp::Add,
            syn::BinOp::And(_) => ast::BinOp::And,
            syn::BinOp::BitAnd(_) => ast::BinOp::BitAnd,
            syn::BinOp::BitXor(_) => ast::BinOp::BitXor,
            syn::BinOp::BitOr(_) => ast::BinOp::BitOr,
            syn::BinOp::Div(_) => ast::BinOp::Div,
            syn::BinOp::Eq(_) => ast::BinOp::Eq,
            syn::BinOp::Lt(_) => ast::BinOp::Lt,
            syn::BinOp::Gt(_) => ast::BinOp::Gt,
            syn::BinOp::Mul(_) => ast::BinOp::Mul,
            syn::BinOp::Ne(_) => ast::BinOp::Neq,
            syn::BinOp::Or(_) => ast::BinOp::Or,
            syn::BinOp::Rem(_) => ast::BinOp::Rem,
            syn::BinOp::Shl(_) => ast::BinOp::Shl,
            syn::BinOp::Shr(_) => ast::BinOp::Shr,
            syn::BinOp::Sub(_) => ast::BinOp::Sub,
            other => panic!("unsupported: {other:?}"),
        }
    }

    fn graft_eq_binop(rust_eq_binop: &syn::BinOp) -> ast::BinOp {
        match rust_eq_binop {
            syn::BinOp::AddEq(_) => ast::BinOp::Add,
            syn::BinOp::SubEq(_) => ast::BinOp::Sub,
            syn::BinOp::MulEq(_) => ast::BinOp::Mul,
            syn::BinOp::DivEq(_) => ast::BinOp::Div,
            syn::BinOp::RemEq(_) => ast::BinOp::Rem,
            syn::BinOp::BitXorEq(_) => ast::BinOp::BitXor,
            syn::BinOp::BitAndEq(_) => ast::BinOp::BitAnd,
            syn::BinOp::ShlEq(_) => ast::BinOp::Shl,
            syn::BinOp::ShrEq(_) => ast::BinOp::Shr,
            other => panic!("unsupported for eq binop: {other:?}"),
        }
    }

    pub fn graft_stmt(&mut self, rust_stmt: &syn::Stmt) -> ast::Stmt<Annotation> {
        return match rust_stmt {
            // variable declarations
            syn::Stmt::Local(local) => graft_local_stmt(self, local),
            // Expressions
            syn::Stmt::Expr(expr) => graft_expr_stmt(self, expr),
            // ExpressionsThings that end with a semi-colon
            syn::Stmt::Semi(semi, _b) => graft_semi_stmt(self, semi),
            // Handle declared functions
            syn::Stmt::Item(item) => graft_item_stmt(self, item),
        };

        /// Handle declarations, i.e. `let a: u32 = 200;`
        fn graft_local_stmt(graft_config: &mut Graft, local: &syn::Local) -> ast::Stmt<Annotation> {
            let (ident, data_type, mutable): (String, ast_types::DataType, bool) = match &local.pat
            {
                syn::Pat::Type(pat_type) => {
                    let (dt, mutable): (ast_types::DataType, bool) =
                        graft_config.pat_type_to_data_type_and_mutability(pat_type);
                    let ident: String = Graft::pat_to_name(&pat_type.pat);

                    (ident, dt, mutable)
                }
                syn::Pat::Ident(d) => {
                    // This would indicate that the explicit type is missing
                    let ident = d.ident.to_string();
                    panic!("Missing explicit type in declaration of '{ident}'");
                }
                other => panic!("unsupported: {other:?}"),
            };

            let init = local.init.as_ref().unwrap();
            let init_expr = init.1.as_ref();
            let ast_expt = graft_config.graft_expr(init_expr);
            let let_stmt = ast::LetStmt {
                var_name: ident,
                data_type,
                expr: ast_expt,
                mutable,
            };
            ast::Stmt::Let(let_stmt)
        }

        /// Handle expressions
        fn graft_expr_stmt(graft_config: &mut Graft, expr: &syn::Expr) -> ast::Stmt<Annotation> {
            match expr {
                syn::Expr::While(while_stmt) => {
                    let expr_while = while_stmt;
                    let while_condition = graft_config.graft_expr(&expr_while.cond);
                    let while_stmts: Vec<ast::Stmt<Annotation>> = while_stmt
                        .body
                        .stmts
                        .iter()
                        .map(|x| graft_config.graft_stmt(x))
                        .collect_vec();

                    let while_stmt = ast::WhileStmt {
                        condition: while_condition,
                        block: ast::BlockStmt { stmts: while_stmts },
                    };
                    ast::Stmt::While(while_stmt)
                }
                syn::Expr::If(if_expr) => {
                    let if_condition = graft_config.graft_expr(&if_expr.cond);
                    let then_stmts: Vec<ast::Stmt<Annotation>> = if_expr
                        .then_branch
                        .stmts
                        .iter()
                        .map(|x| graft_config.graft_stmt(x))
                        .collect_vec();
                    let else_stmts: Vec<ast::Stmt<Annotation>> = match if_expr.else_branch.as_ref()
                    {
                        Some(else_stmts) => match else_stmts.1.as_ref() {
                            syn::Expr::Block(block) => block
                                .block
                                .stmts
                                .iter()
                                .map(|x| graft_config.graft_stmt(x))
                                .collect(),
                            other => panic!("unsupported: {other:?}"),
                        },
                        None => vec![],
                    };

                    let if_stmt = ast::IfStmt {
                        condition: if_condition,
                        then_branch: ast::BlockStmt { stmts: then_stmts },
                        else_branch: ast::BlockStmt { stmts: else_stmts },
                    };
                    ast::Stmt::If(if_stmt)
                }
                syn::Expr::Block(syn::ExprBlock {
                    attrs: _attrs,
                    label: _label,
                    block,
                }) => {
                    let stmts: Vec<ast::Stmt<Annotation>> = block
                        .stmts
                        .iter()
                        .map(|x| graft_config.graft_stmt(x))
                        .collect_vec();
                    ast::Stmt::Block(ast::BlockStmt { stmts })
                }
                other => panic!("unsupported expression. make sure to end statements by semi-colon and to explicitly 'return':\n{other:?}"),
            }
        }

        /// Handle things that end with a semi-colon
        fn graft_semi_stmt(graft_config: &mut Graft, semi: &syn::Expr) -> ast::Stmt<Annotation> {
            match semi {
                syn::Expr::Return(ret_expr) => {
                    let optional_ret_expr = ret_expr
                        .expr
                        .as_ref()
                        .map(|ret_expr| graft_config.graft_expr(ret_expr));
                    ast::Stmt::Return(optional_ret_expr)
                }
                syn::Expr::Call(call_exp) => {
                    // Handle a function call that's not an assignment or a return expression
                    let ast_fn_call = graft_config.graft_call_exp(call_exp);

                    match ast_fn_call {
                        ast::Expr::FnCall(fncall) => ast::Stmt::FnCall(fncall),
                        _ => panic!("function call as a statement cannot be a literal"),
                    }
                }
                syn::Expr::Assign(syn::ExprAssign {
                    attrs: _,
                    left,
                    eq_token: _,
                    right,
                }) => {
                    // let identifier_expr = assign.left.as_ref();
                    let left_expr = graft_config.graft_expr(left);
                    let left_ident = match left_expr {
                        ast::Expr::Var(ident) => ident,
                        _ => {
                            panic!("Left-hand-side of tuple operator must be a declared variable. Declare more bindings if needed. Failed to parse expression: {left_expr} as an identifier");
                        }
                    };
                    let right_expr = graft_config.graft_expr(right);
                    let assign_stmt = ast::AssignStmt {
                        identifier: left_ident,
                        expr: right_expr,
                    };
                    ast::Stmt::Assign(assign_stmt)
                }
                // Handle expressions of the type `i += 1`
                syn::Expr::AssignOp(syn::ExprAssignOp {
                    attrs: _,
                    left,
                    op,
                    right,
                }) => {
                    // let identifier_expr = assign.left.as_ref();
                    let identifier_expr = graft_config.graft_expr(left);
                    let identifier = match identifier_expr {
                        ast::Expr::Var(ident) => ident,
                        _ => {
                            panic!("Left-hand-side of tuple operator must be a declared variable. Declare more bindings if needed. Failed to parse expression: {identifier_expr} as an identifier");
                        }
                    };
                    let assign_expr = graft_config.graft_binop_eq_expr(left, op, right);
                    let assign_stmt = ast::AssignStmt {
                        identifier,
                        expr: assign_expr,
                    };

                    ast::Stmt::Assign(assign_stmt)
                }
                syn::Expr::MethodCall(method_call_expr) => {
                    let grafted = graft_config.graft_method_call(method_call_expr);
                    match grafted {
                        ast::Expr::MethodCall(mc) => ast::Stmt::MethodCall(mc),
                        _ => panic!("Statement method call must graft to method call"),
                    }
                }
                syn::Expr::Macro(expr_macro) => {
                    let ident = Graft::path_to_ident(&expr_macro.mac.path);
                    assert_eq!(
                        "assert", ident,
                        "Can currently only handle `assert!` macro. Got: {ident}"
                    );

                    // The macro tokens are interpreted as an expression.
                    // We do not currently allow text associated with an assert statement,
                    // as I could not figure out how to parse such a token stream that an
                    // `assert( expr, "description" )` has.
                    let tokens = &expr_macro.mac.tokens;
                    let tokens_as_expr_syn: syn::Expr = parse_quote! { #tokens };
                    let tokens_as_expr = graft_config.graft_expr(&tokens_as_expr_syn);
                    ast::Stmt::Assert(ast::AssertStmt {
                        expression: tokens_as_expr,
                    })
                }
                syn::Expr::Match(syn::ExprMatch {
                    attrs: _,
                    match_token: _,
                    expr,
                    brace_token: _,
                    arms,
                }) => {
                    let match_expression = graft_config.graft_expr(expr);
                    let mut match_arms = vec![];
                    for arm in arms.iter() {
                        let syn::Arm {
                            attrs: _,
                            pat,
                            guard: _,
                            fat_arrow_token: _,
                            body,
                            comma: _,
                        }: &syn::Arm = arm;
                        let arm_body = graft_expr_stmt(graft_config, body);

                        // TODO: Add support for `_` matching
                        let arm_body = if let ast::Stmt::Block(block_stmt) = arm_body {
                            block_stmt
                        } else {
                            panic!("Expected block statement for match-arm's body")
                        };

                        let match_condition = match pat {
                            syn::Pat::Box(_) => todo!(),
                            syn::Pat::Ident(_) => todo!(),
                            syn::Pat::Lit(_) => todo!(),
                            syn::Pat::Macro(_) => todo!(),
                            syn::Pat::Or(_) => todo!(),
                            syn::Pat::Path(syn::PatPath {
                                attrs: _,
                                qself: _,
                                path,
                            }) => {
                                // Enums that are in prelude can be matched with only the variant name, like `None`
                                // instead of `Result::None`
                                let enum_case = Graft::path_to_ident(path);
                                let enum_case_split = enum_case.split("::").collect_vec();
                                let (type_name, variant_name) = match enum_case_split.len() {
                                    1 => (None, enum_case_split[0].to_owned()),
                                    2 => (Some(enum_case_split[0].to_owned()), enum_case_split[1].to_owned()),
                                    _ => panic!("Expected `<Type>::<VariantName>` or `VariantName` for enum match case. Got: `{enum_case}`"),
                                };

                                ast::MatchCondition::EnumVariant(ast::EnumVariantSelector {
                                    data_bindings: Vec::default(),
                                    type_name,
                                    variant_name,
                                })
                            }
                            syn::Pat::Range(_) => todo!(),
                            syn::Pat::Reference(_) => todo!(),
                            syn::Pat::Rest(_) => todo!(),
                            syn::Pat::Slice(_) => todo!(),
                            syn::Pat::Struct(_) => todo!(),
                            syn::Pat::Tuple(_) => todo!(),
                            syn::Pat::TupleStruct(syn::PatTupleStruct {
                                attrs: _,
                                pat,
                                path,
                            }) => {
                                // Enums that are in prelude can be matched with only the variant name, like `None`
                                // instead of `Result::None`
                                let enum_case = Graft::path_to_ident(path);
                                let enum_case_split = enum_case.split("::").collect_vec();
                                let (type_name, variant_name) = match enum_case_split.len() {
                                    1 => (None, enum_case_split[0].to_owned()),
                                    2 => (Some(enum_case_split[0].to_owned()), enum_case_split[1].to_owned()),
                                    _ => panic!("Expected `<Type>::<VariantName>` or `<VariantName>` for enum match case. Got: `{enum_case}`"),
                                };

                                let mut data_bindings = vec![];
                                for pat_elem in pat.elems.iter() {
                                    match pat_elem {
                                        syn::Pat::Ident(ident) => {
                                            data_bindings.push(ast::PatternMatchedBinding {
                                                mutable: ident.mutability.is_some(),
                                                name: ident.ident.to_string(),
                                            });
                                        }
                                        syn::Pat::Wild(_) => {
                                            assert!(
                                                pat.elems.len().is_one(),
                                                "For now, wildcard binding must be only binding"
                                            );
                                        }
                                        other => {
                                            panic!("unsupported binding for match-arm: {other:?}")
                                        }
                                    }
                                }

                                ast::MatchCondition::EnumVariant(ast::EnumVariantSelector {
                                    type_name,
                                    variant_name,
                                    data_bindings,
                                })
                            }
                            syn::Pat::Type(_) => todo!(),
                            syn::Pat::Verbatim(_) => todo!(),
                            syn::Pat::Wild(syn::PatWild {
                                attrs: _,
                                underscore_token: _,
                            }) => ast::MatchCondition::CatchAll,
                            _ => todo!(),
                        };
                        match_arms.push(ast::MatchArm {
                            match_condition,
                            body: arm_body,
                        });
                    }

                    ast::Stmt::Match(ast::MatchStmt {
                        arms: match_arms,
                        match_expression,
                    })
                }
                other => panic!("unsupported: {other:#?}"),
            }
        }

        /// Handle locally declared functions: `fn foo(input: BFieldElement) -> BFieldelement { return input * input; }`
        fn graft_item_stmt(graft_config: &mut Graft, item: &syn::Item) -> ast::Stmt<Annotation> {
            match item {
                syn::Item::Fn(item_fn) => {
                    ast::Stmt::FnDeclaration(graft_config.graft_fn_decl(item_fn))
                }
                other => panic!("unsupported: {other:#?}"),
            }
        }
    }
}

pub fn item_fn(item: syn::Item) -> syn::ItemFn {
    match item {
        syn::Item::Fn(item_fn) => item_fn,
        other => panic!("item_fn: expected fn, found: {other:#?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn big_mmr_function() {
        let tokens: syn::Item = parse_quote! {
            fn calculate_new_peaks_from_leaf_mutation(
                old_peaks: Vec<Digest>,
                new_leaf: Digest,
                leaf_count: u64,
                auth_path: Vec<Digest>,
                leaf_index: u64,
            ) -> Vec<Digest> {
                // let (mut acc_mt_index, peak_index) =
                let acc_mt_index_and_peak_index: (u64, u32) = leaf_index_to_mt_index_and_peak_index(leaf_index, leaf_count);
                let mut acc_hash: Digest = new_leaf;
                let mut i: u32 = 0u32;
                while acc_mt_index_and_peak_index.0 != 1u64 {
                    let ap_element: Digest = auth_path[i];
                    if acc_mt_index_and_peak_index.0 % 2u64 == 1u64 {
                        // Node with `acc_hash` is a right child
                        acc_hash = H::hash_pair(ap_element, acc_hash);
                    } else {
                        // Node with `acc_hash` is a left child
                        acc_hash = H::hash_pair(acc_hash, ap_element);
                    }

                    acc_mt_index_and_peak_index.0 = acc_mt_index_and_peak_index.0 / 2u64;
                    i = i + 1u32;
                }

                let mut calculated_peaks: Vec<Digest> = old_peaks.to_vec();
                calculated_peaks[acc_mt_index_and_peak_index.1] = acc_hash;

                return calculated_peaks;
        }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn make_a_list() {
        let tokens: syn::Item = parse_quote! {
            fn make_and_return_a_list() -> Vec<u64> {
                let mut a: Vec<u64> = Vec::<u64>::default();
                let mut b: Vec<u64> = Vec::default();
                a.push(43u64);
                a.push(10u64);
                a.pop().unwrap();

                return a;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn leaf_count_to_node_count() {
        let tokens: syn::Item = parse_quote! {
                fn leaf_count_to_node_count(leaf_count: u64) -> u64 {
                    if leaf_count == 0u64 {
                        return 0u64;
                    }

                    let rightmost_leaf_leaf_index: u64 = leaf_count - 1u64;
                    let non_leaf_nodes_left: u64 = non_leaf_nodes_left(rightmost_leaf_leaf_index);
                    let node_index_of_rightmost_leaf: u64 = leaf_index_to_node_index(rightmost_leaf_leaf_index);

                    let mut non_leaf_nodes_after: u64 = 0u64;
                    let mut node_index: u64 = node_index_of_rightmost_leaf;
                    let mut right_count: u64 = right_lineage_length(node_index);
                    while right_count != 0u64 {
                        non_leaf_nodes_after = non_leaf_nodes_after + 1u64;
                        // go to parent (parent of right child has node index plus 1)
                        node_index = node_index + 1u64;
                        right_count = right_count - 1u64;
                    }

                    // Number of nodes is: non-leafs after, non-leafs before, and leaf count
                    return non_leaf_nodes_after + non_leaf_nodes_left + leaf_count;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn right_lineage_length() {
        let tokens: syn::Item = parse_quote! {
            fn right_lineage_length(node_index: u64) -> u64 {
                let bit_width: u64 = u64::BITS - u64::leading_zeros(node_index);
                let npo2: u64 = 1u64 << bit_width;

                let dist: u64 = npo2 - node_index;

                let ret: u64 = if (bit_width) < dist {
                    right_lineage_length(node_index - (npo2 >> 1u64) + 1u64)
                } else {
                    (dist - 1u64)
                };

                return ret;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn mmr_leftmost_ancestor() {
        let tokens: syn::Item = parse_quote! {
            fn leftmost_ancestor(node_index: u64) -> (u64, u32) {
                // let h = u128::BITS - node_index.leading_zeros() - 1;
                let h: u32 = u64::BITS - u64::leading_zeros(node_index) - 1u32;
                let ret: u64 = (1u64 << (h + 1u64)) - 1u64;

                return (ret, h);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn mmr_left_child() {
        let tokens: syn::Item = parse_quote! {
                fn left_child(node_index: u64, height: u64) -> u64 {
                    return node_index - (1u64 << height);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn function_call_no_args() {
        let tokens: syn::Item = parse_quote! {
            fn method_call() -> () {
                pop();
                push();
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn function_call_with_args() {
        let tokens: syn::Item = parse_quote! {
            fn method_call(lhs: u32, pointer: BFieldElement) -> () {
                pop(lhs);
                push(pointer, lhs);
                let foo: u32 = barbarian(7u32);

                return (pointer, foo, greek(barbarian(barbarian(greek(199u64)))));
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u64_algebra() {
        let tokens: syn::Item = parse_quote! {
            fn u64_algebra(lhs: u64, rhs: u64) -> (u64, u64, u64, u64, u64, u64, u64) {
                let a: u64 = lhs + rhs;
                let b: u64 = lhs - rhs;
                let c: u64 = lhs * rhs;
                let d: u64 = lhs / rhs;
                let e: u64 = 1u64 << 17u64;
                let f: u64 = 1u64 << lhs;
                let g: u64 = 1u64 >> rhs;

                return (a, b, c, d, e, f, g);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u32_algebra() {
        let tokens: syn::Item = parse_quote! {
            fn u32_algebra(lhs: u32, rhs: u32) -> (u32, u32, u32, u32) {
                let a: u32 = lhs + rhs;
                let b: u32 = lhs - rhs;
                let c: u32 = lhs * rhs;
                let d: u32 = lhs / rhs;
                let e: u32 = 1u32 << 17u32;
                let f: u32 = 1u32 << lhs;
                let g: u32 = 1u32 >> rhs;
                let h: u32 = lhs % 2u32;
                let i: bool = (lhs % 2u32) == 0u32;

                // Verify correct precedence handling
                let j: bool = (lhs + 14u32) * 117u32 - ((1u32 - (2u32 - rhs)) - (lhs - rhs));

                return (d, e, f, g);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn boolean_algebra() {
        let tokens: syn::Item = parse_quote! {
            fn boolean_algebra(lhs: bool, rhs: bool) -> (bool, bool, bool, bool, bool, bool) {
                let a: bool = lhs && rhs;
                let b: bool = lhs ^ rhs;
                let c: bool = lhs || rhs;
                let d: bool = true;
                let e: bool = false;
                let f: bool = true && false || false ^ false;

                return (a, b, c, d, e);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn and_and_xor_u32() {
        let tokens: syn::Item = parse_quote! {
            fn and_and_xor_u32(lhs: u32, rhs: u32) -> (u32, u32) {
                let a: u32 = lhs & rhs;
                let b: u32 = lhs ^ rhs;
                return (a, b);
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn bfe_add_return_expr() {
        let tokens: syn::Item = parse_quote! {
            fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
                return lhs + rhs;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn bfe_add_return_var() {
        let tokens: syn::Item = parse_quote! {
            fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
                let sum: BFieldElement = lhs + rhs;
                return sum;
            }
        };

        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }

    #[test]
    fn u32_swap() {
        let tokens: syn::Item = parse_quote! {
            fn swap_u32(lhs: u32, rhs: u32) -> (u32, u32) {
                return (rhs, lhs);
            }
        };
        match &tokens {
            syn::Item::Fn(item_fn) => {
                get_standard_setup!(ast_types::ListType::Safe, graft_config, libraries);
                let _ret = graft_config.graft_fn_decl(item_fn);
            }
            _ => panic!("unsupported"),
        }
    }
}
