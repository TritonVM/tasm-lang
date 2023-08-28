use triton_vm::BFieldElement;

use crate::{ast, graft};

use super::Library;

#[derive(Clone, Debug)]
pub struct BFieldCodecLib;

impl Library for BFieldCodecLib {
    fn get_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn get_method_name(
        &self,
        _method_name: &str,
        _receiver_type: &crate::ast::DataType,
    ) -> Option<String> {
        None
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &crate::ast::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
    ) -> crate::ast::FnSignature {
        panic!("BFieldCodecLib does not contain any methods")
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
    ) -> crate::ast::FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &crate::ast::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        todo!()
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        todo!()
    }

    fn get_graft_function_name(&self, full_name: &str) -> Option<String> {
        if full_name.contains("::decode") {
            Some(full_name.to_owned())
        } else {
            None
        }
    }

    fn graft_function(
        &self,
        fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        if !fn_name.contains("::decode") {
            return None;
        }

        // Graft function to remove last `unwrap()`.

        // Fetch the returned type
        let split_fn_name: Vec<_> = fn_name.split("::").collect();
        assert_eq!(
            2,
            split_fn_name.len(),
            "Can only handle pattern <Type>::decode. Got: {fn_name}"
        );
        let return_type = split_fn_name[0].to_owned();

        // panic!("fn_name: {fn_name}\nargs: {args:#?}. return_type: {return_type}");

        // Maybe we can just return the pointer/struct?
        // For that we would have to fish out the argument tp `load_from_memory`
        // TODO: Maybe `address` can be an expression and doesn't have to be a literal?
        // println!("args\n{args:#?}");
        // let ret: ast::Expr<super::Annotation> =
        //     ast::Expr::Lit(ast::ExprLit::Struct(return_type, BFieldElement::new(1)));
        let ret: ast::Expr<super::Annotation> =
            ast::Expr::Lit(ast::ExprLit::MemPointer(ast::MemPointerLiteral {
                // TODO: mem_pointer_address shouldn't be hardcoded here!
                mem_pointer_address: BFieldElement::new(1),
                struct_name: return_type,
            }));
        Some(ret)
    }

    fn graft_method(
        &self,
        rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        // println!("bfcgm:\nrust_method_call\n{rust_method_call:#?}");
        const UNWRAP_NAME: &str = "unwrap";

        // Remove `unwrap` if method call is `T::decode(...).unwrap()`
        let last_method_name = rust_method_call.method.to_string();

        if last_method_name != UNWRAP_NAME {
            return None;
        }

        match rust_method_call.receiver.as_ref() {
            syn::Expr::Call(ca) => {
                let preceding_function_call = graft::graft_call_exp(ca);
                if matches!(
                    preceding_function_call,
                    // ast::Expr::Lit(ast::ExprLit::Struct(_, _))
                    ast::Expr::Lit(ast::ExprLit::MemPointer(_))
                ) {
                    Some(preceding_function_call)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
