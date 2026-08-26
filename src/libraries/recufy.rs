pub(crate) mod vm_proof_iter;

use super::Library;
use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::ast_types::StructType;
use crate::ast_types::StructVariant;
use crate::composite_types::CompositeTypes;
use crate::composite_types::TypeContext;

#[derive(Debug)]
pub(crate) struct RecufyLib;

impl Library for RecufyLib {
    fn handle_function_call(
        &self,
        _full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(&self, _method_name: &str, _receiver_type: &ast_types::DataType) -> bool {
        false
    }

    fn method_name_to_signature(
        &self,
        _fn_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        panic!()
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> ast::FnSignature {
        panic!()
    }

    fn call_method(
        &self,
        _method_name: &str,
        _receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        panic!()
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<ast_types::DataType>,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<tasm_lib::prelude::triton_vm::prelude::LabelledInstruction> {
        panic!()
    }
}

impl RecufyLib {
    /// The type of the response to a FRI query, as sent by the prover.
    pub(crate) fn fri_response_type_context() -> TypeContext {
        let struct_type = StructType {
            name: "FriResponse".to_owned(),
            is_copy: false,
            variant: StructVariant::named_fields(vec![
                (
                    "auth_structure".to_owned(),
                    ast_types::DataType::List(Box::new(ast_types::DataType::Digest)),
                ),
                (
                    "revealed_leaves".to_owned(),
                    ast_types::DataType::List(Box::new(ast_types::DataType::Xfe)),
                ),
            ]),
        };

        TypeContext {
            composite_type: struct_type.into(),
            methods: vec![],
            associated_functions: vec![],
        }
    }
}
