use tasm_lib::list::LIST_METADATA_SIZE;
use tasm_lib::memory::dyn_malloc;
use tasm_lib::triton_vm::prelude::*;

use super::Library;
use crate::ast;
use crate::ast_types;
use crate::ast_types::DataType;
use crate::composite_types::CompositeTypes;
use crate::tasm_code_generator::write_n_words_to_memory_leaving_address;
use crate::tasm_code_generator::CompilerState;

const ENCODE_METHOD_NAME: &str = "encode";

#[derive(Clone, Debug)]
pub(crate) struct BFieldCodecLib;

impl BFieldCodecLib {
    fn encode_method_signature(
        &self,
        receiver_type: &crate::ast_types::DataType,
    ) -> ast::FnSignature {
        ast::FnSignature {
            name: ENCODE_METHOD_NAME.to_owned(),
            args: vec![ast_types::AbstractArgument::ValueArgument(
                ast_types::AbstractValueArg {
                    name: "value".to_owned(),
                    data_type: receiver_type.to_owned(),
                    mutable: false,
                },
            )],
            output: ast_types::DataType::List(Box::new(ast_types::DataType::Bfe)),
            arg_evaluation_order: Default::default(),
        }
    }

    pub(super) fn encode_method(
        method_name: &str,
        receiver_type: &DataType,
        state: &mut CompilerState,
    ) -> (String, Vec<LabelledInstruction>) {
        let encoding_length = receiver_type.bfield_codec_static_length().unwrap();
        let list_size_in_memory = (LIST_METADATA_SIZE + encoding_length) as i32;

        let dyn_malloc_label = state.import_snippet(Box::new(dyn_malloc::DynMalloc));

        let encode_subroutine_label =
            format!("{method_name}_{}", receiver_type.label_friendly_name());
        let encode_subroutine_code = triton_asm!(
                {encode_subroutine_label}:
                                    // _ [value]

                    call {dyn_malloc_label}
                                    // _ [value] *list

                    // write length
                    push {encoding_length}
                    swap 1
                    write_mem 1     // _ [value] (*list + 1)
                                    // _ [value] *word_0

                    {&write_n_words_to_memory_leaving_address(encoding_length)}
                                    // _ (*last_word + 1)

                    push {-list_size_in_memory}
                    add             // _ *list
                    return
        );
        (encode_subroutine_label, encode_subroutine_code)
    }
}

impl Library for BFieldCodecLib {
    fn handle_function_call(
        &self,
        _full_name: &str,
        _qualified_self_type: &Option<DataType>,
    ) -> bool {
        false
    }

    fn handle_method_call(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
    ) -> bool {
        if method_name != ENCODE_METHOD_NAME {
            return false;
        }

        if receiver_type.bfield_codec_static_length().is_none() {
            panic!(
                ".encode() can only be called on values with a statically known length. \
                    Got:  {receiver_type:#?}"
            );
        }

        true
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> crate::ast::FnSignature {
        if method_name != ENCODE_METHOD_NAME || receiver_type.bfield_codec_static_length().is_none()
        {
            panic!(
                "Unknown method in BFieldCodecLib. \
                Got: {method_name} on receiver_type: {receiver_type}"
            );
        }

        self.encode_method_signature(receiver_type)
    }

    fn function_name_to_signature(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        _qualified_self_type: &Option<DataType>,
        _composite_types: &mut CompositeTypes,
    ) -> crate::ast::FnSignature {
        todo!()
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &crate::ast_types::DataType,
        _args: &[crate::ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        if method_name != ENCODE_METHOD_NAME || receiver_type.bfield_codec_static_length().is_none()
        {
            panic!(
                "Unknown method in BFieldCodecLib. \
                Got: {method_name} on receiver_type: {receiver_type}"
            );
        }

        let (encode_subroutine_label, encode_subroutine_code) =
            Self::encode_method(method_name, receiver_type, state);

        state.add_subroutine(encode_subroutine_code.try_into().unwrap());

        triton_asm!(call {
            encode_subroutine_label
        })
    }

    fn call_function(
        &self,
        _fn_name: &str,
        _type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
        _qualified_self_type: &Option<DataType>,
    ) -> Vec<LabelledInstruction> {
        todo!()
    }
}
