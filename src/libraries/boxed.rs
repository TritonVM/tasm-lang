use num::One;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::ast;
use crate::ast_types;
use crate::graft::Graft;
use crate::subroutine::SubRoutine;
use crate::tasm_code_generator::write_n_words_to_memory_leaving_address;

use super::Library;

#[derive(Debug)]
pub struct Boxed;

const FUNCTION_NAME_NEW_BOX: &str = "Box::new";
const AS_REF_METHOD_NAME: &str = "as_ref";
const TO_OWNED_METHOD_NAME: &str = "to_owned";

impl Library for Boxed {
    fn get_function_name(&self, full_name: &str) -> Option<String> {
        if full_name == FUNCTION_NAME_NEW_BOX {
            return Some(full_name.to_owned());
        }

        None
    }

    fn get_method_name(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
    ) -> Option<String> {
        if matches!(method_name, AS_REF_METHOD_NAME | TO_OWNED_METHOD_NAME) {
            if let ast_types::DataType::Boxed(_inner) = receiver_type {
                return Some(method_name.to_owned());
            }
        }

        None
    }

    fn method_name_to_signature(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _type_checker_state: &crate::type_checker::CheckState,
    ) -> ast::FnSignature {
        match method_name {
            AS_REF_METHOD_NAME => Self::as_ref_method_signature(receiver_type),
            TO_OWNED_METHOD_NAME => Self::to_owned_method_signature(receiver_type),
            _ => panic!("unsupported method name"),
        }
    }

    fn function_name_to_signature(
        &self,
        full_name: &str,
        type_parameter: Option<ast_types::DataType>,
        args: &[ast::Expr<super::Annotation>],
    ) -> ast::FnSignature {
        if full_name == FUNCTION_NAME_NEW_BOX {
            assert!(
                args.len().is_one(),
                "Box::new only takes one argument. Got: {args:?}"
            );
            assert!(
                type_parameter.is_some(),
                "Box::new needs a type parameter due to restricted type inference"
            );
            return new_box_function_signature(&type_parameter.unwrap());
        }

        panic!("AllocBoxed does not known function with name {full_name}")
    }

    fn call_method(
        &self,
        method_name: &str,
        receiver_type: &ast_types::DataType,
        _args: &[ast::Expr<super::Annotation>],
        _state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<LabelledInstruction> {
        assert!(matches!(receiver_type, ast_types::DataType::Boxed(_inner)));
        assert!(matches!(
            method_name,
            AS_REF_METHOD_NAME | TO_OWNED_METHOD_NAME
        ));

        // This can simply be implemented as the identity function, I think.
        // The only reason we need this function is that Rustc demands it.
        triton_asm!()
    }

    fn call_function(
        &self,
        full_name: &str,
        type_parameter: Option<crate::ast_types::DataType>,
        _args: &[crate::ast::Expr<super::Annotation>],
        state: &mut crate::tasm_code_generator::CompilerState,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        if full_name == FUNCTION_NAME_NEW_BOX {
            let box_new: SubRoutine = new_box_function_body(&type_parameter.unwrap(), state)
                .try_into()
                .unwrap();
            let box_new_label = box_new.get_label();
            state.add_library_function(box_new);

            return triton_asm!(call { box_new_label });
        }

        panic!("AllocBoxed does not known function with name {full_name}")
    }

    fn get_graft_function_name(&self, _full_name: &str) -> Option<String> {
        None
    }

    fn graft_function(
        &self,
        _graft_config: &mut Graft,
        _fn_name: &str,
        _args: &syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
        _type_parameter: Option<crate::ast_types::DataType>,
    ) -> Option<crate::ast::Expr<super::Annotation>> {
        panic!("AllocBoxed cannot graft functions")
    }

    fn graft_method(
        &self,
        _graft_config: &mut Graft,
        _rust_method_call: &syn::ExprMethodCall,
    ) -> Option<ast::Expr<super::Annotation>> {
        None
    }
}

impl Boxed {
    fn as_ref_method_signature(receiver_type: &ast_types::DataType) -> ast::FnSignature {
        assert!(matches!(receiver_type, ast_types::DataType::Boxed(_inner)));

        let argument = ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
            name: "value".to_owned(),
            data_type: receiver_type.to_owned(),
            mutable: false,
        });

        ast::FnSignature {
            name: String::from(AS_REF_METHOD_NAME),
            args: vec![argument],
            output: receiver_type.to_owned(),
            arg_evaluation_order: Default::default(),
        }
    }

    fn to_owned_method_signature(receiver_type: &ast_types::DataType) -> ast::FnSignature {
        let ast_types::DataType::Boxed(inner_type) = receiver_type else {
            panic!("\"{TO_OWNED_METHOD_NAME}\" can only be called on boxed types");
        };
        let inner_type = *inner_type.to_owned();
        assert!(matches!(
            inner_type,
            ast_types::DataType::List(_, _) | ast_types::DataType::Array(_)
        ));

        let argument = ast_types::AbstractArgument::ValueArgument(ast_types::AbstractValueArg {
            name: "self".to_owned(),
            data_type: receiver_type.to_owned(),
            mutable: false,
        });

        ast::FnSignature {
            name: String::from(TO_OWNED_METHOD_NAME),
            args: vec![argument],
            output: inner_type,
            arg_evaluation_order: Default::default(),
        }
    }
}

/// BEFORE: _ [value] mem_address_start
/// AFTER:  _ mem_address_start
fn move_top_stack_value_to_memory_keep_address(top_value_size: usize) -> Vec<LabelledInstruction> {
    let mut code = write_n_words_to_memory_leaving_address(top_value_size);

    // reset memory address to its initial value
    let decrement_value = -(top_value_size as isize);
    code.extend(triton_asm!(push {decrement_value} add));

    code
}

fn new_box_function_signature(inner_type: &ast_types::DataType) -> ast::FnSignature {
    ast::FnSignature {
        name: format!("box_new_{}", inner_type.label_friendly_name()),
        args: vec![ast_types::AbstractArgument::ValueArgument(
            ast_types::AbstractValueArg {
                name: "x".to_owned(),
                data_type: inner_type.to_owned(),
                mutable: false,
            },
        )],
        output: ast_types::DataType::Boxed(Box::new(inner_type.to_owned())),
        arg_evaluation_order: Default::default(),
    }
}

/// ```text
/// BEFORE: _ [value]
/// AFTER: _ *value
/// ```
fn new_box_function_body(
    inner_type: &ast_types::DataType,
    state: &mut crate::tasm_code_generator::CompilerState,
) -> Vec<LabelledInstruction> {
    let dyn_malloc = state.import_snippet(Box::new(tasm_lib::memory::dyn_malloc::DynMalloc));
    let entrypoint = format!("box_new_{}", inner_type.label_friendly_name());
    let inner_type_size = inner_type.stack_size(); // This is the maximum size that the enum can take up

    let store_words_code = inner_type.store_to_memory(state);

    triton_asm!(
        {entrypoint}:
            // dynamically allocate enough memory
            push {inner_type_size}
            call {dyn_malloc}

            // _ [x] *memory_address
            {&store_words_code}

            // _ *memory_address
            return
    )
}
