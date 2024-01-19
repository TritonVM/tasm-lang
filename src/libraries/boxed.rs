use num::One;
use tasm_lib::memory::dyn_malloc::DynMalloc;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::ast;
use crate::ast_types;
use crate::graft::Graft;
use crate::subroutine::SubRoutine;

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
            let call_box_new = call_new_box(&type_parameter.unwrap(), state);
            return call_box_new;
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
fn call_new_box(
    inner_type: &ast_types::DataType,
    state: &mut crate::tasm_code_generator::CompilerState,
) -> Vec<LabelledInstruction> {
    let entrypoint = format!("box_new_{}", inner_type.label_friendly_name());
    let call_function = triton_asm!(call { entrypoint });
    if state.contains_subroutine(&entrypoint) {
        return call_function;
    }

    let value_pointer_pointer = state.static_memory_allocation(1);
    let dyn_malloc_label = state.import_snippet(Box::new(DynMalloc));
    let store_value = inner_type.store_to_memory(state);
    const BIG_ALLOCATION: u32 = u32::MAX;
    let subroutine = triton_asm!(
        {entrypoint}:

            // 1. Allocate:
            // In order to do that, we need to know how many words to allocate, or we just
            // allocate *enough*
            push {BIG_ALLOCATION}
            call {dyn_malloc_label}

            // _ [value] *value

            // 2. store the *value in statically allocated memory,
            // as the callee to store a value might not know how to preserve this value.
            dup 0
            push {value_pointer_pointer}
            write_mem 1
            pop 1
            // _ [value] *value

            // 3. Call `store_to_memory` on the data type.
            {&store_value}
            // _

            // 4. Retrieve the stored pointer
            push {value_pointer_pointer}
            hint value_pointer_pointer = stack[0]
            read_mem 1
            pop 1
            // _ *value

            // 5. Return
            return
    );

    let subroutine: SubRoutine = subroutine.try_into().unwrap();

    state.add_library_function(subroutine);

    call_function
}
