use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
fn trivial_local_function_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_local_function() {
            fn foo() {
                return;
            }
            return;
        }
    })
}

#[allow(dead_code)]
fn local_function_type_error_in_fn_decl() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_local_function() {
            fn foo() -> bool {
                return;
            }
            return;
        }
    })
}

#[allow(dead_code)]
fn local_function_type_error_in_fn_call() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_local_function() -> u32 {
            fn foo() -> bool {
                return false;
            }
            let a: u32 = foo();
            return a;
        }
    })
}

#[allow(dead_code)]
fn local_function_with_return_value_bool() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_local_function() -> bool {
            fn foo() -> bool {
                return false;
            }
            let a: bool = foo();
            return a;
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn trivial_local_function_test() {
        graft_check_compile_prop(&trivial_local_function_rast());
    }

    #[should_panic]
    #[test]
    fn local_function_type_error_in_fn_decl_test() {
        graft_check_compile_prop(&local_function_type_error_in_fn_decl());
    }

    #[should_panic]
    #[test]
    fn local_function_type_error_in_fn_call_test() {
        graft_check_compile_prop(&local_function_type_error_in_fn_call());
    }

    #[test]
    fn local_function_with_return_value_bool_test() {
        graft_check_compile_prop(&local_function_with_return_value_bool());
    }
}
