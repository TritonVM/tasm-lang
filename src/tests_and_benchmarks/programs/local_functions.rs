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

#[allow(dead_code)]
fn add_with_inner_mul_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn quad_sum(a: u32, b: u32) -> u32 {
            fn multiply_u32s(x: u32, y: u32) -> u32 {
                return x * y;
            }

            return multiply_u32s(a + b, 4);
        }
    })
}

#[allow(dead_code)]
fn factorial_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
    fn factorial(n: u64) -> u64 {
        fn fact_helper(n: u64, acc: u64) -> u64 {
            return if n == 0u64 {
                acc
            } else {
                fact_helper(n - 1, acc * n)
            };
        }

        return fact_helper(n, 1);
    }})
}

#[allow(dead_code)]
fn recursive_sum_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        /// f(n) = \Sum_{i=0}^n{\Sum_{j=0}^i(j)}
        fn recursive_sum(n: u32) -> u64 {
            fn inner_sum(n: u32, acc: u64) -> u64 {
                return if n == 0u32 {
                    acc
                } else {
                    inner_sum(n - 1, acc + n as u64)
                };
            }

            return if n == 0u32 {
                0u64
            } else {
                recursive_sum(n - 1) + inner_sum(n, 0)
            };
        }
    })
}

#[cfg(test)]
mod run_tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk_programs,
        test_helpers::{io_native, ozk_parsing, shared_test::*},
    };

    #[test]
    fn recursive_sum_test() {
        // Adding this local function here, because the function is a bit
        // hard to reason about. It calculates f(n) = \Sum_{i=0}^n{\Sum_{j=0}^i(j)}
        #[allow(clippy::needless_return)]
        fn recursive_sum(n: u32) -> u64 {
            fn inner_sum(n: u32, acc: u64) -> u64 {
                return if n == 0u32 {
                    acc
                } else {
                    inner_sum(n - 1, acc + n as u64)
                };
            }

            return if n == 0u32 {
                0u64
            } else {
                recursive_sum(n - 1) + inner_sum(n, 0)
            };
        }

        multiple_compare_prop_with_stack(
            &recursive_sum_rast(),
            vec![
                InputOutputTestCase::new(vec![u32_lit(0)], vec![u64_lit(0)]),
                InputOutputTestCase::new(vec![u32_lit(1)], vec![u64_lit(1)]),
                InputOutputTestCase::new(vec![u32_lit(2)], vec![u64_lit(4)]),
                InputOutputTestCase::new(vec![u32_lit(3)], vec![u64_lit(10)]),
                InputOutputTestCase::new(vec![u32_lit(4)], vec![u64_lit(20)]),
                InputOutputTestCase::new(vec![u32_lit(5)], vec![u64_lit(recursive_sum(5))]),
                InputOutputTestCase::new(vec![u32_lit(6)], vec![u64_lit(recursive_sum(6))]),
                InputOutputTestCase::new(vec![u32_lit(7)], vec![u64_lit(recursive_sum(7))]),
                InputOutputTestCase::new(vec![u32_lit(10)], vec![u64_lit(recursive_sum(10))]),
                InputOutputTestCase::new(vec![u32_lit(20)], vec![u64_lit(recursive_sum(20))]),
            ],
        )
    }

    #[test]
    fn add_with_inner_mul_run_test() {
        multiple_compare_prop_with_stack(
            &add_with_inner_mul_rast(),
            vec![
                InputOutputTestCase::new(vec![u32_lit(100), u32_lit(33)], vec![u32_lit(133 * 4)]),
                InputOutputTestCase::new(vec![u32_lit(0), u32_lit(0)], vec![u32_lit(0)]),
                InputOutputTestCase::new(vec![u32_lit(1), u32_lit(0)], vec![u32_lit(4)]),
                InputOutputTestCase::new(vec![u32_lit(0), u32_lit(1)], vec![u32_lit(4)]),
                InputOutputTestCase::new(vec![u32_lit(2), u32_lit(2)], vec![u32_lit(16)]),
                InputOutputTestCase::new(
                    vec![u32_lit(100_000), u32_lit(100_000_000)],
                    vec![u32_lit(400_400_000)],
                ),
            ],
        )
    }

    #[test]
    fn factorial_with_inner_function_test() {
        multiple_compare_prop_with_stack(
            &factorial_rast(),
            vec![
                InputOutputTestCase::new(vec![u32_lit(0)], vec![u32_lit(1)]),
                InputOutputTestCase::new(vec![u32_lit(1)], vec![u32_lit(1)]),
                InputOutputTestCase::new(vec![u32_lit(2)], vec![u32_lit(2)]),
                InputOutputTestCase::new(vec![u32_lit(3)], vec![u32_lit(6)]),
                InputOutputTestCase::new(vec![u32_lit(4)], vec![u32_lit(24)]),
                InputOutputTestCase::new(vec![u32_lit(5)], vec![u32_lit(120)]),
            ],
        )
    }
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use super::*;
    use crate::tests_and_benchmarks::{
        ozk_programs,
        test_helpers::{io_native, ozk_parsing, shared_test::*},
    };

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

    #[test]
    fn add_with_inner_mul_test() {
        graft_check_compile_prop(&add_with_inner_mul_rast());
    }
}
