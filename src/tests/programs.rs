use crate::graft::item_fn;
use syn::parse_quote;

#[allow(dead_code)]
pub fn inferred_literals() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn main() {
            // infer from let context
            let a: u32 = 0;
            let b: u64 = 1;

            // infer as lhs and rhs
            // block scope ensures stack isn't exhausted
            {
                let add_1: u32 = a + 1;
                let add_2: u32 = 2 + a;
                let add_3: u64 = b + 3;
                let add_4: u64 = 4 + b;
            }

            {
                let sub_1: u32 = a - 5;
                let sub_2: u32 = 6 - a;
                let sub_3: u64 = b - 7;
                let sub_4: u64 = 8 - b;
            }

            {
                let bit_and_1: u32 = a & 9;
                let bit_and_2: u32 = 10 & a;
                let bit_and_3: u64 = b & 11;
                let bit_and_4: u64 = 12 & b;
            }

            {
                let bit_xor_1: u32 = a ^ 13;
                let bit_xor_2: u32 = 14 ^ a;
                let bit_xor_3: u64 = b ^ 15;
                let bit_xor_4: u64 = 16 ^ b;
            }

            // div only supports the denominator 2
            {
                let div_1: u32 = a / 2;
                // let div_2: u32 = 18 / a;
                let div_3: u64 = b / 2;
                // let div_4: u64 = 20 / b;
            }

            // mul is only implemented for u32, not u64
            {
                let mul_1: u32 = a * 21;
                let mul_2: u32 = 22 * a;
                // let mul_3: u64 = b * 23;
                // let mul_4: u64 = 24 * b;
            }

            // rem is not implemented yet
            // {
            //     let rem_1: u32 = a % 25;
            //     let rem_2: u32 = 26 % a;
            //     let rem_3: u64 = b % 27;
            //     let rem_4: u64 = 28 % b;
            // }

            {
                // let bit_shl_1: u32 = 1 << 3; // result must be a u64
                // let bit_shl_2: u32 = 1 << a; // result must be a u64
                // let bit_shl_3: u32 = 2 << 1; // lhs must currently be 1
                // let bit_shl_4: u32 = a << 1; // lhs must currently be 1
                // let bit_shl_5: u64 = b << 5; // lhs must currently be 1
                let bit_shl_6: u64 = 1 << 5;
                let bit_shl_7: u64 = 1 << a;
                // let bit_shl_7: u64 = 6 << 7; // lhs must currently be 1
                // let bit_shl_8: u64 = 8 << a; // u64 shifting by u32 only, rhs must currently be constant
            }

            // shr is not implemented yet
            // {
            //     let bit_shr_6: u64 = 1 >> 5;
            //     let bit_shr_7: u64 = 1 >> a;
            // }

            {
                let three: u64 = tasm::tasm_arithmetic_u64_add(1, 2);
            }

            {
                let mut arr: Vec<u64> = Vec::<u64>::default();
                arr[0] = b;
                arr[a] = b + 1;
                arr[2 * a + 3] = 1 << (4 / a + 5);

                arr.push(4);
            }

            return;
        }
    })
}

#[allow(dead_code)]
pub fn nop_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn nop_nop() {
            return;
        }
    })
}

#[allow(dead_code)]
pub fn add_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_u32_overwrite_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_u32_overwrite(lhs: u32, rhs: u32) -> u32 {
            let mut c: u32 = lhs + rhs;
            c = c + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u32_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u32_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u32(rhs: u32, lhs: u32) -> u32 {
            let c: u32 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u64_rast_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn sub_u64_rast_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn sub_u64_test(rhs: u64, lhs: u64) -> u64 {
            let c: u64 = lhs - rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_bfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_bfe(lhs: BFieldElement, rhs: BFieldElement) -> BFieldElement {
            let c: BFieldElement = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_xfe_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn add_xfe(lhs: XFieldElement, rhs: XFieldElement) -> XFieldElement {
            let c: XFieldElement = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn add_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        // using `add_u64` as function name here would create a name-clash
        // between a dependency and the function we are compiling.
        fn add_u64_test(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs + rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn and_bool_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn boolean_and_bool(lhs: bool, rhs: bool) -> bool {
            let c: bool = lhs && rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn bitwise_and_u32_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u32(lhs: u32, rhs: u32) -> u32 {
            let c: u32 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn bitwise_and_u64_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn bitwise_and_u64(lhs: u64, rhs: u64) -> u64 {
            let c: u64 = lhs & rhs;
            return c;
        }
    })
}

#[allow(dead_code)]
pub fn right_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_child(node_index: u64) -> u64 {
            return node_index - 1u64;
        }
    })
}

#[allow(dead_code)]
pub fn left_child_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn left_child(node_index: u64, height: u32) -> u64 {
            // return node_index - pow2(height);
            // return node_index - 2u64.pow(height);
            return node_index - (1u64 << height);
            // return node_index - 2u32.pow(height);

        }
    })
}

#[allow(dead_code)]
pub fn right_lineage_length_stmt_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        pub fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index) + 1u32;
            let npo2: u64 = 1u64 << bit_width;
            let dist: u64 = npo2 - node_index;

            let mut ret: u32 = 0u32;
            if (bit_width as u64) < dist {
                ret = right_lineage_length(node_index - (npo2 / 2u64) + 1u64);
            } else {
                ret = (dist - 1u64) as u32;
            }

            return ret;
        }
    })
}

#[allow(dead_code)]
pub fn right_lineage_length_expr_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn right_lineage_length(node_index: u64) -> u32 {
            let bit_width: u32 = tasm::tasm_arithmetic_u64_log_2_floor(node_index) + 1u32;
            let npo2: u64 = 1u64 << bit_width;
            let dist: u64 = npo2 - node_index;

            // let bit_width_u64: u64 = bit_width.into();
            let bit_width_u64: u64 = bit_width as u64;
            let ret: u32 = if bit_width_u64 < dist {
                right_lineage_length(node_index - (npo2 / 2u64) + 1u64)
            } else {
                (dist - 1u64) as u32
            };

            return ret;
        }
    })
}

#[allow(dead_code)]
pub fn simple_sub() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_sub(a: u32, b: u32) -> u32 {
            return a - b;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_div_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u32 {
            return 100u32 - 14u32 / 2u32 + 1u32;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_div_u64() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_div() -> u64 {
            return 100u64 - 14u64 / 2u64 + 1u64;
        }
    })
}

#[allow(dead_code)]
pub fn operator_evaluation_ordering_with_mul() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn complicated_expression_with_mul() -> u32 {
            return 380u32 - 14u32 * 2u32 * 10u32 + 1u32 - 41u32 * 1u32;
        }
    })
}

#[allow(dead_code)]
pub fn lt_u32() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn lt_for_u32_test_function() -> bool {
            let a: u32 = 14u32;
            let b: u32 = 20u32;
            let c: u32 = 10u32;
            let bool_0: bool = a < b;
            let bool_1: bool = b < c;
            let bool_2: bool = c < a;

            return bool_1 || bool_2;
        }
    })
}

#[allow(dead_code)]
pub fn simple_while_loop() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn simple_while_loop() -> u32 {
            let mut acc: u32 = 0u32;
            let mut i: u32 = 0u32;
            while i < 101u32 {
                acc = acc + i;
                i = i + 1u32;
            }

            return acc;
        }
    })
}

#[allow(dead_code)]
pub fn longer_while_loop() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn longer_while_loop(a: u32) -> u64 {
            // Should return `1641 + a`
            let mut ret: u64 = 600u64;
            let var0: u64 = 77u64;
            let var1: u64 = 10u64;
            let mut j: u64 = 23u64;
            let mut i: u32 = 0u32;
            while i < var1 as u32 {
                i = i + 1u32;
                ret = ret + var0;
                ret = ret + j;
                j = j - 1u64;
            }

            ret = 9u64 + ret + a as u64;

            return ret + var0;
        }
    })
}

#[allow(dead_code)]
pub fn while_loop_with_declarations() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn while_loop_with_declarations(a: u32) -> u64 {
            // Should return `1641 + a`
            let mut ret: u64 = 600u64;
            let var0: u64 = 77u64;
            let var1: u64 = 10u64;
            let mut j: u64 = 23u64;
            let mut i: u32 = 0u32;
            while i < var1 as u32 {
                let g: u32 = 10000u32;
                i = i + 1u32;
                ret = ret + var0;
                ret = ret + j;
                j = j - 1u64;
            }

            ret = 9u64 + ret + a as u64;

            return ret + var0;
        }
    })
}

#[allow(dead_code)]
pub fn code_block() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn code_block(a: u64) -> u32 {
            let b: u64 = a + 2u64;
            {
                let c: u32 = 0u32;
                let d: u64 = 1u64;
                let e: u64 = a + b + d;
            }

            return a as u32 + b as u32;
        }
    })
}

#[allow(dead_code)]
pub fn simple_list_support() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn make_short_list() -> (Vec<u64>, u32, u64, u64) {
            let mut a: Vec<u64> = Vec::<u64>::default();
            a.push(2000u64);
            a.push(3000u64);
            a.push(4000u64);
            let b: u64 = a.pop().unwrap();
            let len: u32 = a.len() as u32;

            a[1] = 5000u64;

            let d: u64 = a[0];

            return (a, len, b, d);
        }
    })
}

#[allow(dead_code)]
pub fn tuple_support() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_many() -> (bool, u32, u64) {
            let a: bool = true;
            let b: u32 = 42u32;
            let c: u64 = 100u64;

            return (a, b, c);
        }
    })
}

#[allow(dead_code)]
pub fn mut_list_argument() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn foo(values: &mut Vec<u64>) {
            let mut i: u64 = 0u64;
            while i < 10u64 {
                values.push(i);
                i += 1u64;
            }

            return;
        }
    })
}

#[allow(dead_code)]
pub fn missing_mut_keyword() -> syn::ItemFn {
    item_fn(parse_quote!(
        fn missing_mut() {
            let a = 5000u64;
            a = a + 1;
        }
    ))
}
