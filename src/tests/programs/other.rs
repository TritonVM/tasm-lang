use syn::parse_quote;

use crate::graft::item_fn;

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
                let mut arr: Vec<u64> = Vec::<u64>::with_capacity(16u32);
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
            let mut a: Vec<u64> = Vec::<u64>::with_capacity(17);
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
pub fn return_tuple_element_0() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_tuple_element() -> bool {
            let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
            return tuple.0;
        }
    })
}

#[allow(dead_code)]
pub fn return_tuple_element_1() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_tuple_element() -> u32 {
            let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
            return tuple.1;
        }
    })
}

#[allow(dead_code)]
pub fn return_tuple_element_2() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_tuple_element() -> u64 {
            let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
            return tuple.2;
        }
    })
}

#[allow(dead_code)]
pub fn return_tuple_element_3() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_tuple_element() -> u64 {
            let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
            return tuple.3;
        }
    })
}

#[allow(dead_code)]
pub fn return_tuple_element_4() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn return_tuple_element() -> u32 {
            let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
            return tuple.4;
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

#[allow(dead_code)]
pub fn allow_mutable_tuple_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn allow_mutable_tuple() -> (u64, u64) {
            let mut tuple: (u64, u64) = (1u64, 2u64);
            tuple.0 = 3u64;
            let b: u32 = 100;
            let c: u32 = 101;
            tuple.1 = 4u64;
            let d: (u64, u32) = (1000u64, 2000u32);

            return tuple;
        }
    })
}

#[allow(dead_code)]
pub fn allow_mutable_triplet_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn allow_mutable_triplet() -> (u64, u64, u32) {
            let mut tuple: (u64, u64, u32) = (1u64, 2u64, 3u32);
            tuple.0 = 4u64;
            tuple.1 = 5u64;
            tuple.2 = 6u32;

            return tuple;
        }
    })
}

#[allow(dead_code)]
pub fn overwrite_values_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn overwrite_values() -> (u32, u32) {
            let mut a: u32 = 100;
            a = 200;
            a = 300;
            a = a + 300;
            a = a + a;

            return (a, a + 100u32);
        }
    })
}

#[allow(dead_code)]
pub fn allow_mutable_tuple_complicated_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn allow_mutable_tuple_complicated() -> (u64, u64) {
            let a: u64 = 1u64;
            let mut tuple: (u64, u64) = (1u64, 2u64);
            tuple.0 = 3u64;
            let b: u32 = 100;
            let c: u32 = 101;
            tuple.1 = 4u64;
            let d: (u64, u32) = (1000u64, 2000u32);

            return tuple;
        }
    })
}

#[allow(dead_code)]
pub fn polymorphic_vectors_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn polymorphic_vectors() -> (Vec<u32>, u32, Vec<u64>, u32) {
            let mut a: Vec<u32> = Vec::<u32>::with_capacity(16);
            let mut b: Vec<u64> = Vec::<u64>::with_capacity(16);
            a.push(1000u32);
            a.push(2000u32);
            b.push(3000u64);
            let a_len: u32 = a.len() as u32;
            let b_len: u32 = b.len() as u32;

            return (a, a_len, b, b_len);
        }
    })
}

#[cfg(test)]
mod compile_and_typecheck_tests {
    use super::*;
    use crate::tests::shared_test::graft_check_compile_prop;

    #[test]
    fn inferred_literals_test() {
        graft_check_compile_prop(&inferred_literals());
    }

    #[test]
    fn nop_test() {
        graft_check_compile_prop(&nop_rast());
    }

    #[test]
    fn simple_while_loop_test() {
        graft_check_compile_prop(&simple_while_loop());
    }

    #[test]
    fn complicated_while_loop_test() {
        graft_check_compile_prop(&longer_while_loop());
    }

    #[test]
    fn while_loop_with_declarations_test() {
        graft_check_compile_prop(&while_loop_with_declarations());
    }

    #[test]
    fn code_block_test() {
        graft_check_compile_prop(&code_block());
    }

    #[test]
    fn simple_list_support_test() {
        graft_check_compile_prop(&simple_list_support());
    }

    #[test]
    fn tuple_support_test() {
        graft_check_compile_prop(&tuple_support());
    }

    #[test]
    fn mut_list_argument_test() {
        graft_check_compile_prop(&mut_list_argument());
    }

    #[should_panic]
    #[test]
    fn missing_mut_keyword_test() {
        graft_check_compile_prop(&missing_mut_keyword());
    }

    #[test]
    fn allow_mutable_tuple_test() {
        graft_check_compile_prop(&allow_mutable_tuple_rast());
    }
}

#[cfg(test)]
mod run_tests {
    use std::collections::HashMap;

    use num::{One, Zero};
    use rand::{thread_rng, Rng};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use super::*;
    use crate::{ast, tests::shared_test::*};

    #[test]
    fn simple_while_loop_run_test() {
        compare_prop_with_stack(&simple_while_loop(), vec![], vec![u32_lit(5050)]);
    }

    #[test]
    fn complicated_while_loop_test() {
        compare_prop_with_stack(
            &longer_while_loop(),
            vec![u32_lit(1000)],
            vec![u64_lit(2641)],
        );
        compare_prop_with_stack(
            &while_loop_with_declarations(),
            vec![u32_lit(2000)],
            vec![u64_lit(3641)],
        );
        compare_prop_with_stack(
            &while_loop_with_declarations(),
            vec![u32_lit(2001)],
            vec![u64_lit(3642)],
        );
    }

    #[test]
    fn code_block_run_test() {
        fn prop_code_block(input: u64) {
            let inputs = vec![u64_lit(input)];
            let outputs = vec![u32_lit(2 * (input as u32) + 2)];
            compare_prop_with_stack(&code_block(), inputs, outputs);
        }

        let mut rng = thread_rng();
        for _ in 0..10 {
            prop_code_block(rng.gen_range(0..(1u64 << 30)));
        }
    }

    #[test]
    fn simple_list_support_run_test() {
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_new;
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_pop;
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_push;

        let inputs = vec![];
        let outputs = vec![
            bfe_lit(BFieldElement::one()),
            u32_lit(2),
            u64_lit(4000),
            u64_lit(2000),
        ];

        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

        // Free-pointer
        // `2 * 17 + 2` is used by list. `1` is used by the dynamic allocator.
        let total_memory_used = 2 * 17 + 2 + 1;
        assert!(memory
            .insert(BFieldElement::zero(), BFieldElement::new(total_memory_used))
            .is_none());

        let list_pointer = BFieldElement::one();
        safe_list_new(list_pointer, 17, &mut memory);

        let elem_0 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
        safe_list_push(list_pointer, elem_0.clone(), &mut memory, elem_0.len());

        let elem_1 = vec![BFieldElement::new(5000), BFieldElement::new(0)];
        safe_list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

        let elem_2 = vec![BFieldElement::new(4000), BFieldElement::new(0)];
        safe_list_push(list_pointer, elem_2.clone(), &mut memory, elem_2.len());

        safe_list_pop(list_pointer, &mut memory, ast::DataType::U64.size_of());

        let input_memory = HashMap::default();
        compare_prop_with_stack_and_memory(
            &simple_list_support(),
            inputs,
            outputs,
            input_memory,
            Some(memory),
        );
    }

    #[test]
    fn tuple_support_run_test() {
        let outputs = vec![bool_lit(true), u32_lit(42), u64_lit(100)];

        compare_prop_with_stack(&tuple_support(), vec![], outputs);
    }

    #[test]
    fn return_tuple_element() {
        // let tuple: (bool, u32, u64, u64, u32) = (true, 42u32, 10000000000u64, 60000000000u64, 20000u32);
        compare_prop_with_stack(&return_tuple_element_0(), vec![], vec![bool_lit(true)]);
        compare_prop_with_stack(&return_tuple_element_1(), vec![], vec![u32_lit(42)]);
        compare_prop_with_stack(
            &return_tuple_element_2(),
            vec![],
            vec![u64_lit(10_000_000_000)],
        );
        compare_prop_with_stack(
            &return_tuple_element_3(),
            vec![],
            vec![u64_lit(60_000_000_000)],
        );
        compare_prop_with_stack(&return_tuple_element_4(), vec![], vec![u32_lit(20000)]);
    }

    #[test]
    fn simple_list_support_test() {
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_new;
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_push;

        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::one();
        safe_list_new(list_pointer, 43, &mut memory);

        let elem_1 = vec![BFieldElement::new(2000), BFieldElement::new(0)];
        safe_list_push(list_pointer, elem_1.clone(), &mut memory, elem_1.len());

        let mut expected_final_memory = memory.clone();
        for i in 0..10 {
            let elem_i = vec![BFieldElement::new(i), BFieldElement::new(0)];
            safe_list_push(
                list_pointer,
                elem_i.clone(),
                &mut expected_final_memory,
                elem_i.len(),
            );
        }

        compare_prop_with_stack_and_memory(
            &mut_list_argument(),
            vec![bfe_lit(list_pointer)],
            vec![],
            memory,
            Some(expected_final_memory),
        );
    }

    #[test]
    fn allow_mutable_tuple_test() {
        compare_prop_with_stack(
            &allow_mutable_tuple_rast(),
            vec![],
            vec![u64_lit(3), u64_lit(4)],
        );
    }

    #[test]
    fn allow_mutable_tuple_complicated_test() {
        compare_prop_with_stack(
            &allow_mutable_tuple_complicated_rast(),
            vec![],
            vec![u64_lit(3), u64_lit(4)],
        );
    }

    #[test]
    fn allow_mutable_triplet_test() {
        compare_prop_with_stack(
            &allow_mutable_triplet_rast(),
            vec![],
            vec![u64_lit(4), u64_lit(5), u32_lit(6)],
        );
    }

    #[test]
    fn overwrite_values_test() {
        compare_prop_with_stack(
            &overwrite_values_rast(),
            vec![],
            vec![u32_lit(1200), u32_lit(1300)],
        );
    }

    #[test]
    fn polymorphic_vectors_test() {
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_new;
        use tasm_lib::rust_shadowing_helper_functions::safe_list::safe_list_push;

        let inputs = vec![];
        let outputs = vec![
            bfe_lit(BFieldElement::new(1)),
            bfe_lit(BFieldElement::new(2)),
            bfe_lit(BFieldElement::new(16 + 2 + 1)),
            bfe_lit(BFieldElement::new(1)),
        ];

        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

        // Dynamic allocator
        let space_used = 1 + 16 + 2 + 16 * 2 + 2;
        assert!(memory
            .insert(BFieldElement::zero(), BFieldElement::new(space_used))
            .is_none());

        let list_pointer_a = BFieldElement::one();
        safe_list_new(list_pointer_a, 16, &mut memory);

        let list_pointer_b = BFieldElement::new(16 + 2 + 1);
        safe_list_new(list_pointer_b, 16, &mut memory);

        let elem_1 = vec![BFieldElement::new(1000)];
        safe_list_push(list_pointer_a, elem_1.clone(), &mut memory, elem_1.len());

        let elem_2 = vec![BFieldElement::new(2000)];
        safe_list_push(list_pointer_a, elem_2.clone(), &mut memory, elem_2.len());

        let elem_3 = vec![BFieldElement::new(3000), BFieldElement::new(0)];
        safe_list_push(list_pointer_b, elem_3.clone(), &mut memory, elem_3.len());

        let input_memory = HashMap::default();
        compare_prop_with_stack_and_memory(
            &polymorphic_vectors_rast(),
            inputs,
            outputs,
            input_memory,
            Some(memory),
        );
    }
}
