use crate::tests_and_benchmarks::test_helpers::shared_test::item_fn;
use syn::parse_quote;
use tasm_lib::triton_vm::prelude::*;

fn spill_u64_values_to_memory_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_u64_values_to_memory() -> (u64, u64, u64) {
            let a: u64 = 100;
            let b: u64 = 200;
            let c: u64 = 300;
            let d: u64 = 400;
            let e: u64 = 500;
            let f: u64 = 600;
            let g: u64 = 700;
            let h: u64 = 800;
            let i: u64 = 900;
            let j: u64 = 1000;
            let k: u64 = 1100;

            return (a, b, d);
        }
    })
}

fn spill_u32_values_to_memory_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_u32_values_to_memory() -> (u32, u32, u32, u32, u32) {
            let a: u32 = 100;
            let b: u32 = 200;
            let c: u32 = 300;
            let d: u32 = 400;
            let e: u32 = 500;
            let f: u32 = 600;
            let g: u32 = 700;
            let h: u32 = 800;
            let i: u32 = 900;
            let j: u32 = 1000;
            let k: u32 = 1100;
            let l: u32 = 1200;
            let m: u32 = 1300;
            let n: u32 = 1400;
            let o: u32 = 1500;
            let p: u32 = 1600;
            let q: u32 = 1700;
            let r: u32 = 1800;
            let s: u32 = 1900;
            let t: u32 = 2000;
            let u: u32 = 2100;
            let v: u32 = 2200;
            let w: u32 = 2300;
            let x: u32 = 2400;
            let y: u32 = 2500;
            let z: u32 = 2600;
            let æ: u32 = 2700;
            let ø: u32 = 2800;
            let å: u32 = 2900;

            return (v, e, j, l, e);
        }
    })
}

fn long_expression_that_must_spill_1_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn long_expression_that_must_spill_1(input: u64) -> (u64, u32, u64, u64, u64, u64) {
            let a: u64 = 100;
            let b: u64 = 200;
            let c: u64 = 300;
            let d: u32 = 400;
            let e: u64 = 500;
            let f: u64 = 600;
            let g: u64 = 700;
            let h: u64 = 800;
            let i: u64 = 900;
            let j: u64 = 1000;
            let k: u64 = 1100;
            let l: u64 = 1200;
            let m: u64 = 1300;
            let n: u64 = 1400;

            let res0: u64 = 10 * a + 10 * b + 10 * c + 10 * d as u64 - e - f + 10000 + input;
            let res1: u32 = (
                2u64 * a                // 200
                + 2u64 * b              // 600
                + 2u64 * c              // 1200
                + 2u64 * (d as u64)     // 2000
                + 2u64 * e              // 3000
                + 2u64 * f              // 4200
                + 2u64 * g              // 5600
                + 2u64 * h              // 7200
                + 2u64 * i              // 9000
                + 2u64 * j              // 11000
                + 2u64 * k              // 13200
                + 2u64 * l              // 15600
                + 2u64 * m              // 18200
                + 2u64 * n              // 21000
            ) as u32;
            return (res0, res1, h, i, j, k);
        }
    })
}

fn long_expression_that_must_spill_2_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn long_expression_that_must_spill_2(a: u64, b: u32, c: u64, d: u64) -> u32 {

            let val1: u64 = 100;
            let val2: u64 = 200;
            let val3: u64 = 300;
            let val4: u64 = 400;
            let val5: u64 = 500;
            let val6: u64 = 500;
            let val7: u64 = 500;
            let val8: u64 = 500;
            let val9: u64 = 500;
            let val10: u64 = 500;

            let res: u32 = (1u32 + d as u32) + 2u32 * a as u32 + 3u32 * b + 4u32 * d as u32 + 200u32 + 300u32 + val10 as u32 + val1 as u32;

            return res;
        }
    })
}

fn spill_many_types() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn all_types() -> (bool, u32, u64, BFieldElement, XFieldElement, Digest, u32) {
            let s0: bool = tasm::tasm_io_read_stdin___bool();
            let s1: u32 = tasm::tasm_io_read_stdin___u32() * 2;
            let s2: u64 = tasm::tasm_io_read_stdin___u64() * 2;
            let s3: BFieldElement = tasm::tasm_io_read_stdin___bfe();
            let s4: XFieldElement = tasm::tasm_io_read_stdin___xfe();
            let s5: Digest = tasm::tasm_io_read_stdin___digest();
            let s6: u32 = s1 * s1;
            let s7: u64 = s2 * s2;
            let s8: Digest = tasm::tasm_io_read_stdin___digest();
            let s9: Digest = tasm::tasm_io_read_stdin___digest();
            let s10: Digest = tasm::tasm_io_read_stdin___digest();
            let s11: Digest = tasm::tasm_io_read_stdin___digest();
            let s12: Digest = tasm::tasm_io_read_stdin___digest();
            let s13: Digest = tasm::tasm_io_read_stdin___digest();

            return (s0, s1, s2, s3, s4, s8, s6);
        }
    })
}

fn spill_to_memory_overwrite_values() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn overwrite_values() -> (u32, u32) {
            let mut a: u32 = 100;
            let s10: Digest = tasm::tasm_io_read_stdin___digest();
            let s11: Digest = tasm::tasm_io_read_stdin___digest();
            let s12: Digest = tasm::tasm_io_read_stdin___digest();
            let s13: Digest = tasm::tasm_io_read_stdin___digest();
            a = a + 1;

            return (a, a + 1u32);
        }
    })
}

fn spill_to_memory_in_branch_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_to_memory_in_branch(a: u64, b: u64, c: u64, d: u64, e: u64, f: u64) -> (u64, u64) {
            let mut g: u64 = a + b;
            let h: u64 = c + d;
            let i: u64 = e + f;
            let mut j: u64 = 0;

            if h < i {
                // In this branch `g` is updated, but that update must happen
                // in memory since `g` lives in memory because of the other
                // branch.
                g = 2 * g + a + b;
                // g = 2;
            } else {
                // In this branch `g` is spilled to memory
                let k: u64 = 0;
                let l: u64 = 0;
                let m: u64 = 0;
                let n: u64 = 0;
                let o: u64 = 0;
                let p: u64 = 0;
                let q: u64 = 0;
                let r: u64 = 0;
                let s: u64 = 0;
                let t: u64 = 0;
                let u: u64 = 0;
                j = g;
            }



            return (g, j);
        }
    })
}

fn spill_tuple_to_memory_index_0_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_tuple_to_memory_index_0(a: u64) -> (u64, u32, u64, BFieldElement) {
            let mut res: (u64, u32, u64, BFieldElement) = (100u64, 200u32, 300u64, BFieldElement::new(400u64));
            let val0: (u64, u32, u64, BFieldElement) = (1000u64, 2000u32, 3000u64, BFieldElement::new(4000u64));
            let val1: (u64, u32, u64, BFieldElement) = (10000u64, 20000u32, 30000u64, BFieldElement::new(40000u64));
            let val2: (u64, u32, u64, BFieldElement) = (100000u64, 200000u32, 300000u64, BFieldElement::new(400000u64));
            let val3: (u64, u32, u64, BFieldElement) = (1000000u64, 2000000u32, 3000000u64, BFieldElement::new(4000000u64));
            let val4: u64 = 0;
            let val5: u64 = 0;
            let val6: u64 = 0;
            let val7: u64 = 0;
            let val8: u64 = 0;
            let val9: u64 = 0;
            let val10: u64 = 0;
            let val11: u64 = 0;
            let val12: u64 = 0;
            res.0 = a;

            return res;
        }
    })
}

fn spill_tuple_to_memory_index_1_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_tuple_to_memory_index_1(a: u32) -> (u64, u32, u64, BFieldElement) {
            let mut res: (u64, u32, u64, BFieldElement) = (100u64, 200u32, 300u64, BFieldElement::new(400u64));
            let val0: (u64, u32, u64, BFieldElement) = (1000u64, 2000u32, 3000u64, BFieldElement::new(4000u64));
            let val1: (u64, u32, u64, BFieldElement) = (10000u64, 20000u32, 30000u64, BFieldElement::new(40000u64));
            let val2: (u64, u32, u64, BFieldElement) = (100000u64, 200000u32, 300000u64, BFieldElement::new(400000u64));
            let val3: (u64, u32, u64, BFieldElement) = (1000000u64, 2000000u32, 3000000u64, BFieldElement::new(4000000u64));
            let val4: u64 = 0;
            let val5: u64 = 0;
            let val6: u64 = 0;
            let val7: u64 = 0;
            let val8: u64 = 0;
            let val9: u64 = 0;
            let val10: u64 = 0;
            let val11: u64 = 0;
            let val12: u64 = 0;
            res.1 = a;

            return res;
        }
    })
}

fn spill_tuple_to_memory_index_2_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_tuple_to_memory_index_2(a: u64) -> (u64, u32, u64, BFieldElement) {
            let mut res: (u64, u32, u64, BFieldElement) = (100u64, 200u32, 300u64, BFieldElement::new(400u64));
            let val0: (u64, u32, u64, BFieldElement) = (1000u64, 2000u32, 3000u64, BFieldElement::new(4000u64));
            let val1: (u64, u32, u64, BFieldElement) = (10000u64, 20000u32, 30000u64, BFieldElement::new(40000u64));
            let val2: (u64, u32, u64, BFieldElement) = (100000u64, 200000u32, 300000u64, BFieldElement::new(400000u64));
            let val3: (u64, u32, u64, BFieldElement) = (1000000u64, 2000000u32, 3000000u64, BFieldElement::new(4000000u64));
            let val4: u64 = 0;
            let val5: u64 = 0;
            let val6: u64 = 0;
            let val7: u64 = 0;
            let val8: u64 = 0;
            let val9: u64 = 0;
            let val10: u64 = 0;
            let val11: u64 = 0;
            let val12: u64 = 0;
            res.2 = a;

            return res;
        }
    })
}

fn spill_tuple_to_memory_index_3_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_tuple_to_memory_index_3(a: BFieldElement) -> (u64, u32, u64, BFieldElement) {
            let mut res: (u64, u32, u64, BFieldElement) = (100u64, 200u32, 300u64, BFieldElement::new(400u64));
            let val0: (u64, u32, u64, BFieldElement) = (1000u64, 2000u32, 3000u64, BFieldElement::new(4000u64));
            let val1: (u64, u32, u64, BFieldElement) = (10000u64, 20000u32, 30000u64, BFieldElement::new(40000u64));
            let val2: (u64, u32, u64, BFieldElement) = (100000u64, 200000u32, 300000u64, BFieldElement::new(400000u64));
            let val3: (u64, u32, u64, BFieldElement) = (1000000u64, 2000000u32, 3000000u64, BFieldElement::new(4000000u64));
            let val4: u64 = 0;
            let val5: u64 = 0;
            let val6: u64 = 0;
            let val7: u64 = 0;
            let val8: u64 = 0;
            let val9: u64 = 0;
            let val10: u64 = 0;
            let val11: u64 = 0;
            let val12: u64 = 0;
            res.3 = a;

            return res;
        }
    })
}

fn spill_many_bindings_to_memory_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn spill_many_bindings_to_memory(a: u64, b: u64, c: u64, d: u64, e: u64, f: u64) -> u64 {
            let val0: u64 = 0;
            let val1: u64 = 0;
            let val2: u64 = 0;
            let mut res: u64 = a * 2u64 + (b * 2u64 + (c * 2u64 + (d * 2u64 + (e * 2u64 + f * 2u64)))) + 2u64 * a + 2u64 * b + (true || false) as u64;
            let val3: u64 = 0;
            let val4: u64 = 0;
            let val5: u64 = 0;
            let val6: u64 = 0;
            let val7: u64 = 0;
            let val8: u64 = 0;
            let val9: u64 = 0;
            let val10: u64 = 0;
            res = res + a + b + c + d + e + f + res;
            res = res * 2u64 + a * 2u64 + ((b * 2u64 + c * 2u64) as u32) as u64 + (d * 2u64 + e * 2u64 + f * 2u64) * 2u64 + a + b + c;

            return res;
        }
    })
}

fn big_branches_spill_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn big_branches_spill(a: u64, b: u64, c: u64, d: u64, e: u64, f: u64) -> (u64, u64) {
            let mut g: u64 = a + b;
            if a > b {
                let val10: u64 = 100;
                if b > c {
                    if d == 0u64 {
                        let val0: u64 = 0;
                        let val1: u64 = 0;
                        let val2: u64 = 0;
                        let val3: u64 = 0;
                        let val4: u64 = 0;
                        let val5: u64 = 0;
                        let val6: u64 = 0;
                        let val7: u64 = 0;
                        let val8: u64 = 0;
                        let val9: u64 = 0;
                        g = g * 2u64 + val10;
                    } else {
                        let val0: u64 = 2;
                        let val1: u64 = 0;
                        let val2: u64 = 0;
                        let val3: u64 = 0;
                        let val4: u64 = 0;
                        let val5: u64 = 0;
                        let val6: u64 = 0;
                        let val7: u64 = 0;
                        let val8: u64 = 0;
                        let val9: u64 = 0;
                        g = 1 + val0 + val10;
                    }
                } else {
                    let val0: u64 = 2;
                    let val1: u64 = 0;
                    let val2: u64 = 0;
                    let val3: u64 = 0;
                    let val4: u64 = 0;
                    let val5: u64 = 0;
                    let val6: u64 = 0;
                    let val7: u64 = 0;
                    let val8: u64 = 0;
                    let val9: u64 = 0;
                    g = 2 + val0 + val10;
                }
            } else {
                let val10: u64 = 200u64;
                let val0: u64 = 2;
                let val1: u64 = 0;
                let val2: u64 = 0;
                let val3: u64 = 0;
                let val4: u64 = 0;
                let val5: u64 = 0;
                let val6: u64 = 0;
                let val7: u64 = 0;
                let val8: u64 = 0;
                let val9: u64 = 0;
                g = 3 + val0 + val10 + c;
            }



            return (a, g);
        }
    })
}

fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_2_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn ensure_dyn_malloc_and_static_malloc_do_not_interfere() -> Vec<u32> {
            let val0: (u64, u64, u64, u64, u64) = (10420u64, 10421u64, 10422u64, 10423u64, 10424u64);
            let val1: (u64, u64, u64, u64, u64) = (20420u64, 20421u64, 20422u64, 20423u64, 20424u64);
            let mut b: Vec<u32> = Vec::<u32>::default();
            let mut a: Vec<Digest> = Vec::<Digest>::default();

            let mut i: usize = 0;
            while i < 16usize {
                b.push(i as u32 + 200u32);
                i = i + 1;
            }

            return b;
        }
    })
}

fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_3_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn ensure_dyn_malloc_and_static_malloc_do_not_interfere() -> Vec<u32> {
            let mut b: Vec<u32> = Vec::<u32>::default();
            let mut a: Vec<Digest> = Vec::<Digest>::default();

            let mut i: usize = 0;
            while i < 16usize {
                b.push(i as u32 + 200u32);
                i = i + 1;
            }

            return b;
        }
    })
}

fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_4_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn ensure_dyn_malloc_and_static_malloc_do_not_interfere() -> (Vec<u32>, u64) {
            // Write code that spills into memory which constitutes static memory allocation.
            // Then create a vector that also lives in memory and verify that updates to its
            // elements does not interfere with statically allocated memory.
            let val0: (u64, u64, u64, u64, u64) = (10420u64, 10421u64, 10422u64, 10423u64, 10424u64);
            let val1: (u64, u64, u64, u64, u64) = (20420u64, 20421u64, 20422u64, 20423u64, 20424u64);
            let mut b: Vec<u32> = Vec::<u32>::default();
            let mut a: Vec<Digest> = Vec::<Digest>::default();

            let mut i: usize = 0;
            while i < 16usize {
                b.push(i as u32 + 200u32);
                i = i + 1;
            }

            // This ensures that val0 is spilled to memory
            let val2 : (u64, u64, u64, u64, u64) = val0;
            return (b, val0.1);
        }
    })
}

fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_5_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
            fn ensure_dyn_malloc_and_static_malloc_do_not_interfere() -> (Vec<u32>, u64, u64, u64, u64, u64, Vec<Digest>) {
            // Write code that spills into memory which constitutes static memory allocation.
            // Then create a vector that also lives in memory and verify that updates to its
            // elements does not interfere with statically allocated memory.
            let val0: (u64, u64, u64, u64, u64) = (10420u64, 10421u64, 10422u64, 10423u64, 10424u64);
            let val1: (u64, u64, u64, u64, u64) = (20420u64, 20421u64, 20422u64, 20423u64, 20424u64);
            let mut b: Vec<u32> = Vec::<u32>::default();
            let mut a: Vec<Digest> = Vec::<Digest>::default();

            let mut i: usize = 0;
            while i < 16usize {
                b.push(i as u32 + 200u32);
                i = i + 1;
            }

            // This ensures that val0 is spilled to memory
            let val2 : (u64, u64, u64, u64, u64) = val0;
            return (b, val0.1, val2.4, val1.0, val1.2, val1.3, a);
        }
    })
}

#[cfg(test)]
mod run_tests {
    use itertools::Itertools;
    use tasm_lib::twenty_first::shared_math::other::random_elements;

    use crate::ast_types::DataType;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    use super::*;

    #[test]
    fn spill_u64_values_to_memory_test() {
        compare_prop_with_stack_safe_lists(
            &spill_u64_values_to_memory_rast(),
            vec![],
            vec![u64_lit(100), u64_lit(200), u64_lit(400)],
        );
    }

    #[test]
    fn spill_u32_values_to_memory_test() {
        compare_prop_with_stack_safe_lists(
            &spill_u32_values_to_memory_rast(),
            vec![],
            vec![
                u32_lit(2200),
                u32_lit(500),
                u32_lit(1000),
                u32_lit(1200),
                u32_lit(500),
            ],
        );
    }

    #[test]
    fn long_expression_that_must_spill_1_test() {
        compare_prop_with_stack_safe_lists(
            &long_expression_that_must_spill_1_rast(),
            vec![u64_lit(107)],
            vec![
                u64_lit(19007),
                u32_lit(21000),
                u64_lit(800),
                u64_lit(900),
                u64_lit(1000),
                u64_lit(1100),
            ],
        );
    }

    #[test]
    fn long_expression_that_must_spill_2_test() {
        compare_prop_with_stack_safe_lists(
            &long_expression_that_must_spill_2_rast(),
            vec![u64_lit(1), u32_lit(2), u64_lit(3), u64_lit(4u64)],
            vec![u32_lit(1129)],
        );
    }

    // spill_to_memory_overwrite_values
    #[test]
    fn spill_to_memory_overwrite_values_test() {
        let digests: Vec<Digest> = random_elements(4);
        let reversed_digest = digests
            .iter()
            .map(|digest| {
                let mut digest = digest.encode();
                digest.reverse();
                digest
            })
            .concat();
        compare_prop_with_stack_and_ins_safe_lists(
            &spill_to_memory_overwrite_values(),
            vec![],
            vec![u32_lit(101), u32_lit(102)],
            None,
            [reversed_digest].concat(),
            NonDeterminism::new(vec![]),
        );
    }

    #[test]
    fn spill_to_memory_in_branch_test_1() {
        compare_prop_with_stack_safe_lists(
            &spill_to_memory_in_branch_rast(),
            vec![
                u64_lit(1),
                u64_lit(2),
                u64_lit(3),
                u64_lit(4),
                u64_lit(5),
                u64_lit(6),
            ],
            vec![u64_lit(9), u64_lit(0)],
        );
    }

    #[test]
    fn spill_to_memory_in_branch_test_2() {
        compare_prop_with_stack_safe_lists(
            &spill_to_memory_in_branch_rast(),
            vec![
                u64_lit(6),
                u64_lit(5),
                u64_lit(4),
                u64_lit(3),
                u64_lit(2),
                u64_lit(1),
            ],
            vec![u64_lit(11), u64_lit(11)],
        );
    }

    #[test]
    fn spill_tuple_to_memory_simple_test() {
        let one_billion: u32 = 1_000_000_000;
        let ten_billion: u64 = 10_000_000_000;
        let hun_billion: u64 = 100_000_000_000;
        let mil_billion: u64 = 1_000_000_000_000;
        compare_prop_with_stack_safe_lists(
            &spill_tuple_to_memory_index_0_rast(),
            vec![u64_lit(ten_billion)],
            vec![
                u64_lit(ten_billion),
                u32_lit(200),
                u64_lit(300),
                bfe_lit(400u64.into()),
            ],
        );

        compare_prop_with_stack_safe_lists(
            &spill_tuple_to_memory_index_1_rast(),
            vec![u32_lit(one_billion)],
            vec![
                u64_lit(100),
                u32_lit(one_billion),
                u64_lit(300),
                bfe_lit(400u64.into()),
            ],
        );
        compare_prop_with_stack_safe_lists(
            &spill_tuple_to_memory_index_2_rast(),
            vec![u64_lit(hun_billion)],
            vec![
                u64_lit(100),
                u32_lit(200),
                u64_lit(hun_billion),
                bfe_lit(400u64.into()),
            ],
        );
        compare_prop_with_stack_safe_lists(
            &spill_tuple_to_memory_index_3_rast(),
            vec![bfe_lit(mil_billion.into())],
            vec![
                u64_lit(100),
                u32_lit(200),
                u64_lit(300),
                bfe_lit(mil_billion.into()),
            ],
        );
    }

    #[test]
    fn spill_many_types_test() {
        let my_bool = true;
        let my_u32 = 125;
        let my_u64 = 1255;
        let my_bfe = BFieldElement::new(420);
        let my_xfe = XFieldElement::new([
            BFieldElement::new(1045),
            BFieldElement::new(1047),
            BFieldElement::new(1049),
        ]);
        let digests: Vec<Digest> = random_elements(7);
        let mut reversed_xfe = my_xfe.encode();
        reversed_xfe.reverse();
        let reversed_digest = digests
            .iter()
            .map(|digest| {
                let mut digest = digest.encode();
                digest.reverse();
                digest
            })
            .concat();
        compare_prop_with_stack_and_ins_safe_lists(
            &spill_many_types(),
            vec![],
            vec![
                bool_lit(my_bool),
                u32_lit(my_u32 * 2),
                u64_lit(my_u64 * 2),
                bfe_lit(my_bfe),
                xfe_lit(my_xfe),
                digest_lit(digests[1]),
                u32_lit(my_u32 * my_u32 * 4),
            ],
            None,
            [
                vec![BFieldElement::new(my_bool as u64)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                reversed_xfe,
                reversed_digest,
            ]
            .concat(),
            NonDeterminism::new(vec![]),
        )
    }

    #[test]
    fn spill_many_bindings_to_memory_test() {
        compare_prop_with_stack_safe_lists(
            &spill_many_bindings_to_memory_rast(),
            vec![
                u64_lit(6),
                u64_lit(5),
                u64_lit(4),
                u64_lit(3),
                u64_lit(2),
                u64_lit(1),
            ],
            vec![u64_lit(371)],
        );
    }

    #[test]
    fn big_branches_spill_test() {
        compare_prop_with_stack_safe_lists(
            &big_branches_spill_rast(),
            vec![
                u64_lit(6),
                u64_lit(5),
                u64_lit(4),
                u64_lit(0),
                u64_lit(2),
                u64_lit(1),
            ],
            vec![u64_lit(6), u64_lit(122)],
        );
        compare_prop_with_stack_safe_lists(
            &big_branches_spill_rast(),
            vec![
                u64_lit(6),
                u64_lit(5),
                u64_lit(4),
                u64_lit(3),
                u64_lit(2),
                u64_lit(1),
            ],
            vec![u64_lit(6), u64_lit(103)],
        );
        compare_prop_with_stack_safe_lists(
            &big_branches_spill_rast(),
            vec![
                u64_lit(6),
                u64_lit(6),
                u64_lit(6),
                u64_lit(6),
                u64_lit(6),
                u64_lit(6),
            ],
            vec![u64_lit(6), u64_lit(211)],
        );
        compare_prop_with_stack_safe_lists(
            &big_branches_spill_rast(),
            vec![
                u64_lit(6),
                u64_lit(5),
                u64_lit(5),
                u64_lit(3),
                u64_lit(2),
                u64_lit(1),
            ],
            vec![u64_lit(6), u64_lit(104)],
        );
    }

    #[test]
    fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_test_1() {
        let function = item_fn(parse_quote! {
            fn ensure_dyn_malloc_and_static_malloc_do_not_interfere() -> Vec<u32> {
                // Write code that spills into memory which constitutes static memory allocation.
                // Then create a vector that also lives in memory and verify that updates to its
                // elements does not interfere with statically allocated memory.
                let val0: (u64, u64, u64, u64, u64) = (10420u64, 10421u64, 10422u64, 10423u64, 10424u64);
                let val1: (u64, u64, u64, u64, u64) = (20420u64, 20421u64, 20422u64, 20423u64, 20424u64);
                let mut b: Vec<u32> = Vec::<u32>::default();
                let mut a: Vec<Digest> = Vec::<Digest>::default();

                let mut i: usize = 0;
                while i < 16usize {
                    b.push(i as u32 + 200u32);
                    i = i + 1;
                }

                // This ensures that val0 is spilled to memory
                let val2 : (u64, u64, u64, u64, u64) = val0;
                return b;
            }
        });

        let exec_result = execute_with_stack_safe_lists(
            &function,
            vec![],
            DataType::List(Box::new(DataType::U64)).stack_size() as isize,
        )
        .unwrap();

        let final_vm_memory = exec_result.final_ram;
        let list_pointer = exec_result.final_stack.last().unwrap();
        println!("list_pointer: {list_pointer}");
        println!(
            "memory value at list_pointer: {}",
            final_vm_memory[list_pointer]
        );
        let expected_list = (0..16).map(|i| u32_lit(200 + i)).collect_vec();
        assert_list_equal(expected_list, *list_pointer, &final_vm_memory)
    }

    #[test]
    fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_test_2() {
        let exec_result = execute_with_stack_safe_lists(
            &ensure_dyn_malloc_and_static_malloc_do_not_interfere_2_rast(),
            vec![],
            DataType::List(Box::new(DataType::U64)).stack_size() as isize,
        )
        .unwrap();

        let list_pointer = exec_result.final_stack.last().unwrap();
        let expected_list = (0..16).map(|i| u32_lit(200 + i)).collect_vec();
        assert_list_equal(expected_list, *list_pointer, &exec_result.final_ram)
    }

    #[test]
    fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_test_3() {
        let exec_result = execute_with_stack_safe_lists(
            &ensure_dyn_malloc_and_static_malloc_do_not_interfere_3_rast(),
            vec![],
            DataType::List(Box::new(DataType::U64)).stack_size() as isize,
        )
        .unwrap();

        let list_pointer = exec_result.final_stack.last().unwrap();
        let expected_list = (0..16).map(|i| u32_lit(200 + i)).collect_vec();
        assert_list_equal(expected_list, *list_pointer, &exec_result.final_ram)
    }

    #[test]
    fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_test_4() {
        // First run the program to get the list's pointer that is stored as the third-to-last
        // output.
        let exec_result = execute_with_stack_safe_lists(
            &ensure_dyn_malloc_and_static_malloc_do_not_interfere_4_rast(),
            vec![],
            DataType::List(Box::new(DataType::U64)).stack_size() as isize + 2,
        )
        .unwrap();

        let list_pointer = exec_result.final_stack[exec_result.final_stack.len() - 3];

        // Check against expected list
        let expected_list = (0..16).map(|i| u32_lit(200 + i)).collect_vec();
        assert_list_equal(expected_list, list_pointer, &exec_result.final_ram);

        // Check the other return value
        compare_prop_with_stack_safe_lists(
            &ensure_dyn_malloc_and_static_malloc_do_not_interfere_4_rast(),
            vec![],
            vec![bfe_lit(list_pointer), u64_lit(10421)],
        );
    }

    #[test]
    fn ensure_dyn_malloc_and_static_malloc_do_not_interfere_test_5() {
        // First run the program to get the list's pointer that is stored as the 11th-to-last
        // output.
        let exec_result = execute_with_stack_safe_lists(
            &ensure_dyn_malloc_and_static_malloc_do_not_interfere_5_rast(),
            vec![],
            12,
        )
        .unwrap();

        let list_pointer_b = exec_result.final_stack[exec_result.final_stack.len() - 12];
        let list_pointer_a = exec_result.final_stack[exec_result.final_stack.len() - 1];

        // Check against expected list
        let expected_list_b = (0..16).map(|i| u32_lit(200 + i)).collect_vec();
        assert_list_equal(expected_list_b, list_pointer_b, &exec_result.final_ram);

        let expected_list_a = vec![];
        assert_list_equal(expected_list_a, list_pointer_a, &exec_result.final_ram);

        // Check the other return value
        compare_prop_with_stack_safe_lists(
            &ensure_dyn_malloc_and_static_malloc_do_not_interfere_5_rast(),
            vec![],
            vec![
                bfe_lit(list_pointer_b),
                u64_lit(10421),
                u64_lit(10424),
                u64_lit(20420),
                u64_lit(20422),
                u64_lit(20423),
                bfe_lit(list_pointer_a),
            ],
        );
    }
}
