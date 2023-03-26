use syn::parse_quote;

use crate::graft::item_fn;

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
fn spill_many_types() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn all_types() -> (bool, u32, u64, BFieldElement, XFieldElement, Digest, u32) {
            let s0: bool = tasm::tasm_io_read_stdin_bool();
            let s1: u32 = tasm::tasm_io_read_stdin_u32() * 2;
            let s2: u64 = tasm::tasm_io_read_stdin_u64() * 2;
            let s3: BFieldElement = tasm::tasm_io_read_stdin_bfe();
            let s4: XFieldElement = tasm::tasm_io_read_stdin_xfe();
            let s5: Digest = tasm::tasm_io_read_stdin_digest();
            let s6: u32 = s1 * s1;
            let s7: u64 = s2 * s2;
            let s8: Digest = tasm::tasm_io_read_stdin_digest();
            let s9: Digest = tasm::tasm_io_read_stdin_digest();
            let s10: Digest = tasm::tasm_io_read_stdin_digest();
            let s11: Digest = tasm::tasm_io_read_stdin_digest();
            let s12: Digest = tasm::tasm_io_read_stdin_digest();
            let s13: Digest = tasm::tasm_io_read_stdin_digest();

            return (s0, s1, s2, s3, s4, s8, s6);
        }
    })
}

#[allow(dead_code)]
pub fn spill_to_memory_overwrite_values() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn overwrite_values() -> (u32, u32) {
            let mut a: u32 = 100;
            let s10: Digest = tasm::tasm_io_read_stdin_digest();
            let s11: Digest = tasm::tasm_io_read_stdin_digest();
            let s12: Digest = tasm::tasm_io_read_stdin_digest();
            let s13: Digest = tasm::tasm_io_read_stdin_digest();
            a = a + 1;

            return (a, a + 1u32);
        }
    })
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
fn big_branches_spill_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn big_branches_spill(a: u64, b: u64, c: u64, d: u64, e: u64, f: u64) -> (u64, u64) {
            let mut g: u64 = a + b;
            if a > b {
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
                        g = g * 2u64;
                    } else {
                        g = 0;
                    }
                } else {
                    g = 0;
                }
            } else {
                g = 0;
            }



            return (a, g);
        }
    })
}

#[cfg(test)]
mod run_tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use triton_vm::bfield_codec::BFieldCodec;
    use twenty_first::shared_math::{
        b_field_element::BFieldElement, other::random_elements, tip5::Digest,
        x_field_element::XFieldElement,
    };

    use super::*;
    use crate::tests::shared_test::*;

    #[test]
    fn spill_u64_values_to_memory_test() {
        compare_prop_with_stack(
            &spill_u64_values_to_memory_rast(),
            vec![],
            vec![u64_lit(100), u64_lit(200), u64_lit(400)],
        );
    }

    #[test]
    fn spill_u32_values_to_memory_test() {
        compare_prop_with_stack(
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
        compare_prop_with_stack(
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
        compare_prop_with_stack(
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
        compare_prop_with_stack_and_memory_and_ins(
            &spill_to_memory_overwrite_values(),
            vec![],
            vec![u32_lit(101), u32_lit(102)],
            HashMap::default(),
            None,
            vec![reversed_digest].concat(),
            vec![],
        );
    }

    #[test]
    fn spill_to_memory_in_branch_test_1() {
        compare_prop_with_stack(
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
        compare_prop_with_stack(
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
        compare_prop_with_stack_and_memory_and_ins(
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
            HashMap::default(),
            None,
            vec![
                vec![BFieldElement::new(my_bool as u64)],
                vec![my_u32.into()],
                split(my_u64),
                vec![my_bfe],
                reversed_xfe,
                reversed_digest,
            ]
            .concat(),
            vec![],
        )
    }

    #[test]
    fn spill_many_bindings_to_memory_test() {
        compare_prop_with_stack(
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
        compare_prop_with_stack(
            &big_branches_spill_rast(),
            vec![
                u64_lit(6),
                u64_lit(5),
                u64_lit(4),
                u64_lit(3),
                u64_lit(2),
                u64_lit(1),
            ],
            vec![u64_lit(6), u64_lit(0)],
        );
    }
}
