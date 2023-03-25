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
fn long_expression_that_must_spill_rast() -> syn::ItemFn {
    item_fn(parse_quote! {
        fn long_expression_that_must_spill(input: u64) -> (u64, u32, u64, u64, u64, u64) {
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

#[cfg(test)]
mod run_tests {
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
    fn long_expression_that_must_spill_test() {
        compare_prop_with_stack(
            &long_expression_that_must_spill_rast(),
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
}
