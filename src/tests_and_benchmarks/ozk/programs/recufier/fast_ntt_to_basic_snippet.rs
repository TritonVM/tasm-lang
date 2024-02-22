#![allow(clippy::manual_swap)]

use num::One;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::prelude::ModPowU32;

#[allow(clippy::ptr_arg)]
#[allow(clippy::vec_init_then_push)]
#[allow(dead_code)]
fn xfe_ntt(x: &mut Vec<XFieldElement>, omega: BFieldElement) {
    fn bitreverse(mut n: u32, l: u32) -> u32 {
        let mut r: u32 = 0;
        let mut i: u32 = 0;
        while i < l {
            r = (r << 1) | (n & 1);
            n >>= 1;
            i += 1;
        }

        return r;
    }

    let size: u32 = x.len() as u32;
    let log_2_size: u32 = u32::BITS - size.leading_zeros() - 1;

    {
        let mut k: u32 = 0;
        while k != size {
            let rk: u32 = bitreverse(k, log_2_size);
            if k < rk {
                // TODO: Use `swap` here instead, once it's implemented in `tasm-lib`
                // That will give us a shorter cycle count
                // x.swap(rk as usize, k as usize);
                let rk_val: XFieldElement = x[rk as usize];
                x[rk as usize] = x[k as usize];
                x[k as usize] = rk_val;
            }

            k += 1;
        }
    }

    let mut m: u32 = 1;

    let mut outer_count: u32 = 0;
    while outer_count != log_2_size {
        // for _ in 0..log_2_of_n {
        let w_m: BFieldElement = omega.mod_pow_u32(size / (2 * m));
        let mut k: u32 = 0;
        while k < size {
            let mut w: BFieldElement = BFieldElement::one();
            let mut j: u32 = 0;
            while j != m {
                // for j in 0..m {
                let u: XFieldElement = x[(k + j) as usize];
                let mut v: XFieldElement = x[(k + j + m) as usize];
                v *= w;
                x[(k + j) as usize] = u + v;
                x[(k + j + m) as usize] = u - v;
                w *= w_m;

                j += 1;
            }

            k += 2 * m;
        }

        m *= 2;

        outer_count += 1;
    }

    return;
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use tasm_lib::triton_vm::prelude::*;

    use tasm_lib::twenty_first::prelude::XFieldElement;

    use crate::tests_and_benchmarks::ozk::ozk_parsing::*;
    use crate::tests_and_benchmarks::test_helpers::shared_test::bfe_lit;
    use crate::tests_and_benchmarks::test_helpers::shared_test::compare_compiled_prop_with_stack_and_ins;

    #[test]
    fn fast_xfe_ntt_to_basic_snippet_test() {
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "fast_ntt_to_basic_snippet", "xfe_ntt");
        let compiled = compile_for_test(&entrypoint_location);
        let list_pointer = BFieldElement::new(100);
        let list_length = BFieldElement::new(1);
        let item = XFieldElement::new([50, 0, 0].map(BFieldElement::new));
        let init_memory = [list_length]
            .into_iter()
            .chain(item.coefficients)
            .enumerate()
            .map(|(i, v)| (BFieldElement::from(i as u32) + list_pointer, v))
            .collect();
        let non_determinism = NonDeterminism::default().with_ram(init_memory);

        let root_of_unity = BFieldElement::new(1);
        let input_args = vec![bfe_lit(list_pointer), bfe_lit(root_of_unity)];

        compare_compiled_prop_with_stack_and_ins(
            &compiled,
            input_args,
            vec![],
            None,
            vec![],
            non_determinism,
        );

        // Output what we came for: A `BasicSnippet` implementation constructed by the compiler
        let entrypoint_location =
            EntrypointLocation::disk("recufier", "fast_ntt_to_basic_snippet", "xfe_ntt");
        let rust_ast = entrypoint_location.extract_entrypoint();
        let as_bs = compile_to_basic_snippet(rust_ast, HashMap::default());
        println!("{}", as_bs);
    }
}
