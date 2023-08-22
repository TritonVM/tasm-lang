use itertools::Itertools;
use triton_vm::BFieldElement;

use crate::tests_and_benchmarks::test_helpers::io_native::{pub_input, pub_output};

pub(crate) fn main() {
    fn local_function(input: BFieldElement) -> BFieldElement {
        return input * BFieldElement::new(2);
    }
    let mut input_values: Vec<BFieldElement> = Vec::<BFieldElement>::with_capacity(200);

    let length_indication: usize = pub_input().value() as usize;
    let mut i: usize = 0;
    while i < length_indication {
        input_values.push(pub_input());
        i += 1;
    }
    let output_values: Vec<BFieldElement> =
        input_values.into_iter().map(local_function).collect_vec();

    let mut j: usize = 0;
    while j < length_indication {
        pub_output(output_values[j]);
        j += 1;
    }

    return;
}
