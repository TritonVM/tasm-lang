// Allows the use of input/output on the native architecture
use crate::tests_and_benchmarks::test_helpers::io_native;
use triton_vm::BFieldElement;

pub fn pub_output(x: BFieldElement) {
    return io_native::pub_output(x);
}

pub fn main() {
    let a: BFieldElement = BFieldElement::new(14);
    let b: BFieldElement = BFieldElement::new(15);
    let c: BFieldElement = a + b;
    pub_output(c);
    return;
}
