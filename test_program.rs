fn main() {
    fn foo(a: BFieldElement, b: BFieldElement) -> BFieldElement {
        return a + 2 * b;
    }

    let c: BFieldElement = foo(BFieldElement::new(21), BFieldElement::new(10));
    tasm::tasmlib_io_write_to_stdout___bfe(c);

    return;
}
