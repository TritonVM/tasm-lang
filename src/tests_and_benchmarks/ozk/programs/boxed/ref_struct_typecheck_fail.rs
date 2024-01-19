#![allow(clippy::needless_borrow)]
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// Expect a type check error
struct SomeStruct(u64);

impl SomeStruct {
    #[allow(dead_code)]
    fn new(value: u64) -> SomeStruct {
        return SomeStruct(value + 0xabcde123u64);
    }

    // This function expects `Box<SomeStruct>` as receiver but is called with a `SomeStruct` as receiver.
    // So the type check should fail.
    #[allow(dead_code)]
    fn valued(&self) -> u64 {
        return self.0;
    }
}

#[allow(dead_code)]
fn main() {
    let a: SomeStruct = SomeStruct::new(tasm::tasm_io_read_stdin___u64());
    tasm::tasm_io_write_to_stdout___u64(a.valued()); // <-- Type check should fail here
    return;
}

#[cfg(test)]
mod test {
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;

    #[should_panic]
    #[test]
    fn ref_struct_typecheck_fail_test() {
        let entrypoint = EntrypointLocation::disk("boxed", "ref_struct_typecheck_fail", "main");
        let _test_program =
            ozk_parsing::compile_for_test(&entrypoint, crate::ast_types::ListType::Unsafe);
    }
}
