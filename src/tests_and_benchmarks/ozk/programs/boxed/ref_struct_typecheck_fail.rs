#![allow(clippy::needless_borrow)]
use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// Since struct is not copy, you cannot just call `&self` methods
// without explicitly creating a `Box<NonCopyStruct>` value.
struct NonCopyStruct(u64);

impl NonCopyStruct {
    #[allow(dead_code)]
    fn new(value: u64) -> NonCopyStruct {
        return NonCopyStruct(value + 0xabcde123u64);
    }

    #[allow(dead_code)]
    fn valued(&self) -> u64 {
        return self.0;
    }
}

#[allow(dead_code)]
fn main() {
    let a: NonCopyStruct = NonCopyStruct::new(tasm::tasm_io_read_stdin___u64());
    tasm::tasm_io_write_to_stdout___u64((&a).valued());
    return;
}

mod tests {
    use crate::tests_and_benchmarks::ozk::ozk_parsing;

    #[should_panic]
    #[test]
    fn ref_struct_typecheck_fail_test() {
        let _test_program = ozk_parsing::compile_for_test(
            "boxed",
            "ref_struct_typecheck_fail",
            "main",
            crate::ast_types::ListType::Unsafe,
        );
    }
}
