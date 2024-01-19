use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

enum EnumSimple {
    A,
    B(u128),
}

/// Verify that boxing an enum works
fn main() {
    let a: EnumSimple = EnumSimple::A;
    let a_boxed: Box<EnumSimple> = Box::<EnumSimple>::new(a);
    match a_boxed.as_ref() {
        EnumSimple::A => {
            tasm::tasm_io_write_to_stdout___u32(5);
        }
        EnumSimple::B(_) => {
            panic!();
        }
    };

    let read_from_stdin: u128 = tasm::tasm_io_read_stdin___u128();
    let b: EnumSimple = EnumSimple::B(read_from_stdin);
    let b_boxed: Box<EnumSimple> = Box::<EnumSimple>::new(b);
    match b_boxed.as_ref() {
        EnumSimple::A => {
            panic!();
        }
        EnumSimple::B(b_value) => {
            assert!(read_from_stdin == *b_value);
            tasm::tasm_io_write_to_stdout___u128(*b_value);
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use self::tasm::wrap_main_with_io;
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::test_helpers::shared_test::TritonVMTestCase;
    use rand::random;
    use triton_vm::NonDeterminism;
    use twenty_first::shared_math::bfield_codec::BFieldCodec;

    #[test]
    fn box_enum_simple_test() {
        let stdin: u128 = random();
        let std_in = stdin.encode();
        let native_output = wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
        let entrypoint = EntrypointLocation::disk("enums", "box_enum_simple", "main");
        let vm_output = TritonVMTestCase::new(entrypoint)
            .with_std_in(std_in)
            .expect_stack_difference(0)
            .execute()
            .unwrap();
        assert_eq!(native_output, vm_output.output);
    }
}
