use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use arbitrary::Arbitrary;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, Arbitrary)]
enum SimpleEnum {
    A,
    B(u32),
    C(BFieldElement, u32, u32),
}

#[allow(clippy::assertions_on_constants)]
fn main_a() {
    let a: SimpleEnum = SimpleEnum::A;
    let a_boxed: Box<SimpleEnum> = Box::<SimpleEnum>::new(a);
    match a_boxed.as_ref() {
        SimpleEnum::A => {}
        _ => {
            assert!(false);
        }
    };

    return;
}

#[allow(clippy::assertions_on_constants)]
fn main_b() {
    let b: SimpleEnum = SimpleEnum::B(404);
    let b_boxed: Box<SimpleEnum> = Box::<SimpleEnum>::new(b);
    match b_boxed.as_ref() {
        SimpleEnum::B(value) => {
            tasm::tasm_io_write_to_stdout___u32(*value);
        }
        _ => {
            assert!(false);
        }
    };

    return;
}

#[allow(clippy::assertions_on_constants)]
fn main_c() {
    let c: SimpleEnum = SimpleEnum::C(BFieldElement::new(404), 502, 200);
    let c_boxed: Box<SimpleEnum> = Box::<SimpleEnum>::new(c);
    match c_boxed.as_ref() {
        SimpleEnum::C(bfe, u32_0, u32_1) => {
            tasm::tasm_io_write_to_stdout___u32(*u32_1);
            tasm::tasm_io_write_to_stdout___u32(*u32_0);
            tasm::tasm_io_write_to_stdout___bfe(*bfe);
        }
        _ => {
            assert!(false);
        }
    };

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_test() {
        let stdin = vec![];
        let nondeterminism = NonDeterminism::default();

        for (func, func_name) in [
            (&(main_a as fn()), "main_a"),
            (&(main_b as fn()), "main_b"),
            (&(main_c as fn()), "main_c"),
        ] {
            println!("testing: {func_name}");
            let native_output =
                rust_shadows::wrap_main_with_io(func)(stdin.clone(), nondeterminism.clone());
            let test_program = ozk_parsing::compile_for_test(
                "boxed",
                "simple_enum",
                func_name,
                crate::ast_types::ListType::Unsafe,
            );
            println!("code:\n\n{}", test_program.iter().join("\n"));
            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &test_program,
                vec![],
                stdin.clone(),
                NonDeterminism::default(),
                0,
            )
            .unwrap();
            if native_output != vm_output.output {
                panic!(
                    "expected:\n{}\n\ngot:\n{}",
                    native_output.iter().join(","),
                    vm_output.output.iter().join(",")
                );
            }
        }
    }
}
