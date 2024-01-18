use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use arbitrary::Arbitrary;
use num::Zero;
use triton_vm::BFieldElement;
use twenty_first::shared_math::bfield_codec::BFieldCodec;

#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, Arbitrary)]
enum SimpleEnum {
    A,
    B(u32),
    C(BFieldElement, u32, u32),
}

#[allow(clippy::assertions_on_constants)]
fn to_memory_a() {
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
fn to_memory_b() {
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
fn to_memory_c() {
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

#[allow(clippy::assertions_on_constants)]
fn in_memory_a() {
    let a_boxed: Box<SimpleEnum> =
        SimpleEnum::decode(&tasm::load_from_memory(BFieldElement::zero())).unwrap();
    match a_boxed.as_ref() {
        SimpleEnum::A => {}
        _ => {
            assert!(false);
        }
    };

    return;
}

#[allow(clippy::assertions_on_constants)]
fn in_memory_b() {
    let b_boxed: Box<SimpleEnum> =
        SimpleEnum::decode(&tasm::load_from_memory(BFieldElement::zero())).unwrap();
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
fn in_memory_c() {
    let c_boxed: Box<SimpleEnum> =
        SimpleEnum::decode(&tasm::load_from_memory(BFieldElement::zero())).unwrap();
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
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;
    use itertools::Itertools;
    use triton_vm::NonDeterminism;

    #[test]
    fn box_test() {
        let stdin = vec![];
        let no_nondeterminism = NonDeterminism::default();
        let a_encoded = NonDeterminism::default().with_ram(
            SimpleEnum::A
                .encode()
                .into_iter()
                .enumerate()
                .map(|(k, v)| (BFieldElement::new(k as u64), v))
                .collect(),
        );
        let b_encoded = NonDeterminism::default().with_ram(
            SimpleEnum::B(4000)
                .encode()
                .into_iter()
                .enumerate()
                .map(|(k, v)| (BFieldElement::new(k as u64), v))
                .collect(),
        );
        let c_encoded = NonDeterminism::default().with_ram(
            SimpleEnum::C(BFieldElement::new(5000), 5005, 5010)
                .encode()
                .into_iter()
                .enumerate()
                .map(|(k, v)| (BFieldElement::new(k as u64), v))
                .collect(),
        );

        for (func, func_name, non_determinism) in [
            (&(to_memory_a as fn()), "to_memory_a", &no_nondeterminism),
            (&(to_memory_b as fn()), "to_memory_b", &no_nondeterminism),
            (&(to_memory_c as fn()), "to_memory_c", &no_nondeterminism),
            (&(in_memory_a as fn()), "in_memory_a", &a_encoded),
            (&(in_memory_b as fn()), "in_memory_b", &b_encoded),
            (&(in_memory_c as fn()), "in_memory_c", &c_encoded),
        ] {
            let native_output =
                rust_shadows::wrap_main_with_io(func)(stdin.clone(), non_determinism.to_owned());
            let entrypoint_location =
                EntrypointLocation::disk("boxed", "enum_to_memory_simple", func_name);
            let test_program = ozk_parsing::compile_for_test(
                &entrypoint_location,
                crate::ast_types::ListType::Unsafe,
            );
            println!("code:\n\n{}", test_program.iter().join("\n"));
            println!(
                "ram: {{{}}}",
                non_determinism
                    .ram
                    .iter()
                    .map(|(k, v)| format!("({k}: {v})"))
                    .join("\n")
            );
            println!(
                "b_encoded:\n[{}]",
                SimpleEnum::B(4000).encode().iter().join(", ")
            );
            let vm_output = execute_compiled_with_stack_and_ins_for_test(
                &test_program,
                vec![],
                stdin.clone(),
                non_determinism.to_owned(),
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
