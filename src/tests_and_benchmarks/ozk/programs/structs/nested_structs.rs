use rand::distr::StandardUniform;
use rand::prelude::Distribution;
use rand::Rng;
use tasm_lib::prelude::TasmObject;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::math::other::random_elements;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(TasmObject, BFieldCodec, Clone)]
struct InnerInnerInnerInnerStruct {
    pub(crate) a: u128,
    pub(crate) b: BFieldElement,
    pub(crate) c: bool,
    pub(crate) d: Vec<Vec<Vec<Vec<Vec<Digest>>>>>,
    pub(crate) e: Digest,
}

#[derive(TasmObject, BFieldCodec, Clone)]
struct InnerInnerInnerStruct {
    pub(crate) a: Digest,
    pub(crate) b: Vec<u128>,
    pub(crate) c: BFieldElement,
    pub(crate) d: u128,
    pub(crate) e: Vec<XFieldElement>,
    pub(crate) f: InnerInnerInnerInnerStruct,
}

#[derive(TasmObject, BFieldCodec, Clone)]
struct InnerInnerStruct {
    pub(crate) a: Digest,
    pub(crate) b: Vec<BFieldElement>,
    pub(crate) c: u64,
    pub(crate) d: u128,
    pub(crate) e: InnerInnerInnerStruct,
    pub(crate) f: Vec<InnerInnerInnerStruct>,
}

#[derive(TasmObject, BFieldCodec)]
struct InnerStruct {
    pub(crate) a: Digest,
    pub(crate) b: InnerInnerStruct,
    pub(crate) c: Vec<InnerInnerStruct>,
}

#[derive(TasmObject, BFieldCodec)]
struct TestStuctNested {
    pub(crate) a: InnerStruct,
    pub(crate) b: Vec<InnerStruct>,
    pub(crate) c: InnerInnerStruct,
    pub(crate) d: Vec<InnerInnerStruct>,
    pub(crate) e: u64,
    pub(crate) f: Vec<Vec<Vec<InnerInnerStruct>>>,
    pub(crate) g: Vec<Vec<Vec<Vec<u128>>>>,
}

fn main() {
    let test_struct: Box<TestStuctNested> =
        TestStuctNested::decode(&tasm::load_from_memory(BFieldElement::new(300))).unwrap();

    tasm::tasmlib_io_write_to_stdout___digest(test_struct.a.a);
    tasm::tasmlib_io_write_to_stdout___u64(test_struct.a.b.c);
    tasm::tasmlib_io_write_to_stdout___u64(test_struct.a.c[1].c);
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.b[2].c[2].f[3].b[5]);
    tasm::tasmlib_io_write_to_stdout___u64(test_struct.e);
    tasm::tasmlib_io_write_to_stdout___u64(test_struct.c.c);
    tasm::tasmlib_io_write_to_stdout___digest(test_struct.f[1][1][1].a);
    tasm::tasmlib_io_write_to_stdout___xfe(test_struct.f[1][1][1].f[3].e[4]);
    tasm::tasmlib_io_write_to_stdout___xfe(test_struct.f[2][2][2].f[2].e[2]);
    tasm::tasmlib_io_write_to_stdout___u64(test_struct.f[2][2][2].f[2].e.len() as u64);
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.g[0][1][2][3]);
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.g[3][2][1][0]);
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.g[0][3][2][1]);
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.a.b.e.f.a);
    tasm::tasmlib_io_write_to_stdout___bfe(test_struct.a.b.e.f.b);
    tasm::tasmlib_io_write_to_stdout___bool(test_struct.a.b.e.f.c);
    tasm::tasmlib_io_write_to_stdout___digest(test_struct.a.b.e.f.d[1][1][0][1][0]);
    tasm::tasmlib_io_write_to_stdout___digest(test_struct.a.b.e.f.e);

    tasm::tasmlib_io_write_to_stdout___u32(test_struct.a.b.e.f.d[1][1][0][1].len() as u32);
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.a.b.e.f.d[1][1][0].len() as u32);
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.a.b.e.f.d[1][1].len() as u32);
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.a.b.e.f.d[1].len() as u32);
    tasm::tasmlib_io_write_to_stdout___u32(test_struct.a.b.e.f.d.len() as u32);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::random;

    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;
    use crate::tests_and_benchmarks::test_helpers::shared_test::init_memory_from;

    use super::*;

    impl Distribution<InnerInnerInnerInnerStruct> for StandardUniform {
        fn sample<R: Rng + ?Sized>(&self, _rng: &mut R) -> InnerInnerInnerInnerStruct {
            // Don't set this value to more than 2, as it
            // will cause the program to take too long to run
            let g_length_inner = 2;
            let g_length_quartic = 2;

            let inner_most_vec = vec![random_elements(g_length_inner); g_length_quartic];
            let second_inner_vec = vec![inner_most_vec; g_length_quartic];
            let third_inner_vec = vec![second_inner_vec; g_length_quartic];

            InnerInnerInnerInnerStruct {
                a: random(),
                b: random(),
                c: random(),
                d: vec![third_inner_vec; g_length_quartic],
                e: random(),
            }
        }
    }

    impl Distribution<InnerInnerInnerStruct> for StandardUniform {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> InnerInnerInnerStruct {
            let b_length = rng.random_range(6..=16);
            let e_length = rng.random_range(5..=20);
            InnerInnerInnerStruct {
                a: random(),
                b: random_elements(b_length),
                c: random(),
                d: random(),
                e: random_elements(e_length),
                f: random(),
            }
        }
    }

    impl Distribution<InnerInnerStruct> for StandardUniform {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> InnerInnerStruct {
            let b_length = rng.random_range(0..=10);
            let f_length = rng.random_range(4..=10);
            InnerInnerStruct {
                a: random(),
                b: random_elements(b_length),
                c: random(),
                d: random(),
                e: random(),
                f: random_elements(f_length),
            }
        }
    }

    impl Distribution<InnerStruct> for StandardUniform {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> InnerStruct {
            let c_length = rng.random_range(3..=14);
            InnerStruct {
                a: random(),
                b: random(),
                c: random_elements(c_length),
            }
        }
    }

    impl Distribution<TestStuctNested> for StandardUniform {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TestStuctNested {
            let b_length = rng.random_range(3..=10);
            let d_length = rng.random_range(3..=10);
            let f_length_cubed = rng.random_range(3..=5);
            let g_length_quartic = rng.random_range(4..=5);

            let inner_most_vec = vec![random_elements(g_length_quartic); g_length_quartic];
            let second_inner_vec = vec![inner_most_vec; g_length_quartic];

            TestStuctNested {
                a: random(),
                b: random_elements(b_length),
                c: random(),
                d: random_elements(d_length),
                e: random(),
                f: vec![vec![random_elements(f_length_cubed); f_length_cubed]; f_length_cubed],
                g: vec![second_inner_vec; g_length_quartic],
            }
        }
    }

    #[test]
    fn nested_structs_test() {
        let test_struct: TestStuctNested = random();
        let non_determinism = init_memory_from(&test_struct, BFieldElement::new(300));
        let stdin = vec![];

        let expected_output = [
            test_struct.a.a.encode(),
            test_struct.a.b.c.encode(),
            test_struct.a.c[1].c.encode(),
            test_struct.b[2].c[2].f[3].b[5].encode(),
            test_struct.e.encode(),
            test_struct.c.c.encode(),
            test_struct.f[1][1][1].a.encode(),
            test_struct.f[1][1][1].f[3].e[4].encode(),
            test_struct.f[2][2][2].f[2].e[2].encode(),
            (test_struct.f[2][2][2].f[2].e.len() as u64).encode(),
            test_struct.g[0][1][2][3].encode(),
            test_struct.g[3][2][1][0].encode(),
            test_struct.g[0][3][2][1].encode(),
            test_struct.a.b.e.f.a.encode(),
            test_struct.a.b.e.f.b.encode(),
            test_struct.a.b.e.f.c.encode(),
            test_struct.a.b.e.f.d[1][1][0][1][0].encode(),
            test_struct.a.b.e.f.e.encode(),
            (test_struct.a.b.e.f.d[1][1][0][1].len() as u32).encode(),
            (test_struct.a.b.e.f.d[1][1][0].len() as u32).encode(),
            (test_struct.a.b.e.f.d[1][1].len() as u32).encode(),
            (test_struct.a.b.e.f.d[1].len() as u32).encode(),
            (test_struct.a.b.e.f.d.len() as u32).encode(),
        ]
        .concat();

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(stdin.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location = EntrypointLocation::disk("structs", "nested_structs", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            stdin,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.public_output);
        if expected_output != vm_output.public_output {
            panic!(
                "expected_output:\n {}, got:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.public_output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
