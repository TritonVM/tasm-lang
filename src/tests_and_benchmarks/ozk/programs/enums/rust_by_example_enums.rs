use num::One;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

// Create an `enum` to classify a web event. Note how both
// names and type information together specify the variant:
// `PageLoad != PageUnload` and `KeyPress(char) != Paste(String)`.
// Each is different and independent.
enum WebEvent {
    // An `enum` variant may either be `unit-like`,
    PageLoad,
    PageUnload,
    // like tuple structs,
    KeyPress(u32),
    Paste(u128),
    Click(u64, u64),
    // or c-like structures.
    // Click { x: i64, y: i64 }, (not supported by tasm-lang yet)
}

fn main() {
    // A function which takes a `WebEvent` enum as an argument and
    // returns nothing.
    fn inspect(event: WebEvent) {
        match event {
            WebEvent::PageLoad => {
                tasm::tasmlib_io_write_to_stdout___u32(0);
            }
            WebEvent::PageUnload => {
                tasm::tasmlib_io_write_to_stdout___u32(1);
            }
            // Destructure `c` from inside the `enum` variant.
            WebEvent::KeyPress(c) => {
                tasm::tasmlib_io_write_to_stdout___u32(2);
                tasm::tasmlib_io_write_to_stdout___u32(c);
            }
            WebEvent::Paste(s) => {
                tasm::tasmlib_io_write_to_stdout___u32(3);
                tasm::tasmlib_io_write_to_stdout___u128(s);
            }
            // Destructure `Click` into `x` and `y`.
            WebEvent::Click(x, y) => {
                tasm::tasmlib_io_write_to_stdout___u32(4);
                tasm::tasmlib_io_write_to_stdout___u64(x);
                tasm::tasmlib_io_write_to_stdout___u64(y);
            }
        };

        return;
    }

    let pressed: WebEvent = WebEvent::KeyPress(120);
    let pasted: WebEvent = WebEvent::Paste(1u128 << 99);
    let click: WebEvent = WebEvent::Click(20, 80);
    let load: WebEvent = WebEvent::PageLoad;
    let unload: WebEvent = WebEvent::PageUnload;

    inspect(load);
    inspect(unload);
    inspect(pressed);
    inspect(pasted);
    inspect(click);

    return;
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use num::Zero;
    use tasm_lib::triton_vm::prelude::*;

    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::execute_compiled_with_stack_and_ins_for_test;

    #[test]
    fn rust_by_example_enums_test() {
        let non_determinism = NonDeterminism::default();
        let expected_output = [
            vec![BFieldElement::zero()],
            vec![BFieldElement::one()],
            vec![BFieldElement::new(2), BFieldElement::new(120)],
            vec![BFieldElement::new(3)],
            (1u128 << 99).encode(),
            vec![BFieldElement::new(4)],
            (20u64).encode(),
            (80u64).encode(),
        ]
        .concat();
        let std_in = vec![];

        // Run test on host machine
        let native_output =
            rust_shadows::wrap_main_with_io(&main)(std_in.clone(), non_determinism.clone());
        assert_eq!(native_output, expected_output);

        // Run test on Triton-VM
        let entrypoint_location =
            EntrypointLocation::disk("enums", "rust_by_example_enums", "main");
        let test_program = ozk_parsing::compile_for_test(&entrypoint_location);
        println!("executing:\n{}", test_program.iter().join("\n"));
        let vm_output = execute_compiled_with_stack_and_ins_for_test(
            &test_program,
            vec![],
            std_in,
            non_determinism,
            0,
        )
        .unwrap();
        // assert_eq!(expected_output, vm_output.public_output);
        if expected_output != vm_output.public_output {
            panic!(
                "expected_output:\n{}\ngot:\n{}. Code was:\n{}",
                expected_output.iter().join(", "),
                vm_output.public_output.iter().join(", "),
                test_program.iter().join("\n")
            );
        }
    }
}
