use tasm_lib::triton_vm::prelude::*;

use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;

#[derive(Clone, Copy)]
struct Point {
    x: u32,
    y: u32,
}

#[derive(Clone, Copy)]
enum Shape {
    Dot,
    Line(u32),
    Rectangle(Point),
}

impl Shape {
    fn size(self) -> u32 {
        match self {
            Shape::Dot => 1,
            Shape::Line(length) => length,
            Shape::Rectangle(corner) => corner.x * corner.y,
        }
    }
}

/// Structs, enums, tuples and `Option`s with inferred types.
#[allow(clippy::manual_unwrap_or_default, clippy::manual_unwrap_or)] // `unwrap_or` is not supported by the compiler
fn main() {
    let x = tasm::tasmlib_io_read_stdin___u32() % 1000;
    let y = tasm::tasmlib_io_read_stdin___u32() % 1000;
    let selector = tasm::tasmlib_io_read_stdin___u32() % 3;

    let corner = Point { x, y };
    let shape = if selector == 0 {
        Shape::Dot
    } else if selector == 1 {
        Shape::Line(x + y)
    } else {
        Shape::Rectangle(corner)
    };

    let pair = (shape.size(), corner.x + corner.y);
    let maybe = if pair.0 > pair.1 {
        Some(pair.0 - pair.1)
    } else {
        None
    };
    let difference = match maybe {
        Some(value) => value,
        None => 0,
    };
    let boxed = Box::new(maybe);

    tasm::tasmlib_io_write_to_stdout___u32(pair.0);
    tasm::tasmlib_io_write_to_stdout___u32(pair.1);
    tasm::tasmlib_io_write_to_stdout___u32(difference);
    tasm::tasmlib_io_write_to_stdout___bool(boxed.is_some());

    return;
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests_and_benchmarks::ozk::ozk_parsing::EntrypointLocation;
    use crate::tests_and_benchmarks::ozk::rust_shadows;
    use crate::tests_and_benchmarks::test_helpers::shared_test::*;

    #[test]
    fn inferred_composites_test() {
        for _ in 0..5 {
            // All inputs fit in a `u32`, so that they can be read as any type
            let std_in: Vec<BFieldElement> = (0..3)
                .map(|_| BFieldElement::new(rand::random::<u32>() as u64))
                .collect();
            let native_output =
                rust_shadows::wrap_main_with_io(&main)(std_in.clone(), NonDeterminism::default());
            let entrypoint =
                EntrypointLocation::disk("type_inference", "inferred_composites", "main");
            let vm_output = TritonVMTestCase::new(entrypoint)
                .with_std_in(std_in)
                .execute()
                .unwrap();
            assert_eq!(native_output, vm_output.public_output);
        }
    }
}
