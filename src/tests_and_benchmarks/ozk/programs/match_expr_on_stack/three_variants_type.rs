use crate::tests_and_benchmarks::ozk::rust_shadows as tasm;
use crate::triton_vm::prelude::Digest;

pub(super) enum ThreeVariants {
    A,
    B(u128),
    C(Digest),
}

impl ThreeVariants {
    #[allow(clippy::collapsible_else_if)]
    pub(super) fn random_from_std_in() -> ThreeVariants {
        let input: u32 = tasm::tasmlib_io_read_stdin___u32();
        let discriminant: u32 = input % 3;
        return if discriminant == 0 {
            ThreeVariants::A
        } else {
            if discriminant == 1 {
                ThreeVariants::B(1u128 << 101)
            } else {
                ThreeVariants::C(tasm::tasmlib_io_read_stdin___digest())
            }
        };
    }
}
