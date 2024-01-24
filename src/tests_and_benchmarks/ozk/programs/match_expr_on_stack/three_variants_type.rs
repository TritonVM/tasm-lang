use tasm_lib::Digest;

pub(super) enum ThreeVariants {
    A,
    B(u128),
    C(Digest),
}
