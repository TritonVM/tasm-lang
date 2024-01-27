use num::One;
use tasm_lib::triton_vm::prelude::*;
use tasm_lib::twenty_first::shared_math::traits::PrimitiveRootOfUnity;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ArithmeticDomain {
    pub(crate) offset: BFieldElement,
    pub(crate) generator: BFieldElement,
    pub(crate) length: usize,
}

impl ArithmeticDomain {
    /// Create a new domain with the given length.
    /// No offset is applied, but can added through [`with_offset()`](Self::with_offset).
    pub(crate) fn of_length(length: usize) -> ArithmeticDomain {
        return ArithmeticDomain {
            offset: BFieldElement::one(),
            generator: ArithmeticDomain::generator_for_length(length as u64),
            length,
        };
    }

    /// Set the offset of the domain.
    pub(crate) fn with_offset(mut self, offset: BFieldElement) -> ArithmeticDomain {
        self.offset = offset;

        return self;
    }

    /// Derive a generator for a domain of the given length.
    /// The domain length must be a power of 2.
    pub(crate) fn generator_for_length(domain_length: u64) -> BFieldElement {
        fn is_power_of_two(val: u64) -> bool {
            return val != 0u64 && (val & (val - 1)) == 0u64;
        }

        assert!(0 == domain_length || is_power_of_two(domain_length));

        return BFieldElement::primitive_root_of_unity(domain_length).unwrap();
    }
}
