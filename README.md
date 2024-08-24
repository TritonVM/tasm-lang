# tasm-lang

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![GitHub CI](https://github.com/TritonVM/tasm-lang/actions/workflows/main.yml/badge.svg)](https://github.com/TritonVM/tasm-lang/actions)

A compiler from a subset of Rust to [Triton VM assembly](https://github.com/TritonVM/triton-vm).

## Restrictions
This compiler only handles a small part of the Rust language.
- There are no `for` loops, use `while` instead.
- All functions must end with the `return` keyword and functions may only contain *one* `return`
  statement, i.e. early returns are not possible.
- All declarations must use explicit types.

And more ...

## How to use
Executing `cargo run <input-file.rs> <output-file> [-v]` will create an assembler file in `output-file.tasm` of the compilation of the
`main` function in `<intput-file.rs>`. If you run `cargo install --path .`, this compiler can be executed as `tasm-lang`.

## Examples
### Arithmetic with `u32` numbers
Solution to [Project Euler Problem 1](https://projecteuler.net/problem=1)
>If we list all the natural numbers below $10$ that are multiples of $3$ or $5$, we get $3, 5, 6$ and $9$. The sum of these multiples is $23$.
Find the sum of all the multiples of $3$ or $5$ below $1000$.

```rust
fn main() {
    let mut i: u32 = 1;
    let mut acc: u32 = 0;

    while i < 1000 {
        if i % 3 == 0 || i % 5 == 0 {
            acc += i;
        }

        i += 1;
    }

    tasm::tasmlib_io_write_to_stdout___u32(acc);

    return;
}
```

Notice that the result is printed to std-out through helper functions from the [`tasm-lib`](https://github.com/TritonVM/tasm-lib) library.
Also notice that the above code is valid Rust code that will run on both Triton VM and your host-machine, provided that the host-machine
implementation of `tasmlib_io_write_to_stdout___u32` is available.

### Lists
Solution to [Project Euler Problem 7](https://projecteuler.net/problem=7)
> By listing the first six prime numbers: $2, 3, 5, 7, 11$, and $13$, we can see that the $6^{th}$ prime is $13$.
What is the $10\,001^{st}$ prime number?

```rust
fn main() {
    // Execute with `10001` in standard input to find the 10,001th prime

    let index_of_prime_to_find: u32 = tasm::tasmlib_io_read_stdin___u32();
    let log2_of_desired_index: u32 = index_of_prime_to_find.ilog2();
    let sieve_size: u32 = index_of_prime_to_find * log2_of_desired_index;
    let mut primes: Vec<bool> = Vec::<bool>::default();

    // 0 and 1 are not primes
    primes.push(false);
    primes.push(false);

    // Initialize all cells to `true`
    let mut tmp_vec_initializer: u32 = 2;
    while tmp_vec_initializer < sieve_size {
        primes.push(true);
        tmp_vec_initializer += 1;
    }

    let mut num_primes_found: u32 = 1;
    let mut prime_candidate: u32 = 3;
    let mut last_prime_found: u32 = prime_candidate;
    while num_primes_found < index_of_prime_to_find {
        if primes[prime_candidate as usize] {
            num_primes_found += 1;
            last_prime_found = prime_candidate;
            let mut multiples_of_found_prime: u32 = 2 * prime_candidate;
            while multiples_of_found_prime < sieve_size {
                primes[multiples_of_found_prime as usize] = false;
                multiples_of_found_prime += prime_candidate;
            }
        }
        prime_candidate += 2;
    }

    tasm::tasmlib_io_write_to_stdout___u32(last_prime_found);

    return;
}
```

### Object-oriented Programming

Notice that there is full support for types native to Triton VM like `BFieldElement`, `XFieldElement`, and `Digest`.
And support for an object-oriented programming style and the initialization of memory at program execution start. In this
example, the program must be initialized with an encoded structure `TestStructure` at position `0` in memory.

```rust
#[derive(TasmObject, BFieldCodec)]
struct TestStruct {
    a: BFieldElement,
    b: BFieldElement,
    c: u32,
    d: u64,
}

impl TestStruct {
    fn ab_sum(&self) -> BFieldElement {
        return self.a + self.b;
    }

    fn cd_sum(&self, other_value: u64) -> u128 {
        return self.c as u128 + self.d as u128 + other_value as u128;
    }
}

fn main() {
    let test_struct: Box<TestStruct> =
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(2))).unwrap();
    let other_value: u64 = 2023;
    tasm::tasmlib_io_write_to_stdout___bfe(test_struct.ab_sum());
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.cd_sum(other_value));
    return;
}
```

For many more code examples, see [programs](./src/tests_and_benchmarks/ozk/programs).
