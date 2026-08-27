# tasm-lang

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![GitHub CI](https://github.com/TritonVM/tasm-lang/actions/workflows/main.yml/badge.svg)](https://github.com/TritonVM/tasm-lang/actions)

A compiler from a subset of Rust to [Triton VM assembly](https://github.com/TritonVM/triton-vm).

## How it works
The compiler uses `rustc` itself as its front-end: a program is parsed, name-resolved, type-checked
and borrow-checked by `rustc`, and `rustc`'s typed intermediate representation of the program (THIR)
is then lowered to the compiler's own abstract syntax tree, from which Triton assembly is generated.
This means that all of Rust's type checking and type inference comes for free, and that types like
`u32` vs `usize`, literal types, method resolution, auto-referencing, etc. behave exactly like in
Rust.

The types and functions that are native to Triton VM or provided by [`tasm-lib`](https://github.com/TritonVM/tasm-lib)
-- `BFieldElement`, `XFieldElement`, `Digest`, `Tip5`, `tasm::tasmlib_*`, ... -- are declared to
`rustc` through a stub prelude. The native types are opaque there, and occupy 1, 3 and 5 words on
Triton VM's stack respectively; their host-machine representation plays no role.

Because the front-end links against `rustc`'s internals, building this crate requires a nightly
toolchain with the `rustc-dev` component. The `rust-toolchain.toml` file in this repository takes
care of that when using `rustup`.

## Restrictions
This compiler only handles a part of the Rust language. Everything that is compiled must be
valid Rust, but not all valid Rust can be compiled.
- There are no `for` loops, use `while` instead.
- There is no `loop`, `break`, or `continue`.
- Closures, traits, generics, and references beyond `&`/`&mut` in argument position are not supported.
- Functions must end with a `return` statement.
- `usize` is a 32-bit type.

And more ...

## How to use
### Install the Compiler
- Clone this repo
- `cd tasm-lang`
- `cargo install --path .`

### Compile for Triton VM
Executing `tasm-lang <input-file.rs> <output-file>` will create an assembler file in `<output-file>.tasm` of the compilation of the
`main` function in `<intput-file.rs>`.

### Execute Assembler
The compiled assembler code can be executed through Triton VM's `run` function, or through [triton-tui](https://github.com/TritonVM/triton-tui),
the debugger for Triton VM. triton-tui allows you to step through the assember one instruction at a time and see the state of the virtual
machine before each instruction. If you've installed triton-tui, you can execute your compiled assembler:

```
triton-tui <output-file.tasm>
```
<!---
TODO: Add better explanation of how to set std-in, secret-in and initialize nondeterminstic memory, or improve interface for this
in Triton VM.
-->
See the examples in triton-tui to see how to set initial conditions (like input streams) on Triton VM executions.

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
        TestStruct::decode(&tasm::load_from_memory(BFieldElement::new(0))).unwrap();
    let other_value: u64 = 2023;
    tasm::tasmlib_io_write_to_stdout___bfe(test_struct.ab_sum());
    tasm::tasmlib_io_write_to_stdout___u128(test_struct.cd_sum(other_value));
    return;
}
```

For many more code examples, see [programs](./src/tests_and_benchmarks/ozk/programs).
