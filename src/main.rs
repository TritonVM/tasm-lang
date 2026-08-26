// The library links against `rustc`'s shared libraries, which bring their own
// copy of `std`. Linking `rustc_driver` here too makes this binary use that
// same copy, instead of linking `std` twice.
#![feature(rustc_private)]
extern crate rustc_driver;

fn main() {
    tasm_lang::main()
}
