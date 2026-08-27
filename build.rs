//! The front-end of this compiler links against `rustc_driver` and friends,
//! which are shared libraries living in the toolchain's sysroot. Tell the
//! linker where to find them at runtime.

use std::process::Command;

fn main() {
    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_owned());
    let output = Command::new(rustc)
        .args(["--print", "sysroot"])
        .output()
        .expect("`rustc --print sysroot` must succeed");
    let sysroot = String::from_utf8(output.stdout).unwrap().trim().to_owned();
    println!("cargo:rustc-link-arg=-Wl,-rpath,{sysroot}/lib");
    println!("cargo:rerun-if-env-changed=RUSTC");
    println!("cargo:rerun-if-env-changed=RUSTUP_TOOLCHAIN");
}
