#[cfg(test)]
mod compile_and_typecheck_tests {
    use crate::graft::item_fn;
    use crate::tests_and_benchmarks::shared_test::graft_check_compile_prop;
    use syn::parse_quote;

    fn typescript_native_coin_rast() -> syn::ItemFn {
        item_fn(parse_quote! {
            fn typescript_native_coin() {

                let amount_in: u64 = tasm::tasm_io_read_stdin_u64();
                let amount_out: u64 = tasm::tasm_io_read_stdin_u64();
                // let amount_in: u128 = tasm::tasm_io_read_stdin_u128();
                // let amount_out: u128 = tasm::tasm_io_read_stdin_u128();

                assert!(amount_in == amount_out);
                return;
            }
        })
    }

    #[test]
    fn typescript_native_coin_test() {
        let tasm = graft_check_compile_prop(&typescript_native_coin_rast());
        println!("{tasm}");
        let program = triton_opcodes::program::Program::from_code(&tasm).unwrap();
        println!("Program:\n\n");
        println!("{program}");
    }
}
