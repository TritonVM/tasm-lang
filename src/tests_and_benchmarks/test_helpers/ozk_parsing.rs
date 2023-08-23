use std::fs;

fn extract_main(parsed_file: syn::File) -> Option<syn::ItemFn> {
    for item in parsed_file.items {
        if let syn::Item::Fn(func) = item {
            if func.sig.ident == "main" {
                return Some(func);
            }
        }
    }
    None
}

pub fn parse_main(module_name: &str) -> (syn::ItemFn, String) {
    let path = format!(
        "{}/src/tests_and_benchmarks/ozk/programs/{module_name}.rs",
        env!("CARGO_MANIFEST_DIR"),
    );
    let content = fs::read_to_string(&path).expect("Unable to read file {path}");
    let parsed_file: syn::File = syn::parse_str(&content).expect("Unable to parse rust code");
    let parsed = extract_main(parsed_file);
    match parsed {
        Some(parsed) => (parsed, module_name.to_owned()),
        None => panic!("Failed to parse file {path}"),
    }
}
