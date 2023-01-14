use quote::quote;
use syn::parse_quote;

fn foo() {
    let tokens = quote! {
        fn swap_digests(digest_b: Digest, digest_a: Digest) {
            return (digest_a, digest_b);
        }
    };

    println!("{:#?}", tokens)
}

fn bar() {
    let tokens: syn::Item = parse_quote! {
        fn swap_digests(digest_b: Digest, digest_a: Digest) -> (Digest, Digest) {
            let index = blabla();
            return (digest_a, digest_b);
        }
    };

    match &tokens {
        syn::Item::Fn(item_fn) => {
            println!("{:#?}", item_fn);
        }
        _ => panic!("unsupported"),
    }

    // match tokens {
    //     syn::File {
    //         shebang,
    //         attrs,
    //         items,
    //     } => {
    //         // println!("{:#?}", items[0]);

    //         match &items[0] {
    //             syn::Item::Fn(item_fn) => {
    //                 println!("{:#?}", item_fn);
    //             }
    //             _ => panic!("unsupported"),
    //         }
    //     }
    // }

    // println!("{:#?}", tokens);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_foo() {
        foo();
    }

    #[test]
    fn test_bar() {
        bar();
    }
}
