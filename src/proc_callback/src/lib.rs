extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use std::collections::VecDeque;
// use syn::DeriveInput;

#[proc_macro]
pub fn dest_callback(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);
    let mut input: VecDeque<_> = input.into_iter().collect();
    let last = input.pop_back().unwrap();
    // let input: DeriveInput = syn::parse(input).unwrap();
    let output = quote! {
        fn apply() -> Box<Fn(&Vec<i32>) -> Option<i32>> {
            Box::new(#last)
        }
    };
    output.into()
}
