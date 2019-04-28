extern crate proc_macro;

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
// use std::collections::VecDeque;
// use syn::DeriveInput;
use proc_macro2::{Ident, TokenTree};

fn to_ident(token: TokenTree) -> Ident {
    match token {
        TokenTree::Group(g) => to_ident(g.stream().into_iter().next().unwrap()),
        TokenTree::Ident(i) => i,
        _ => panic!(),
    }
}

#[proc_macro]
pub fn dest_callback(input: TokenStream) -> TokenStream {

    let mut input = proc_macro2::TokenStream::from(input).into_iter();
    let res = to_ident(input.next().unwrap());
    let terms = input.next().unwrap();
    let symbols = input.next().unwrap();
    let attr = input.next().unwrap();
    let callback = input.next().unwrap();

    if let TokenTree::Group(attr) = attr {

        // println!("{:?}", attr);

        if let (TokenTree::Group(terms), TokenTree::Group(symbols), TokenTree::Group(callback)) =
            (terms, symbols, callback)
        {

            if let Some(attr) = attr.stream().into_iter().next() {

                match to_ident(attr).to_string().as_str() {

                    "raw" => {
                        let output = quote! {
                            fn apply() -> Box<Fn(&Ast<#res>) -> Option<#res>> {
                                Box::new(#callback)
                            }
                        };
                        output.into()
                    }
                    any @ _ => {
                        panic!(format!("Unknown wrapping attribute: {}", any));
                    }

                }

            } else {
                let ast = quote! { &Ast<#res> };
                let tok = quote! { &Token };
                let mut type_param = quote! {};
                let mut dest = quote! {};
                let terms: HashSet<_> = terms
                    .stream()
                    .into_iter()
                    .map(to_ident)
                    .map(|x| x.to_string())
                    .collect();
                let symbols: Vec<_> = symbols
                    .stream()
                    .into_iter()
                    .map(to_ident)
                    .map(|x| x.to_string())
                    .collect();
                let mut idx = 0usize;
                for symbol in symbols.iter() {
                    if terms.contains(symbol) {
                        // is terminal
                        type_param = quote! { #type_param #tok, };
                        dest = quote! { #dest ast.childs[#idx].as_token(), };
                    } else {
                        // else
                        type_param = quote! { #type_param #ast, };
                        dest = quote! { #dest ast.childs[#idx].as_ast(), };
                    }
                    idx += 1;
                }
                let fn_type = quote! { Fn(#type_param) -> Option<#res> };

                let output = quote! {
                    fn apply() -> Box<Fn(&Ast<#res>) -> Option<#res>> {
                        let cb: Box<#fn_type> = Box::new(#callback);
                        Box::new(move |ast: &Ast<#res>| -> Option<#res> {
                            cb(#dest)
                        })
                    }
                };
                // println!("YIELDS    ==>   {}", output.to_string());
                output.into()
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }

}