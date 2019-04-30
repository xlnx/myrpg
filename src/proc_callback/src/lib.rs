extern crate proc_macro;

use std::collections::HashSet;


use proc_macro::TokenStream;
use quote::quote;
use regex;
// use syn::DeriveInput;
use proc_macro2::TokenTree;

fn unwrap_single(token: TokenTree) -> TokenTree {
    match token {
        TokenTree::Group(g) => g.stream().into_iter().next().unwrap(),
        val @ _ => val,
    }
}

fn unwrap_literal(token: &str) -> Option<String> {
    if token.len() < 2 || token.chars().next().unwrap() != token.chars().rev().next().unwrap() {
        return None;
    }
    match token.chars().next().unwrap() {
        '\'' | '\"' => {
            let val = &token[1..token.len() - 1];
            Some(String::from(val))
        }
        _ => None,
    }
}

#[proc_macro]
pub fn classify_symbols(input: TokenStream) -> TokenStream {

    let mut input = proc_macro2::TokenStream::from(input).into_iter();

    let mut symbols = vec![];
    let mut terminals = vec![];
    let mut begin_terminal = false;

    while let Some(token) = input.next() {
        let tok_literal = unwrap_single(token).to_string();
        if tok_literal == "@" {
            terminals = terminals.into_iter().rev().collect();
            begin_terminal = true;
        } else {
            if !begin_terminal {
                terminals.push((tok_literal, input.next().unwrap()));
            } else {
                match unwrap_literal(tok_literal.as_str()) {
                    Some(orig) => {
                        let reg = regex::escape(orig.as_str());
                        let terminal = (
                            String::from("\"") + orig.as_str() + "\"",
                            (quote! { #reg }).into_iter().next().unwrap(),
                        );
                        if terminals.iter().all(|x| x.0 != tok_literal) {
                            terminals.push(terminal);
                        }
                    }
                    _ => {
                        symbols.push(tok_literal);
                    }
                }
            }
        }
    }

    terminals = terminals.into_iter().rev().collect();

    let mut non_terminals_quote = quote! {
        let mut non_terminals = ::std::collections::HashSet::new();
    };
    for symbol in symbols.iter() {
        non_terminals_quote = quote! {
            #non_terminals_quote
            non_terminals.insert(#symbol);
        };
    }

    let mut terminals_quote = quote! {
        let mut terminals = vec![];
    };
    for (terminal, regex) in terminals.iter() {
        terminals_quote = quote! {
            #terminals_quote
            terminals.push((#terminal, #regex));
        };
    }

    // println!("{:?}", symbols);
    // println!("{:?}", terminals);

    let output = quote! {
        fn apply() -> (Vec<(&'static str, &'static str)>, ::std::collections::HashSet<&'static str>) {
            ({ #terminals_quote terminals },
            { #non_terminals_quote non_terminals })
        }
    };

    output.into()

}

#[proc_macro]
pub fn wrap_callback(input: TokenStream) -> TokenStream {

    let mut input = proc_macro2::TokenStream::from(input).into_iter();

    let res = input.next().unwrap();
    let terminals = input.next().unwrap();
    let symbols = input.next().unwrap();
    let attr = input.next().unwrap();
    let callback = input.next().unwrap();

    if let TokenTree::Group(attr) = attr {
        if let (
            TokenTree::Group(terminals),
            TokenTree::Group(symbols),
            TokenTree::Group(callback),
        ) = (terminals, symbols, callback)
        {

            if let Some(attr) = attr.stream().into_iter().next() {

                match unwrap_single(attr).to_string().as_str() {

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
                let terminals: HashSet<_> = terminals
                    .stream()
                    .into_iter()
                    .map(unwrap_single)
                    .map(|x| x.to_string())
                    .collect();
                let symbols: Vec<_> = symbols
                    .stream()
                    .into_iter()
                    .map(unwrap_single)
                    .map(|x| x.to_string())
                    .filter(|x| x != "_")
                    .collect();
                let mut idx = 0usize;
                for symbol in symbols.iter() {
                    if terminals.contains(symbol) || unwrap_literal(symbol.as_str()).is_some() {
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