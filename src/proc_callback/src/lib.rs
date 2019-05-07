extern crate proc_macro;

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use regex::Regex;
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
    let mut new_terminals = vec![];
    let mut begin_terminal = false;

    let sep_word = Regex::new("^.*\\b$").unwrap();

    while let Some(token) = input.next() {
        // println!("{:?}", token);
        let tok_literal = unwrap_single(token).to_string();
        if tok_literal == "@" {
            terminals = terminals.into_iter().rev().collect();
            begin_terminal = true;
        } else {
            if !begin_terminal {
                let reg = input.next().unwrap();
                if let Some(TokenTree::Group(cb)) = input.next() {
                    let cb = if let Some(cb) = cb.stream().into_iter().next() {
                        quote! { Some(Box::new(move #cb)) }
                    } else {
                        quote! { None }
                    };
                    terminals.push((tok_literal, quote! {#reg}, cb));
                } else {
                    panic!();
                }
            } else {
                match unwrap_literal(tok_literal.as_str()) {
                    Some(orig) => {
                        let reg = regex::escape(orig.as_str())
                            + if sep_word.is_match(orig.as_str()) {
                                "\\b"
                            } else {
                                ""
                            };
                        // println!("{}", reg);
                        let terminal = (
                            String::from("\"") + orig.as_str() + "\"",
                            quote! { #reg },
                            quote! { None },
                        );
                        if new_terminals.iter().all(
                            |x: &(String, proc_macro2::TokenStream, proc_macro2::TokenStream)| {
                                x.0 != tok_literal
                            },
                        ) {
                            new_terminals.push(terminal);
                        }
                    }
                    _ => {
                        symbols.push(tok_literal);
                    }
                }
            }
        }
    }

    new_terminals.sort_by(|a, b| a.0.len().partial_cmp(&b.0.len()).unwrap());
    terminals.append(&mut new_terminals);

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
        let mut terminals: Vec<(
            &'static str,
            &'static str,
            Option<Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>,
        )> = vec![];
    };
    for (terminal, regex, callback) in terminals.iter() {
        terminals_quote = quote! {
            #terminals_quote
            terminals.push((#terminal, #regex, #callback));
        };
    }

    // println!("{:?}", symbols);
    // println!("{:?}", terminals);

    let output = quote! {
        fn apply() -> (Vec<(&'static str, &'static str, Option<Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>)>,
                ::std::collections::HashSet<&'static str>) {
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

    // let attr = input.next().unwrap();
    // let callback = input.next().unwrap();

    let res_type = quote! {(
        Option<Box<Fn(&Ast<#res>) -> Option<#res>>>,
        Option<Box<Fn(&mut Ast<#res>) -> ()>>,
    )};

    let mut src = quote! {};

    if let (TokenTree::Group(terminals), TokenTree::Group(symbols)) = (terminals, symbols) {
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

        while let Some(TokenTree::Group(event)) = input.next() {
            let mut stream = event.stream().into_iter();
            let mut attrs = HashSet::new();

            if let Some(TokenTree::Group(attr_stream)) = stream.next() {
                let mut attr_stream = attr_stream.stream().into_iter();
                while let Some(attr) = attr_stream.next() {
                    attrs.insert(unwrap_single(attr).to_string());
                }
            } else {
                panic!();
            }

            if let Some(TokenTree::Group(callback)) = stream.next() {
                let mut callback = quote! { #callback };

                if attrs.contains("reduce") {
                    src = quote! {
                        #src
                        evt.1 = Some(Box::new(#callback));
                    };
                } else {
                    if !attrs.contains("raw") {
                        let ast = quote! { &Ast<#res> };
                        let tok = quote! { &Token };
                        let mut type_param = quote! {};
                        let mut dest = quote! {};
                        let mut idx = 0usize;
                        for symbol in symbols.iter() {
                            if terminals.contains(symbol)
                                || unwrap_literal(symbol.as_str()).is_some()
                            {
                                // is terminal
                                type_param = quote! { #type_param #tok, };
                                dest = quote! { #dest ast.children[#idx].as_token(), };
                            } else {
                                // else
                                type_param = quote! { #type_param #ast, };
                                dest = quote! { #dest ast.children[#idx].as_ast(), };
                            }
                            idx += 1;
                        }
                        let fn_type = quote! { Fn(#type_param) -> Option<#res> };
                        callback = quote! {
                            {
                                let cb: Box<#fn_type> = Box::new(#callback);
                                Box::new(move |ast: &Ast<#res>| -> Option<#res> {
                                    cb(#dest)
                                })
                            }
                        };
                    }

                    src = quote! {
                        #src
                        evt.0 = Some(Box::new(#callback));
                    }
                }
            } else {
                panic!();
            }

            // println!("{}", src.to_string());
        }
    } else {
        panic!();
    }

    let output = quote! {
        fn apply() -> #res_type {
            let mut evt: #res_type = (None, None);
            #src
            evt
        }
    };
    // println!("YIELDS    ==>   {}", output.to_string());
    output.into()
}
