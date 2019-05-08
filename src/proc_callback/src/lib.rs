extern crate proc_macro;

use lalr_util::{parse_util::*, rule::*, symbol::*};
use std::collections::{HashMap, HashSet};

mod comp;
use comp::*;

mod index;
use index::*;

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

fn do_classify_symbols(
    input: TokenStream,
) -> Vec<(String, proc_macro2::TokenStream, proc_macro2::TokenStream)> {

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

    terminals.into_iter().rev().collect()
}

#[proc_macro]
pub fn classify_symbols(input: TokenStream) -> TokenStream {

    let terminals = do_classify_symbols(input);

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

    let output = quote! {
        fn apply() -> Vec<(&'static str, &'static str, Option<Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>)> {
            { #terminals_quote terminals }
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

#[proc_macro]
pub fn build_lalr_table(input: TokenStream) -> TokenStream {
    let mut input = proc_macro2::TokenStream::from(input).into_iter();

    let lex = if let TokenTree::Group(grp) = input.next().unwrap() {
        grp.stream()
    } else {
        panic!()
    };
    let grammar = if let TokenTree::Group(grp) = input.next().unwrap() {
        grp.stream()
    } else {
        panic!()
    };

    let lex: Vec<String> = do_classify_symbols(lex.into())
        .into_iter()
        .map(|x| x.0)
        .collect();
    let mut lang: Vec<(String, Vec<Vec<String>>)> = grammar
        .into_iter()
        .map(|rule_set| {
            let mut rule_set = if let TokenTree::Group(grp) = rule_set {
                grp.stream().into_iter()
            } else {
                panic!()
            };
            let name = rule_set.next().unwrap();
            (
                unwrap_single(name).to_string(),
                rule_set
                    .map(|rule| {
                        let rule = if let TokenTree::Group(grp) = rule {
                            grp.stream().into_iter()
                        } else {
                            panic!();
                        };
                        rule.map(|patt| unwrap_single(patt).to_string())
                            .filter(|x| x != "_")
                            .collect()
                    })
                    .collect(),
            )
        })
        .collect();

    lang.insert(0, ("@".into(), vec![(vec![lang[0].0.clone()])]));

    let symbols: Vec<_> = lex
        .iter()
        .map(|x| Symbol::from(x.as_str()).as_terminal())
        .collect();

    let terms_set: HashSet<Symbol> = symbols.iter().map(|x| *x).collect();

    // make params
    let mut rule_id: usize = 0;
    let mut grammar = Grammar::new();
    for (lang_item, lang_rules) in lang.into_iter() {
        let src = Symbol::from(lang_item.as_str()).as_non_terminal();
        let mut rules = vec![];
        for lang_patts in lang_rules.into_iter() {
            let ss: Vec<_> = lang_patts.iter().map(|x| x.as_str()).collect();
            let rule = Rule::from(rule_id, src, &ss, &terms_set);
            rule_id += 1;
            rules.push(rule);
        }
        grammar.insert(RuleSet { rules, src });
    }

    let mut first: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();

    // insert symbols
    for symbol in symbols.iter() {
        first.index_mut_or_insert(*symbol).insert(*symbol);
    }

    // insert non-symbols
    for (src, rule_set) in grammar.iter() {
        for rule in rule_set.iter() {
            for symbol in rule.symbols.iter() {
                first.index_mut_or_insert(*symbol);
            }
        }
        first.index_mut_or_insert(*src);
    }

    // make first
    println!("Computing First Set...");
    make_first(&mut first, &grammar);

    let mut closures: Vec<Closure<()>> = vec![];
    let mut goto: Vec<_> = vec![];
    // make closures
    println!("Computing Closures...");
    make_closures(&mut closures, &mut goto, &first, unsafe {
        &*(&grammar as *const Grammar<()>)
    });

    println!("Computing Look Ahead Tokens...");
    loop {
        let mut add_la = false;

        for state in 0..closures.len() {
            // println!("{}", state);
            let closure = closures.get(state).unwrap();
            for item in closure.expanded(&first).iter() {
                if let Some(det_sym) = item.rule.symbols.get(item.pos) {
                    let goto = *goto[state].get(det_sym).unwrap();
                    let cl = closures.get_mut(goto).unwrap();
                    if let Some(next) = item.next() {
                        let mut old_next = cl.take(&next).unwrap();
                        if next.gt_some_what(&old_next) {
                            old_next.insert(next);
                            add_la = true;
                        }
                        cl.insert(old_next);
                    }
                }
            }
        }

        if !add_la {
            break;
        }
    }

    println!("Building LALR Action Table...");
    let mut action: Vec<HashMap<Symbol, Action<()>>> = vec![];
    for _ in 0..goto.len() {
        action.push(HashMap::new());
    }

    for (state, line) in goto.iter().enumerate() {
        for (symbol, next_state) in line.iter() {
            action[state].insert(*symbol, Action::Shift(*next_state));
        }
    }

    for (state, closure) in closures.iter().enumerate() {
        for item in closure.expanded(&first).iter() {
            if item.is_complete() {
                for symbol in item.la.iter() {
                    if action[state].contains_key(&symbol) {
                        let mut resolve = None;
                        let curr_action = action[state].get(&symbol).unwrap();
                        match curr_action {
                            Action::Shift(new_state) => {
                                if closures[*new_state]
                                    .iter()
                                    .all(|new_item| new_item.rule.src == item.rule.src)
                                {
                                    resolve = Some(
                                        closures[*new_state]
                                            .iter()
                                            .all(|new_item| new_item.rule.id > item.rule.id),
                                    )
                                }
                            }
                            Action::Reduce(rule) => {
                                if rule.src == item.rule.src {
                                    resolve = Some(rule.id > item.rule.id)
                                }
                            }
                            _ => {}
                        }
                        if let Some(resolve) = resolve {
                            if resolve {
                                action[state].insert(
                                    *symbol,
                                    if item.rule == grammar.origin() {
                                        Action::Accept
                                    } else {
                                        Action::Reduce(item.rule)
                                    },
                                );
                            }
                        } else {
                            let prev = match curr_action {
                                Action::Shift(new_state) => {
                                    format!("Shifting to state {:?}", closures[*new_state])
                                }
                                Action::Reduce(rule) => format!("Reducing {:?}", rule),
                                Action::Accept => format!("Accept"),
                            };
                            panic!(format!(
								"Conflict action found when receiving {:?}:\n#0: {}\n#1: Reducing {:?}\nCan't build parse table",
								symbol,
								prev,
								item
							));
                        }
                    } else {
                        action[state].insert(
                            *symbol,
                            if item.rule == grammar.origin() {
                                Action::Accept
                            } else {
                                Action::Reduce(item.rule)
                            },
                        );
                    }
                }
            }
        }
    }

    // println!("Closures = ");
    // for (state, closure) in closures.iter().enumerate() {
    //     println!("#{} = {:?}", state, closure);
    // }

    // println!("\nActions = ");
    // for (state, line) in action.iter().enumerate() {
    //     println!("#{} = {{", state);
    //     for (sym, action) in line.iter() {
    //         println!("  {:?}: {:?}", sym, action);
    //     }
    //     println!("}}");
    // }

    println!(
        "\n{}\n  States = {}\n  Actions = {}",
        "<Build Summary>",
        action.len(),
        action.iter().map(|line| line.len()).sum::<usize>()
    );

    let mut src = quote! {
        let mut action = vec![];
    };

    for state in action.iter() {
        let mut actions = quote! {};
        for (symbol, action) in state.iter() {
            let symbol = if symbol.is_bottom() {
                quote! {BOTTOM}
            } else {
                let s = symbol.as_str();
                if symbol.is_terminal() {
                    quote! { Symbol::from(#s).as_terminal() }
                } else {
                    quote! { Symbol::from(#s).as_non_terminal() }
                }
            };
            let action = match action {
                Action::Accept => quote! {CompactAction::Accept},
                Action::Reduce(rule) => {
                    let id = rule.id;
                    quote! {CompactAction::Reduce(#id)}
                }
                Action::Shift(new_state) => quote! {CompactAction::Shift(#new_state)},
            };
            actions = quote! {
                #actions
                action.insert(#symbol, #action);
            };
        }
        src = quote! {
            #src
            action.push({
                let mut action = ::std::collections::HashMap::new();
                #actions
                action
            });
        };
    }

    src = quote! {
        #src
        action
    };

    (quote! {
        fn apply() -> Vec<::std::collections::HashMap<Symbol, CompactAction>>
        { #src }
    })
    .into()
}