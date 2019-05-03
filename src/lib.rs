#[macro_use]
extern crate ref_thread_local;
extern crate serde_json;

use std::collections::{HashMap, HashSet, VecDeque};
use std::marker::PhantomData;

use regex::{Regex, RegexSet};

pub mod symbol;
use symbol::*;

mod util;
#[allow(unused_imports)]
use util::*;

pub mod ast;
use ast::*;

pub mod wrapper;

mod rule;
use rule::*;

mod parse_util;
use parse_util::*;

pub mod lang;

mod index;
use index::*;

mod formatter;

pub use proc_callback::*;

fn make_first<T>(first: &mut HashMap<Symbol, HashSet<Symbol>>, grammar: &Grammar<T>) {
    let mut add_sub: bool;
    loop {
        add_sub = false;
        for (src, rule_set) in grammar.iter() {
            // i am curr param
            let my_firsts = first.get(&src).unwrap();
            let mut new_firsts = vec![];
            // each rule generated my me
            for rule in rule_set.iter() {
                // this rule generates eps
                let mut has_empty = rule.symbols.len() == 0;
                // each symbol in this rule
                for symbol in rule.symbols.iter() {
                    has_empty = false;
                    let firsts = first.get(symbol).unwrap();
                    // each symbol of first[symbol]
                    for first_elem in firsts.iter() {
                        if *first_elem == BOTTOM {
                            // this symbol generates eps
                            has_empty = true;
                        } else if !my_firsts.contains(first_elem) {
                            new_firsts.push(*first_elem);
                        }
                    }
                    // this symbol cant generate eps
                    if !has_empty {
                        break;
                    }
                }
                // this rule generates eps
                if has_empty && !my_firsts.contains(&BOTTOM) {
                    new_firsts.push(BOTTOM);
                }
            }
            // now add new firsts into first[me]
            let my_firsts_mut = first.get_mut(&src).unwrap();
            for first_elem in new_firsts.iter() {
                my_firsts_mut.insert(*first_elem);
                add_sub = true;
            }
        }
        // first closure is full
        if !add_sub {
            break;
        }
    }
}

fn make_follow<T>(
    first: &HashMap<Symbol, HashSet<Symbol>>,
    follow: &mut HashMap<Symbol, HashSet<Symbol>>,
    grammar: &Grammar<T>,
) {
    let mut add_sub: bool;
    loop {
        add_sub = false;
        for (src, rule_set) in grammar.iter() {
            for rule in rule_set.iter() {
                for (this_symbol, prev_symbol) in rule
                    .symbols
                    .iter()
                    .skip(1)
                    .rev()
                    .zip(rule.symbols.iter().rev().skip(1))
                {
                    if prev_symbol.is_non_terminal() {
                        let mut new_follows = vec![];
                        if this_symbol.is_non_terminal() {
                            let this_first = first.get(this_symbol).unwrap();
                            let prev_follow = follow.get(prev_symbol).unwrap();

                            // add all first[curr] to follow[prev]
                            for first_elem in this_first.iter() {
                                if *first_elem != BOTTOM && !prev_follow.contains(first_elem) {
                                    new_follows.push(*first_elem);
                                }
                            }
                            // if this elem yields eps
                            if this_first.contains(&BOTTOM) {
                                // add all follow[me] to follow[prev]
                                for follow_elem in follow.get(&src).unwrap() {
                                    if !prev_follow.contains(&follow_elem) {
                                        new_follows.push(*follow_elem);
                                    }
                                }
                            }
                        } else if !follow.get(prev_symbol).unwrap().contains(this_symbol) {
                            new_follows.push(*this_symbol);
                        }

                        // add all new follows to follow[prev]
                        let prev_follow_mut = follow.get_mut(prev_symbol).unwrap();
                        for follow in new_follows {
                            prev_follow_mut.insert(follow);
                            add_sub = true;
                        }
                    }
                }

                if let Some(back) = rule.symbols.iter().rev().next() {
                    let back_follow = follow.get(back).unwrap();
                    let mut new_follows = vec![];
                    if back.is_non_terminal() {
                        for follow_elem in follow.get(&src).unwrap() {
                            if !back_follow.contains(follow_elem) {
                                new_follows.push(*follow_elem);
                            }
                        }
                    }
                    let back_follow_mut = follow.get_mut(back).unwrap();
                    for follow in new_follows {
                        back_follow_mut.insert(follow);
                        add_sub = true;
                    }
                }
            }
        }
        // this closure follow is full
        if !add_sub {
            break;
        }
    }
}

fn make_closures<'a, T>(
    closures: &mut Vec<Closure<'a, T>>,
    goto: &mut Vec<HashMap<Symbol, usize>>,
    grammar: &'a Grammar<T>,
) {
    let mut origin = Closure::new(grammar);
    let rule = grammar.origin();
    origin.insert(Item { rule, pos: 0 });

    let mut incoming = VecDeque::new();
    incoming.push_back(origin);

    while let Some(closure) = incoming.pop_front() {
        let expanded = closure.expanded();
        let curr_state = closures.len();
        closures.push(closure);
        let mut following = HashMap::<Symbol, Closure<_>>::new();
        for item in expanded.into_iter() {
            if let Some(next_item) = item.next() {
                let symbol = item.symbol().unwrap();
                if following.contains_key(&symbol) {
                    following.get_mut(&symbol).unwrap().insert(next_item);
                } else {
                    let mut new_closure = Closure::new(grammar);
                    new_closure.insert(next_item);
                    following.insert(symbol, new_closure);
                }
            }
        }
        // println!("{:?}", following);
        goto.push(HashMap::new());
        for (symbol, new_closure) in following.into_iter() {
            if closures.contains(&new_closure) {
                let new_state = closures
                    .iter()
                    .enumerate()
                    .find(|x| x.1 == &new_closure)
                    .unwrap()
                    .0;
                goto[curr_state].insert(symbol, new_state);
            } else if incoming.contains(&new_closure) {
                let new_state = closures.len()
                    + incoming
                        .iter()
                        .enumerate()
                        .find(|x| x.1 == &new_closure)
                        .unwrap()
                        .0;
                goto[curr_state].insert(symbol, new_state);
            } else {
                let new_state = closures.len() + incoming.len();
                goto[curr_state].insert(symbol, new_state);
                incoming.push_back(new_closure);
            }
        }
    }
}

pub trait LRLang {
    type Output;

    fn new<'a>() -> (
        Vec<(&'a str, &'a str, Option<Box<Fn(&mut Token) -> ()>>)>,
        Vec<(
            &'a str,
            Vec<(
                Vec<&'a str>,
                (
                    Option<Box<Fn(&Ast<Self::Output>) -> Option<Self::Output>>>,
                    Option<Box<Fn(&mut Ast<Self::Output>) -> ()>>,
                ),
            )>,
        )>,
    );
}

#[allow(dead_code)]
pub struct LRParser<'a, T: LRLang> {
    lex_rules: Vec<(Symbol, Regex, Option<Box<Fn(&mut Token) -> ()>>)>,
    lex_rules_set: RegexSet,

    closures: Vec<Closure<'a, T::Output>>,
    grammar: Grammar<T::Output>,
    symbols: Vec<Symbol>,
    action: Vec<HashMap<Symbol, Action<'a, T::Output>>>,
    phantom: PhantomData<T>,
}

impl<'a, T> LRParser<'a, T>
where
    T: LRLang,
{
    pub fn format_actions(&self) -> String {
        let width = 6;
        let mut res = String::new();
        res += &format!("{:<width$}", "", width = width);
        for symbol in self.symbols.iter() {
            res += &format!("{:<width$?}", symbol, width = width);
        }
        res += "\n";
        for (state, line) in self.action.iter().enumerate() {
            res += &format!("{:<width$}", state, width = width);
            for symbol in self.symbols.iter() {
                if let Some(action) = line.get(symbol) {
                    res += &format!("{:<width$?}", action, width = width);
                } else {
                    res += &format!("{:<width$}", "", width = width);
                }
            }
            res += "\n"
        }
        res
    }

    pub fn new() -> Self {
        let (lex, lang) = T::new();

        // make symbols
        let mut lex_rules = vec![];

        let mut symbols = vec![];
        for (lex_name, lex_patt, lex_cb) in lex.into_iter() {
            let symbol = Symbol::from(lex_name).as_terminal();
            symbols.push(symbol);
            let patt = format!(r"^\s*({})", lex_patt);
            lex_rules.push((symbol, patt, lex_cb));
        }
        let lex_rules_set = RegexSet::new(lex_rules.iter().map(|x| &x.1)).unwrap();
        let lex_rules: Vec<_> = lex_rules
            .into_iter()
            .map(|x| (x.0, Regex::new(&x.1).unwrap(), x.2))
            .collect();
        let terms_set: HashSet<Symbol> = symbols.iter().map(|x| *x).collect();

        // make params
        let mut rule_id: usize = 0;
        let mut grammar = Grammar::new();
        for (lang_item, lang_rules) in lang.into_iter() {
            let src = Symbol::from(lang_item).as_non_terminal();
            let mut rules = vec![];
            for (lang_patts, lang_evt) in lang_rules.into_iter() {
                let ss = lang_patts;
                let rule = {
                    let mut rule = Rule::from(rule_id, src, &ss, &terms_set);
                    if let Some(handle_exec) = lang_evt.0 {
                        rule.handle_exec = handle_exec;
                    }
                    if let Some(handle_reduce) = lang_evt.1 {
                        rule.handle_reduce = Some(handle_reduce);
                    }
                    rule
                };
                rule_id += 1;
                rules.push(rule);
            }
            grammar.insert(RuleSet { rules, src });
        }

        let mut first: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();
        let mut follow: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();

        // insert symbols
        for symbol in symbols.iter() {
            first.index_mut_or_insert(*symbol).insert(*symbol);
            follow.index_mut_or_insert(*symbol);
        }

        symbols.push(BOTTOM);

        // insert non-symbols
        for (src, rule_set) in grammar.iter() {
            for rule in rule_set.iter() {
                for symbol in rule.symbols.iter() {
                    first.index_mut_or_insert(*symbol);
                    if symbol.is_non_terminal() {
                        follow.index_mut_or_insert(*symbol).insert(BOTTOM);
                    }
                }
            }
            first.index_mut_or_insert(*src);
            follow.index_mut_or_insert(*src).insert(BOTTOM);
            symbols.push(*src);
        }
        follow
            .index_mut_or_insert(grammar.origin().src)
            .insert(BOTTOM);

        // make first
        make_first(&mut first, &grammar);
        // make follow
        make_follow(&first, &mut follow, &grammar);

        // println!("FIRST = {:?}", first);
        // println!("FOLLOW = {:?}", follow);

        let mut closures: Vec<Closure<'a, T::Output>> = vec![];
        let mut goto: Vec<_> = vec![];
        // make closures
        make_closures(&mut closures, &mut goto, unsafe {
            &*(&grammar as *const Grammar<T::Output>)
        });

        let mut action: Vec<HashMap<Symbol, _>> = vec![];
        for _ in 0..goto.len() {
            action.push(HashMap::new());
        }

        for (state, line) in goto.iter().enumerate() {
            for (symbol, next_state) in line.iter() {
                action[state].insert(*symbol, Action::Shift(*next_state));
            }
        }

        for (state, closure) in closures.iter().enumerate() {
            for item in closure.expanded().iter() {
                if item.is_complete() {
                    for symbol in follow.get(&item.rule.src).unwrap().iter() {
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
									item.rule
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

        let parser = LRParser {
            lex_rules,
            lex_rules_set,
            grammar,
            symbols,
            closures,
            action,
            phantom: PhantomData,
        };

        // println!("Closures = {}", closures.as_string());
        // println!("{}", parser.format_actions());

        parser
    }

    fn next<'b>(&self, chunk: &mut TextChunk<'b>) -> Option<Token<'b>> {
        // println!("{:?}", self.lex_rules);
        // println!("{:?}", self.lex_rules_set);
        // self.lex_rules_set.ma
        for (val, rule, cb) in self.lex_rules.iter() {
            if let Some(caps) = rule.captures(chunk.text) {
                let m = caps.get(1).unwrap();
                let (s, t) = (m.start(), m.end());
                let mut beg = 0;
                while let Some(pos) = chunk.text[beg..s].find(|c: char| c == '\n') {
                    chunk.pos = (chunk.pos.0 + 1, 0);
                    beg += pos + 1;
                    chunk.line = &chunk.text[beg..];
                }
                chunk.pos.1 += (s - beg) as u32;
                // .find('\n');
                let mut tok = Token {
                    symbol: *val,
                    val: &chunk.text[s..t],
                    pos: chunk.pos,
                };
                // println!("yield ====> {:?}", tok);
                beg = s;
                while let Some(pos) = chunk.text[s..t].find(|c: char| c == '\n') {
                    chunk.pos = (chunk.pos.0 + 1, 0);
                    beg += pos + 1;
                    chunk.line = &chunk.text[beg..];
                }
                chunk.pos.1 += (t - beg) as u32;
                chunk.text = &chunk.text[t..];
                // println!("{:?}", tok);
                if let Some(cb) = cb {
                    (*cb)(&mut tok);
                }
                return Some(tok);
            }
        }

        None
    }

    fn do_reduce(&self, rule: &'a Rule<T::Output>, env: &mut ParseEnv<'a, T::Output>) {
        let ParseEnv {
            ast_stack,
            term_stack,
            states,
            ..
        } = env;

        let ast_size = rule.ast_cnt as usize;
        let term_size = rule.term_cnt as usize;

        let len = ast_stack.len();
        let sub_ast: Vec<_> = ast_stack.split_off(len - ast_size).into_iter().collect();
        let len = term_stack.len();
        let sub_term: Vec<_> = term_stack.split_off(len - term_size).into_iter().collect();
        let len = states.len();
        states.split_off(len - ast_size - term_size);

        let mut children = vec![];
        let mut it_ast = sub_ast.into_iter();
        let mut it_term = sub_term.into_iter();

        for dummy in rule.iter() {
            if dummy.is_non_terminal() {
                children.push(AstNode::Ast(it_ast.next().unwrap()));
            } else {
                children.push(AstNode::Token(it_term.next().unwrap()));
            }
        }

        let mut ast = Ast {
            symbol: rule.src,
            children,
            rule,
        };
        if let Some(ref handle_reduce) = rule.handle_reduce {
            (*handle_reduce)(&mut ast);
        }
        // println!(
        //     "yields => {:?}",
        //     ast // decode(id)
        // );
        ast_stack.push_back(ast);
        let curr_state = *states.back().unwrap();
        if let Action::Shift(next_state) = self.action[curr_state].get(&rule.src).unwrap() {
            states.push_back(*next_state);
        } else {
            panic!();
        }
    }

    fn do_match(
        &self,
        symbol: Symbol,
        env: &mut ParseEnv<'a, T::Output>,
    ) -> Result<bool, ParsingError<'a>> {
        let ParseEnv {
            states,
            ast_stack,
            term_stack,
            chunk,
            ..
        } = env;
        let state = *states.back().unwrap();
        // println!("");
        // // println!("ast_stack = {:?}", ast_stack);
        // // let term_vec: Vec<_> = term_stack.iter().map(|x|decode(*x)).collect();
        // println!("term_stack = {:?}", term_stack);
        // println!("states = {:?}", states);
        // // println!("action = {:?}", self.action[state]);
        // println!(
        // 	"ACTION[{:?}][{:?}] = {:?}",
        // 	state,
        // 	symbol,
        // 	self.action[state].get(&symbol)
        // );
        match self.action[state].get(&symbol) {
            Some(action) => {
                match action {
                    Action::Shift(new_state) => {
                        states.push_back(*new_state);
                        let token = std::mem::replace(&mut env.token, None);
                        term_stack.push_back(token.unwrap());
                        env.token = self.next(chunk);
                    }
                    Action::Accept => {
                        if ast_stack.len() == 1 && env.token.is_none() && term_stack.is_empty() {
                            return Ok(true);
                        }
                    }
                    Action::Reduce(rule) => {
                        self.do_reduce(rule, env);
                    }
                };
                Ok(false)
            }
            None => Err(env.report_error()),
        }
    }

    pub fn parse(&self, text: &'a str) -> Result<Ast<T::Output>, ParsingError<'a>> {
        let mut env = ParseEnv::from(text);
        env.token = self.next(&mut env.chunk);

        loop {
            let symbol = if env.token.is_none() {
                BOTTOM
            } else {
                env.token.as_ref().unwrap().symbol
            };
            match self.do_match(symbol, &mut env) {
                Ok(true) => break,
                Err(err) => {
                    // println!("{:?}", self.closures[*env.states.back().unwrap()]);
                    return Err(err);
                }
                _ => {}
            }
        }

        let ParseEnv {
            ast_stack: mut deq_ast,
            ..
        } = env;
        let ast = deq_ast.remove(0).unwrap();

        Ok(ast)
    }
}
