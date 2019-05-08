use std::collections::{HashMap, HashSet, VecDeque};
use std::marker::PhantomData;

use regex::{Regex, RegexSet};

pub use lalr_util::{ast::*, log::*, parse_util::*, rule::*, symbol::*};

pub mod lang;

mod index;
use index::*;

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

fn make_closures<'a, T>(
    closures: &mut Vec<Closure<'a, T>>,
    goto: &mut Vec<HashMap<Symbol, usize>>,
    first: &HashMap<Symbol, HashSet<Symbol>>,
    grammar: &'a Grammar<T>,
) {
    let mut origin = Closure::new(grammar);
    let rule = grammar.origin();
    let mut la = HashSet::new();
    la.insert(BOTTOM);
    origin.insert(Item { rule, pos: 0, la });

    let mut incoming = VecDeque::new();
    incoming.push_back(origin);

    while let Some(closure) = incoming.pop_front() {
        let expanded = closure.expanded(first);
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

        Vec<(
            &'a str,
            &'a str,
            Option<Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>,
        )>,
        Vec<(
            &'a str,
            Vec<(
                Vec<&'a str>,
                (
                    Option<Box<Fn(&Ast<Self::Output>) -> Option<Self::Output>>>,
                    Option<Box<Fn(&mut Ast<Self::Output>) -> ()>>,
                ),
                Vec<&'a str>,
            )>,
        )>,
    );
}

#[allow(dead_code)]
pub struct LRParser<'a, T: LRLang> {
    lex_rules: Vec<(
        Symbol,
        Regex,
        Option<Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>,
    )>,
    lex_rules_set: RegexSet,

    grammar: Grammar<T::Output>,
    action: Vec<HashMap<Symbol, Action<'a, T::Output>>>,
    phantom: PhantomData<T>,
}

pub struct TokenCtrl {
    discard_token: bool,
    new_source: Option<String>,
    new_location: Option<(usize, usize)>,
}

impl TokenCtrl {
    pub fn discard(&mut self) -> &mut Self {
        self.discard_token = true;
        self
    }
    pub fn set_source_file(&mut self, src: &str) -> &mut Self {
        self.new_source = Some(String::from(src));
        self
    }
    pub fn set_location(&mut self, loc: (usize, usize)) -> &mut Self {
        self.new_location = Some(loc);
        self
    }
}

impl<'a, T> LRParser<'a, T>
where
    T: LRLang,
{
    pub fn new() -> Self {
        let (lex, lang) = T::new();

        // make symbols
        let mut lex_rules = vec![];

        let mut symbols = vec![];
        for (lex_name, lex_patt, lex_cb) in lex.into_iter() {
            let symbol = Symbol::from(lex_name).as_terminal();
            symbols.push(symbol);
            let patt = format!(r"^\s*?({})", lex_patt);
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
            for (lang_patts, lang_evt, lang_json_attr) in lang_rules.into_iter() {
                let ss = lang_patts;
                let rule = {
                    let mut rule = Rule::from(rule_id, src, &ss, &terms_set);
                    if let Some(handle_exec) = lang_evt.0 {
                        rule.handle_exec = handle_exec;
                    }
                    if let Some(handle_reduce) = lang_evt.1 {
                        rule.handle_reduce = Some(handle_reduce);
                    }
                    for attr in lang_json_attr.iter() {
                        rule.attributes.insert(String::from(*attr));
                    }
                    rule
                };
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
        make_first(&mut first, &grammar);

        let mut closures: Vec<Closure<'a, T::Output>> = vec![];
        let mut goto: Vec<_> = vec![];
        // make closures
        make_closures(&mut closures, &mut goto, &first, unsafe {
            &*(&grammar as *const Grammar<T::Output>)
        });

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

        let parser = LRParser {
            lex_rules,
            lex_rules_set,
            grammar,
            action,
            phantom: PhantomData,
        };

        parser
    }

    fn next<'b>(&self, chunk: &mut TextChunk<'b>) -> Option<Token<'b>> {
        // println!("{:?}", self.lex_rules);
        // println!("{:?}", self.lex_rules_set);
        // self.lex_rules_set.ma
        loop {
            let mut found = false;
            for (symbol, rule, cb) in self.lex_rules.iter() {
                if let Some(caps) = rule.captures(chunk.text) {
                    let m = caps.get(2).unwrap_or(caps.get(1).unwrap());
                    let (s, t) = (m.start(), m.end());
                    let mut beg = 0;
                    while let Some(pos) = chunk.text[beg..s].find(|c: char| c == '\n') {
                        chunk.pos = (chunk.pos.0 + 1, 0);
                        beg += pos + 1;
                        chunk.line = &chunk.text[beg..];
                    }
                    chunk.pos.1 += s - beg;

                    let token_val = &chunk.text[s..t];
                    let tok_begin = chunk.pos;

                    beg = s;
                    while let Some(pos) = chunk.text[s..t].find(|c: char| c == '\n') {
                        chunk.pos = (chunk.pos.0 + 1, 0);
                        beg += pos + 1;
                        chunk.line = &chunk.text[beg..];
                    }
                    chunk.pos.1 += t - beg;
                    chunk.text = &chunk.text[t..];
                    let mut tok = Token {
                        symbol: *symbol,
                        val: token_val,
                        pos: (tok_begin, chunk.pos),
                    };
                    found = true;
                    if let Some(cb) = cb {
                        let mut ctrl = TokenCtrl {
                            discard_token: false,
                            new_location: None,
                            new_source: None,
                        };
                        (*cb)(&mut tok, &mut ctrl);
                        if let Some(new_source) = ctrl.new_source {
                            chunk.file_name = new_source.into();
                        }
                        if let Some(new_location) = ctrl.new_location {
                            chunk.pos = new_location;
                        }
                        if ctrl.discard_token {
                            break;
                        }
                    }
                    return Some(tok);
                }
            }
            if !found {
                return None;
            }
        }
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

        let mut ast = Ast::from(rule.src, children, rule);
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
        //     "ACTION[{:?}][{:?}] = {:?}",
        //     state,
        //     symbol,
        //     self.action[state].get(&symbol)
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

    pub fn parse(&self, text: &'a str, logger: &mut Logger) -> Result<Ast<T::Output>, ()> {
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
                    logger.log(&err.into());
                    return Err(());
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
