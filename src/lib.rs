use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use regex::{Regex, RegexSet};

pub use lalr_util::{ast::*, log::*, parse_util::*, rule::*, symbol::*};

pub mod lang;

pub use proc_callback::*;

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
        Vec<HashMap<Symbol, CompactAction>>,
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
        let (lex, lang, action) = T::new();

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

        let mut rule_id: usize = 0;
        let mut grammar = Grammar::new();
        let mut rule_refs = vec![];

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
            let set = RuleSet { rules, src };
            {
                for i in 0..set.rules.len() {
                    let rule = &set.rules[i];
                    rule_refs.push(unsafe { &*(rule as *const _) });
                }
            }
            grammar.insert(set);
        }

        let action: Vec<HashMap<Symbol, _>> = //vec![];
            action.into_iter()
            .map(|state| {
                let mut line = HashMap::new();
                for (symbol, action) in state.iter() {
                    line.insert(*symbol, match action {
                        CompactAction::Accept => Action::Accept,
                        CompactAction::Reduce(rule_id) => Action::Reduce(rule_refs[*rule_id]),
                        CompactAction::Shift(new_state) => Action::Shift(*new_state)
                    });
                }
                line
            })
            .collect();

        let parser = LRParser {
            lex_rules,
            lex_rules_set,
            grammar,
            action,
            phantom: PhantomData,
        };

        parser
    }

    fn next<'b>(&self, chunk: &mut TextChunk<'b>) -> Result<Option<Token<'b>>, ()> {
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
                    while let Some(pos) = chunk.text[beg..t].find(|c: char| c == '\n') {
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
                    return Ok(Some(tok));
                }
            }
            if !found {
                if chunk.text.trim() == "" {
                    return Ok(None);
                } else {
                    return Err(())
                }
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
                        let token = std::mem::replace(&mut env.token, Ok(None));
                        term_stack.push_back(token.unwrap().unwrap());
                        env.token = self.next(chunk);
                    }
                    Action::Accept => {
                        if ast_stack.len() == 1 && env.token.as_ref().unwrap().is_none() && term_stack.is_empty() {
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
            match env.token {
                Ok(ref token) => {
                    let symbol = if env.token.as_ref().unwrap().is_none() {
                        BOTTOM
                    } else {
                        token.as_ref().unwrap().symbol
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
                Err(_) => {
                    let err = env.report_error();
                    logger.log(&err.into());
                    return Err(());
                }
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
