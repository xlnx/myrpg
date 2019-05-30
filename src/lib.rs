use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use regex::Regex;

pub use lalr_util::{ast::*, log::*, parse_util::*, rule::*, symbol::*};

pub mod lang;

pub use proc_callback::*;
pub use lalr_util::symbol::Symbol;
use lalr_util::parse_util::SourceFileMark;

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

pub trait LexerProvider {
    fn new(rules: Vec<(Symbol, Regex)>) -> Self;
    fn emit(&self, val: &str) -> Option<(Symbol, usize)>;
}

pub struct DefaultRegexLexer {
    lex_rules: Vec<(Symbol, Regex)>
}

impl LexerProvider for DefaultRegexLexer {
    fn new(lex_rules: Vec<(Symbol, Regex)>) -> Self {
        return DefaultRegexLexer {lex_rules}
    }
    fn emit(&self, val: &str) -> Option<(Symbol, usize)> {
        for (symbol, rule) in self.lex_rules.iter() {
            if let Some(caps) = rule.captures(val) {
                let m = caps.get(2).unwrap_or(caps.get(1).unwrap());
                let (_, t) = (m.start(), m.end());
                return Some((*symbol, t))
            }
        }
        None
    }
}

#[allow(dead_code)]
pub struct LRParser<'a, T: LRLang, L: LexerProvider = DefaultRegexLexer> {
    lex_cbs: HashMap<Symbol, Box<Fn(&mut Token, &mut TokenCtrl) -> ()>>,
    lexer: L,
    grammar: Grammar<T::Output>,
    action: Vec<HashMap<Symbol, Action<'a, T::Output>>>,
    phantom: PhantomData<T>,
}

pub struct TokenCtrl {
    discard_token: bool,
    new_location: Option<(String, (usize, usize))>,
}

impl TokenCtrl {
    pub fn discard(&mut self) -> &mut Self {
        self.discard_token = true;
        self
    }
    pub fn set_location(&mut self, src: &str, loc: (usize, usize)) -> &mut Self {
        self.new_location = Some((String::from(src), loc));
        self
    }
}

impl<'a, T, L> LRParser<'a, T, L>
    where
        T: LRLang,
        L: LexerProvider
{
    pub fn new() -> Self {
        let (lex, lang, action) = T::new();

        // make symbols
        let mut lex_rules = vec![];
        let mut lex_cbs = HashMap::new();

        let mut symbols = vec![];
        for (lex_name, lex_patt, lex_cb) in lex.into_iter() {
            let symbol = Symbol::from(lex_name).as_terminal();
            symbols.push(symbol);
            let patt = format!(r"^({})", lex_patt);
            if let Some(cb) = lex_cb {
                lex_cbs.insert(symbol, cb);
            }
            lex_rules.push((symbol, patt));
        }
        let lex_rules: Vec<_> = lex_rules
            .into_iter()
            .map(|x| (x.0, Regex::new(&x.1).unwrap()))
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
            lex_cbs,
            lexer: L::new(lex_rules),
            grammar,
            action,
            phantom: PhantomData,
        };

        parser
    }

    fn forward_newline(text: &mut &str, end: usize, counter: &mut (usize, usize)) {
        let mut beg = 0;
        while let Some(pos) = text[beg..end].find(|c: char| c == '\n') {
            counter.0 = counter.0 + 1;
            counter.1 = 0;
            beg += pos + 1;
        }
        counter.1 += end - beg;
        *text = &text[end..];
    }

    fn next<'b>(&self, chunk: &mut TextChunk<'b>, sources: &mut Vec<SourceFileMark>) -> Result<Option<Token<'b>>, ()> {
        // println!("{:?}", self.lex_rules);
        // println!("{:?}", self.lex_rules_set);
        // self.lex_rules_set.ma
        loop {
            chunk.offset += chunk.dx;
            chunk.dx = 0;

            let mut text= &chunk.text[chunk.offset..];
            let spaces = text.len() - text.trim_start().len();
            Self::forward_newline(&mut text, spaces, &mut chunk.pos);
            chunk.offset += spaces;

            if text.len() == 0 {
                return Ok(None);
            }

            if let Some((symbol, t)) = self.lexer.emit(text) {
                let token_val = &text[..t];
                let tok_begin = chunk.pos;

                Self::forward_newline(&mut text, t, &mut chunk.pos);
                chunk.dx = t;

                let mut tok = Token {
                    symbol,
                    val: token_val,
                    pos: (tok_begin, chunk.pos),
                };

                if let Some(cb) = self.lex_cbs.get(&symbol) {
                    let mut ctrl = TokenCtrl {
                        discard_token: false,
                        new_location: None,
                    };
                    (*cb)(&mut tok, &mut ctrl);
                    if let Some((file, src_pos)) = ctrl.new_location {
                        sources.push(SourceFileMark{
                            name: file,
                            raw_pos: tok.pos.1,
                            src_pos
                        });
                    }
                    if ctrl.discard_token {
                        continue;
                    }
                }
                return Ok(Some(tok));
            } else {
                return Err(());
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
            sources,
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
                        env.token = self.next(chunk, sources);
                    }
                    Action::Accept => {
                        if ast_stack.len() == 1
                            && env.token.as_ref().unwrap().is_none()
                            && term_stack.is_empty()
                        {
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

    pub fn parse(&self, text: &'a str, logger: &mut Logger) -> Result<(Ast<T::Output>, Vec<SourceFileMark>), ()> {
        let mut env = ParseEnv::from(text);
        env.token = self.next(&mut env.chunk, &mut env.sources);

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

        Ok((ast, env.sources))
    }
}
