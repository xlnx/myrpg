use std::collections::{hash_map, HashMap, HashSet};
use std::hash::{Hash, Hasher};

use super::ast::{Ast, AstNode};
use super::symbol::Symbol;

pub struct Rule<T> {
    pub id: usize,
    pub src: Symbol,
    pub ast_cnt: u16,
    pub term_cnt: u16,
    pub symbols: Vec<Symbol>,
    pub attributes: HashSet<String>,

    pub handle_reduce: Option<Box<Fn(&mut Ast<T>) -> ()>>,
    pub handle_exec: Box<Fn(&Ast<T>) -> Option<T>>,
}

impl<T> Rule<T> {
    pub fn iter(&self) -> std::slice::Iter<Symbol> {
        self.symbols.iter()
    }
    pub fn from(
        id: usize,
        src: Symbol,
        symbol_literals: &Vec<&str>,
        terminals: &HashSet<Symbol>,
    ) -> Self {
        assert_eq!(src.is_non_terminal(), true);
        let mut rule = Rule {
            id,
            src,
            ast_cnt: 0,
            term_cnt: 0,
            symbols: vec![],
            attributes: HashSet::new(),

            handle_reduce: None,
            handle_exec: Box::new(|ast: &Ast<T>| -> Option<T> {
                let mut res = None;
                for ast in ast.children.iter() {
                    if let AstNode::Ast(ast) = ast {
                        res = (*ast.rule.handle_exec)(&ast);
                    }
                }
                res
            }),
        };
        for literal in symbol_literals.iter() {
            let symbol = Symbol::from(*literal).as_terminal();
            if terminals.contains(&symbol) {
                rule.symbols.push(symbol.as_terminal());
            } else {
                rule.symbols.push(symbol.as_non_terminal());
            }
        }
        for symbol in rule.symbols.iter() {
            if symbol.is_non_terminal() {
                rule.ast_cnt += 1;
            } else {
                rule.term_cnt += 1;
            }
        }
        rule
    }
}

impl<T> std::fmt::Debug for Rule<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // let data = self.symbols.iter().map(|x| decode(*x)).collect::<Vec<&str>>();
        let mut _res = write!(f, "{:?} ->", self.src);
        if self.symbols.len() == 0 {
            write!(f, " {}", "$")
        } else {
            for symbol in self.symbols.iter() {
                _res = write!(f, " {:?}", symbol);
            }
            write!(f, "")
        }
    }
}

impl<'a, T> PartialEq for &'a Rule<T> {
    fn eq(&self, other: &&'a Rule<T>) -> bool {
        let ptr: *const Rule<T> = *self;
        let ptr_other: *const Rule<T> = *other;
        ptr == ptr_other
    }
}

impl<'a, T> Eq for &'a Rule<T> {}

impl<'a, T> Hash for &'a Rule<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Rule<T> = *self;
        ptr.hash(state)
    }
}

pub struct RuleSet<T> {
    pub rules: Vec<Rule<T>>,
    pub src: Symbol,
}

impl<T> RuleSet<T> {
    pub fn iter(&self) -> std::slice::Iter<Rule<T>> {
        self.rules.iter()
    }
}

impl<T> std::fmt::Debug for RuleSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.rules)
    }
}

impl<T> PartialEq for RuleSet<T> {
    fn eq(&self, other: &RuleSet<T>) -> bool {
        self.src == other.src
    }
}

impl<T> Eq for RuleSet<T> {}

pub struct Grammar<T>(HashMap<Symbol, RuleSet<T>>, Option<Rule<T>>);

impl<T> Grammar<T> {
    pub fn new() -> Self {
        Grammar(HashMap::new(), None)
    }
    pub fn origin(&self) -> &Rule<T> {
        self.1.as_ref().unwrap()
    }
    pub fn insert(&mut self, val: RuleSet<T>) -> &mut Self {
        if self.1.is_none() {
            self.1 = Some(val.rules.into_iter().next().unwrap());
        } else {
            self.0.insert(val.src, val);
        }
        self
    }
    pub fn get_rule_set(&self, src: Symbol) -> &RuleSet<T> {
        if let Some(rule_set) = self.0.get(&src) {
            rule_set
        } else {
            panic!(format!("Use of undeclared rule set: {:?}", src));
        }
    }
    pub fn iter(&self) -> hash_map::Iter<Symbol, RuleSet<T>> {
        self.0.iter()
    }
}

impl<T> std::fmt::Debug for Grammar<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
