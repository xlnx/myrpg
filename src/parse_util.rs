use std::collections::{BTreeSet, VecDeque};
use std::hash::{Hash, Hasher};

use pretty::{Doc, *};

use crate::ast::{Ast, Token};
use crate::log::{Item as LogItem, Severity, SourceFileLocation};
use crate::rule::{Grammar, Rule};
use crate::symbol::Symbol;
use crate::util::{AsString, ToDoc};

pub struct Item<'a, T> {
    pub rule: &'a Rule<T>,
    pub pos: usize,
}

impl<'a, T> Item<'a, T> {
    pub fn symbol(&self) -> Option<Symbol> {
        if let Some(symbol) = self.rule.symbols.get(self.pos) {
            Some(*symbol)
        } else {
            None
        }
    }
    pub fn is_complete(&self) -> bool {
        self.rule.symbols.len() == self.pos
    }
    pub fn next(&self) -> Option<Item<'a, T>> {
        if self.is_complete() {
            None
        } else {
            let Item { rule, pos } = self;
            Some(Item {
                rule,
                pos: *pos + 1,
            })
        }
    }
}

impl<'a, T> PartialEq for Item<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        let ptr: *const Rule<T> = self.rule;
        let other_ptr: *const Rule<T> = other.rule;
        ptr == other_ptr && self.pos == other.pos
    }
}

impl<'a, T> Eq for Item<'a, T> {}

impl<'a, T> Ord for Item<'a, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let ptr: *const Rule<T> = self.rule;
        let other_ptr: *const Rule<T> = other.rule;
        (ptr, self.pos).cmp(&(other_ptr, other.pos))
    }
}

impl<'a, T> PartialOrd for Item<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a, T> Hash for Item<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Rule<T> = self.rule;
        ptr.hash(state);
        self.pos.hash(state)
    }
}

impl<'a, T> ToDoc for Item<'a, T> {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        let mut item = String::new();
        let mut idx = self.pos as i32;
        for symbol in self.rule.symbols.iter() {
            if idx == 0 {
                item += ". ";
            }
            item += &format!("{:?} ", symbol);
            idx -= 1;
        }
        if idx == 0 {
            item += ". ";
        }
        Doc::as_string(format!("{:?} -> {}", self.rule.src, item))
    }
}

impl<'a, T> std::fmt::Debug for Item<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

pub struct Closure<'a, T>(BTreeSet<Item<'a, T>>, &'a Grammar<T>);

impl<'a, T> Closure<'a, T> {
    pub fn iter(&self) -> std::collections::btree_set::Iter<Item<'a, T>> {
        self.0.iter()
    }
    pub fn into_iter(self) -> std::collections::btree_set::IntoIter<Item<'a, T>> {
        self.0.into_iter()
    }
    pub fn insert(&mut self, item: Item<'a, T>) {
        self.0.insert(item);
    }
    pub fn new(grammar: &'a Grammar<T>) -> Self {
        Closure(BTreeSet::new(), grammar)
    }
    fn expand(&mut self, item: &Item<'a, T>) {
        if !self.0.contains(&item) {
            let Item { rule, pos } = item;
            self.insert(Item { rule, pos: *pos });
            if let Some(x) = rule.symbols.get(*pos) {
                // Some non-terminal
                if x.is_non_terminal() {
                    // X -> ??
                    for rule in self.1.get_rule_set(*x).iter() {
                        self.expand(&Item { rule, pos: 0 });
                    }
                }
            }
        }
    }
    pub fn expanded(&self) -> Closure<'a, T> {
        let mut closure = Closure(BTreeSet::new(), &self.1);
        for item in self.0.iter() {
            closure.expand(item);
        }
        closure
    }
}

impl<'a, T> PartialEq for Closure<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        let ptr: *const Grammar<T> = self.1;
        let other_ptr: *const Grammar<T> = other.1;
        ptr == other_ptr && self.0 == other.0
    }
}

impl<'a, T> Eq for Closure<'a, T> {}

impl<'a, T> Hash for Closure<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Grammar<T> = self.1;
        self.0.hash(state);
        ptr.hash(state)
    }
}

impl<'a, T> ToDoc for Closure<'a, T> {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        Doc::as_string("{")
            .append(Doc::newline())
            .nest(2)
            .append(
                Doc::intersperse(
                    self.0.iter().enumerate().map(|x| {
                        let (_i, item) = x;
                        format!("{:?}", item)
                    }),
                    Doc::Newline,
                )
                .group()
                .nest(2),
            )
            .append(Doc::newline())
            .append(Doc::as_string("}"))
    }
}

impl<'a, T> std::fmt::Debug for Closure<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl<'a, T> ToDoc for Vec<Closure<'a, T>> {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        Doc::as_string("[")
            .append(Doc::newline())
            .append(Doc::intersperse(
                self.iter().enumerate().map(|entry| {
                    let (idx, closure) = entry;
                    Doc::as_string(format!("#{} = ", idx)).append(closure.to_doc())
                }),
                Doc::newline(),
            ))
            .append(Doc::newline())
            .append("]")
    }
}

#[derive(Clone)]
pub struct TextChunk<'a> {
    pub pos: (usize, usize),
    pub text: &'a str,
    pub line: &'a str,
    pub file_name: String,
}

impl<'a> std::convert::From<&'a str> for TextChunk<'a> {
    fn from(text: &'a str) -> Self {
        TextChunk {
            pos: (0, 0),
            text,
            line: text,
            file_name: "<unknown>".into(),
        }
    }
}

pub struct ParsingError<'a> {
    chunk: TextChunk<'a>,
    token: Option<Token<'a>>,
}

impl<'a> std::convert::From<ParsingError<'a>> for LogItem<'a> {
    fn from(item: ParsingError<'a>) -> Self {
        LogItem {
            level: Severity::Error,
            location: Some(SourceFileLocation {
                name: item.chunk.file_name,
                line: item.chunk.line,
                from: if let Some(ref token) = item.token {
                    token.pos.0
                } else {
                    item.chunk.pos
                },
                to: if let Some(ref token) = item.token {
                    token.pos.1
                } else {
                    (item.chunk.pos.0, item.chunk.pos.1 + 1)
                },
            }),
            message: if let Some(ref token) = item.token {
                format!("unexpected token: {:?}", token.val)
            } else {
                format!("unexpected eof")
            },
        }
    }
}

pub struct ParseEnv<'a, T> {
    pub chunk: TextChunk<'a>,
    pub token: Option<Token<'a>>,
    pub states: VecDeque<usize>,
    pub term_stack: VecDeque<Token<'a>>,
    pub ast_stack: VecDeque<Ast<'a, T>>,
}

impl<'a, T> std::convert::From<&'a str> for ParseEnv<'a, T> {
    fn from(text: &'a str) -> Self {
        ParseEnv {
            chunk: TextChunk::from(text),
            token: None,
            states: VecDeque::from(vec![0]),
            term_stack: VecDeque::new(),
            ast_stack: VecDeque::new(),
        }
    }
}

impl<'a, T> ParseEnv<'a, T> {
    pub fn report_error(&self) -> ParsingError<'a> {
        ParsingError {
            chunk: self.chunk.clone(),
            token: self.token.clone(),
        }
    }
}

pub enum Action<'a, T> {
    Accept,
    Reduce(&'a Rule<T>),
    Shift(usize),
}

impl<'a, T> std::fmt::Debug for Action<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let val = match self {
            Action::Accept => String::from("acc"),
            Action::Reduce(_rule) => String::from("r"),
            Action::Shift(dst) => format!("s{}", dst),
        };
        if let Some(width) = f.width() {
            write!(f, "{:<width$}", val, width = width)
        } else {
            write!(f, "{}", val)
        }
    }
}
