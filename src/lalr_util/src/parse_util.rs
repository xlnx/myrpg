use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};

use pretty::{Doc, *};

use crate::ast::{Ast, Token};
use crate::log::{LogItem, Severity, SourceFileLocation, SourceLocationProvider};
use crate::rule::{Grammar, Rule};
use crate::symbol::Symbol;
use crate::util::{AsString, ToDoc};

pub struct Item<'a, T> {
    pub rule: &'a Rule<T>,
    pub pos: usize,
    pub la: HashSet<Symbol>,
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
            let Item { rule, pos, la } = self;
            Some(Item {
                rule,
                pos: *pos + 1,
                la: la.clone(),
            })
        }
    }
    pub fn gt_some_what(&self, other: &Self) -> bool {
        self.la.difference(&other.la).next().is_some()
    }
    pub fn insert(&mut self, other: Self) {
        self.la = self.la.union(&other.la).map(|x| *x).collect();
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
        Doc::as_string(format!("{:?} -> {}, {:?}", self.rule.src, item, self.la))
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
    pub fn take(&mut self, item: &Item<'a, T>) -> Option<Item<'a, T>> {
        self.0.take(&item)
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
    fn expand(&mut self, item: &Item<'a, T>, first: &HashMap<Symbol, HashSet<Symbol>>) -> bool {
        let la = if let Some(old_item) = self.0.get(&item) {
            // println!("{:?}", item);
            // println!("{:?}", );
            // la has no item.
            if !item.gt_some_what(&old_item) {
                return false;
            }
            self.0
                .take(&item)
                .unwrap()
                .la
                .union(&item.la)
                .map(|x| *x)
                .collect()
        } else {
            item.la.clone()
        };
        let Item { rule, pos, .. } = item;
        self.insert(Item {
            rule,
            pos: *pos,
            la: la.clone(),
        });
        if let Some(x) = rule.symbols.get(*pos) {
            // Some non-terminal
            if x.is_non_terminal() {
                // X -> ??
                let la = if rule.symbols.len() - 1 == *pos {
                    la.clone()
                } else {
                    let next = rule.symbols.get(*pos + 1).unwrap();
                    first.get(next).unwrap().clone()
                };
                for rule in self.1.get_rule_set(*x).iter() {
                    self.expand(
                        &Item {
                        rule,
                        pos: 0,
                        la: la.clone() //: HashSet::new(),
                    },
                        first,
                    );
                }
            }
        }
        true
    }
    pub fn expanded(&self, first: &HashMap<Symbol, HashSet<Symbol>>) -> Closure<'a, T> {
        let mut closure = Closure(BTreeSet::new(), &self.1);
        for item in self.0.iter() {
            closure.expand(item, first);
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
    pub text: &'a str,
    pub offset: usize,
    pub dx: usize,
    pub pos: (usize, usize),
}

impl<'a> TextChunk<'a> {
    pub fn get_line(&self) -> &'a str {
        &self.text[self.text[..self.offset].rfind('\n')
            .map_or(0, |x| x+1)..]
    }
}

impl<'a> std::convert::From<&'a str> for TextChunk<'a> {
    fn from(text: &'a str) -> Self {
        TextChunk {
            text,
            offset: 0,
            dx: 0,
            pos: (0, 0),
        }
    }
}

pub struct ParsingError<'a> {
    provider: SourceLocationProvider<'a>,   // source file
    from: (usize, usize),        // source file location
    to: (usize, usize),        // source file location
    token: Result<Option<Token<'a>>, ()>,
}

impl<'a> std::convert::From<ParsingError<'a>> for LogItem<'a> {
    fn from(item: ParsingError<'a>) -> Self {
        let ParsingError{
            provider,
            from,
            to,
            token
        } = item;
        LogItem {
            level: Severity::Error,
            location: Some(SourceFileLocation{ provider, from, to }),
            message: match token {
                Ok(Some(token)) => format!("unexpected token: {:?}", token.val),
                Ok(None) => format!("unexpected eof"),
                _ => format!("unknown stray: ")
            },
        }
    }
}

pub struct SourceFileMark {
    pub name: String,
    pub raw_pos: (usize, usize),     // loc of end of the mark in raw file
    pub src_pos: (usize, usize)      // loc in source file
}

pub struct ParseEnv<'a, T> {
    pub sources: Vec<SourceFileMark>,
    pub chunk: TextChunk<'a>,
    pub token: Result<Option<Token<'a>>, ()>,
    pub states: VecDeque<usize>,
    pub term_stack: VecDeque<Token<'a>>,
    pub ast_stack: VecDeque<Ast<'a, T>>,
}

impl<'a, T> std::convert::From<&'a str> for ParseEnv<'a, T> {
    fn from(text: &'a str) -> Self {
        ParseEnv {
            sources: vec![],
            chunk: TextChunk::from(text),
            token: Ok(None),
            states: VecDeque::from(vec![0]),
            term_stack: VecDeque::new(),
            ast_stack: VecDeque::new(),
        }
    }
}

fn calc_pos(x: &(usize, usize), an: &(usize, usize), dx: &(usize, usize)) -> (usize, usize) {
    if x.0 == an.0 {
        // same line with file mark
        (x.0 - an.0 + dx.0, x.1 - an.1 + dx.1)
    } else {
        (x.0 - an.0 + dx.0, x.1)
    }
}

pub trait SourceMap<'a> {
    fn map_error(&self, begin: &(usize, usize), end: &(usize, usize))
        ->
        Option<(SourceLocationProvider<'a>, (usize, usize), (usize, usize))>;
}

fn find_by_raw_src_range(this: &Vec<SourceFileMark>, row: usize, col: usize) -> Option<&SourceFileMark> {
    let x = this.binary_search_by(|mark| {
        mark.raw_pos.cmp(&(row, col))
    });
    let x = match x {
        Err(x) => if let 0 = x {
            return None
        } else {
            x - 1       // start
        },
        Ok(x) => x
    };
    this.get(x)
}

impl<'a> SourceMap<'a> for Vec<SourceFileMark> {
    fn map_error(&self, begin: &(usize, usize), end: &(usize, usize))
        ->
        Option<(SourceLocationProvider<'a>, (usize, usize), (usize, usize))>
    {
        if let Some(SourceFileMark{
            name: ref src,
            raw_pos: an,
            src_pos: dx
        }) = find_by_raw_src_range(&self, begin.0, begin.1) {
            let from= calc_pos(begin, &an, &dx);
            let to= calc_pos(end, &an, &dx);
            Some((SourceLocationProvider::File(src.clone()), from, to))
        } else {
            None
        }
    }
}

impl<'a, T> ParseEnv<'a, T> {
    pub fn report_error(&self) -> ParsingError<'a> {
        let (provider, from, to) = match self.token {
            Ok(Some(ref token)) => {
                self.sources.map_error(&token.pos.0, &token.pos.1)
                    .unwrap_or((
                        SourceLocationProvider::Line(self.chunk.get_line()),
                        token.pos.0,
                        token.pos.1
                    ))
            }
            _ => {
                self.sources.map_error(&self.chunk.pos, &self.chunk.pos)
                    .unwrap_or((
                        SourceLocationProvider::Line(self.chunk.get_line()),
                        self.chunk.pos,
                        self.chunk.pos
                    ))
            }
        };
        let token = self.token.clone();
        ParsingError { provider, from, to, token }
    }
}

pub enum Action<'a, T> {
    Accept,
    Reduce(&'a Rule<T>),
    Shift(usize),
}

#[derive(Debug)]
pub enum CompactAction {
    Accept,
    Reduce(usize),
    Shift(usize),
}

impl<'a, T> std::fmt::Debug for Action<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let val = match self {
            Action::Accept => String::from("acc"),
            Action::Reduce(rule) => format!("{:?}", rule),
            Action::Shift(dst) => format!("s{}", dst),
        };
        if let Some(width) = f.width() {
            write!(f, "{:<width$}", val, width = width)
        } else {
            write!(f, "{}", val)
        }
    }
}
