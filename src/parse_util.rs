use std::collections::{BTreeSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::io::Write;

use pretty::{Doc, *};

use super::ast::{Ast, Token};
use super::rule::{Grammar, Rule};
use super::symbol::Symbol;
use super::util::{AsString, ToDoc};
use colored::Colorize;

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
    pub pos: (u32, u32),
    pub text: &'a str,
    pub line: &'a str,
}

impl<'a> TextChunk<'a> {
    pub fn from(text: &'a str) -> Self {
        TextChunk {
            pos: (0, 0),
            text,
            line: text,
        }
    }
}

pub struct ParsingError<'a> {
    chunk: TextChunk<'a>,
    token: Option<Token<'a>>,
}

impl<'a> std::fmt::Debug for ParsingError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

fn replace_tab(text: &str, anchors: &mut [usize]) -> String {
    const TAB_SIZE: usize = 2;

    let mut res = String::with_capacity(128);
    let mut anchor_idx: usize = 0;
    let mut dx: usize = 0;
    for (idx, ch) in text.chars().enumerate() {
        if ch == '\t' {
            for _j in 0..TAB_SIZE {
                res.push(' ');
            }
            while anchor_idx < anchors.len() && anchors[anchor_idx] <= idx {
                anchors[anchor_idx] += dx;
                anchor_idx += 1;
            }
            dx += TAB_SIZE - 1;
        } else {
            res.push(ch);
        }
    }
    while anchor_idx < anchors.len() {
        anchors[anchor_idx] += dx;
        anchor_idx += 1;
    }
    res
}

impl<'a> ParsingError<'a> {
    pub fn as_string(&self) -> String {
        let mut res = Vec::new();
        let line = String::from(
            if let Some(pos) = self.chunk.line.find(|c: char| c == '\n') {
                &self.chunk.line[..pos]
            } else {
                self.chunk.line
            },
        );
        let trimmed = line.trim_end();

        let (begin, end, msg) = if let Some(Token { val, pos, .. }) = self.token {
            (
                pos.1 as usize,
                self.token.as_ref().unwrap().val.len() + pos.1 as usize,
                format!("{}:{}: Unexpected Token: {:?}", pos.0, pos.1, val),
            )
        } else {
            (
                trimmed.len(),
                trimmed.len() + 1,
                format!("{}:{}: Unexpected EOF", self.chunk.pos.0, self.chunk.pos.1),
            )
        };

        let mut pos = [begin, end];
        let trimmed = replace_tab(trimmed, &mut pos);

        //        let (begin, end) = (pos[0], pos[1]);
        let [begin, end] = pos;

        writeln!(res, "{}", msg).unwrap();

        write!(
            res,
            " {:>4} |{}",
            self.chunk.pos.0 + 1,
            &trimmed.as_str()[..begin]
        )
        .unwrap();
        write!(res, "{}", &trimmed.as_str()[begin..end].red()).unwrap();
        writeln!(res, "{}", &trimmed.as_str()[end..]).unwrap();
        write!(
            res,
            " {:>4} |{}",
            "",
            std::iter::repeat(' ').take(begin).collect::<String>()
        )
        .unwrap();
        write!(res, "{}", "^".red()).unwrap();
        writeln!(
            res,
            "{}",
            std::iter::repeat('~')
                .take(end - begin - 1)
                .collect::<String>()
        )
        .unwrap();
        //        writeln!(
        //            res,
        //            "{}:{}: Unexpected EOF",
        //            self.chunk.pos.0, self.chunk.pos.1
        //        )
        //            .unwrap();
        //        res
        String::from_utf8(res).unwrap()
    }
    //    pub fn write_to(&self, writer: &mut Write) -> io::Result<()> {
    //        let line = String::from(
    //            if let Some(pos) = self.chunk.line.find(|c: char| c == '\n') {
    //                &self.chunk.line[..pos]
    //            } else {
    //                self.chunk.line
    //            },
    //        );
    //        let trimmed = line.trim_end();
    //        writeln!(writer, "{}", trimmed)?;
    //        if let Some(Token { val, pos, .. }) = self.token {
    //            write!(
    //                writer,
    //                "{}^",
    //                std::iter::repeat(' ')
    //                    .take(pos.1 as usize)
    //                    .collect::<String>()
    //            )?;
    //            writeln!(
    //                writer,
    //                "{}",
    //                std::iter::repeat('~')
    //                    .take(self.token.as_ref().unwrap().val.len() - 1)
    //                    .collect::<String>()
    //            )?;
    //            writeln!(writer, "{}:{}: Unexpected Token: {:?}", pos.0, pos.1, val)?;
    //        } else {
    //            writeln!(
    //                writer,
    //                "{}^",
    //                std::iter::repeat(' ')
    //                    .take(trimmed.len())
    //                    .collect::<String>()
    //            )?;
    //            writeln!(
    //                writer,
    //                "{}:{}: Unexpected EOF",
    //                self.chunk.pos.0, self.chunk.pos.1
    //            )?;
    //        }
    //
    //        Ok(())
    //    }
}

pub struct ParseEnv<'a, T> {
    pub chunk: TextChunk<'a>,
    pub token: Option<Token<'a>>,
    pub states: VecDeque<usize>,
    pub term_stack: VecDeque<Token<'a>>,
    pub ast_stack: VecDeque<Ast<'a, T>>,
}

impl<'a, T> ParseEnv<'a, T> {
    pub fn from(text: &'a str) -> Self {
        ParseEnv {
            chunk: TextChunk::from(text),
            token: None,
            states: VecDeque::from(vec![0]),
            term_stack: VecDeque::new(),
            ast_stack: VecDeque::new(),
        }
    }
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
