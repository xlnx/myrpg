use ptree::*;
use pretty::{Doc, *};
use serde::{
    ser::{SerializeSeq, SerializeStruct, SerializeTuple},
    Serialize, Serializer,
};

use super::formatter::AstFormatter;
use super::rule::Rule;
use super::symbol::Symbol;
use super::util::{AsString, ToDoc};

#[derive(Clone)]
pub struct Token<'a> {
    pub symbol: Symbol,
    pub val: &'a str,
    pub pos: ((usize, usize), (usize, usize)),
}

pub trait Location {
    fn begin(&self) -> (usize, usize);
    fn end(&self) -> (usize, usize);
}

impl<'a> Location for Token<'a> {
    fn begin(&self) -> (usize, usize) {
        self.pos.0
    }
    fn end(&self) -> (usize, usize) {
        self.pos.1
    }
}

impl<'a> Serialize for Token<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let ((a, b), (c, d)) = self.pos;
        let mut seq = serializer.serialize_tuple(3)?;
        {
            seq.serialize_element(&self.symbol)?;
            seq.serialize_element(&self.val)?;
            seq.serialize_element(&(a, b, c, d))?;
        }
        seq.end()
    }
}

impl<'a> Token<'a> {
    pub fn as_str(&self) -> &'a str {
        self.val
    }
    pub fn as_symbol(&self) -> &'static str {
        self.symbol.as_str()
    }
}

impl<'a> std::fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}={:?}", self.symbol, self.val)
    }
}

//#[derive(Serialize)]
pub struct Ast<'a, T> {
    pub symbol: Symbol,
    pub children: Vec<AstNode<'a, T>>,
    pub pos: ((usize, usize), (usize, usize)),
    pub(crate) rule: &'a Rule<T>,
}

impl<'a, T> Location for Ast<'a, T> {
    fn begin(&self) -> (usize, usize) {
        self.pos.0
    }
    fn end(&self) -> (usize, usize) {
        self.pos.1
    }
}

fn serialize_flatten<'a, S, T>(children: &Vec<AstNode<'a, T>>, seq: &mut S::SerializeSeq)
where
    S: Serializer,
{
    for child in children.iter() {
        if let AstNode::Ast(ast) = child {
            if ast.rule.attributes.contains("flatten") {
                serialize_flatten::<S, T>(&ast.children, seq);
            } else {
                seq.serialize_element(&child).unwrap();
            }
        } else {
            seq.serialize_element(&child).unwrap();
        }
    }
}

struct SerializeChildren<'a, 'b, T>(&'a Vec<AstNode<'b, T>>);

impl<'a, 'b, T> Serialize for SerializeChildren<'a, 'b, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(None)?;
        serialize_flatten::<S, T>(&self.0, &mut seq);
        seq.end()
    }
}

impl<'a, T> Serialize for Ast<'a, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self.rule.attributes.contains("flatten") {
            serializer.serialize_some(&SerializeChildren(&self.children))
        } else {
            let mut state = serializer.serialize_struct("Ast", 3)?;
            {
                let ((a, b), (c, d)) = self.pos;
                state.serialize_field("type", &self.symbol)?;
                state.serialize_field("pos", &(a, b, c, d))?;
                state.serialize_field("children", &SerializeChildren(&self.children))?;
            }
            state.end()
        }
    }
}

impl<'a, T> Ast<'a, T> {
    pub fn from(symbol: Symbol, children: Vec<AstNode<'a, T>>, rule: &'a Rule<T>) -> Self {
        let mut pos = (
            (std::usize::MAX, std::usize::MAX),
            (std::usize::MIN, std::usize::MIN),
        );
        for child in children.iter() {
            pos.0 = pos.0.min(child.begin());
            pos.1 = pos.0.max(child.end());
        }
        Ast {
            symbol,
            children,
            rule,
            pos,
        }
    }
    pub fn gen(&self) -> Option<T> {
        (*self.rule.handle_exec)(&self)
    }
    pub fn to_json(&self) -> String {
        serde_json::to_string(&self).unwrap()
    }
    pub fn to_json_pretty(&self) -> String {
        let mut w = Vec::with_capacity(128);
        let fmt = AstFormatter::new();
        //        let fmt = PrettyFormatter::new();
        let mut ser = serde_json::Serializer::with_formatter(&mut w, fmt);
        self.serialize(&mut ser).unwrap();
        let string = unsafe {
            // We do not emit invalid UTF-8.
            String::from_utf8_unchecked(w)
        };
        string
    }
    pub fn print_tree(&self) {
        let json = serde_json::from_str(self.to_json_pretty().as_str()).unwrap();
        let mut builder = TreeBuilder::new("@".to_string());
        match json {
            serde_json::Value::Object(ref map) => {
                to_tree_impl(&json, &mut builder);
            }
            serde_json::Value::Array(ref arr) => {
                for node in arr.iter() {
                    to_tree_impl(&node, &mut builder);
                }
            }
            _ => {}
        }
        let tree = builder.build();
        print_tree(&tree).unwrap();
    }
}

fn to_tree_impl(node: &serde_json::Value, builder: &mut TreeBuilder) {
    match node {
        serde_json::Value::Object(map) => {
            builder.begin_child(map["type"].as_str().unwrap().into());
            let children = map["children"].as_array().unwrap();
            for child in children.iter() {
                to_tree_impl(&child, builder);
            }
            builder.end_child();
        }
        serde_json::Value::Array(arr) => {
            builder.add_empty_child(arr[1].as_str().unwrap().into());
        }
        _ => {}
    }
}

impl<'a, T> std::fmt::Debug for Ast<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl<'a, T> ToDoc for Ast<'a, T> {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        let doc = Doc::Newline.append(Doc::as_string(self.symbol.as_str()));
        if self.children.len() > 0 {
            doc.append(Doc::text("{"))
                .append(
                    Doc::intersperse(self.children.iter().map(|x| x.to_doc()), Doc::Nil)
                        .nest(2)
                        .group(),
                )
                .append(Doc::Newline)
                .append(Doc::text("}"))
        } else {
            doc
        }
    }
}

#[derive(Debug)]
pub enum AstNode<'a, T> {
    Ast(Ast<'a, T>),
    Token(Token<'a>),
}

impl<'a, T> AstNode<'a, T> {
    pub fn is_ast(&self) -> bool {
        if let AstNode::Ast(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_token(&self) -> bool {
        if let AstNode::Token(_) = self {
            true
        } else {
            false
        }
    }
}

impl<'a, T> Location for AstNode<'a, T> {
    fn begin(&self) -> (usize, usize) {
        match self {
            AstNode::Ast(ast) => ast.begin(),
            AstNode::Token(tok) => tok.begin(),
        }
    }
    fn end(&self) -> (usize, usize) {
        match self {
            AstNode::Ast(ast) => ast.end(),
            AstNode::Token(tok) => tok.end(),
        }
    }
}

impl<'a, T> Serialize for AstNode<'a, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match &self {
            AstNode::Ast(ast) => serializer.serialize_some(ast),
            AstNode::Token(tok) => serializer.serialize_some(tok),
        }
    }
}

impl<'a, T> ToDoc for AstNode<'a, T> {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            AstNode::Ast(ast) => ast.to_doc(),
            AstNode::Token(token) => Doc::Newline.append(Doc::as_string(format!("{:?}", token))),
        }
    }
}

impl<'a, T> AstNode<'a, T> {
    pub fn as_ast(&self) -> &Ast<T> {
        if let AstNode::Ast(ast) = &self {
            ast
        } else {
            panic!("failed to unwrap astnode -> ast");
        }
    }
    pub fn as_token(&self) -> &Token {
        if let AstNode::Token(tok) = &self {
            tok
        } else {
            panic!("failed to unwrap astnode -> token");
        }
    }
}
