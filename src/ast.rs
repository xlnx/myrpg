use pretty::{Doc, *};
use serde::{Serialize, Serializer, ser::{SerializeStruct, SerializeTuple} };


use super::rule::Rule;
use super::symbol::Symbol;
use super::util::{AsString, ToDoc};
use super::formatter::{AstFormatter};

#[derive(Clone)]
pub struct Token<'a> {
	pub symbol: Symbol,
	pub val: &'a str,
	pub pos: (u32, u32),
}

impl<'a> Serialize for Token<'a> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
		where
			S: Serializer {
		let mut seq = serializer.serialize_tuple(2 )?;
		{
			seq.serialize_element(&self.symbol)?;
			seq.serialize_element(&self.val)?;
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
	pub(crate) rule: &'a Rule<T>,
}

impl<'a, T> Serialize for Ast<'a, T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer {
		let mut state = serializer.serialize_struct("Ast", 2)?;
		{
			state.serialize_field("type", &self.symbol)?;
			state.serialize_field("children", &self.children)?;
		}
		state.end()
	}
}

impl<'a, T> Ast<'a, T> {
	pub fn gen(&self) -> Option<T> {
		(*self.rule.handle_exec)(&self)
	}
	pub fn to_json(&self) -> String {
		serde_json::to_string(&self).unwrap()
	}
	pub fn to_json_pretty(&self) -> String {
		let mut w = Vec::with_capacity(128);
		let fmt = AstFormatter::new();
		let mut ser = serde_json::Serializer::with_formatter(&mut w, fmt);
		self.serialize(&mut ser).unwrap();
		let string = unsafe {
			// We do not emit invalid UTF-8.
			String::from_utf8_unchecked(w)
		};
		string
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

impl<'a, T> Serialize for AstNode<'a, T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
		where
			S: Serializer {
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
