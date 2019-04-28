use pretty::{Doc, *};


use super::rule::Rule;
use super::symbol::Symbol;
use super::util::{AsString, ToDoc};

pub struct Token<'a> {
	pub(crate) symbol: Symbol,
	pub val: &'a str,
	pub pos: (u32, u32),
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

pub struct Ast<'a, T> {
	pub(crate) symbol: Symbol,
	pub childs: Vec<AstNode<'a, T>>,
	pub(crate) rule: &'a Rule<T>,
}

impl<'a, T> Ast<'a, T> {
	pub fn gen(&self) -> Option<T> {
		(*self.rule.handler)(&self)
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
		if self.childs.len() > 0 {
			doc.append(Doc::text("{"))
				.append(
					Doc::intersperse(self.childs.iter().map(|x| x.to_doc()), Doc::Nil)
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
