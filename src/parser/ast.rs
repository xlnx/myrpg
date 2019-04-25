use pretty::{Doc, *};

use super::rule::Rule;
use super::util::decode;

pub struct Token<'a> {
	pub id: i64,
	pub val: &'a str,
	pub pos: (u32, u32),
}

impl<'a> std::fmt::Debug for Token<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{:?}={:?}", decode(self.id), self.val)
	}
}

pub struct Ast<'a, T> {
	pub(crate) id: i64,
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
		write!(f, "{}", self.to_string(4))
	}
}

impl<'a, T> Ast<'a, T> {
	fn to_doc(&self) -> Doc<BoxDoc<()>> {
		let doc = Doc::Newline.append(Doc::as_string(decode(self.id)));
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
	pub fn to_string(&self, width: usize) -> String {
		let mut w = Vec::new();
		self.to_doc().render(width, &mut w).unwrap();
		String::from_utf8(w).unwrap()
	}
	pub(crate) fn from(id: i64) -> Self {
		Ast {
            id,
            childs: vec![],
            rule: unsafe { &*(0 as * const Rule<T>) }
            // sub_ast: vec![],
            // sub_term: vec![],
        }
	}
}

#[derive(Debug)]
pub enum AstNode<'a, T> {
	Ast(Ast<'a, T>),
	Token(Token<'a>),
}

impl<'a, T> AstNode<'a, T> {
	fn to_doc(&self) -> Doc<BoxDoc<()>> {
		match self {
			AstNode::Ast(ast) => ast.to_doc(),
			AstNode::Token(token) => Doc::Newline
				.append(Doc::as_string(decode(token.id)))
				.append(Doc::as_string("="))
				.append(Doc::as_string(token.val)),
		}
	}
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
