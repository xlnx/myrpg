use std::collections::{HashSet, VecDeque};

use pretty::{Doc, *};

use super::ast::{Ast, Token};
use super::rule::Rule;
use super::util::{decode, ToDoc};

pub struct Item<'a, T> {
	pub rule: &'a Rule<T>,
	pub pos: usize,
}

impl<'a, T> std::fmt::Debug for Item<'a, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut item = String::new();
		let mut idx = self.pos as i32;
		for patt in self.rule.patts.iter() {
			if idx == 0 {
				item += ". ";
			}
			item += &format!("{} ", decode(*patt));
			idx -= 1;
		}
		if idx == 0 {
			item += ". ";
		}
		write!(f, "{} -> {}", decode(self.rule.src), item)
	}
}

pub type Closure<'a, T> = HashSet<Item<'a, T>>;

impl<'a, T> ToDoc for Closure<'a, T> {
	fn to_doc(&self) -> Doc<BoxDoc<()>> {
		Doc::Newline.nest(2).append(
			Doc::intersperse(
				self.iter().enumerate().map(|x| {
					let (_i, item) = x;
					format!("{:?}", item)
				}),
				Doc::Newline,
			)
			.nest(2)
			.group(),
		)
	}
}

impl<'a, T> ToDoc for Vec<Closure<'a, T>> {
	fn to_doc(&self) -> Doc<BoxDoc<()>> {
		Doc::as_string("Vec<Closure>").append(Doc::Newline).append(
			Doc::intersperse(self.iter().enumerate()
			.map(|x| {
				let (i, item) = x;
				item.to_doc();
				Doc::as_string(format!("{}", i))
				.append(item.to_doc())
			}), Doc::Newline)
				// .nest(2)
				// .group(),
		)
	}
}

#[derive(Debug)]
pub struct ParseEnv<'a, T> {
	pub tokens: VecDeque<Token<'a>>,
	pub states: VecDeque<usize>,
	pub term_stack: VecDeque<Token<'a>>,
	pub ast_stack: VecDeque<Ast<'a, T>>,
	pub new_asts: VecDeque<(Ast<'a, T>, i64)>,
}

impl<'a, T> ParseEnv<'a, T> {
	pub fn new() -> Self {
		ParseEnv {
			tokens: VecDeque::new(),
			states: VecDeque::new(),
			term_stack: VecDeque::new(),
			ast_stack: VecDeque::new(),
			new_asts: VecDeque::new(),
		}
	}
}

#[derive(Debug)]
pub enum ActionType {
	Accept,
	MoveIn,
	Reduce,
	Hold,
}

pub struct Action<'a, T> {
	pub flag: ActionType,
	pub rule: Option<&'a Rule<T>>,
}

impl<'a, T> std::fmt::Debug for Action<'a, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(
			f,
			"Action {{ flag: {:?}, rule: {:?} }}",
			self.flag, self.rule
		)
	}
}

impl<'a, T> Action<'a, T> {
	pub fn from(ty: ActionType) -> Self {
		Action {
			flag: ty,
			rule: None,
		}
	}
}