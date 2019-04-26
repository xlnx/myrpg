use std::collections::{HashSet, VecDeque};

use super::ast::{Ast, Token};
use super::rule::Rule;

#[derive(Debug)]
pub struct Item<'a, T> {
	pub rule: &'a Rule<T>,
	pub pos: usize,
}

pub type Closure<'a, T> = HashSet<Item<'a, T>>;

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

#[derive(Debug)]
pub struct Action<'a, T> {
	pub flag: ActionType,
	pub rule: Option<&'a Rule<T>>,
}

impl<'a, T> Action<'a, T> {
	pub fn from(ty: ActionType) -> Self {
		Action {
			flag: ty,
			rule: None,
		}
	}
}