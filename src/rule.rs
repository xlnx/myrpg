use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use super::ast::{Ast, AstNode};
use super::util::{decode, hash};

pub struct Rule<T> {
	pub src: i64,
	pub ast_cnt: u16,
	pub term_cnt: u16,
	pub patts: Vec<i64>,
	pub handler: Box<Fn(&Ast<T>) -> Option<T>>,
}

impl<T> Rule<T> {
	pub fn from(ss: &Vec<&str>, set: &HashSet<i64>) -> Self {
		let mut rule = Rule {
			src: 0,
			ast_cnt: 0,
			term_cnt: 0,
			patts: vec![],
			handler: Box::new(|ast: &Ast<T>| -> Option<T> {
				let mut res = None;
				for ast in ast.childs.iter() {
					if let AstNode::Ast(ast) = ast {
						res = (*ast.rule.handler)(&ast);
					}
				}
				res
			}),
		};
		for s in ss.iter() {
			let ha = hash(s);
			if set.contains(&ha) {
				rule.patts.push(ha);
			} else {
				rule.patts.push(!ha);
			}
		}
		for patt in rule.patts.iter() {
			if *patt <= 0 {
				rule.ast_cnt += 1;
			} else {
				rule.term_cnt += 1;
			}
		}
		rule
	}
	pub fn from_with_handler(
		ss: &Vec<&str>,
		set: &HashSet<i64>,
		handler: Box<Fn(&Ast<T>) -> Option<T>>,
	) -> Self {
		let mut rule = Self::from(ss, set);
		rule.handler = handler;
		rule
	}
}

impl<T> std::fmt::Debug for Rule<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let data = self.patts.iter().map(|x| decode(*x)).collect::<Vec<&str>>();
		write!(f, "Rule {:?}", data)
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

pub struct Param<T> {
	pub rules: Vec<Rule<T>>,
	pub item: i64,
}

impl<T> Param<T> {
	pub fn from(item: &str, mut rules: Vec<Rule<T>>) -> Self {
		let item = !hash(item);
		for rule in rules.iter_mut() {
			rule.src = item;
		}
		Param { item, rules }
	}
}

impl<T> std::fmt::Debug for Param<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(
			f,
			"Param {{ rules: {:?}, item: {:?} }}",
			self.rules,
			decode(self.item)
		)
	}
}