use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use regex::{Regex, RegexSet};

mod util;
use util::{hash, BOTTOM, EPS};

pub mod ast;
use ast::{Ast, AstNode, Token};

pub mod wrapper;

mod rule;
use rule::{Param, Rule};

mod parse_util;
use parse_util::{Action, ActionType, Closure, Item, ParseEnv};

pub mod lang;

mod index;
use index::IndexMutOrInsert;

fn make_first<T>(first: &mut HashMap<i64, HashSet<i64>>, params: &Vec<Param<T>>) {
	let mut add_sub: bool;
	loop {
		add_sub = false;
		for param in params.iter() {
			// i am curr param
			let my_firsts = first.get(&param.item).unwrap();
			let mut new_firsts = vec![];
			// each rule generated my me
			for rule in param.rules.iter() {
				// this rule generates eps
				let mut has_empty = false;
				// each elem in this rule
				for elem in rule.patts.iter() {
					has_empty = false;
					let firsts = first.get(elem).unwrap();
					// each elem of first[elem]
					for first_elem in firsts.iter() {
						if *first_elem == EPS {
							// this elem generates eps
							has_empty = true;
						} else if !my_firsts.contains(first_elem) {
							new_firsts.push(*first_elem);
						}
					}
					// this elem cant generate eps
					if !has_empty {
						break;
					}
				}
				// this rule generates eps
				if has_empty && !my_firsts.contains(&EPS) {
					new_firsts.push(EPS);
				}
			}
			// now add new firsts into first[me]
			let my_firsts_mut = first.get_mut(&param.item).unwrap();
			for first_elem in new_firsts.iter() {
				my_firsts_mut.insert(*first_elem);
				add_sub = true;
			}
		}
		// first closure is full
		if !add_sub {
			break;
		}
	}
}

fn make_follow<T>(
	first: &HashMap<i64, HashSet<i64>>,
	follow: &mut HashMap<i64, HashSet<i64>>,
	params: &Vec<Param<T>>,
) {
	let mut add_sub: bool;
	loop {
		add_sub = false;
		for param in params.iter() {
			for rule in param.rules.iter() {
				for (curr_elem, prev_elem) in rule
					.patts
					.iter()
					.skip(1)
					.rev()
					.zip(rule.patts.iter().rev().skip(1))
				{
					if *prev_elem < 0i64 {
						let mut new_follows = vec![];
						if *curr_elem < 0i64 {
							let curr_first = first.get(curr_elem).unwrap();
							let prev_follow = follow.get(prev_elem).unwrap();

							// add all first[curr] to follow[prev]
							for first_elem in curr_first.iter() {
								if *first_elem != EPS && !prev_follow.contains(first_elem) {
									new_follows.push(*first_elem);
								}
							}
							// if this elem yields eps
							if curr_first.contains(&EPS) {
								// add all follow[me] to follow[prev]
								for follow_elem in follow.get(&param.item).unwrap() {
									if !prev_follow.contains(&follow_elem) {
										new_follows.push(*follow_elem);
									}
								}
							}
						} else if !follow.get(prev_elem).unwrap().contains(curr_elem) {
							new_follows.push(*curr_elem);
						}

						// add all new follows to follow[prev]
						let prev_follow_mut = follow.get_mut(prev_elem).unwrap();
						for follow in new_follows {
							prev_follow_mut.insert(follow);
							add_sub = true;
						}
					}
				}

				let back = rule.patts.iter().rev().next().unwrap();
				let back_follow = follow.get(back).unwrap();
				let mut new_follows = vec![];
				if *back < 0i64 {
					for follow_elem in follow.get(&param.item).unwrap() {
						if !back_follow.contains(follow_elem) {
							new_follows.push(*follow_elem);
						}
					}
				}
				let back_follow_mut = follow.get_mut(back).unwrap();
				for follow in new_follows {
					back_follow_mut.insert(follow);
					add_sub = true;
				}
			}
		}
		// this closure follow is full
		if !add_sub {
			break;
		}
	}
}

pub trait LRLang {
	type Output;

	fn new<'a>() -> (
		Vec<(&'a str, &'a str)>,
		Vec<(
			&'a str,
			Vec<(
				Vec<&'a str>,
				Option<Box<Fn(&Ast<Self::Output>) -> Option<Self::Output>>>,
			)>,
		)>,
	);
}

#[allow(dead_code)]
pub struct LRParser<'a, T: LRLang> {
	lex_rules: Vec<(i64, Regex)>,
	lex_rules_set: RegexSet,
	params: Vec<Param<T::Output>>,
	terms: Vec<i64>,
	first: HashMap<i64, HashSet<i64>>,
	follow: HashMap<i64, HashSet<i64>>,
	parent_of: HashMap<&'a Rule<T::Output>, i64>,
	param_of: HashMap<i64, &'a Param<T::Output>>,
	index_of: HashMap<&'a Rule<T::Output>, usize>,
	closures: Vec<Closure<'a, T::Output>>,
	action: Vec<HashMap<i64, Action<'a, T::Output>>>,
	goto: Vec<HashMap<i64, usize>>,
	phantom: PhantomData<T>,
}

struct TextChunk<'a> {
	text: &'a str,
}

impl<'a> TextChunk<'a> {
	fn from(text: &'a str) -> Self {
		TextChunk { text }
	}
}

impl<'a, T> LRParser<'a, T>
where
	T: LRLang,
{
	fn find_empty(&self, val: i64) -> Option<&'a Rule<T::Output>> {
		let param = self.param_of.get(&val).unwrap();
		for rule in param.rules.iter() {
			if rule.patts.len() == 1 && rule.patts[0] == EPS {
				return Some(rule);
			} else {
				let mut is_empty = true;
				for elem in rule.patts.iter() {
					is_empty = self.first.get(&elem).unwrap().contains(&EPS);
					if !is_empty {
						break;
					}
				}
				if is_empty && rule.patts[0] != val {
					return self.find_empty(rule.patts[0]);
				}
			}
		}
		None
	}
	fn register_sub(&mut self, val: i64, rule: Option<&'a Rule<T::Output>>, state: usize) {
		if let None = self.action[state].get(&val) {
			self.action[state].insert(
				val,
				Action {
					flag: ActionType::Hold,
					rule: rule,
				},
			);
			self.goto[state].insert(val, self.closures.len());
			if let Some(param) = self.param_of.get(&val) {
				for rule in param.rules.iter() {
					for elem in rule.patts.iter() {
						self.register_sub(*elem, Some(rule), state);
						if !self.first[elem].contains(&EPS) {
							break;
						}
					}
				}
			}
		}
	}

	fn expand_closure(&mut self, closure: &Closure<T::Output>, state: usize) {
		for item in closure.iter() {
			if item.pos > 0
				&& self
					.first
					.get(&item.rule.patts[item.pos - 1])
					.unwrap()
					.contains(&EPS)
			{
				if item.rule.patts.len() != item.pos {
					self.register_sub(
						item.rule.patts[item.pos],
						self.find_empty(item.rule.patts[item.pos - 1]),
						state,
					);
				} else {
					for i in 0..self.terms.len() {
						let term = self.terms[i];
						if let None = self.action[state].get(&term) {
							let rule = self.find_empty(item.rule.patts[item.pos - 1]);
							self.action[state].insert(
								term,
								Action {
									flag: ActionType::Hold,
									rule: rule,
								},
							);
							self.goto[state].insert(term, self.closures.len());
						}
					}
				}
			}
		}
	}

	fn make_closure(&mut self, mut closure: Closure<'a, T::Output>, state: usize) {
		self.expand_closure(&closure, state);

		let mut gen_sub: bool;
		loop {
			gen_sub = false;

			let mut items = vec![];
			for item in closure.iter() {
				if item.rule.patts.len() != item.pos {
					let mut pos = item.pos;
					loop {
						if let Some(param) = self.param_of.get(&item.rule.patts[pos]) {
							for rule in param.rules.iter() {
								if rule.patts.len() != 1 || rule.patts[0] != EPS {
									let item = Item { rule: rule, pos: 0 };
									if !closure.contains(&item) {
										items.push(item);
									}
								}
							}
						}
						if !self
							.first
							.get(&item.rule.patts[pos])
							.unwrap()
							.contains(&EPS) || {
							pos += 1;
							pos
						} == item.pos
						{
							break;
						}
					}
				}
			}
			for item in items.into_iter() {
				closure.insert(item);
				gen_sub = true;
			}

			if !gen_sub {
				break;
			}
		}

		// println!("===>   {:?}", closure);
		self.closures.push(closure);
		self.goto.push(HashMap::new());
		self.action.push(HashMap::new());
	}

	fn init(&mut self) {
		let mut origin: Closure<T::Output> = Closure::new();
		unsafe {
			let rule: *const Rule<T::Output> = &self.params[0].rules[0];
			origin.insert(Item {
				rule: &*rule,
				pos: 0,
			});
		}
		self.make_closure(origin, 0);

		let mut add_sub: bool;
		loop {
			add_sub = false;
			for state in 0..self.closures.len() {
				for item in self.closures[state].iter() {
					if item.rule.patts.len() == item.pos {
						for elem in self
							.follow
							.get(&self.parent_of.get(&item.rule).unwrap())
							.unwrap()
							.iter()
						{
							if {
								if let Some(Action {
									flag: ActionType::MoveIn,
									..
								}) = self.action[state].get(elem)
								{
									false
								} else {
									true
								}
							} {
								self.action[state].insert(
									*elem,
									Action {
										flag: if item.rule as *const Rule<T::Output>
											== &self.params[0].rules[0]
										{
											ActionType::Accept
										} else {
											ActionType::Reduce
										},
										rule: Some(item.rule),
									},
								);
							}
						}
					}
				}
				for i in 0..self.terms.len() {
					let term = self.terms[i];
					if {
						if let None = self.goto[state].get(&term) {
							true
						} else {
							false
						}
					} || if let Some(Action {
						flag: ActionType::Hold,
						..
					}) = self.action[state].get(&term)
					{
						true
					} else {
						false
					} {
						let mut new = Closure::new();
						for item in self.closures[state].iter() {
							if item.pos != item.rule.patts.len()
								&& item.rule.patts[item.pos] == term
							{
								// println!("Here {}", state);
								new.insert(Item {
									rule: item.rule,
									pos: item.pos + 1,
								});
							}
						}
						if !new.is_empty() {
							let mut is_sub = false;
							let mut rule_ptr = None;

							let mut dest_state: usize = 0;
							for dest_i in 0..self.closures.len() {
								is_sub = true;
								for item in new.iter() {
									if rule_ptr.is_none()
										|| self.index_of.get(&item.rule).unwrap()
											< self.index_of.get(&rule_ptr.unwrap()).unwrap()
									{
										rule_ptr = Some(item.rule);
									}
									if !self.closures[dest_i].contains(&item) {
										is_sub = false;
										break;
									}
								}
								dest_state = dest_i;
								if is_sub {
									break;
								}
							}
							if is_sub {
								self.goto[state].insert(term, dest_state);
								self.action[state].insert(term, Action::from(ActionType::MoveIn));
								let this: *const Self = self;
								unsafe {
									self.expand_closure(&(*this).closures[dest_state], state);
								}
							} else {
								self.goto[state].insert(term, self.closures.len());
								self.make_closure(new, state);
								add_sub = true;
								match self.action[state].get(&term) {
									Some(val) => match val.flag {
										ActionType::Accept => panic!(),
										ActionType::MoveIn => panic!(),
										ActionType::Reduce => {
											self.action[state].insert(
												term,
												Action {
													flag: ActionType::MoveIn,
													rule: rule_ptr,
												},
											);
										}
										_ => {}
									},
									None => {
										self.action[state]
											.insert(term, Action::from(ActionType::MoveIn));
									}
								}
							}
						}
					}
				}
			}

			if !add_sub {
				break;
			}
		}
	}

	pub fn new() -> Self {
		let (lex, lang) = T::new();

		// make terms
		let mut lex_rules: HashMap<_, String> = HashMap::new();
		let mut terms = vec![];
		for (lex_name, lex_patt) in lex.iter() {
			let ha = hash(lex_name);
			terms.push(ha);
			lex_rules.insert(ha, format!(r"^\s*({})", lex_patt));
		}
		let terms_set: HashSet<i64> = terms.iter().map(|x| *x).collect();

		// make params
		let mut params = vec![];
		for (lang_item, lang_rules) in lang.into_iter() {
			let mut rules = vec![];
			for (lang_patts, lang_cb) in lang_rules.into_iter() {
				let ss = lang_patts;
				let rule = if let Some(cb) = lang_cb {
					Rule::from_with_handler(&ss, &terms_set, cb)
				} else {
					Rule::from(&ss, &terms_set)
				};
				rules.push(rule);
			}
			let param = Param::from(lang_item, rules);
			params.push(param);
		}

		let mut parent_of = HashMap::new();
		let mut index_of = HashMap::new();

		let mut first: HashMap<i64, HashSet<i64>> = HashMap::new();
		let mut follow: HashMap<i64, HashSet<i64>> = HashMap::new();

		// insert terms
		for term in terms.iter() {
			first.index_mut_or_insert(*term).insert(*term);
			follow.index_mut_or_insert(*term);
		}
		// insert params
		for param in params.iter() {
			for rule in param.rules.iter() {
				for patt in rule.patts.iter() {
					first.index_mut_or_insert(*patt);
					if *patt < 0 {
						follow.index_mut_or_insert(*patt).insert(BOTTOM);
					}
				}
			}
			first.index_mut_or_insert(param.item);
			follow.index_mut_or_insert(param.item).insert(BOTTOM);
			terms.push(param.item);
		}
		// insert eps
		first.index_mut_or_insert(EPS).insert(EPS);
		first.index_mut_or_insert(!EPS).insert(EPS);

		// make first
		make_first(&mut first, &params);
		// make follow
		make_follow(&first, &mut follow, &params);

		terms.push(BOTTOM);

		// index using params::rule!
		let mut index: usize = 0;
		let mut param_of = HashMap::<i64, &'a Param<T::Output>>::new();
		for param in params.iter() {
			for rule in param.rules.iter() {
				unsafe {
					let rule_ptr: *const Rule<T::Output> = rule;
					parent_of.insert(&*rule_ptr, param.item);
					index_of.insert(&*rule_ptr, index);
					index += 1;
				}
			}
			unsafe {
				let param_ptr: *const Param<T::Output> = param;
				param_of.insert(param.item, &*param_ptr);
			}
		}

		let set = RegexSet::new(lex_rules.iter().map(|x| x.1)).unwrap();
		let le: Vec<(i64, Regex)> = lex_rules
			.iter()
			.map(|x| (*x.0, Regex::new(x.1).unwrap()))
			.collect();

		let mut parser = LRParser {
			lex_rules: le,
			lex_rules_set: set,
			params: params,
			terms: terms,
			first: first,
			follow: follow,
			parent_of: parent_of,
			index_of: index_of,
			param_of: param_of,
			closures: vec![],
			action: vec![],
			goto: vec![],
			phantom: PhantomData,
		};

		parser.init();

		// println!("CLOSURE = {:?}", parser.closures);
		// println!("ACTION = {:?}", parser.action);
		// println!("GOTO = {:?}", parser.goto);

		parser
	}

	fn next<'b>(&self, chunk: &mut TextChunk<'b>) -> Option<Token<'b>> {
		// println!("{:?}", self.lex_rules);
		// println!("{:?}", self.lex_rules_set);
		// self.lex_rules_set.ma
		for (val, rule) in self.lex_rules.iter() {
			if let Some(caps) = rule.captures(chunk.text) {
				let m = caps.get(1).unwrap();
				let (s, t) = (m.start(), m.end());
				let tok = Token {
					id: *val,
					val: &chunk.text[s..t],
					pos: (0, 0),
				};
				// println!("yield ====> {:?}", tok);
				chunk.text = &chunk.text[t..];
				return Some(tok);
			}
		}

		None
	}

	fn do_merge(&self, rule: &'a Rule<T::Output>, env: &mut ParseEnv<'a, T::Output>) {
		let ParseEnv {
			ast_stack,
			new_asts,
			term_stack,
			states,
			..
		} = env;

		let ast_size = rule.ast_cnt as usize;
		let term_size = rule.term_cnt as usize;

		let len = ast_stack.len();
		let sub_ast: Vec<_> = ast_stack.split_off(len - ast_size).into_iter().collect();
		let len = term_stack.len();
		let sub_term: Vec<_> = term_stack.split_off(len - term_size).into_iter().collect();
		let len = states.len();
		states.split_off(len - ast_size - term_size);
		let id = *self.parent_of.get(&rule).unwrap();

		let mut childs = vec![];
		let mut it_ast = sub_ast.into_iter();
		let mut it_term = sub_term.into_iter();

		for dummy in rule.patts.iter() {
			if *dummy <= 0i64 {
				childs.push(AstNode::Ast(it_ast.next().unwrap()));
			} else {
				childs.push(AstNode::Token(it_term.next().unwrap()));
			}
		}

		let ast = Ast {
            id,
            childs,
            rule
            // sub_ast,
            // sub_term,
        };
		// println!(
		//     "yields => {:?}",
		//     ast // decode(id)
		// );
		new_asts.push_back((ast, *self.parent_of.get(&rule).unwrap()));
	}

	fn do_match(&self, term: i64, env: &mut ParseEnv<'a, T::Output>) -> bool {
		let ParseEnv {
			states,
			ast_stack,
			new_asts,
			tokens,
			term_stack,
			..
		} = env;
		let state = *states.back().unwrap();
		// println!("");
		// println!("ast_stack = {:?}", ast_stack);
		// // let term_vec: Vec<_> = term_stack.iter().map(|x|decode(*x)).collect();
		// println!("term_stack = {:?}", term_stack);
		// println!(
		//     "new_asts = {:?}",
		//     new_asts.iter().map(|x| &x.0).collect::<Vec<&Ast>>()
		// );
		// println!("states = {:?}", states);
		// // println!("action = {:?}", self.action[state]);
		// println!(
		//     "ACTION[{:?}][{:?}] = {:?}",
		//     state,
		//     decode(term),
		//     self.action[state].get(&term)
		// );
		match self.action[state].get(&term) {
			Some(action) => match action.flag {
				ActionType::MoveIn => {
					states.push_back(*self.goto[state].get(&term).unwrap());
					if term >= 0 {
						term_stack.push_back(tokens.pop_front().unwrap());
					} else {
						ast_stack.push_back(new_asts.pop_back().unwrap().0);
					}
				}
				ActionType::Hold => {
					states.push_back(0);
					ast_stack.push_back(Ast::from(0));
					self.do_merge(action.rule.unwrap(), env);
				}
				ActionType::Accept => {
					if ast_stack.len() == 1
						&& tokens.is_empty() && new_asts.is_empty()
						&& term_stack.is_empty()
					{
						return true;
					}
				}
				ActionType::Reduce => {
					self.do_merge(action.rule.unwrap(), env);
				}
			},
			None => {
				panic!();
			}
		}
		false
	}

	pub fn parse<'b>(&self, text: &'b str) {
		let mut chunk = TextChunk::from(text);
		// let token = self.next(&mut chunk);
		let mut env = ParseEnv::new();
		while let Some(token) = self.next(&mut chunk) {
			env.tokens.push_back(token);
		}
		env.states.push_back(0);

		// println!("closure = {:?}", self.closures);

		loop {
			let term = match env.new_asts.back() {
				Some(ast) => ast.1,
				_ => {
					if env.tokens.is_empty() {
						BOTTOM
					} else {
						env.tokens.front().unwrap().id
					}
				}
			};
			if self.do_match(term, &mut env) {
				break;
			}
		}

		let ParseEnv {
			ast_stack: mut deq_ast,
			..
		} = env;
		let ast = deq_ast.remove(0).unwrap();
		(*ast.rule.handler)(&ast);
	}
}
