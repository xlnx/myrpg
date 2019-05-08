use lalr_util::{parse_util::*, rule::*, symbol::*};
use std::collections::{HashMap, HashSet, VecDeque};


pub fn make_first<T>(first: &mut HashMap<Symbol, HashSet<Symbol>>, grammar: &Grammar<T>) {
	let mut add_sub: bool;
	loop {
		add_sub = false;
		for (src, rule_set) in grammar.iter() {
			// i am curr param
			let my_firsts = first.get(&src).unwrap();
			let mut new_firsts = vec![];
			// each rule generated my me
			for rule in rule_set.iter() {
				// this rule generates eps
				let mut has_empty = rule.symbols.len() == 0;
				// each symbol in this rule
				for symbol in rule.symbols.iter() {
					has_empty = false;
					let firsts = first.get(symbol).unwrap();
					// each symbol of first[symbol]
					for first_elem in firsts.iter() {
						if *first_elem == BOTTOM {
							// this symbol generates eps
							has_empty = true;
						} else if !my_firsts.contains(first_elem) {
							new_firsts.push(*first_elem);
						}
					}
					// this symbol cant generate eps
					if !has_empty {
						break;
					}
				}
				// this rule generates eps
				if has_empty && !my_firsts.contains(&BOTTOM) {
					new_firsts.push(BOTTOM);
				}
			}
			// now add new firsts into first[me]
			let my_firsts_mut = first.get_mut(&src).unwrap();
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

pub fn make_closures<'a, T>(
	closures: &mut Vec<Closure<'a, T>>,
	goto: &mut Vec<HashMap<Symbol, usize>>,
	first: &HashMap<Symbol, HashSet<Symbol>>,
	grammar: &'a Grammar<T>,
) {
	let mut origin = Closure::new(grammar);
	let rule = grammar.origin();
	let mut la = HashSet::new();
	la.insert(BOTTOM);
	origin.insert(Item { rule, pos: 0, la });

	let mut incoming = VecDeque::new();
	incoming.push_back(origin);

	while let Some(closure) = incoming.pop_front() {
		let expanded = closure.expanded(first);
		let curr_state = closures.len();
		closures.push(closure);
		let mut following = HashMap::<Symbol, Closure<_>>::new();
		for item in expanded.into_iter() {
			if let Some(next_item) = item.next() {
				let symbol = item.symbol().unwrap();
				if following.contains_key(&symbol) {
					following.get_mut(&symbol).unwrap().insert(next_item);
				} else {
					let mut new_closure = Closure::new(grammar);
					new_closure.insert(next_item);
					following.insert(symbol, new_closure);
				}
			}
		}
		// println!("{:?}", following);
		goto.push(HashMap::new());
		for (symbol, new_closure) in following.into_iter() {
			if closures.contains(&new_closure) {
				let new_state = closures
					.iter()
					.enumerate()
					.find(|x| x.1 == &new_closure)
					.unwrap()
					.0;
				goto[curr_state].insert(symbol, new_state);
			} else if incoming.contains(&new_closure) {
				let new_state = closures.len()
					+ incoming
						.iter()
						.enumerate()
						.find(|x| x.1 == &new_closure)
						.unwrap()
						.0;
				goto[curr_state].insert(symbol, new_state);
			} else {
				let new_state = closures.len() + incoming.len();
				goto[curr_state].insert(symbol, new_state);
				incoming.push_back(new_closure);
			}
		}
	}
}
