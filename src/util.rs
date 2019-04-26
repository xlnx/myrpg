use std::collections::{HashMap, *};
use std::hash::*;

pub const EPS: i64 = 0i64;
pub const BOTTOM: i64 = 1i64;

use ref_thread_local::RefThreadLocal;

ref_thread_local! {
	static managed SYMBOL: HashMap<i64, String> = HashMap::new();
}

pub fn hash(x: &str) -> i64 {
	use hash_map::DefaultHasher;
	let mut s = DefaultHasher::new();
	s.write(x.as_bytes());
	let val = s.finish() as i64 & (-1i64 as u64 >> 1) as i64;
	SYMBOL.borrow_mut().insert(val, String::from(x));
	SYMBOL.borrow_mut().insert(!val, String::from(x));
	// println!("{} => {} | {}", x, val, !val);
	val
}

pub fn decode(val: i64) -> &'static str {
	match val {
		EPS => return &"EPS",
		BOTTOM => return &"BOTTOM",
		_ => {}
	}
	match SYMBOL.borrow().get(&val) {
		Some(val) => unsafe { &*(val.as_str() as *const str) },
		_ => {
			panic!("unknown value");
		}
	}
}
