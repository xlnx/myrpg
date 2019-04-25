use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use super::parse_util::{Item};
use super::rule::{Rule};

impl<'a, T> PartialEq for Item<'a, T> {
	fn eq(&self, other: &Self) -> bool {
		let ptr: *const Rule<T> = self.rule;
		let other_ptr: *const Rule<T> = other.rule;
		ptr == other_ptr && self.pos == other.pos
	}
}

impl<'a, T> Eq for Item<'a, T> {}

impl<'a, T> Hash for Item<'a, T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let ptr: *const Rule<T> = self.rule;
		ptr.hash(state)
	}
}

pub trait IndexMutOrInsert {
	type Key;
	type Item;

	fn index_mut_or_insert(&mut self, key: Self::Key) -> &mut Self::Item;
}

impl<K, V> IndexMutOrInsert for HashMap<K, HashSet<V>>
where
	K: Copy + Eq + Hash,
	V: Copy + Eq + Hash,
{
	type Key = K;
	type Item = HashSet<V>;

	fn index_mut_or_insert(&mut self, key: Self::Key) -> &mut Self::Item {
		self.entry(key).or_insert(HashSet::<V>::new())
	}
}
