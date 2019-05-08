use std::collections::{HashMap, HashSet};
use std::hash::Hash;

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
