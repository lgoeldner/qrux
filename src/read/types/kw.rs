use crate::lazy::Lazy;
use ecow::EcoString;
use std::{
    collections::HashMap, fmt::Debug, hash::{DefaultHasher, Hash, Hasher}, ops::Index, sync::RwLock
};

static KW_TABLE: Lazy<RwLock<Table<EcoString>>> = Lazy::new(Default::default);

/// Global Table containing Keywords. Works like a Hashset without collision soundness,
/// returns a unique key for each Keyword
pub struct Table<K> {
    keys: HashMap<u64, K>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Keyword(u64);

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            KW_TABLE.read().expect("Keyword Table is poisoned")[self]
        )
    }
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Keyword({self})")
    }
}

impl Keyword {
    pub fn new(mut kw: EcoString) -> Self {
        // if keyword is not prefixed with ':', add ':'
        if !kw.starts_with(':') {
            let mut new = EcoString::with_capacity(kw.len() + 1);
            new.push(':');
            new.push_str(&kw);
            kw = new;
        }

        if KW_TABLE
            .read()
            .expect("Keyword Table is poisoned")
            .contains(&kw)
        {
            return Self(Table::get_key(&kw));
        }

        KW_TABLE
            .write()
            .expect("Keyword Table is poisoned")
            .insert(kw)
    }

    pub fn inspect_inner<R>(&self, f: impl Fn(&str) -> R) -> R {
        f(&KW_TABLE.read().expect("KW Table is poisoned")[self][1..])
    }
}

impl<K> Table<K>
where
    K: Hash,
{
    pub fn new() -> Self {
        Self::default()
    }

    fn get_key(k: &K) -> u64 {
        let mut h = DefaultHasher::new();
        k.hash(&mut h);
        h.finish()
    }

    pub fn contains(&self, kw: &K) -> bool {
        let k = Self::get_key(kw);
        self.keys.contains_key(&k)
    }

    pub fn insert(&mut self, k: K) -> Keyword {
        let key = Self::get_key(&k);
        self.keys.insert(key, k);
        Keyword(key)
    }

    pub fn get(&self, k: &Keyword) -> Option<&K> {
        self.keys.get(&k.0)
    }
}

impl<K> Index<&Keyword> for Table<K> {
    type Output = K;

    fn index(&self, index: &Keyword) -> &Self::Output {
        self.keys.get(&index.0).expect("Keyword doesn't exist")
    }
}

impl<K> Default for Table<K> {
    fn default() -> Self {
        Self {
            keys: HashMap::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keys() {
        assert_eq!(Table::get_key(&"test"), Table::get_key(&"test"));
    }
}
