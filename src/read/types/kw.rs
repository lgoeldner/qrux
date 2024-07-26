use std::{collections::HashMap, marker::PhantomData, sync::RwLock};

use ecow::EcoString;
use slab::Slab;
use slotmap::{new_key_type, DefaultKey, Key, SlotMap};

use crate::lazy::Lazy;

new_key_type! {
    pub struct Keyword;
}

static KEYWORD_SLAB: Lazy<RwLock<SlotMap<Keyword, EcoString>>> = Lazy::new(Default::default);

// pub struct Keyword(usize);
impl Keyword {
    pub fn new(kw: EcoString) -> Self {
        assert_eq!(&kw[..1], ":");

        KEYWORD_SLAB
            .write()
            .expect("Keyword Table is poisoned")
            .insert(kw)
    }

    pub fn inner_val<R>(&self, f: impl Fn(&str) -> R) -> R {
        f(&KEYWORD_SLAB.read().expect("Keyword Table is poisoned")[*self][1..])
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({:?})",
            KEYWORD_SLAB.read().expect("Keyword Table is poisoned")[*self],
            self
        )
    }
}

pub struct Table<K, V> {
    keys: HashMap<usize, V>,
    next: usize,
    _p: PhantomData<K>,
}

impl<K, V> Table<K, V> {
    pub fn new() -> Self {
        Self {
            keys: HashMap::new(),
            next: 0,
            _p: PhantomData,
        }
    }

    pub fn insert(&mut self, k: K, v: V) -> usize {
        let next = self.next;
        self.next += 1;

        self.keys.insert(next, v);
        next
    }
}

impl <K, S> Table<K, S> where S: AsRef<str> {
	pub fn get(&self, k: &K) -> Option<&S> {
		self.keys.get(&k)
	}
}

impl<K, V> Default for Table<K, V> {
    fn default() -> Self {
        Self::new()
    }
}
