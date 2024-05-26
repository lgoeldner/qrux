use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::expr;
use crate::read::{Expr, QxErr};

use self::core::{builtins, cmp_ops, int_ops, list_builtins};

mod core;

#[derive(Clone, Default)]
pub struct Inner {
    pub(crate) outer: Option<Env>,
    pub(crate) data: RefCell<HashMap<Rc<str>, Expr>>,
}

impl std::fmt::Debug for Inner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Env::Inner")
            .field("outer", &self.outer)
            .field("data", &self.data)
            .finish()
    }
}

#[derive(Clone, Default, Debug)]
pub struct Env(Rc<Inner>);

impl Drop for Inner {
    fn drop(&mut self) {
        // TODO: solve memory leak when env is created with a closure capturing the env,
        // creating a reference cycle
    }
}

fn core_map(inp: &str) -> Option<Expr> {
    builtins(inp)
        .or_else(|| cmp_ops(inp))
        .or_else(|| int_ops(inp))
        .or_else(|| list_builtins(inp))
}

impl Inner {
    #[must_use]
    pub fn new_env(outer: Option<Env>) -> Env {
        Env(Rc::new(Self {
            outer,
            data: RefCell::default(),
        }))
    }

    #[must_use]
    pub fn with_outer(outer: Env) -> Env {
        Self::new_env(Some(outer))
    }

    pub fn outer(&self) -> Option<Rc<Self>> {
        self.outer.as_ref().map(|it| &it.0).cloned()
    }
}

impl Env {
    #[must_use]
    pub fn with_outer(outer: Self) -> Self {
        Inner::new_env(Some(outer))
    }

    pub(crate) fn inner(&self) -> Rc<Inner> {
        self.0.clone()
    }

    #[allow(clippy::must_use_candidate)]
    pub fn remove(&self, ident: &str) -> Option<Expr> {
        self.0.data.borrow_mut().remove(ident)
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Expr> {
        self.find(ident).map_or_else(
            || core_map(ident),
            |env| env.0.data.borrow().get(ident).cloned(),
        )
    }

    pub fn set(&mut self, ident: &Rc<str>, val: Expr) {
        self.0.data.borrow_mut().insert(Rc::clone(ident), val);
    }

    #[must_use]
    pub fn find(&self, ident: &str) -> Option<Self> {
        // check if self contains the key,
        self.0
            .data
            .borrow()
            .contains_key(ident)
            // then return self
            .then(|| self.clone())
            // or delegate to the outer env
            .or_else(|| self.0.outer.as_ref().and_then(|it| it.find(ident)))
    }
}
