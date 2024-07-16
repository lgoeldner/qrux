use std::collections::HashMap;
use std::rc::Rc;
use std::{cell::RefCell, collections::hash_map::Entry};

use crate::{
    eval::Shadow,
    read::{Expr, QxErr},
};

mod ll_core;
pub use ll_core::{core_func_names, core_map};
use tap::Pipe;

// mod core;
// pub use core::core_map;
// fn core_map(_: &str) -> Option<Expr> {
//     None
// }

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

pub struct AlreadySetError;

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

    /// set to the environment, returns either Expr::Nil or a ShadowError
    pub fn set(&mut self, ident: Rc<str>, val: Expr, over: Shadow) -> Result<Expr, QxErr> {
        if &*ident == "_" {
            Ok(Expr::Nil)
        } else {
            if matches!(over, Shadow::No) && self.0.data.borrow_mut().contains_key(&ident) {
                return Err(QxErr::ShadowErr(ident.to_string()));
            }

            self.0.data.borrow_mut().insert(ident, val);

            Ok(Expr::Nil)
        }
    }

    pub fn data(&self) -> &RefCell<HashMap<Rc<str>, Expr>> {
        &self.0.data
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
