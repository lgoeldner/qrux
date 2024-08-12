use crate::{
    eval::Shadow,
    read::{Expr, QxErr},
};

use ecow::EcoString;
use fxhash::FxHashMap;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use ll_core::core_map;
mod ll_core;

pub type EnvMap = RefCell<FxHashMap<EcoString, Expr>>;

#[derive(Clone, Default)]
pub struct Inner {
    pub(crate) outer: Option<Env>,
    pub(crate) vals: EnvMap,
}

impl std::fmt::Debug for Inner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Env::Inner")
            .field("outer", &self.outer)
            .field("data", &self.vals)
            .finish()
    }
}

#[derive(Clone, Default, Debug)]
pub struct Env(Rc<Inner>);

impl Inner {
    pub fn outer_env(&self) -> Option<Rc<Self>> {
        self.outer.as_ref().map(|it| &it.0).cloned()
    }
}

pub struct AlreadySetError;

impl Env {
    pub fn new(outer: impl Into<Option<Env>>) -> Self {
        Self(Rc::new(Inner {
            outer: outer.into(),
            vals: RefCell::default(),
        }))
    }

    #[must_use]
    pub fn with_outer(outer: Self) -> Self {
        Env::new(outer)
    }

    pub(crate) fn inner(&self) -> Rc<Inner> {
        self.0.clone()
    }

    #[allow(clippy::must_use_candidate)]
    pub fn remove(&self, ident: &str) -> Option<Expr> {
        self.0.vals.borrow_mut().remove(ident)
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Expr> {
        self.find(ident)
            .and_then(|env| env.0.vals.borrow().get(ident).cloned())
    }

    /// set to the environment, returns either `Expr::Nil` or a `ShadowError`
    pub fn set(&self, ident: EcoString, val: Expr, over: Shadow) -> Result<Expr, QxErr> {
        if &*ident != "_" {
            let is_noshadow = matches!(over, Shadow::No);
            let already_contains_key = self.0.vals.borrow_mut().contains_key(&ident);
            let is_closure = matches!(val, Expr::Closure(_));

            if is_noshadow && !is_closure && already_contains_key {
                return Err(QxErr::ShadowErr(ident));
            }

            self.0.vals.borrow_mut().insert(ident, val);
        }

        Ok(Expr::Nil)
    }

    pub fn vals(&self) -> &EnvMap {
        &self.0.vals
    }

    #[must_use]
    pub fn find(&self, ident: &str) -> Option<Self> {
        // check if self contains the key,
        self.0
            .vals
            .borrow()
            .contains_key(ident)
            // then return self
            .then(|| self.clone())
            // or delegate to the outer env
            .or_else(|| self.0.outer.as_ref().and_then(|it| it.find(ident)))
    }

    pub(crate) fn core() -> Env {
        Env(Rc::new(Inner {
            outer: None,
            vals: RefCell::new(core_map()),
        }))
    }
}
