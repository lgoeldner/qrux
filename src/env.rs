use crate::{
    eval::Shadow,
    read::{Expr, QxErr},
};

use std::{cell::RefCell, collections::HashMap, rc::Rc};
use ecow::EcoString;

pub use ll_core::{core_func_names, core_map};
mod ll_core;

#[derive(Clone, Default)]
pub struct Inner {
    pub(crate) outer: Option<Env>,
    pub(crate) vals: RefCell<HashMap<EcoString, Expr>>,
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
    #[must_use]
    pub fn new_env(outer: Option<Env>) -> Env {
        Env(Rc::new(Self {
            outer,
            vals: RefCell::default(),
        }))
    }

    #[must_use]
    pub fn with_outer(outer: Env) -> Env {
        Self::new_env(Some(outer))
    }

    pub fn outer_env(&self) -> Option<Rc<Self>> {
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
        self.0.vals.borrow_mut().remove(ident)
    }

    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Expr> {
        self.find(ident).map_or_else(
            || core_map(ident),
            |env| env.0.vals.borrow().get(ident).cloned(),
        )
    }

    /// set to the environment, returns either `Expr::Nil` or a `ShadowError`
    pub fn set(&self, ident: EcoString, val: Expr, over: Shadow) -> Result<Expr, QxErr> {
        if &*ident != "_" {
            let is_noshadow = matches!(over, Shadow::No);
            let already_contains_key = self.0.vals.borrow_mut().contains_key(&ident);
            let is_closure = matches!(val, Expr::Closure(_));

            if is_noshadow && already_contains_key && !is_closure {
                eprintln!("{ident} already set");
                return Err(QxErr::ShadowErr(ident));
            }

            self.0.vals.borrow_mut().insert(ident, val);
        }

        Ok(Expr::Nil)
    }

    pub fn vals(&self) -> &RefCell<HashMap<EcoString, Expr>> {
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
}
