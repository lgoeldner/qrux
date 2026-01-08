//! Environment and scope management for symbol resolution.
//!
//! This module defines [`Env`], a reference-counted, nested environment used
//! to store symbol bindings (`Expr`) and to resolve identifiers across lexical
//! scopes. It supports shadowing rules, outer environments, and a core
//! environment containing built-in functions.

use crate::{
    eval::Shadow,
    read::{Expr, QxErr},
};

use ecow::EcoString;
use fxhash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

pub use ll_core::core_map;
mod ll_core;

pub type EnvMap = RefCell<FxHashMap<EcoString, Expr>>;

/// The `Env` stores a shared Map of Symbols to their Expressions
/// And also an outer Environment to allow capturing and using variables from outer scopes
/// The "lowest" Env should always be an Env::core, otherwise builtin `Funcs` will not work
#[derive(Clone, Default, Debug)]
pub struct Env(Rc<Inner>);

#[derive(Clone, Default)]
// TODO make this private and encapsulate
pub struct Inner {
    pub(crate) outer: Option<Env>,
    pub(crate) vals: EnvMap,
}

impl Inner {
    /// Get the outer `Env`
    pub fn outer_env(&self) -> Option<Rc<Self>> {
        self.outer.as_ref().map(|it| &it.0).cloned()
    }
}

impl Env {
    /// Creates a new "Base" Env containing natively implemented core `Funcs``
    #[must_use]
    pub(crate) fn core() -> Env {
        Env(Rc::new(Inner {
            outer: None,
            vals: RefCell::new(core_map()),
        }))
    }

    /// create a new `Env`
    /// `outer`: the nested outer scope
    #[must_use]
    pub fn new(outer: impl Into<Option<Env>>) -> Self {
        Self(Rc::new(Inner {
            outer: outer.into(),
            vals: RefCell::default(),
        }))
    }

    // TODO: make private, encapsulate functionality
    pub(crate) fn inner(&self) -> Rc<Inner> {
        self.0.clone()
    }

    /// Remove a binding from the current environment only.
    ///
    /// This does not affect outer environments. Returns the removed expression,
    /// if the identifier was present.
    #[allow(clippy::must_use_candidate)]
    pub fn remove(&self, ident: &str) -> Option<Expr> {
        self.0.vals.borrow_mut().remove(ident)
    }

    /// retrieve a binding
    #[must_use]
    pub fn get(&self, ident: &str) -> Option<Expr> {
        self.find(ident)
            .and_then(|env| env.0.vals.borrow().get(ident).cloned())
    }

    /// Set to the environment, returns either `Expr::Nil` or a `ShadowError`
    /// **Args:**
    /// - `allow_shadow`: if the identifier is allowed to shadow/overwrite a preexisting binding
    pub fn set(&self, ident: EcoString, val: Expr, allow_shadow: Shadow) -> Result<Expr, QxErr> {
        if &*ident != "_" {
            let is_noshadow = matches!(allow_shadow, Shadow::No);
            let already_contains_key = self.0.vals.borrow_mut().contains_key(&ident);
            let is_closure = matches!(val, Expr::Closure(_));

            if is_noshadow && !is_closure && already_contains_key {
                return Err(QxErr::ShadowErr(ident));
            }

            self.0.vals.borrow_mut().insert(ident, val);
        }

        Ok(Expr::Nil)
    }

    /// A direct reference to the Mapping in the `Env`
    pub fn vals(&self) -> &EnvMap {
        &self.0.vals
    }

    /// Find the `Env` containing an identifier.
    ///
    /// If this `Env` doesn't contain it, recursively search in the outer `Env`.
    /// Returns the `Env` containing the `ident`, otherwise `None`
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

// pretty print an `Inner`
impl std::fmt::Debug for Inner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Env::Inner")
            .field("outer", &self.outer)
            .field("data", &self.vals)
            .finish()
    }
}
