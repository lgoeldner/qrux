use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::{cell::RefCell, mem};

use crate::read::Expr;

use self::core::{builtins, cmp_ops, int_ops};

mod core;

#[derive(Debug, Clone, Default)]
pub struct _Inner {
    outer: Option<Env>,
    data: RefCell<HashMap<String, Expr>>,
    _selfref: Weak<_Inner>,
}

#[derive(Debug, Clone, Default)]
pub struct Env(Rc<_Inner>);

impl Drop for _Inner {
    fn drop(&mut self) {

        println!("dropping inner TODO");

    }
}



fn core_map(inp: &str) -> Option<Expr> {
    int_ops(inp)
        .or_else(|| cmp_ops(inp))
        .or_else(|| builtins(inp))
}

impl _Inner {
    
    #[must_use]
    pub fn new_env(outer: Option<Env>) -> Env {
        Env(Rc::new_cyclic(|cycle| Self {
            outer,
            data: RefCell::default(),
            _selfref: cycle.clone(),
        }))
    }

    pub fn with_outer_args(outer: Env, args: &[Expr], argsident: &[impl AsRef<str>]) -> Env {
        let env = Self::with_outer(outer);

        for (arg, ident) in args.iter().zip(argsident) {
            env.0
                .data
                .borrow_mut()
                .insert(ident.as_ref().to_string(), arg.clone());
        }

        env
    }

    #[must_use]
    pub fn with_outer(outer: Env) -> Env {
        Self::new_env(Some(outer))
    }

    pub fn outer(&self) -> Option<Rc<Self>> {
        self.outer.as_ref().map(|it| &it.0).cloned()
    }
}

mod private {
    use super::Env;
    use crate::env::_Inner;
    use std::rc::Rc;

    pub trait Sealed {}
    impl Sealed for Rc<_Inner> {}
    impl Sealed for Env {}
}

impl Env {
    pub fn get(&self, ident: &str) -> Option<Expr> {
        self.find(ident).map_or_else(
            || core_map(ident),
            |env| env.0.data.borrow().get(ident).cloned(),
        )
    }

    pub fn set(&self, ident: &str, val: Expr) {
        self.0.data.borrow_mut().insert(ident.to_owned(), val);
    }

    pub fn find(&self, ident: &str) -> Option<Env> {
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
