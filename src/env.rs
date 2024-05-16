use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::read::{Expr, QxErr};

use self::core::{builtins, cmp_ops, int_ops};

mod core;

#[derive(Clone, Default)]
pub struct Inner {
    outer: Option<Env>,
    data: RefCell<HashMap<String, Expr>>,
    // selfref: Weak<Inner>,
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
        // println!(
        //     "dropping inner TODO, {:?}, {:?}",
        //     self.data,
        //     self.outer.as_ref().map(|it| &it.0.data)
        // );
    }
}

fn core_map(inp: &str) -> Option<Expr> {
    int_ops(inp)
        .or_else(|| cmp_ops(inp))
        .or_else(|| builtins(inp))
}

impl Inner {
    #[must_use]
    pub fn new_env(outer: Option<Env>) -> Env {
        Env(Rc::new_cyclic(|cycle| Self {
            outer,
            data: RefCell::default(),
            // selfref: cycle.clone(),
        }))
    }

    pub fn with_outer_args(
        outer: Env,
        args: &[Expr],
        argsident: &[impl AsRef<str>],
    ) -> Result<Env, QxErr> {
        let env = Self::with_outer(outer);

        if argsident.len() != args.len() {
            return Err(QxErr::TypeErr {
                expected: Expr::List(
                    argsident
                        .iter()
                        .map(|it| Expr::Sym(it.as_ref().to_owned()))
                        .collect::<Vec<_>>(),
                ),
                found: Expr::List(args.to_vec()),
            });
        }

        for (arg, ident) in args.iter().zip(argsident) {
            env.0
                .data
                .borrow_mut()
                .insert(ident.as_ref().to_string(), arg.clone());
        }

        Ok(env)
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
