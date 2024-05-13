use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::read::Expr;

use self::core::{builtins, cmp_ops, int_ops};

mod core;
//
// macro_rules! map {
//     ($($k:expr => $v:expr),* $(,)?) => {{
//         ::core::convert::From::from([$(($k.to_owned(), $v),)*])
//     }};
// }

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Env {
    outer: Option<Rc<Self>>,
    data: RefCell<HashMap<String, Expr>>,
}

fn core_map(inp: &str) -> Option<Expr> {
    int_ops(inp)
        .or_else(|| cmp_ops(inp))
        .or_else(|| builtins(inp))
}

impl Env {
    #[must_use]
    pub fn new() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn with_outer_args(
        outer: Rc<Self>,
        args: &[Expr],
        argsident: &[impl AsRef<str>],
    ) -> Rc<Self> {
        let env = Self::with_outer(outer);

        for (arg, ident) in args.iter().zip(argsident) {
            env.data
                .borrow_mut()
                .insert(ident.as_ref().to_string(), arg.clone());
        }

        env
    }

    pub fn with_outer(outer: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            outer: Some(outer),
            data: RefCell::new(HashMap::new()),
        })
    }

    pub fn outer(&self) -> Option<Rc<Self>> {
        self.outer.clone()
    }
}

mod private {
    use std::rc::Rc;

    use crate::env::Env;

    pub trait Sealed {}
    impl Sealed for Rc<Env> {}
}

pub trait EnvObj: private::Sealed {
    fn get(&self, ident: &str) -> Option<Expr>;
    fn set(&self, ident: &str, val: Expr);
    fn find(&self, ident: &str) -> Option<Rc<Env>>;
}

impl EnvObj for Rc<Env> {
    fn get(&self, ident: &str) -> Option<Expr> {
        self.find(ident).map_or_else(
            || core_map(ident),
            |env| env.data.borrow().get(ident).cloned(),
        )
    }

    fn set(&self, ident: &str, val: Expr) {
        self.data.borrow_mut().insert(ident.to_owned(), val);
    }

    fn find(&self, ident: &str) -> Option<Rc<Env>> {
        // check if self contains the key,
        self.data
            .borrow()
            .contains_key(ident)
            // then return self
            .then(|| Rc::clone(self))
            // or delegate to the outer env
            .or_else(|| {
                self.outer
                    .as_ref()
                    // .or_else(|| )
                    .and_then(|it| it.find(ident))
            })
    }
}
