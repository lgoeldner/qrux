use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::read::Expr;
use crate::{Func, QxErr};

use self::core::{builtins, int_ops};

mod core;

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct Env {
    outer: Option<Rc<Self>>,
    data: RefCell<HashMap<String, Expr>>,
}

fn core_map(inp: &str) -> Option<Expr> {
    let res = int_ops(inp).or_else(|| builtins(inp));

    res
}

fn default_env_map() -> HashMap<String, Expr> {
    macro_rules! int_op_apply2 {
            ($($op:tt)+) => {
                [
                    $((
                        // name
                        stringify!($op).to_owned(),

                        Func(Rc::new(|_ctx, args: &[Expr]| {

                            // builtin operators are variadic
                            let Expr::Int(init) = args[0] else {
                                return Err(QxErr::NoArgs(Some(args.to_vec())));
                            };

                            args
                            .iter()
                            .skip(1)
                            .try_fold(init, |acc, it| {
                                if let Expr::Int(it) = it {
                                    Ok(acc $op it)
                                } else {
                                    Err(QxErr::NoArgs(Some(args.to_vec())))
                                }
                            })
                            .map(|it| Expr::Int(it))
                        } )),
                    ),)+
                ]
            };
        }

    let list: &[(String, Func)] = &int_op_apply2!(+ - * / %);

    let mut map = HashMap::new();

    for (k, v) in list {
        map.insert(k.to_string(), Expr::Func(v.clone()));
    }

    map
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
        self.outer.as_ref().map(|it| Rc::clone(&it))
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
