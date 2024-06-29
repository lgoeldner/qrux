use std::rc::Rc;

use crate::{env::Env, expr};

use super::{Cons, Expr, QxErr};

#[derive(Clone, Debug)]
pub struct Closure {
    pub args_name: Box<[Rc<str>]>,
    pub body: Box<Expr>,
    pub captured: Env,
    pub is_macro: bool,
}

impl Eq for Closure {}
impl PartialEq for Closure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Closure {
    #[must_use]
    pub fn new(args_name: Vec<Rc<str>>, body: Expr, captured: Env, is_macro: bool) -> Self {
        Self {
            args_name: args_name.into(),
            body: Box::new(body),
            captured,
            is_macro,
        }
    }

    // fn load_varargs(&self, args: Cons, env: Env) -> Env {
    //     // load normal args
    //     for (arg, ident) in args[..self.args_name.len() - 1]
    //         .iter()
    //         .zip(self.args_name.iter())
    //     {
    //         env.inner()
    //             .data
    //             .borrow_mut()
    //             .insert(ident.clone(), arg.clone());
    //     }

    //     // insert variadic args
    //     env.inner().data.borrow_mut().insert(
    //         self.args_name.last().unwrap()[1..].to_owned().into(),
    //         Expr::List(args[self.args_name.len() - 1..].into()),
    //     );

    //     env
    // }

    pub fn create_env(&self, args: Cons) -> Result<Env, QxErr> {
        let env = Env::with_outer(self.captured.clone());
        let has_varargs = matches!(self.args_name.last(), Some(s) if s.starts_with('&'));

        let mut names = self.args_name.into_iter().peekable();
        let mut args = args;

        let insert_env = |it, to| env.inner().data.borrow_mut().insert(it, to);

        loop {
            let name = match names.next() {
                Some(name) => name,
                None => break,
            };

            if names.peek().is_none() && has_varargs {
                insert_env(name[1..].into(), Expr::Cons(args.clone()));

                break;
            }

            insert_env(name.clone(), args.car().unwrap());

            args = match args.cdr_opt() {
                Some(args) => args,
                None => return Err(QxErr::NoArgs(None)),
            };
        }

        Ok(env)
    }
}
