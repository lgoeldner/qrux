use std::rc::Rc;

use crate::env::Env;

use super::{Cons, Expr, QxErr};

#[derive(Clone, Debug)]
pub struct Closure {
    pub args_name: Box<[Rc<str>]>,
    pub body: Expr,
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
            body,
            captured,
            is_macro,
        }
    }

    pub fn create_env(&self, args: Cons) -> Result<Env, QxErr> {
        let env = Env::with_outer(self.captured.clone());
        let has_varargs = matches!(self.args_name.last(), Some(s) if s.starts_with('&'));

        let mut names = self.args_name.iter().peekable();
        let mut args = args;

        let insert_env = |it, to| env.inner().data.borrow_mut().insert(it, to);

        while let Some(name) = names.next() {
            if names.peek().is_none() && has_varargs {
                insert_env(name[1..].into(), Expr::Cons(args));

                break;
            }

            insert_env(
                name.clone(),
                match args.car() {
                    Some(arg) => arg,
                    None => return Err(QxErr::NoArgs(Some(args), "create env")),
                },
            );

            args = match args.cdr_opt() {
                Some(args) => args,
                None => return Err(QxErr::NoArgs(None, "create env")),
            };
        }

        Ok(env)
    }
}
