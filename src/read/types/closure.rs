use ecow::{EcoString, EcoVec};

use crate::env::Env;

use super::{Cons, Expr, QxErr};

/// TODO: Add Keyword Arguments, Finish this

struct Args {
    req: EcoVec<EcoString>,
    opt: EcoVec<(EcoString, Expr)>,
}

#[derive(Debug)]
pub struct Closure {
    pub args_name: Box<[EcoString]>,
    pub body: Expr,
    pub captured: Env,
    pub is_macro: bool,
}

impl Clone for Closure {
    fn clone(&self) -> Self {
        unreachable!("Closures Should always live in an `Rc<_>`")
    }
}

impl Eq for Closure {}
impl PartialEq for Closure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Closure {
    #[must_use]
    pub fn new(args_name: Vec<EcoString>, body: Expr, captured: Env, is_macro: bool) -> Self {
        let _hallo = args_name
            .iter()
            .enumerate()
            .find_map(|it| (&**it.1 == "&rest").then_some(it.0))
            .map_or_else(
                || {
                    let _args_name = args_name.clone().into_boxed_slice();
                    0
                },
                |varargs_idx| {
                    println!(
                        "{} - &{} - {}",
                        varargs_idx,
                        varargs_idx,
                        args_name.len() - (varargs_idx + 1)
                    );
                    0
                },
            );

        let args_name = args_name.into_boxed_slice();
        Self {
            args_name,
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
                insert_env(name[1..].into(), Expr::List(args));

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
