use std::rc::Rc;

use crate::{env::Env, expr};

use super::{Expr, QxErr};

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

    fn load_varargs(&self, args: &[Expr], env: Env) -> Env {
        // load normal args
        for (arg, ident) in args[..self.args_name.len() - 1]
            .iter()
            .zip(self.args_name.iter())
        {
            env.inner()
                .data
                .borrow_mut()
                .insert(ident.clone(), arg.clone());
        }

        // insert variadic args
        env.inner().data.borrow_mut().insert(
            self.args_name.last().unwrap()[1..].to_owned().into(),
            Expr::List(args[self.args_name.len() - 1..].into()),
        );

        env
    }

    pub fn create_env(&self, args: &[Expr]) -> Result<Env, QxErr> {
        let env = Env::with_outer(self.captured.clone());
        let has_varargs = matches!(self.args_name.last(), Some(s) if s.starts_with('&'));


        if (!has_varargs && self.args_name.len() != args.len())
            || (has_varargs && self.args_name.len() > args.len())
        {
            return Err(QxErr::TypeErr {
                expected: Box::new(Expr::List(
                    self.args_name
                        .iter()
                        .map(|it| expr!(sym it.as_ref()))
                        .collect::<Rc<[_]>>(),
                )),
                found: Box::new(Expr::List(args.into())),
            });
        }

        if has_varargs {
            Ok(self.load_varargs(args, env))
        } else {
            for (arg, ident) in args.iter().zip(self.args_name.iter()) {
                env.inner()
                    .data
                    .borrow_mut()
                    .insert(ident.clone(), arg.clone());
            }

            Ok(env)
        }
    }
}
