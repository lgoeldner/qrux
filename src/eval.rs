use std::rc::Rc;

use anyhow::{anyhow, Context};

use crate::env::{Env, EnvObj};
use crate::read::Closure;
use crate::{
    read::{Expr, QxErr, AST},
    Runtime,
};

macro_rules! any_err {
    (form: $form:literal) => {
        return Err(QxErr::Any(anyhow!(concat!("Correct Form: ", $form))))
    };
    ($err:literal) => {
        Err(QxErr::Any(anyhow!($err)))
    };
}

macro_rules! ok {
    ($ex:expr) => {
        Ok(Some($ex))
    };
}

impl Runtime {
    // doesnt exist in mal
    pub fn eval_mult(&mut self, ast: AST, env: Option<&Rc<Env>>) -> Result<AST, QxErr> {
        ast.into_iter()
            .map(|it| self.eval(it, env))
            .collect::<Result<_, _>>()
    }

    pub fn eval(&mut self, ast: Expr, env: Option<&Rc<Env>>) -> Result<Expr, QxErr> {
        match ast {
            Expr::List(ref lst) if lst.is_empty() => Ok(ast),
            Expr::List(ref lst) => {
                let ident = if let Expr::Sym(s) = &lst[0] {
                    s.as_str()
                } else {
                    return self.apply_func(ast, env);
                };

                self.specials(ident, lst)?
                    .map_or_else(|| self.apply_func(ast, env), Ok)
            }

            _ => self.replace_eval(ast, env),
        }
    }

    fn specials(&mut self, ident: &str, lst: &[Expr]) -> Result<Option<Expr>, QxErr> {
        match (ident, &lst[1..]) {
            ("def!", [Expr::Sym(ident), expr]) => self.defenv(ident, expr).map(Some),
            ("def!", _) => any_err!(form: "(def! <sym> <expr>)"),

            ("fn*", [Expr::List(args), body]) => {
                let x = args.iter().map(|it| {
                    if let Expr::Sym(s) = it {
                        Ok(s.clone())
                    } else {
                        Err(QxErr::Any(anyhow!("Not a symbol: {it:?}")))
                    }
                });

                let cl = Closure::new(
                    x.collect::<Result<_, _>>()?,
                    body.clone(),
                    Rc::clone(&self.env),
                );

                ok!(Expr::Closure(cl))
            }
            ("fn*", _) => any_err!(form: "(fn* (<args>*) <body>)"),

            ("if", [cond, then, ..]) => {
                let cond = self.eval(cond.clone(), None)?;

                match cond {
                    Expr::Bool(false) | Expr::Nil => self
                        .eval(lst.get(3).ok_or(QxErr::NoArgs(None))?.clone(), None)
                        .map(Some),

                    _ => self.eval(then.clone(), None).map(Some),
                }
            }
            ("if", _) => any_err!(form: "(if <condition> <then> ?<else>)"),

            ("let*", [Expr::List(new_bindings), to_eval]) => {
                // create new env, set as current, old env is now self.env.outer

                self.env = Env::with_outer(Rc::clone(&self.env));

                for pair in new_bindings.chunks_exact(2) {
                    let [Expr::Sym(ident), expr] = pair else {
                        return Err(QxErr::TypeErr {
                            expected: Expr::Sym("<sym>".to_string()),
                            found: pair[0].clone(),
                        });
                    };

                    self.defenv(ident, expr)?;
                }

                let result = self.eval(to_eval.clone(), None)?;
                self.env = self.env.outer().expect("The env was just defined");

                ok!(result)
            }
            ("let*", _) => any_err!(form: "(let* (<sym> <expr>)+ <expr>)"),

            ("prn", [arg]) => {
                println!("{arg}");
                ok!(Expr::Nil)
            }
            ("prn", _) => any_err!(form: "(prn <expr>)"),

            ("do", [start @ .., end]) => {
                for exp in start {
                    self.eval(exp.clone(), None)?;
                }

                self.eval(end.clone(), None).map(Some)
            }
            ("do", _) => any_err!(form: "(do <expr>*)"),

            _ => Ok(None),
        }
    }

    fn apply_func(&mut self, ast: Expr, env: Option<&Rc<Env>>) -> Result<Expr, QxErr> {
        let Expr::List(new) = &self.replace_eval(ast, env)? else {
            unreachable!()
        };

        match new.as_slice() {
            [Expr::Func(func), args @ ..] => func.apply(self, args),
            [Expr::Closure(closure), args @ ..] => closure.apply(self, args),

            [err, ..] => any_err!("Not a Function: {err:?}"),
            [] => Err(QxErr::NoArgs(None)),
        }
    }

    fn defenv(&mut self, ident: &str, expr: &Expr) -> Result<Expr, QxErr> {
        let res = self.eval(expr.clone(), None)?;
        self.env.set(ident, res);

        Ok(Expr::Nil)
    }

    // named eval_ast in mal
    // replaces symbols with their values
    // evaluates lists
    pub fn replace_eval(&mut self, ast: Expr, env: Option<&Rc<Env>>) -> Result<Expr, QxErr> {
        Ok(match ast {
            Expr::Sym(sym) => env
                .unwrap_or(&self.env)
                .get(&sym)
                .context(format!("Unbound identifier [ {sym} ]"))?,
            Expr::List(lst) => Expr::List(self.eval_mult(lst, env)?),
            val => val,
        })
    }
}
