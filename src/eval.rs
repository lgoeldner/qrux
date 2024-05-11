use std::rc::Rc;

use anyhow::{anyhow, Context};

use crate::env::{Env, EnvObj};
use crate::read::Closure;
use crate::{
    read::{Expr, QxErr, AST},
    Runtime,
};

impl Runtime {
    // doesnt exist in mal
    pub fn eval_mult(&mut self, ast: AST) -> Result<AST, QxErr> {
        ast.into_iter()
            .map(|it| self.eval(it))
            .collect::<Result<_, _>>()
    }

    pub fn eval(&mut self, ast: Expr) -> Result<Expr, QxErr> {
        if let Expr::List(lst) = &ast {
            if lst.is_empty() {
                Ok(ast)
            } else {
                match lst.as_slice() {
                    [Expr::Sym(s), Expr::Sym(ident), expr] if s == "def!" => {
                        self.defenv(ident, expr)
                    }
                    // functions
                    [Expr::Sym(s), Expr::List(args), body] if s == "fn*" => {
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

                        Ok(Expr::Closure(cl))
                    }

                    [Expr::Sym(s), cond, then, ..] if s == "if" => {
                        let cond = self.eval(cond.clone())?;

                        match cond {
                            Expr::Bool(false) | Expr::Nil => {
                                self.eval(lst.get(3).ok_or(QxErr::NoArgs(None))?.clone())
                            }

                            _ => self.eval(then.clone()),
                        }
                    }

                    [Expr::Sym(s), Expr::List(new_bindings), to_eval] if s == "let*" => {
                        // create new env, set as current, old env is now self.env.outer

                        self.env = Env::with_outer(Rc::clone(&self.env));

                        for pair in new_bindings.chunks_exact(2) {
                            let [Expr::Sym(ident), expr] = pair else {
                                return Err(QxErr::TypeErr {
                                    expected: Expr::Sym("symbol".to_string()),
                                    found: pair[0].clone(),
                                });
                            };

                            self.defenv(ident, expr)?;
                        }

                        let result = self.eval(to_eval.clone())?;
                        self.env = self.env.outer().expect("The env was just defined");

                        Ok(result)
                    }

                    _ => {
                        let Expr::List(new) = &self.replace_eval(ast)? else {
                            unreachable!()
                        };

                        match new.as_slice() {
                            [Expr::Func(func), args @ ..] => func.apply(self, args),
                            [Expr::Closure(closure), args @ ..] => closure.apply(self, args),

                            _ => Err(QxErr::NoArgs(None))?,
                        }
                    }
                }
            }
        } else {
            self.replace_eval(ast)
        }
    }

    fn defenv(&mut self, ident: &str, expr: &Expr) -> Result<Expr, QxErr> {
        let res = self.eval(expr.clone())?;
        self.env.set(ident, res);

        Ok(Expr::Nil)
    }

    // named eval_ast in mal
    // replaces symbols with their values
    // evaluates lists
    pub fn replace_eval(&mut self, ast: Expr) -> Result<Expr, QxErr> {
        Ok(match ast {
            Expr::Sym(sym) => self
                .env
                .get(&sym)
                .context(format!("Unbound identifier [ {sym} ]"))?,
            Expr::List(lst) => Expr::List(self.eval_mult(lst)?),
            val => val,
        })
    }
}
