use std::rc::Rc;

use anyhow::Context;

use crate::{
    read::{AST, Expr, QxErr},
    Runtime,
};
use crate::env::{Env, EnvObj};

impl Runtime {
    // doesnt exist in mal
    pub fn eval_mult(&mut self, ast: AST) -> Result<AST, QxErr> {
        ast.into_iter()
            .map(|it| self.eval(it))
            .collect::<Result<_, _>>()
    }

    fn eval(&mut self, ast: Expr) -> Result<Expr, QxErr> {
        if let Expr::List(lst) = &ast {
            if lst.len() == 0 {
                Ok(ast)
            } else {
                match lst.as_slice() {
                    [Expr::Sym(s), Expr::Sym(ident), expr] if s == "def!" => {
                        self.defenv(ident, expr)
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

                        let [Expr::Func(func), args @ ..] = new.as_slice() else {
                            Err(QxErr::NoArgs(None))?
                        };

                        func.apply(self, args)
                    }
                }
            }
        } else {
            self.replace_eval(ast)
        }
    }

    fn defenv(&mut self, ident: &String, expr: &Expr) -> Result<Expr, QxErr> {
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
                .context(format!("Unbound identifier: {sym}"))?
                .clone(),
            Expr::List(lst) => Expr::List(self.eval_mult(lst)?),
            val => val,
        })
    }
}
