use anyhow::Context;

use crate::{read::{AST, Expr, QxErr}, Runtime};

impl Runtime {
    pub fn eval_mult(&mut self, ast: AST) -> Result<AST, QxErr> {
        ast.into_iter().map(|it| self._eval(it)).collect::<Result<_, _>>()
    }

    fn apply(_: ()) {
        todo!()
    }

    fn _eval(&mut self, ast: Expr) -> Result<Expr, QxErr> {
        if let Expr::List(lst) = &ast {
            if lst.len() == 0 {
                Ok(ast)
            } else {
                let Expr::List(new) = &self.eval_ast(ast)? else {
                    unreachable!()
                };

                let [Expr::Func(func), args @ ..] = &new.as_slice() else {
                    Err(QxErr::NoArgs(None))?
                };
                
                func.apply(self, args)
            }
        } else {
            self.eval_ast(ast)
        }
    }

    pub fn eval_ast(&mut self, ast: Expr) -> Result<Expr, QxErr> {
        Ok(match ast {
            Expr::Sym(sym) => self.env.get(&sym).context("Symbol not found")?.clone(),
            Expr::List(lst) => Expr::List(self.eval_mult(lst)?),
            val => val,
        })
    }
}