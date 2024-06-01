use crate::env::{Env, Inner};
use crate::read::types::closure::Closure;
use crate::{expr, Apply};
use crate::{
    read::{Expr, QxErr},
    Runtime,
};
use anyhow::Ok as __aok;
use anyhow::{anyhow, Context};
use std::ops::ControlFlow;
use std::rc::Rc;

// helper for error because we can't use `?`
// needed due to ControlFlow for TCO
macro_rules! err {
    // for special form args errors
    (form: $form:literal) => {
        return ControlFlow::Break(Err(QxErr::Any(anyhow!(concat!("Correct Form: ", $form)))))
    };

    ($err:literal) => {
        Err(QxErr::Any(anyhow!($err)))
    };

    (break $err:literal) => {
        ControlFlow::Break(Err(QxErr::Any(anyhow!($err))))
    };
    (break $err:expr) => {
        ControlFlow::Break(Err($err))
    };
}

macro_rules! break_ok {
    ($ex:expr) => {
        ControlFlow::Break(Ok($ex))
    };
}

macro_rules! early_ret {
    ($e:expr) => {
        match $e {
            Ok(o) => o,
            Err(e) => return ControlFlow::Break(Err(e)),
        }
    };

    ($e:expr, or $err:expr) => {
        match $e {
            Some(o) => o,
            None => return ControlFlow::Break(Err($err)),
        }
    };
}

#[derive(Debug)]
struct EvalTco {
    ast: Expr,
    env: Option<Env>,
}

impl Runtime {
    pub fn eval(&mut self, mut ast: Expr, mut env: Option<Env>) -> Result<Expr, QxErr> {
        loop {
            ast = self.macroexpand(ast, &env)?;
            match ast {
                Expr::List(ref empty) if empty.is_empty() => return Ok(ast.clone()),

                Expr::List(ref list) => {
                    let list = list.clone();

                    let ident = list.first().ok_or(QxErr::NoArgs(None))?;

                    match self.apply(ident, list.clone(), env.clone()) {
                        ControlFlow::Break(res) => return res,

                        ControlFlow::Continue(EvalTco {
                            ast: new_ast,
                            env: new_env,
                        }) => {
                            ast = new_ast;
                            env = new_env;
                            continue;
                        }
                    }
                }

                _ => return self.replace_eval(ast, env),
            };
        }
    }

    fn apply(
        &mut self,
        ident: &Expr,
        lst: Rc<[Expr]>,
        env: Option<Env>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let Expr::Sym(ref ident) = ident else {
            return self.apply_func(Expr::List(lst), env);
        };

        match (ident as &str, &lst[1..]) {
            ("val!", [Expr::Sym(ident), expr]) => ControlFlow::Break(self.defenv(ident, expr, env)),
            ("val!", _) => err!(form: "(val! <sym> <expr>)"),

            // form: (try* <expr> (catch* <sym> <expr>))
            ("try*", [try_expr, Expr::List(catch)])
                if matches!(
                    &**catch,
                    [
                    Expr::Sym(catch),
                    Expr::Sym(_catch_to),
                    _catch_expr
                    ] if &**catch == "catch*"
                ) =>
            {
                self.eval_trycatch(try_expr, &env, catch)
            }

            ("def?", [Expr::Sym(s)]) => env
                .as_ref()
                .unwrap_or(&self.env)
                .get(s)
                .unwrap_or(Expr::Nil)
                .apply(Ok)
                .apply(ControlFlow::Break),

            ("defmacro!", [Expr::Sym(ident), Expr::List(args), body]) => {
                let closure = self.create_closure(&env, args, body, true);
                match closure {
                    ControlFlow::Break(Ok(Expr::Closure(cl))) => {
                        self.env.set(ident, Expr::Closure(cl));
                        break_ok!(expr!(nil))
                    }

                    _ => unreachable!(),
                }
            }
            ("defmacro!", _) => err!(form: "(defmacro! <name> <args> <body>)"),

            ("mexp", ast @ [Expr::Sym(_), ..]) => {
                ControlFlow::Break(self.macroexpand(Expr::List(ast.into()), &env))
            }
			("mexp", _) => err!(form: "(mexp (<macro> <args>*))"),

            ("fn*", [Expr::List(args), body]) => self.create_closure(&env, args, body, false),
            ("fn*", _) => err!(form: "(fn* (<args>*) <body>)"),

            ("if", [cond, then, ..]) => self.eval_if(&lst, env, cond, then),
            ("if", _) => err!(form: "(if <condition> <then> ?<else>)"),

            ("let*", [Expr::List(new_bindings), to_eval]) => {
                self.eval_do(&env, new_bindings, to_eval)
            }
            ("let*", _) => err!(form: "(let* (<sym> <expr>)+ <expr>)"),

            ("quote", [expr]) => ControlFlow::Break(Ok(expr.clone())),
            ("quote", _) => err!(form: "(quote <expr>)"),

            ("quasiquote", [expr]) => ControlFlow::Continue(EvalTco {
                ast: quasiquote(expr),
                env,
            }),
            ("qqex", [expr]) => ControlFlow::Break(Ok(quasiquote(expr))),
            ("quasiquote", _) => err!(form: "(quasiquote <expr>)"),

            ("do", exprs) => {
                let last_expr = match exprs.last() {
                    None => err!(form: "(do <expr>*)"),
                    Some(expr) => expr,
                };

                if exprs.len() > 1 {
                    for exp in &exprs[..exprs.len() - 1] {
                        early_ret!(self.eval(exp.clone(), env.clone()));
                    }
                }

                ControlFlow::Continue(EvalTco {
                    ast: last_expr.clone(),
                    env,
                })
            }
            _ => self.apply_func(Expr::List(lst.to_vec().into_boxed_slice().into()), env),
        }
    }

    fn eval_trycatch(
        &mut self,
        try_expr: &Expr,
        env: &Option<Env>,
        catch: &Rc<[Expr]>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let res = self.eval(try_expr.clone(), env.clone());
        if let Err(e) = res {
            let Expr::Sym(catch_to_sym) = &catch[1] else {
                unreachable!()
            };

            let mut err_env = Env::with_outer(env.as_ref().unwrap_or_else(|| &self.env).clone());

            err_env.set(
                catch_to_sym,
                match e {
                    QxErr::LispErr(e) => e,
                    _ => Expr::String(e.to_string().into()),
                },
            );

            ControlFlow::Continue(EvalTco {
                ast: catch[2].clone(),
                env: Some(err_env),
            })
        } else {
            ControlFlow::Break(res)
        }
    }

    fn eval_do(
        &mut self,
        env: &Option<Env>,
        new_bindings: &Rc<[Expr]>,
        to_eval: &Expr,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        // create new env, set as current, old env is now self.env.outer

        let mut env = Env::with_outer(env.as_ref().unwrap_or_else(|| &self.env).clone());

        for pair in new_bindings.chunks_exact(2) {
            let [Expr::Sym(ident), expr] = pair else {
                return ControlFlow::Break(Err(QxErr::TypeErr {
                    expected: Box::new(expr!(sym "<sym>")),
                    found: Box::new(pair[0].clone()),
                }));
            };

            let val = self.eval(expr.clone(), Some(env.clone()));
            env.set(ident, early_ret!(val));
        }

        ControlFlow::Continue(EvalTco {
            ast: to_eval.clone(),
            env: Some(env),
        })
    }

    fn eval_if(
        &mut self,
        lst: &[Expr],
        env: Option<Env>,
        cond: &Expr,
        then: &Expr,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let cond = early_ret!(self.eval(cond.clone(), env.clone()));

        match cond {
            Expr::Bool(false) | Expr::Nil => lst.get(3).map_or_else(
                || ControlFlow::Break(Ok(Expr::Nil)),
                |else_branch| {
                    ControlFlow::Continue(EvalTco {
                        ast: else_branch.clone(),
                        env,
                    })
                },
            ),
            _ => ControlFlow::Continue(EvalTco {
                ast: then.clone(),
                env,
            }),
        }
    }

    fn create_closure(
        &mut self,
        env: &Option<Env>,
        args: &[Expr],
        body: &Expr,
        is_macro: bool,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let args = args
            .iter()
            .map(|it| match it {
                Expr::Sym(s) => Ok(Rc::clone(s)),
                _ => Err(QxErr::Any(anyhow!("Not a symbol: {it:?}"))),
            })
            .collect::<Result<_, _>>();

        let cl = Closure::new(
            early_ret!(args),
            body.clone(),
            env.as_ref().unwrap_or(&self.env).clone(),
            is_macro,
        );

        break_ok!(Expr::Closure(Rc::new(cl)))
    }

    fn apply_func(
        &mut self,
        ast: Expr,
        env: Option<Env>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let new = match self.replace_eval(ast, env) {
            Ok(Expr::List(new)) => new,
            Ok(wrong) => return err!(break "Not a List: {wrong:?}"),
            Err(e) => return err!(break e),
        };

        match &*new {
            [Expr::Func(func), args @ ..] => ControlFlow::Break(func.apply(self, args)),

            [Expr::Closure(cl), args @ ..] => ControlFlow::Continue(EvalTco {
                ast: *cl.body.clone(),
                env: Some(early_ret!(cl.create_env(args))),
            }),

            [err, ..] => err!(break "Not a Function: {err:?}"),
            [] => err!(break QxErr::NoArgs(None)),
        }
    }

    fn defenv(
        &mut self,
        ident: &Rc<str>,
        expr: &Expr,
        mut env: Option<Env>,
    ) -> Result<Expr, QxErr> {
        let res = self.eval(expr.clone(), env.clone())?;
        env.as_mut().unwrap_or(&mut self.env).set(ident, res);

        Ok(Expr::Nil)
    }

    // named eval_ast in mal
    // replaces symbols with their values
    // evaluates lists
    pub fn replace_eval(&mut self, ast: Expr, env: Option<Env>) -> Result<Expr, QxErr> {
        Ok(match ast {
            Expr::Sym(sym) => env
                .unwrap_or_else(|| self.env.clone())
                .get(&sym)
                .or_else(|| is_special_form(&sym).then_some(expr!(sym sym.clone())))
                .context(format!("Unbound identifier [ {sym} ]"))?,

            Expr::List(l) => Expr::List(
                l.iter()
                    .map(|it| self.eval(it.clone(), env.clone()))
                    .collect::<Result<_, _>>()?,
            ),
            val => val,
        })
    }

    /// Returns the macro and args to it,
    /// if the AST is a list, whose first element is a macro in the environment
    fn is_macro_call<'a>(
        &mut self,
        ast: &'a Expr,
        env: &Option<Env>,
    ) -> Option<(Rc<Closure>, &'a [Expr])> {
        if let Expr::List(lst) = ast {
            if let [Expr::Sym(sym), ..] = &**lst {
                match env.as_ref().unwrap_or(&self.env).get(sym) {
                    Some(Expr::Closure(cl)) => cl.is_macro.then_some((cl, &lst[1..])),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn macroexpand(&mut self, mut ast: Expr, env: &Option<Env>) -> Result<Expr, QxErr> {
        while let Some((macro_cl, args)) = self.is_macro_call(&ast, env) {
            let new_env = macro_cl.create_env(args)?;

            ast = self.eval(*macro_cl.body.clone(), Some(new_env))?;
        }

        Ok(ast)
    }
}

fn is_special_form(sym: &str) -> bool {
    matches!(
        sym,
        "val!"
            | "fn*"
            | "if"
            | "let*"
            | "do"
            | "quote"
            | "quasiquote"
            | "unquote"
            | "splice-unquote"
            | "defmacro!"
    )
}

fn qq_list(elts: &[Expr]) -> Expr {
    let mut acc = vec![];
    for elt in elts.iter().rev() {
        if let Expr::List(v) = elt {
            if v.len() == 2 {
                if let Expr::Sym(ref s) = v[0] {
                    if &**s == "splice-unquote" {
                        acc = vec![expr!(concat), v[1].clone(), Expr::List(acc.into())];
                        continue;
                    }
                }
            }
        }
        acc = vec![expr!(cons), quasiquote(elt), Expr::List(acc.into())];
    }
    Expr::List(acc.into())
}

fn quasiquote(ast: &Expr) -> Expr {
    match ast {
        Expr::List(v) => {
            if v.len() == 2 {
                if let Expr::Sym(ref s) = v[0] {
                    if &**s == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            qq_list(v)
        }
        _ => ast.clone(),
    }
}
