use crate::env::Env;
use crate::read::types::closure::Closure;
use crate::read::{cons, Cons, ConsCell, ExprType};
use crate::{expr, special_form, Apply, Func};
use crate::{
    read::{Expr, QxErr},
    Runtime,
};
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
                Expr::Cons(Cons(None)) => return Ok(ast),

                Expr::Cons(list @ Cons(Some(_))) => {
                    let ident = &list.car().ok_or(QxErr::NoArgs(None))?;

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
        lst: Cons,
        env: Option<Env>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let Expr::Sym(ref ident) = ident else {
            return self.apply_func(Expr::Cons(lst), env);
        };

        let xs = lst.cdr();
        let args = xs.clone().into_iter();

        match ident as &str {
            "val!" => special_form! {
                args, "(val! <sym> <expr>)";
                [Expr::Sym(ident), expr] => {
                    ControlFlow::Break(self.defenv(&ident, &expr, env))
                }
            },

            "defmacro!" => special_form! {
                args, "(defmacro! <name> <args> <body>)";
                [Expr::Sym(ident), Expr::Cons(cl_args), body] => {
                    let ControlFlow::Break(Ok(cl @ Expr::Closure(_)))
                            = self.create_closure(&env, cl_args, &body, true)
                    else {
                        unreachable!();
                    };

                    self.env.set(&ident, cl);

                    break_ok!(expr!(nil))
                }
            },

            "try*" => special_form! {
                args, "(try* <expr> (catch* <sym> <expr>)";
                [try_expr, Expr::Cons(catch)] => {
                    self.eval_trycatch(&try_expr, &env, catch)
                }
            },

            "fn*" => special_form! {
                args, "(fn* (<args>*) <body>)";
                [Expr::Cons(args), body] => self.create_closure(&env, args, &body, false)
            },

            "if" => special_form! {
                args, "(if <condition> <then> <else>)";
                [cond, then] ..xs => self.eval_if(xs, env, &cond, &then)
            },

            "let*" => special_form! {
                args, "(let* (<sym> <expr>)+ <expr>)";
                [Expr::Cons(bindings), to_eval] => {
                    self.eval_do(&env, bindings, &to_eval)
                }
            },

            "quote" => special_form! {
                args, "(quote <expr>)";
                [expr] => ControlFlow::Break(Ok(expr.clone()))
            },

            "qqex" => special_form! {
                args, "(qqex <expr>)";
                [expr] => ControlFlow::Break(quasiquote(&expr))
            },

            "quasiquote" => special_form! {
                args, "(quasiquote <expr>)";
                [expr] => ControlFlow::Continue(EvalTco {
                    ast: early_ret!(quasiquote(&expr)),
                    env
                })
            },

            "do" => {
                let mut peekable = args.peekable();

                if peekable.peek().is_none() {
                    err!(form: "(do <expr>*)");
                }

                // evaluate until the last expr, breaking it from the loop
                let last_expr = loop {
                    let it = peekable.next().unwrap();
                    if peekable.peek().is_none() {
                        break it;
                    } else {
                        early_ret!(self.eval(it, env.clone()));
                    }
                };

                // return it using TCO
                ControlFlow::Continue(EvalTco {
                    ast: last_expr,
                    env,
                })
            }

            _ => self.apply_func(Expr::Cons(lst), env),
        }
    }

    fn eval_trycatch(
        &mut self,
        try_expr: &Expr,
        env: &Option<Env>,
        catch: Cons,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        // check that the catch expression is valid
        let mut c_iter = catch.into_iter();
        if !matches!(c_iter.next(), Some(Expr::Sym(catch)) if &*catch == "catch*") {
            err!(form: "(try* <expr> (catch* <sym> <expr>))")
        }

        let Some(Expr::Sym(catch_to_sym)) = c_iter.next() else {
            err!(form: "(try* <expr> (catch* <sym> <expr>))")
        };

        let Some(catch_expr) = c_iter.next() else {
            err!(form: "(try* <expr> (catch* <sym> <expr>))")
        };

        // evaluate the expression
        let res = self.eval(try_expr.clone(), env.clone());

        // if its an error, evaluate the catch expression
        if let Err(e) = res {
            // get the symbol to bind the error to

            // create the env with the error
            let mut err_env = Env::with_outer(env.as_ref().unwrap_or(&self.env).clone());

            // bind the error
            err_env.set(
                &catch_to_sym,
                match e {
                    QxErr::LispErr(e) => e,
                    _ => Expr::String(e.to_string().into()),
                },
            );

            // evaluate the catch expression
            ControlFlow::Continue(EvalTco {
                ast: catch_expr,
                env: Some(err_env),
            })
        } else {
            ControlFlow::Break(res)
        }
    }

    fn eval_do(
        &mut self,
        env: &Option<Env>,
        new_bindings: Cons,
        to_eval: &Expr,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        // create new env, set as current, old env is now self.env.outer

        let mut env = Env::with_outer(env.as_ref().unwrap_or(&self.env).clone());

        for [binding, expr] in new_bindings.pair_iter() {
            let Expr::Sym(ident) = binding else {
                return ControlFlow::Break(Err(QxErr::TypeErr {
                    expected: ExprType::Sym,
                    found: binding.get_type(),
                }));
            };

            let val = self.eval(expr, Some(env.clone()));
            env.set(&ident, early_ret!(val));
        }

        ControlFlow::Continue(EvalTco {
            ast: to_eval.clone(),
            env: Some(env),
        })
    }

    fn eval_if(
        &mut self,
        otherwise: Cons,
        env: Option<Env>,
        cond: &Expr,
        then: &Expr,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let cond = early_ret!(self.eval(cond.clone(), env.clone()));

        match cond {
            Expr::Bool(false) | Expr::Nil => otherwise.car().map_or_else(
                || ControlFlow::Break(Ok(Expr::Nil)),
                |ast| ControlFlow::Continue(EvalTco { ast, env }),
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
        args: Cons,
        body: &Expr,
        is_macro: bool,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let args = args
            .into_iter()
            .map(|it| match it {
                Expr::Sym(s) => Ok(Rc::clone(&s)),
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
        let new = match self.replace_eval(ast, env.clone()) {
            Ok(Expr::Cons(new)) => new,
            Ok(wrong) => return err!(break "Not a List: {wrong:?}"),
            Err(e) => return err!(break e),
        };

        match new.car() {
            Some(Expr::Func(func)) => ControlFlow::Break(Func::apply(&func, self, new.cdr(), env)),

            Some(Expr::Closure(cl)) => ControlFlow::Continue(EvalTco {
                ast: cl.body.clone(),
                env: Some(early_ret!(cl.create_env(new.cdr()))),
            }),

            Some(err) => err!(break "Not a Function: {err:?}"),
            None => err!(break QxErr::NoArgs(None)),
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

            Expr::Cons(l) => Expr::Cons({
                l.into_iter()
                    .map(|it| self.eval(it.clone(), env.clone()))
                    .collect::<Result<_, _>>()?
            }),
            val => val,
        })
    }

    /// Returns the macro and args to it
    /// if the AST is a list, whose first element is a macro
    fn is_macro_call<'a>(
        &mut self,
        ast: &'a Expr,
        env: &Option<Env>,
    ) -> Option<(Rc<Closure>, Cons)> {
        if let Expr::Cons(lst) = ast {
            if let Some(Expr::Sym(sym)) = lst.car() {
                match env.as_ref().unwrap_or(&self.env).get(&sym) {
                    Some(Expr::Closure(cl)) => cl.is_macro.then_some((cl, lst.cdr())),
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

            ast = self.eval(macro_cl.body.clone(), Some(new_env))?;
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
            | "try*"
            | "catch*"
    )
}

fn qq_list(elts: Cons) -> Result<Expr, QxErr> {
    // TODO: rewrite with linked lists
    // let new = elts.into_iter().try_fold(Cons::nil(), |acc, elt| {
    //     // if the el is not unquoted, add (list <el>)
    //     // if its spliced, add <el>
    //     // else add (list '<el>)

	// 		let new = match elt {
	// 			Expr::Cons(c) if matches!(c.car(), Some(Expr::Sym(ref s)) if &**s == "unquote" ) => {
	// 				Expr::Cons(cons(expr!(sym "list"), c.cdr()))
	// 				// expr!(cons expr!(sym "list"), Expr::Cons(c.cdr()))
	// 			},
				
	// 			Expr::Cons(c) if matches!(c.car(), Some(Expr::Sym(ref s)) if &**s == "splice-unquote" ) => {
	// 				let Some(Expr::Cons(inner)) = c.cdr().car() else {
	// 					return Err(QxErr::NoArgs(None))?;
	// 					// unreachable!();
	// 				};

	// 				Expr::Cons(inner)
	// 			}

	// 			elt => expr!(cons expr!(sym "quasiquote"), elt),
	// 		};

	// 		Ok::<_, QxErr>(cons(new, acc))
    // 	})?.apply(|it| {
	// 		cons(expr!(sym "concat"), it.reversed())
	// 	});

    // // todo!()
    // let elts = elts.into_iter().collect::<Vec<_>>();

    let mut acc = vec![];
	let elts = elts.into_iter().collect::<Vec<_>>();
    for elt in elts.into_iter().rev() {
        match elt {
            Expr::Cons(ref v) if v.len_is(2) => {
                if matches!(v.car(), Some(Expr::Sym(ref s)) if &** s == "splice-unquote") {
                    acc = vec![expr!(concat), v.nth(1).ok_or(QxErr::NoArgs(None))?, Expr::Cons(acc.into())];
                    continue;
                }
            }
            _ => {}
        }
        acc = vec![expr!(cons), quasiquote(&elt)?, Expr::Cons(acc.into())];
    }

    Ok(Expr::Cons(acc.into()))
    // Ok(Expr::Cons(new))
}

fn quasiquote(ast: &Expr) -> Result<Expr, QxErr> {
    match ast {
        Expr::Cons(v) => {
            if v.clone().len_is(2) {
                if let Expr::Sym(ref s) = v.car().unwrap() {
                    if &**s == "unquote" {
                        return Ok(v.cdr().car().unwrap());
                    }
                }
            }

            qq_list(v.clone())
        }
        _ => Ok(dbg!(ast.clone())),
    }
}
