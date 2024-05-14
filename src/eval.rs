use std::ops::ControlFlow;
use std::rc::Rc;

use anyhow::{anyhow, Context};

use crate::env::{Env, _Inner};
use crate::read::Closure;
use crate::{
    read::{Expr, QxErr, AST},
    Runtime,
};

macro_rules! err {
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

macro_rules! ret_ok {
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

// macro_rules! special {
// 	(  match $fn:ident, $lst:ident { $($name:literal: $p:pat, args: $form:literal => $exp:expr),+ }) => {

// 		match ($fn, &$lst[1..]) {

// 			$(
// 				($name, $p) => $exp,
// 				($name, _) => err!(form: $form),

// 				_ => Ok(None)
// 			)+

// 		}

// 	};
// }
// return special! {
// 	match ident, lst {
//     	"def!": [Expr::Sym(ident), expr], args: "<sym> <expr>" => self.defenv(ident, expr).map(Some)
// 	}
// };

#[derive(Debug)]
struct EvalTco {
    ast: Expr,
    env: Option<Env>,
}

impl Runtime {
    pub fn eval(&mut self, mut ast: Expr, mut env: Option<Env>) -> Result<Expr, QxErr> {
        'l: loop {
            return match ast {
                Expr::List(ref empty) if empty.is_empty() => Ok(ast.clone()),

                Expr::List(list) => {
                    let ident = list.first().ok_or(QxErr::NoArgs(None))?;

                    match self.apply(ident.clone(), &list, env.clone()) {
                        ControlFlow::Break(res) => res,

                        ControlFlow::Continue(EvalTco {
                            ast: new_ast,
                            env: new_env,
                        }) => {
                            ast = new_ast;
                            env = new_env;
                            continue 'l;
                        }
                    }
                }

                _ => self.replace_eval(ast.clone(), env),
            };
        }
    }

    fn apply(
        &mut self,
        ident: Expr,
        lst: &[Expr],
        env: Option<Env>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let Expr::Sym(ref ident) = ident else {
            return self.apply_func(Expr::List(lst.to_vec()), env);
        };

        match (ident as &str, &lst[1..]) {
            ("def!", [Expr::Sym(ident), expr]) => ControlFlow::Break(self.defenv(ident, expr, env)),
            ("def!", _) => err!(form: "(def! <sym> <expr>)"),

            ("fn*", [Expr::List(args), body]) => self.create_closure(&env, args, body),
            ("fn*", _) => err!(form: "(fn* (<args>*) <body>)"),

            ("if", [cond, then, ..]) => self.eval_if(lst, env, cond, then),
            ("if", _) => err!(form: "(if <condition> <then> ?<else>)"),

            ("let*", [Expr::List(new_bindings), to_eval]) => {
                // create new env, set as current, old env is now self.env.outer

                let env = _Inner::with_outer(self.env.clone());

                for pair in new_bindings.chunks_exact(2) {
                    let [Expr::Sym(ident), expr] = pair else {
                        return ControlFlow::Break(Err(QxErr::TypeErr {
                            expected: Expr::Sym("<sym>".to_string()),
                            found: pair[0].clone(),
                        }));
                    };

                    env.set(ident, expr.clone());
                }

                ControlFlow::Continue(EvalTco {
                    ast: to_eval.clone(),
                    env: Some(env),
                })
            }
            ("let*", _) => err!(form: "(let* (<sym> <expr>)+ <expr>)"),

            // ("prn", [arg]) => {
            //     println!("{arg:?}");
            //     ret_ok!(Expr::Nil)
            // }
            // ("prn", _) => err!(form: "(prn <expr>)"),
            ("do", [start @ .., last_expr]) => {
                for exp in start {
                    early_ret!(self.eval(exp.clone(), env.clone()));
                }

                // self.eval(end.clone(), None).map(Some)
                ControlFlow::Continue(EvalTco {
                    ast: last_expr.clone(),
                    env,
                })
            }
            ("do", _) => err!(form: "(do <expr>*)"),

            _ => self.apply_func(Expr::List(lst.to_vec()), env),
        }
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
            Expr::Bool(false) | Expr::Nil => ControlFlow::Continue(EvalTco {
                ast: early_ret!(lst.get(3).cloned(), or QxErr::NoArgs(None)),
                env,
            }),
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
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let x = args.iter().map(|it| {
            if let Expr::Sym(s) = it {
                Ok(s.clone())
            } else {
                Err(QxErr::Any(anyhow!("Not a symbol: {it:?}")))
            }
        });

        let cl = Closure::new(
            early_ret!(x.collect::<Result<_, _>>()),
            body.clone(),
            env.as_ref().unwrap_or(&self.env).clone(),
        );

        ret_ok!(Expr::Closure(cl))
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

        match new.as_slice() {
            [Expr::Func(func), args @ ..] => ControlFlow::Break(func.apply(self, args)),
            [Expr::Closure(Closure {
                args_name,
                captured,
                body,
            }), args @ ..] => {
                let new_env = Some(_Inner::with_outer_args(captured.clone(), args, args_name));

                ControlFlow::Continue(EvalTco {
                    ast: *body.clone(),
                    env: new_env,
                })
            }

            [err, ..] => err!(break "Not a Function: {err:?}"),
            [] => err!(break QxErr::NoArgs(None)),
        }
    }

    fn defenv(&mut self, ident: &str, expr: &Expr, env: Option<Env>) -> Result<Expr, QxErr> {
        let res = self.eval(expr.clone(), env)?;
        self.env.set(ident, res);

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
                .context(format!("Unbound identifier [ {sym} ]"))?,
            Expr::List(l) => Expr::List(
                l.into_iter()
                    .map(|it| self.eval(it, env.clone()))
                    .collect::<Result<_, _>>()?,
            ),
            val => val,
        })
    }
}
