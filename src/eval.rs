// use std::ops::ControlFlow;
use std::rc::Rc;

use anyhow::{anyhow, Context};

use crate::env::{Env, EnvObj};
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

enum ControlFlow<B, C> {
    Break(B),
    BreakNone,
    Continue(C),
}

struct EvalTco {
    ast: Expr,
    env: Option<Rc<Env>>,
}

impl Runtime {
    // doesnt exist in mal
    pub fn eval_mult(&mut self, ast: AST, env: Option<&Rc<Env>>) -> Result<AST, QxErr> {
        ast.into_iter()
            .map(|it| self.eval(it, env.cloned()))
            .collect::<Result<_, _>>()
    }

    pub fn eval(&mut self, mut ast: Expr, mut env: Option<Rc<Env>>) -> Result<Expr, QxErr> {
        'l: loop {
            return match ast {
                Expr::List(ref lst) if lst.is_empty() => Ok(ast.clone()),
                Expr::List(ref lst) => {
                    let ident = if let Expr::Sym(s) = &lst[0] {
                        s.as_str()
                    } else {
                        return self.apply_func(ast, env.as_ref());
                    };

                    match self.specials(ident, lst, env.clone()) {
                        ControlFlow::Break(res) => res,
                        ControlFlow::BreakNone => self.apply_func(ast, env.as_ref()),
                        ControlFlow::Continue(EvalTco {
                            ast: new_ast,
                            env: new_env,
                        }) => {
                            ast = new_ast;
                            env = new_env;
                            continue 'l;
                        }
                    }
                    // Option::map
                    // self.specials(ident, lst)?
                    //     .map_or_else(|| self.apply_func(ast, env), Ok)
                }

                _ => self.replace_eval(ast.clone(), env.as_ref()),
            };
        }
    }

    fn specials(
        &mut self,
        ident: &str,
        lst: &[Expr],
        env: Option<Rc<Env>>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        match (ident, &lst[1..]) {
            ("def!", [Expr::Sym(ident), expr]) => ControlFlow::Break(self.defenv(ident, expr)),
            ("def!", _) => err!(form: "(def! <sym> <expr>)"),

            ("fn*", [Expr::List(args), body]) => {
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
                    Rc::clone(env.as_ref().unwrap_or(&self.env)),
                );

                ret_ok!(Expr::Closure(cl))
            }
            ("fn*", _) => err!(form: "(fn* (<args>*) <body>)"),

            ("if", [cond, then, ..]) => {
                let cond = early_ret!(self.eval(cond.clone(), None));

                match cond {
                    Expr::Bool(false) | Expr::Nil => ControlFlow::Continue(EvalTco {
                        ast: early_ret!(lst.get(3).cloned(), or QxErr::NoArgs(None)),
                        env: None,
                    }),
                    _ => ControlFlow::Continue(EvalTco {
                        ast: then.clone(),
                        env: None,
                    }),
                }
            }
            ("if", _) => err!(form: "(if <condition> <then> ?<else>)"),

            ("let*", [Expr::List(new_bindings), to_eval]) => {
                // create new env, set as current, old env is now self.env.outer

                let env = Env::with_outer(Rc::clone(&self.env));

                for pair in new_bindings.chunks_exact(2) {
                    let [Expr::Sym(ident), expr] = pair else {
                        return ControlFlow::Break(Err(QxErr::TypeErr {
                            expected: Expr::Sym("<sym>".to_string()),
                            found: pair[0].clone(),
                        }));
                    };

                    env.set(ident, expr.clone());
                    // self.defenv(ident, expr)?;
                }

                // let result = self.eval(to_eval.clone(), Some(&env))?;
                // self.env = self.env.outer().expect("The env was just defined");

                // ret_ok!(result)

                ControlFlow::Continue(EvalTco {
                    ast: to_eval.clone(),
                    env: Some(dbg!(env)),
                })
            }
            ("let*", _) => err!(form: "(let* (<sym> <expr>)+ <expr>)"),

            ("prn", [arg]) => {
                println!("{arg}");
                ret_ok!(Expr::Nil)
            }
            ("prn", _) => err!(form: "(prn <expr>)"),

            ("do", [start @ .., end]) => {
                for exp in start {
                    early_ret!(self.eval(exp.clone(), None));
                }

                // self.eval(end.clone(), None).map(Some)
                ControlFlow::Continue(EvalTco {
                    ast: end.clone(),
                    env: None,
                })
            }
            ("do", _) => err!(form: "(do <expr>*)"),

            _ => ControlFlow::BreakNone,
        }
    }

    fn apply_func(&mut self, ast: Expr, env: Option<&Rc<Env>>) -> Result<Expr, QxErr> {
        let Expr::List(new) = &self.replace_eval(ast, env)? else {
            unreachable!()
        };

        match new.as_slice() {
            [Expr::Func(func), args @ ..] => func.apply(self, args),
            [Expr::Closure(closure), args @ ..] => closure.apply(self, args),

            [err, ..] => err!("Not a Function: {err:?}"),
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
