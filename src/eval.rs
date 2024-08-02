use anyhow::anyhow;
use ecow::EcoString;
use special_form_code::SpecialForm;
use std::{ops::ControlFlow, rc::Rc};
use tap::prelude::*;

use crate::{
    env::Env,
    expr,
    read::{cons, types::closure::Closure, Cons, Expr, ExprType, QxErr},
    special_form, Func, Runtime,
};

#[derive(Clone, Copy, Debug)]
pub enum Shadow {
    Yes,
    No,
}

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
    pub fn eval(&mut self, mut in_ast: Expr, mut env: Option<Env>) -> Result<Expr, QxErr> {
        // loop for TCO
        loop {
            let ast = self.macroexpand(in_ast, &env)?;
            // by default, break the result out of the loop
            break match ast {
                Expr::List(Cons(None)) => Ok(ast),

                Expr::List(ref list) => {
                    let ident = &list.car().unwrap();

                    match self.apply(ident, list.clone(), env.clone()) {
                        ControlFlow::Break(res) => res,

                        ControlFlow::Continue(EvalTco {
                            ast: ast_n,
                            env: env_n,
                        }) => {
                            in_ast = ast_n;
                            env = env_n;

                            continue;
                        }
                    }
                }

                _ => self.replace_eval(ast, env),
            };
        }
    }

    fn apply(
        &mut self,
        ident: &Expr,
        lst: Cons,
        env: Option<Env>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let xs = lst.cdr();
        let args = xs.into_iter();

        let Expr::Sym(ref ident) = ident else {
            return self.apply_func(Expr::List(lst), env);
        };

        let Ok(opcode) = ident.parse() else {
            return self.apply_func(Expr::List(lst), env);
        };

        match opcode {
            SpecialForm::Val => special_form! {
                args, "(val! <sym> <expr>)";
                [Expr::Sym(ident), expr] => {
                    ControlFlow::Break(
                        self.defenv(&ident, &expr, env, Shadow::No)
                    )
                }
            },

            SpecialForm::Del => special_form! {
                args, "(del! <sym>)";
                [Expr::Sym(s)] => {
                    let r = env
                        .as_ref()
                        .unwrap_or(&self.env)
                        .remove(&s)
                        .map_or(Expr::Nil, id);
                    break_ok!(r)
                }
            },

            SpecialForm::Defmacro => special_form! {
                args, "(defmacro! <name> <args> <body>)";
                [Expr::Sym(ident), Expr::List(cl_args), body] => {
                    let ControlFlow::Break(Ok(cl @ Expr::Closure(_)))
                            = self.create_closure(&env, cl_args, &body, true)
                    else {
                        unreachable!();
                    };

                    env.unwrap_or_else(|| self.env.clone()).set(ident, cl, Shadow::Yes).pipe(ControlFlow::Break)
                }
            },

            SpecialForm::Mexp => special_form! {
                args, "(mexp <macro> <&args>)";
                [] ..it => ControlFlow::Break(self.macroexpand(Expr::List(it), &env))
            },

            SpecialForm::Try => special_form! {
                args, "(try* <expr> (catch* <sym> <expr>)";
                [try_expr, Expr::List(catch)] => {
                    self.eval_trycatch(&try_expr, &env, &catch)
                }
            },

            SpecialForm::Fn => special_form! {
                args, "(fn* (<args>*) <body>)";
                [Expr::List(args), body] => self.create_closure(&env, args, &body, false)
            },

            SpecialForm::If => special_form! {
                args, "(if <condition> <then> <else>)";
                [cond, then] ..xs => self.eval_if(&xs, env, &cond, &then)
            },

            SpecialForm::Let => special_form! {
                args, "(let* (<sym> <expr>)+ <expr>)";
                [Expr::List(bindings), to_eval] => {
                    self.eval_let(&env, bindings, &to_eval)
                }
            },

            SpecialForm::Quote => special_form! {
                args, "(quote <expr>)";
                [expr] => ControlFlow::Break(Ok(expr))
            },

            SpecialForm::Qqex => special_form! {
                args, "(qqex <expr>)";
                [expr] => ControlFlow::Break(quasiquote(&expr))
            },

            SpecialForm::QuasiQuote => special_form! {
                args, "(quasiquote <expr>)";
                [expr] => ControlFlow::Continue(EvalTco {
                    ast: early_ret!(quasiquote(&expr)),
                    env
                })
            },

            SpecialForm::Do => {
                let mut peekable = args.peekable();

                if peekable.peek().is_none() {
                    err!(form: "(do <expr>*)");
                }

                // evaluate until the last expr, breaking it from the loop
                let last_expr = loop {
                    let it = peekable.next().unwrap();
                    if peekable.peek().is_none() {
                        break it;
                    }
                    early_ret!(self.eval(it, env.clone()));
                };

                // return it using TCO
                ControlFlow::Continue(EvalTco {
                    ast: last_expr,
                    env,
                })
            }
        }
    }

    fn eval_trycatch(
        &mut self,
        try_expr: &Expr,
        env: &Option<Env>,
        catch: &Cons,
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

        match res {
            Err(QxErr::Fatal(_) | QxErr::Stop) | Ok(_) => ControlFlow::Break(res),

            // if its a recoverable error, evaluate the catch expression
            Err(e) => {
                // get the symbol to bind the error to

                // create the env with the error
                let err_env = Env::with_outer(env.as_ref().unwrap_or(&self.env).clone());

                // bind the error
                err_env
                    .set(
                        catch_to_sym,
                        match e {
                            QxErr::LispErr(e) => e,
                            _ => Expr::String(e.to_string().into()),
                        },
                        Shadow::No,
                    )
                    .unwrap();

                // evaluate the catch expression
                ControlFlow::Continue(EvalTco {
                    ast: catch_expr,
                    env: Some(err_env),
                })
            }
        }
    }

    fn eval_let(
        &mut self,
        env: &Option<Env>,
        new_bindings: Cons,
        to_eval: &Expr,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        // create new env, set as current, old env is now self.env.outer

        let env = Env::with_outer(env.as_ref().unwrap_or(&self.env).clone());

        for [binding, expr] in new_bindings.pair_iter() {
            let Expr::Sym(ident) = binding else {
                return ControlFlow::Break(Err(QxErr::TypeErr {
                    expected: ExprType::Sym,
                    found: binding.get_type(),
                }));
            };

            let val = self.eval(expr, Some(env.clone()));
            early_ret!(env.set(ident, early_ret!(val), Shadow::No));
        }

        ControlFlow::Continue(EvalTco {
            ast: to_eval.clone(),
            env: Some(env),
        })
    }

    fn eval_if(
        &mut self,
        otherwise: &Cons,
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
        let env = env.as_ref().unwrap_or(&self.env).clone();

        let cl = Closure::new(args, body.clone(), env, is_macro);

        break_ok!(Expr::Closure(Rc::new(early_ret!(cl))))
    }

    fn apply_func(
        &mut self,
        ast: Expr,
        env: Option<Env>,
    ) -> ControlFlow<Result<Expr, QxErr>, EvalTco> {
        let new = match self.replace_eval(ast, env.clone()) {
            Ok(Expr::List(new)) => new,
            Ok(wrong) => return err!(break "Not a List: {wrong:?}"),
            Err(e) => return err!(break e),
        };

        match new.car() {
            Some(Expr::Func(func)) => ControlFlow::Break(Func::apply(&func, self, new.cdr(), env)),

            Some(Expr::Closure(cl)) => ControlFlow::Continue(EvalTco {
                ast: cl.body.clone(),
                env: Some(early_ret!(cl.create_env(new.cdr(), self))),
            }),

            Some(Expr::Keyword(k)) => {
                let Some(Expr::Map(m)) = new.cdr().car() else {
                    return err!(break "Not a Map: {new:?}");
                };

                ControlFlow::Break(m.get(&k).map_or_else(|| Expr::Nil, Clone::clone).pipe(Ok))
            }

            Some(err) => err!(break "Not a Function: {err:?}"),

            None => unreachable!(),
        }
    }

    /// Todo: fix memory leak due to ref cycle without UB
    pub fn defenv(
        &mut self,
        ident: &EcoString,
        expr: &Expr,
        mut env: Option<Env>,
        over: Shadow,
    ) -> Result<Expr, QxErr> {
        let res = self.eval(expr.clone(), env.clone())?;
        let env = env.as_mut().unwrap_or(&mut self.env);

        env.set(ident.clone(), res, over)?;

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
                .ok_or_else(|| QxErr::NoDefErr(sym))?,

            Expr::List(l) => Expr::List({
                l.into_iter()
                    .map(|it| self.eval(it, env.clone()))
                    .collect::<Result<_, _>>()?
            }),
            val => val,
        })
    }

    /// Returns the macro and args to it
    /// if the AST is a list, whose first element is a macro
    fn is_macro_call(&self, ast: &Expr, env: &Option<Env>) -> Option<(Rc<Closure>, Cons)> {
        if let Expr::List(lst) = ast {
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
            let new_env = macro_cl.create_env(args, self)?;

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
            | "del!"
            | "try*"
            | "catch*"
    )
}

fn qq_list(elts: &Cons) -> Result<Expr, QxErr> {
    // TODO: rewrite with linked lists
    let mut acc = Cons::nil();

    let rev = elts.clone().reversed();

    for elt in &rev {
        match elt {
            Expr::List(ref v) if v.len_is(2) => {
                if matches!(v.car(), Some(Expr::Sym(ref s)) if &**s == "splice-unquote") {
                    acc = Cons::from(&[
                        expr!(sym "concat"),
                        v.nth(1).ok_or(QxErr::NoArgs(None, "splice-unqote"))?,
                        Expr::List(acc),
                    ]);

                    continue;
                }
            }
            _ => {}
        }

        acc = Cons::from(&[expr!(sym "cons"), quasiquote(&elt)?, Expr::List(acc)]);
    }

    Ok(Expr::List(acc))
}

fn quasiquote(ast: &Expr) -> Result<Expr, QxErr> {
    match ast {
        Expr::List(v) => {
            if v.len_is(2) {
                if let Expr::Sym(ref s) = v.car().unwrap() {
                    if &**s == "unquote" {
                        return Ok(v.cdar().unwrap());
                    }
                }
            }

            qq_list(v)
        }
        _ => Ok({
            //expr!(cons expr!(sym "quote"), ast.clone());
            cons(expr!(sym "quote"), Cons::new(ast.clone())).pipe(Expr::List)
        }),
    }
}

// BUG: splice-unquote is not working
fn _qq_list_new(ast: &Cons) -> Result<Cons, QxErr> {
    let mut y = Vec::new();
    for item in ast {
        match item {
            Expr::List(ref c) if c.len_is(2) => {
                if let Expr::Sym(ref s) = c.car().unwrap() {
                    if &**s == "splice-unquote" {
                        eprintln!("{{c: {c:?}, curr: {y:?}}}");
                        let Some(Expr::List(l)) = c.cdr().car() else {
                            return Err(QxErr::NoArgs(c.clone().into(), "splice-unquote"));
                        };

                        y.extend(_qq_list_new(&l)?.into_iter());
                    }
                }
            }

            _ => y.push(_new_quasiquote(item)?),
        }
    }

    ast.iter()
        .map(_new_quasiquote)
        .collect::<Result<Cons, _>>()
        .map(|it| cons(expr!(sym "list"), it))
}

fn _new_quasiquote(ast: Expr) -> Result<Expr, QxErr> {
    match ast {
        Expr::List(ref c) => {
            if c.len_is(2) {
                if let Expr::Sym(ref s) = c.car().unwrap() {
                    match &**s {
                        "unquote" => {
                            return Ok(c.cdr().car().unwrap());
                        }
                        "splice-unquote" => eprintln!("{c:?}"),
                        _ => {}
                    }
                }
            }

            _qq_list_new(c).map(Expr::List)
        }

        _ => Ok(expr!(cons expr!(sym "quote"), ast)),
    }
}

const fn id<T>(t: T) -> T {
    t
}

mod special_form_code {
    use tap::Pipe;

    pub enum SpecialForm {
        Val,
        Del,
        Defmacro,
        Mexp,
        Try,
        Fn,
        If,
        Let,
        Quote,
        Qqex,
        QuasiQuote,
        Do,
    }

    impl std::str::FromStr for SpecialForm {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "val!" => Self::Val,
                "del!" => Self::Del,
                "defmacro!" => Self::Defmacro,
                "mexp" => Self::Mexp,
                "try*" => Self::Try,
                "fn*" => Self::Fn,
                "if" => Self::If,
                "let*" => Self::Let,
                "quote" => Self::Quote,
                "qqex" => Self::Qqex,
                "quasiquote" => Self::QuasiQuote,
                "do" => Self::Do,
                _ => return Err(()),
            }
            .pipe(Ok)
        }
    }
}
