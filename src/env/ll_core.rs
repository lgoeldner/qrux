use super::Env;
use crate::{
    lazy::Lazy,
    read::{self, types::*, Cons, Expr},
    Func,
};
use anyhow::{anyhow, Context};
use std::{cell::RefCell, ops, rc::Rc, time::SystemTime};

macro_rules! func {
    ($(env: $env:ident,)? $(ctx: $ctx:ident,)? $name:literal;
			[$($arg:pat),*] $($rest:ident @ ..)? => $exp:expr) => {
		Func::new_expr($name, |args: Cons, _env, _ctx| {
				$( let $env = _env;)?
				$(let $ctx = _ctx;)?

				let mut _iter = args.into_iter();

				$(
					let Some($arg) = _iter.next() else { Err(QxErr::NoArgs(Some(args)))? };
				)*

				$( let $rest = _iter.rest(); )?

				if !_iter.is_empty() {
					Err(QxErr::NoArgs(Some(args)))?
				}

				Ok($exp)
			})
	};
}

/// ## DSL for native Func Objects
///
/// example usage:
/// ```ignore
/// 	match ident, args {
/// 		"=", [lhs, rhs] => Expr::Bool(lhs == rhs),
/// 		// bind the remaining arguments to rest and the environment to `my_env.
/// 		// bind ̀`&mut Runtime` to `my_ctx`.
/// 		// return errors using Propagation of a `Result<Expr, QxErr>`
/// 		"println", [expr] .. @ rest; env:my_env; ctx:my_ctx => { println!("{expr:#}"); Err(QxErr::Stop)?; },
/// 	}
/// ```
macro_rules! funcmatch {
    (
		match $it:ident, $_:ident {
			$($name:literal, [$($arg:pat),*]
				$($rest:ident @ ..)? $(;env: $env:ident)? $(;ctx: $ctx:ident)?
			=> $exp:expr,)*
		}) => {

			match $it {
				$(
					$name => func! {
						$(env: $env,)? $(ctx: $ctx,)? $name; [$($arg),*] $($rest @ ..)? => $exp
					},
				)*

				_ => None?,
			}
			.into()
	};
}

pub fn core_map(sym: &str) -> Option<Expr> {
    int_ops(sym)
        .or_else(|| builtins(sym))
        .or_else(|| list_builtins(sym))
}

fn int_op(op: fn(i64, i64) -> Option<i64>, args: Cons) -> Result<Expr, QxErr> {
    if args.len_is(1) {
        Err(QxErr::NoArgs(args.into()))
    } else {
        let mut iter = args.into_iter();

        let first = match iter.next().unwrap() {
            Expr::Int(first) => first,
            el => {
                return Err(QxErr::TypeErr {
                    expected: ExprType::Int,
                    found: el.get_type(),
                })
            }
        };

        iter.try_fold(first, |acc, el| {
            if let Expr::Int(int) = el {
                op(acc, int).ok_or(QxErr::IntOverflowErr)
            } else {
                Err(QxErr::TypeErr {
                    expected: ExprType::Int,
                    found: el.get_type(),
                })
            }
        })
        .map(|it| Expr::Int(it))
    }
}

fn int_ops(ident: &str) -> Option<Expr> {
    match ident {
        "+" => Func::new_expr("+", |args, _, _| int_op(i64::checked_add, args)),
        "-" => Func::new_expr("-", |args, _, _| int_op(i64::checked_sub, args)),
        "*" => Func::new_expr("*", |args, _, _| int_op(i64::checked_mul, args)),
        "/" => Func::new_expr("/", |args, _, _| int_op(i64::checked_div, args)),
        "%" => Func::new_expr("%", |args, _, _| int_op(i64::checked_rem, args)),

        ">" => func! {">"; [Expr::Int(lhs), Expr::Int(rhs)] => Expr::Bool(lhs > rhs) },
        "<" => func! {"<"; [Expr::Int(lhs), Expr::Int(rhs)] => Expr::Bool(lhs < rhs) },
        ">=" => func! {">="; [Expr::Int(lhs), Expr::Int(rhs)] => Expr::Bool(lhs >= rhs) },
        "<=" => func! {"<="; [Expr::Int(lhs), Expr::Int(rhs)] => Expr::Bool(lhs <= rhs) },
        _ => None?,
    }
    .into()
}

static FIRST_TIME: Lazy<SystemTime> = Lazy::new(SystemTime::now);

fn list_builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident, args {
            "concat", [] lists @ .. => {
                lists.into_iter()
                .map(|it| match it {
                    Expr::Cons(it) => it,
                    el => Cons::new(el),
                })
                .reduce(|acc, it| acc.concat(it))
                .map_or(Expr::Nil, |it| Expr::Cons(it))
            },

            "rev", [Expr::Cons(it)] => Expr::Cons(it.reversed()),
            "cons", [el, Expr::Cons(lst)] => Expr::Cons(cons(el, lst)),
            "list", [] rest @ .. => Expr::Cons(rest),
        }
    }
}

fn builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident, args {
            "=", [lhs, rhs] => Expr::Bool(lhs == rhs),

            "println", [expr] => { println!("{expr:#}"); Expr::Nil },
            "prn", [expr] => { println!("{expr}"); Expr::Nil },

            "read-string", [Expr::String(s)] => {
                read::Input(Rc::clone(&s)).tokenize().try_into()?
            },
            "slurp", [Expr::String(s)] => {
                Expr::String(
                    std::fs::read_to_string(&s as &str)
                    .map_err(|err| QxErr::Any(err.into()))?
                    .into()
                )
            },
            "writef", [Expr::String(file_location), Expr::String(to_write)] => {
                match std::fs::write(&*file_location, to_write.as_bytes()) {
                    Ok(()) => Expr::Nil,
                    Err(err) => Err(QxErr::Any(err.into()))?
                }
            },

            "eval", [ast]; env:env; ctx:ctx => ctx.eval(ast, Some(env))?,
            "*ENV*", []; env:env => { println!("{:#?}", env); Expr::Nil },

            "bye", [] => Err(QxErr::Stop)?,
            "fatal", [ex] => Err(QxErr::Fatal(Rc::new(QxErr::LispErr(ex))))?,

            "atom", [ex] => Expr::Atom(Rc::new(RefCell::new(ex))),
            "atom?", [ex] => Expr::Bool(matches!(ex, Expr::Atom(_))),
            "deref", [Expr::Atom(atom)] => {
                match atom.borrow() {
                    inner => inner.clone()
                }
            },
            "reset!", [Expr::Atom(atom), new] => atom.replace(new.clone()),
            "swap!", [Expr::Atom(atom), cl @ Expr::Closure(_)] args @ ..;
                env:env; ctx:ctx => {
                    let res = ctx.eval(
                        Expr::Cons(
                            cons(cl, cons(atom.take(), args))
                        ),
                        Some(env)
                    )?;

                    *RefCell::borrow_mut(&atom) = res.clone();

                    res
            },
			"not", [Expr::Bool(b)] => Expr::Bool(!b),

        }
    }
}
