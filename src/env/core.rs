use std::backtrace::Backtrace;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::rc::Rc;

use anyhow::Context;

use crate::lazy::Lazy;
use crate::read::Expr;
use crate::{read, Func, QxErr};

#[macro_export]
macro_rules! func_expr {
    ($args_pat:pat => $exp:expr) => {
        Func::new_expr(|_ctx, args| {
            if let $args_pat = args {
                Ok($exp)
            } else {
                Err(QxErr::NoArgs(Some(args.to_vec())))
            }
        })
    };

    (ctx: $ident:ident; $args_pat:pat => $exp:expr) => {
        Func::new_expr(|$ident, args| {
            if let $args_pat = args {
                Ok($exp)
            } else {
                Err(QxErr::NoArgs(Some(args.to_vec())))
            }
        })
    };

    (move ctx: $ident:ident; $args_pat:pat => $exp:expr) => {
        Func::new_expr(move |$ident, args| {
            if let $args_pat = args {
                Ok($exp)
            } else {
                Err(QxErr::NoArgs(Some(args.to_vec())))
            }
        })
    };

    // irrefutable patterns
    ( $irref:pat in $expr:expr) => {
        Func::new_expr(|_ctx, args| {
            let $irref = args;
            Ok($expr)
        })
    };

    // side effects only
    ( $irref:pat in $expr:expr; Nil) => {
        Func::new_expr(|_ctx, args| {
            let $irref = args;
            let _ = $expr;
            Ok(Expr::Nil)
        })
    };
}

pub fn cmp_ops(ident: &str) -> Option<Expr> {
    macro_rules! cmp_ops {
		(match $ident:ident => { $($op:tt),+ }) => {
			match $ident {
				$(
					stringify!($op) => func_expr! { [Expr::Int(l), Expr::Int(r)] => Expr::Bool(l $op r) },
				)+
				_ => None?,
			}
		};
	}

    Some(cmp_ops!(match ident => { >, <, <=, >= }))
}

pub fn int_ops(ident: &str) -> Option<Expr> {
    macro_rules! int_op_apply {
			($($op:tt),+) => {
				Some(match ident {
					$(
						stringify!($op) => Func::new_expr(|_ctx, args: &[Expr]| {
							let Expr::Int(init) = args[0] else {
								return Err(QxErr::NoArgs(Some(args.to_vec())));
							};
							args.iter().skip(1)
								.try_fold(init, |acc, it| {
									if let Expr::Int(it) = it {
										Ok(acc $op it)
									} else {
										Err(QxErr::NoArgs(Some(args.to_vec())))
									}
								}).map(Expr::Int)
						}),
					)+
					_ => None?,
				})
			};
		}

    int_op_apply!(+, -, *, /, %)
}

use std::fmt::Write;

pub fn builtins(ident: &str) -> Option<Expr> {
    Some(match ident {
        "=" => func_expr! { [lhs, rhs] => Expr::Bool(lhs == rhs) },
        "list" => func_expr! { it in Expr::List(it.to_vec()) },
        "list?" => func_expr! {
            [maybe_list] => Expr::Bool(matches!(maybe_list, Expr::List(_)))
        },
        "empty?" => func_expr! { [Expr::List(l)] => Expr::Bool(l.is_empty()) },
        "count" => {
            func_expr! {
                [Expr::List(l)] => Expr::Int( l.len().try_into().context("Integer Overflow")? )
            }
        }
        "println" => func_expr! {[expr] => { println!("{expr:#}"); Expr::Nil }},
        "prn" => func_expr! {[expr] => { println!("{expr}"); Expr::Nil }},
        "str" => func_expr! {
            any in
                Expr::String(
                    any.iter()
                    .try_fold(String::new(), |mut acc , it| {
                            write!(acc, "{it:#}")
                            .map(|()| acc)
                        })
                        .map_err(|it| QxErr::Any(it.into()))?
                )
        },

        "read-string" => {
            func_expr! { [Expr::String(s)] => read::Input(s.to_owned()).tokenize().try_into()? }
        }
        "slurp" => func_expr! { [Expr::String(s)] => {
                 Expr::String(
                    std::fs::read_to_string(s).map_err(|err| QxErr::Any(err.into()))?
                )
            }
        },
        "eval" => func_expr! {
            ctx: ctx; [ast] => ctx.eval(ast.clone(), None)?
        },
        "*ENV*" => func_expr! {
            ctx: ctx; [] => { println!("{:#?}", ctx.env); Expr::Nil }
        },
        "trace" => func_expr! {
            _ in {
                println!("{}", Backtrace::force_capture()); Expr::Nil
            }
        },
        "bye" => Func::new_expr(|_, _| Err(QxErr::Stop)),
        "atom" => func_expr! { [expr] => Expr::Atom(Rc::new(RefCell::new(expr.clone()))) },
        "atom?" => func_expr!([expr] => Expr::Bool(matches!(expr, Expr::Atom(_)))),
        "deref" => func_expr! { [Expr::Atom(it)] => it.borrow().clone() },
        "reset!" => func_expr! { [Expr::Atom(atom), new] => atom.replace(new.clone()) },
        "swap!" => func_expr! {ctx:ctx; [Expr::Atom(atom), cl @ Expr::Closure(_), args@..] => {
            let mut expr = vec![cl.clone(), atom.take()];

            expr.append(&mut args.to_vec());

            let res = ctx.eval(Expr::List(expr), None)?;

			*RefCell::borrow_mut(atom) = res.clone();

            res
        } },
        _ => None?,
    })
}
