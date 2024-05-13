use std::backtrace::Backtrace;
use std::borrow::Borrow;

use anyhow::Context;

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
        "str" => {
            func_expr! {any in { println!("{}", any.iter().map(ToString::to_string).collect::<String>()); Expr::Nil }}
        }
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
                println!("[{} {} {}] {}",file!(), line!(), column!(), Backtrace::force_capture()); Expr::Nil
            }
        },
        _ => None?,
    })
}
