use anyhow::Context;

use crate::read::Expr;
use crate::{Func, QxErr};

macro_rules! funcexpr {
    ($args_pat:pat => $exp:expr) => {
        Func::new_expr(|_ctx, args| {
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
					stringify!($op) => funcexpr! { [Expr::Int(l), Expr::Int(r)] => Expr::Bool(l $op r) },
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
        "=" => funcexpr! { [lhs, rhs] => Expr::Bool(lhs == rhs) },
        "list" => funcexpr! { it in Expr::List(it.to_vec()) },
        "list?" => {
            funcexpr! { [maybe_list] => Expr::Bool(matches!(maybe_list, Expr::List(_))) }
        }
        "empty?" => funcexpr! { [Expr::List(l)] => Expr::Bool(l.is_empty()) },
        "count" => {
            funcexpr! {
                [Expr::List(l)] => Expr::Int( l.len().try_into().context("Integer Overflow")? )
            }
        }
        "println" => funcexpr! { it in println!("{it:?}"); Nil },

        _ => None?,
    })
}
