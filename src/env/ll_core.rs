use std::cell::RefCell;
use std::num::TryFromIntError;
use std::ops;
use std::rc::Rc;
use std::time::SystemTime;

use anyhow::{anyhow, Context};

use crate::lazy::Lazy;
use crate::read::Expr;
use crate::{expr, read, Apply, Func, QxErr};

use super::Env;

pub fn core_map(sym: &str) -> Option<Expr> {
    int_ops(sym)
}

macro_rules! int_operation {
	(match $ident:ident { $($op:tt => $fun:ident)* } ) => {
		Some(match $ident {
			$(
				stringify!($op) => Func::new_expr(|args, _| {
					if args.clone().len_is(1) {
						return Err(QxErr::NoArgs(None));
					}

					args.into_iter()
						.try_fold(0_i64, |acc, el| {
							if let Expr::Int(int) = el {
								acc.$fun(int).ok_or(QxErr::IntOverflowErr)
							} else {
								Err(QxErr::TypeErr {
									expected: read::ExprType::Int,
									found: el.get_type(),
								})
							}
						})
						.map(|it| Expr::Int(it))
				}),
			)*
			_ => None?,
		})
	};
}

fn int_ops(ident: &str) -> Option<Expr> {
    // return match ident {
    //     "+" => Func::new_expr(|args, _| {
    //         args.into_iter()
    //             .try_fold(0_i64, |acc, el| {
    //                 if let Expr::Int(int) = el {
    //                     acc.checked_add(int).ok_or(QxErr::IntOverflowErr)
    //                 } else {
    //                     Err(QxErr::TypeErr {
    //                         expected: read::ExprType::Int,
    //                         found: el.get_type(),
    //                     })
    //                 }
    //             })
    //             .map(|it| Expr::Int(it))
    //     }),

    //     _ => None?,
    // }
    // .into();
    int_operation! {
        match ident {
            + => checked_add
            - => checked_sub
            * => checked_mul
            / => checked_div
            % => checked_rem
        }
    }
}
