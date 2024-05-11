use crate::read::Expr;
use crate::{Func, QxErr};

macro_rules! funcexpr {
    (args: $args_pat:pat => $expr:expr) => {
        Func::new_expr(|_ctx, args| {
            if let $args_pat = args {
                Ok($expr)
            } else {
                Err(QxErr::NoArgs(Some(args.to_vec())))
            }
        })
    };
    (args: $irref:ident => $expr:expr) => {
        let $irref = args;
        Ok($expr)
    };
}

pub fn int_ops(ident: &str) -> Option<Expr> {
    macro_rules! int_op_apply {
			($($op:tt),+) => {
				Some(match ident {
					$(
						stringify!($op) => Func::new_expr(|_ctx, args: &[Expr]| {
							// builtin operators are variadic
							let Expr::Int(init) = args[0] else {
								return Err(QxErr::NoArgs(Some(args.to_vec())));
							};

							args.iter()
								.skip(1)
								.try_fold(init, |acc, it| {
									if let Expr::Int(it) = it {
										Ok(acc $op it)
									} else {
										Err(QxErr::NoArgs(Some(args.to_vec())))
									}
								})
								.map(Expr::Int)
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
        "=" => funcexpr! { args: [lhs, rhs] => Expr::Bool(lhs == rhs) },
        "list" => funcexpr! { args: any => Expr::List(any.to_vec()) },
        "list?" => {
            funcexpr! { args: [maybe_list] => Expr::Bool(matches!(maybe_list, Expr::List(_))) }
        }
        "empty?" => funcexpr! { args: [Expr::List(l)] => Expr::Bool(l.is_empty()) },
        // "prn" => funcexpr! { args: [any] => { println!("{any}"); Expr::Nil } },
        _ => None?,
    })
}
