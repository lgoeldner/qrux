use std::cell::RefCell;
use std::num::TryFromIntError;
use std::rc::Rc;
use std::time::SystemTime;

use anyhow::{anyhow, Context};

use crate::lazy::Lazy;
use crate::read::Expr;
use crate::{expr, read, Func, QxErr};

macro_rules! func_expr {
    // handles argument matching and returning
    ($name:literal; $args_pat:pat => $exp:expr) => {
        Func::new_expr(|_, args| {
            if let $args_pat = args {
                Ok($exp)
            } else {
                Err(QxErr::Any(anyhow::anyhow!(
                    "In {}: Expected pattern {}, got: {:?}",
                    stringify!($name),
                    stringify!($args_pat),
                    args
                )))
            }
        })
    };

    // bind context
    (ctx: $ident:ident; $args_pat:pat => $exp:expr) => {
        Func::new_expr(|$ident, args| {
            if let $args_pat = args {
                Ok($exp)
            } else {
                Err(QxErr::NoArgs(Some(args.to_vec())))
            }
        })
    };

    // irrefutable pattern
    ( $irref:pat in $expr:expr) => {
        Func::new_expr(|_ctx, args| {
            let $irref = args;
            Ok($expr)
        })
    };
}

pub fn cmp_ops(ident: &str) -> Option<Expr> {
    macro_rules! cmp_ops {
		(match $ident:ident => { $($op:tt),+ }) => {
			match $ident {
				$(
					stringify!($op) => func_expr! {""; [Expr::Int(l), Expr::Int(r)] => Expr::Bool(l $op r) },
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
							let [Expr::Int(init), ..] = args else {
								return Err(QxErr::NoArgs(Some(args.to_vec())));
							};

							args.iter().skip(1)
								.try_fold(*init, |acc, it| {
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

pub fn list_builtins(ident: &str) -> Option<Expr> {
    Some(match ident {
        "list:contains" => func_expr! {ctx:ctx;
            [Expr::String(search_by) | Expr::Sym(search_by), lst @ Expr::List(_)] =>
            	Expr::Bool(lst.contains_sym(search_by))
        },
        "list" => func_expr! {it in Expr::List(it.iter().cloned().collect()) },
        "list?" => func_expr! {"list?";
            [maybe_list] => Expr::Bool(matches!(maybe_list, Expr::List(_)))
        },
        "empty?" => func_expr! {"empty?"; [Expr::List(l)] => Expr::Bool(l.is_empty()) },
        "count" => {
            func_expr! {"count";
                [Expr::List(l)] => Expr::Int( l.len().try_into().context("Integer Overflow")? )
            }
        }
        "concat" => func_expr! {
            variadic in {
                let mut res = vec![];
                for it in variadic {
                    if let Expr::List(l) = it { res.extend_from_slice(l); }
                    else {
                        return Err(QxErr::TypeErr {
                            expected: Box::new(expr!(List)),
                            found: Box::new(it.clone())
                        });
                    }
                }

                Expr::List(res.into())
            }
        },
        "nth" => func_expr! {"nth"; [Expr::Int(i), Expr::List(l)] => {
            l.get(
                usize::try_from(*i)
                .map_err(|_| QxErr::Any(anyhow!("Integer overflow")))?
            )
            .cloned()
            .unwrap_or(Expr::Nil)
        }},
        "cons" => func_expr! {"cons"; [prepend, Expr::List(to)] => {
            let mut res = vec![prepend.clone()];
            res.extend_from_slice(to);
            Expr::List(res.into())
        }},
        "car" => func_expr! {"car"; [Expr::List(it)] => it.first().cloned().unwrap_or(Expr::Nil) },
        "cdr" => func_expr! {"cdr"; [Expr::List(it)] => {
            if it.len() > 1 { Expr::List(it[1..].into()) }
            else { Expr::Nil }
        } },
        "slice" => slice_expr(),
        "rev" => func_expr! {"rev"; [Expr::List(it)] =>
            Expr::List(it.iter().cloned().rev().collect::<Vec<_>>().into())
        },
        _ => None?,
    })
}

static FIRST_TIME: Lazy<SystemTime> = Lazy::new(SystemTime::now);

pub fn builtins(ident: &str) -> Option<Expr> {
    Some(match ident {
        "time" => func_expr! {"time"; [] =>
            Expr::Int(FIRST_TIME
                .elapsed()
                .map_err(|it| QxErr::Any(it.into()))?
                .as_millis()
                .try_into()
                .map_err(|it: TryFromIntError| QxErr::Any(it.into()))?)
        },
        "=" => func_expr! {"="; [lhs, rhs] => Expr::Bool(lhs == rhs) },
        "println" => func_expr! {"println"; [expr] => { println!("{expr:#}"); Expr::Nil }},
        "prn" => func_expr! {"prn"; [expr] => { println!("{expr}"); Expr::Nil }},
		"sym?" => func_expr! {"sym?"; [maybe_sym] => Expr::Bool(matches!(maybe_sym, Expr::Sym(_))) },
        "str" => func_expr! {
            any in
                Expr::String(
                    any.iter()
                    .try_fold(String::new(), |mut acc , it| {
                            write!(acc, "{it:#}")
                            .map(|()| acc)
                        })
                        .map_err(|it| QxErr::Any(it.into()))?
                        .into_boxed_str().into()
                )
        },
        "sym" => func_expr! {"sym"; [Expr::String(s)] => Expr::Sym(Rc::clone(s)) },
        "read-string" => {
            func_expr! {"read-string"; [Expr::String(s)] => read::Input(Rc::clone(s)).tokenize().try_into()? }
        }
        "slurp" => func_expr! {"slurp"; [Expr::String(s)] => {
                 Expr::String(
                    std::fs::read_to_string(s as &str).map_err(|err| QxErr::Any(err.into()))?.into()
                )
            }
        },
        "writef" => func_expr! {"writef";
            [Expr::String(file_location), Expr::String(to_write)] =>
                match std::fs::write(&**file_location, to_write.as_bytes()) {
                    Ok(()) => Expr::Nil,
                    Err(err) => Err(QxErr::Any(err.into()))?
                }
        },
        "eval" => func_expr! {
            ctx: ctx; [ast] => ctx.eval(ast.clone(), None)?
        },
        "*ENV*" => func_expr! {
            ctx: ctx; [] => { println!("{:#?}", ctx.env); Expr::Nil }
        },
        "bye" => Func::new_expr(|_, _| Err(QxErr::Stop)),
        "fatal" => func_expr! {"fatal"; [expr] =>
            // return the lisp value expr as a fatal error
            Err(QxErr::Fatal(Box::new(QxErr::LispErr(expr.clone()))))?
        },
        "atom" => func_expr! {"atom"; [expr] => Expr::Atom(Rc::new(RefCell::new(expr.clone()))) },
        "atom?" => func_expr! { "atom?";[expr] => Expr::Bool(matches!(expr, Expr::Atom(_))) },
        "deref" => func_expr! { "deref"; [Expr::Atom(it)] => it.borrow().clone() },
        "reset!" => func_expr! {"reset!"; [Expr::Atom(atom), new] => atom.replace(new.clone()) },
        "swap!" => func_expr! {ctx:ctx; [Expr::Atom(atom), cl @ Expr::Closure(_), args@..] => {
            let mut expr = vec![cl.clone(), atom.take()];

            expr.append(&mut args.to_vec());

            let res = ctx.eval(Expr::List(expr.into()), None)?;

            *RefCell::borrow_mut(atom) = res.clone();

            res
        } },
        "throw" => func_expr! {"throw"; [expr] => Err(QxErr::LispErr(expr.clone()))? },
        _ => None?,
    })
}

fn slice_expr() -> Expr {
    func_expr! {
        any in
        match any {
            [Expr::Int(from), Expr::Int(to), Expr::List(l)] => {
                let from = usize::try_from(*from)
                        .map_err(|_| QxErr::Any(anyhow!("Integer overflow")))?;
                let to = usize::try_from(*to)
                    .map_err(|_| QxErr::Any(anyhow!("Integer overflow")))?;
                match l.as_ref()
                    .get(from..to)
                    .map(|it| Expr::List(it.into()))
                {
                    Some(it) => it,
                    None => return Err(
                            QxErr::Any(
                                anyhow!("Slice Index out of bounds: {from} to {to}, length: {}", l.len())
                            )
                        ),
                }
            },
            [Expr::Int(from), Expr::Nil, Expr::List(l)] => {
                    let from = usize::try_from(*from)
                        .map_err(|_| QxErr::Any(anyhow!("Integer overflow")))?;

                    match l.as_ref()
                        .get(from..)
                        .map(|it| Expr::List(it.into())) {
                        Some(it) => it,
                        None => return Err(
                                QxErr::Any(
                                    anyhow!("Slice Index out of bounds: {from} to -, length: {}", l.len())
                                )
                            ),
                    }
            },
            [Expr::Nil, Expr::Int(to), Expr::List(l)] => {
                let to = usize::try_from(*to)
                        .map_err(|_| QxErr::Any(anyhow!("Integer overflow")))?;
                match l.as_ref()
                .get(..to)
                .map(|it| Expr::List(it.into())) {
                    Some(it) => it,
                    None => return Err(
                            QxErr::Any(
                                anyhow!("Slice Index out of bounds: - to {to}, length: {}", l.len())
                            )
                        ),
                }
            }
            _ => return Err(QxErr::NoArgs(Some(any.to_vec()))),
        }
    }
}
