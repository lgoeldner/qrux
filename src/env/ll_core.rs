use anyhow::anyhow;
use tap::Pipe;

use crate::{
    eval::Shadow,
    lazy::Lazy,
    read::{
        self,
        types::{cons, ExprType, QxErr},
        Cons, Expr,
    },
    Func,
};

use std::{cell::RefCell, fmt::Write, num::TryFromIntError, rc::Rc, time::SystemTime};

#[macro_export]
macro_rules! func {
    ($(env: $env:pat,)? $(ctx: $ctx:pat,)? $name:literal;
			[$($arg:pat),*] $($rest:ident @ ..)? => $exp:expr) => {
		Func::new_expr($name, |args: Cons, _env, _ctx| {
				$( let $env = _env;)?
				$(let $ctx = _ctx;)?

				let mut _iter = args.into_iter();

				$(
					let Some($arg) = _iter.next() else { Err(QxErr::NoArgs(Some(args), $name))? };
				)*

				$( let $rest = _iter.rest(); )?

				if !_iter.is_empty() {
					Err(QxErr::NoArgs(Some(args), $name))?
				}



				Ok($exp)
			},
			)
	};
}

/// ## DSL for native Func Objects
///
/// example usage:
/// ```ignore
///     match ident {
///         "=", [lhs, rhs] => Expr::Bool(lhs == rhs),
///         // bind the remaining arguments to rest and the environment to `my_env.
///         // bind ̀`&mut Runtime` to `my_ctx`.
///         // return errors using Propagation of a `Result<Expr, QxErr>`
///         "println", [expr] .. @ rest; env:my_env; ctx:my_ctx => { println!("{expr:#}"); Err(QxErr::Stop)?; },
///     }
/// ```
macro_rules! funcmatch {
    (
		match $it:ident {
			$($name:literal, $(noeval:$_e:expr,)? [$($arg:pat),*]
				$($rest:ident @ ..)? $(,env: $env:pat)? $(,ctx: $ctx:pat)?
			=> $exp:expr,)*
		}) => {

			match $it {
				$(
					$name => func! {
						$(env: $env,)? $(ctx: $ctx,)? $name; [$($arg),*] $($rest @ ..)? => $exp
					},
				)*

				_ => None::<Expr>?,
			}
			.into()
	};
}

#[must_use]
pub fn core_map(sym: &str) -> Option<Expr> {
    int_ops(sym)
        .or_else(|| builtins(sym))
        .or_else(|| list_builtins(sym))
        .or_else(|| typeconvert(sym))
        .or_else(|| str_builtins(sym))
}

pub fn core_func_names() -> Vec<&'static str> {
    vec![
        "+",
        "-",
        "*",
        "/",
        "%",
        ">",
        "<",
        ">=",
        "<=",
        "concat",
        "rev",
        "cons",
        "car",
        "cdr",
        "list",
        "count",
        "empty?",
        "apply",
        "nth",
        "=",
        "t-eq?",
        "t-eq",
        "typeof",
        "println",
        "prn",
        "read-string",
        "slurp",
        "writef",
        "eval",
        "*ENV*",
        "throw",
        "bye",
        "fatal",
        "atom",
        "atom?",
        "deref",
        "reset!",
        "swap!",
        "not",
        "time",
        "str:len",
        "str:splitby",
        // types
        "int",
        "str",
        "bool",
        "str?",
        "list?",
        "sym?",
        // special forms
        "del!",
        "val!",
        "if",
        "let*",
        "do",
        "fn*",
        "defmacro!",
        "try*",
        "catch*",
        "reflect:defsym",
    ]
}

fn int_op(op: fn(i64, i64) -> Option<i64>, args: Cons) -> Result<Expr, QxErr> {
    if args.len_is(0) {
        Err(QxErr::NoArgs(args.into(), "int operator"))
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
        .map(Expr::Int)
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
        match ident {
            "concat", [] lists @ .. => {
                lists.into_iter()
                .map(|it| match it {
                    Expr::List(it) => it,
                    el => Cons::new(el),
                })
                .reduce(Cons::concat)
                .map_or(Expr::Nil, Expr::List)
            },

            "rev", [Expr::List(it)] => Expr::List(it.reversed()),
            "cons", [el, Expr::List(lst)] => Expr::List(cons(el, lst)),
            "car", [Expr::List(lst)] => lst.car().unwrap_or(Expr::Nil),
            "cdr", [Expr::List(lst)] => Expr::List(lst.cdr()),
            "list", [] rest @ .. => Expr::List(rest),
            "count", [Expr::List(lst)] => Expr::Int(lst.into_iter().count().pipe(as_i64)?),
            "empty?", [Expr::List(lst)] => Expr::Bool(lst.len_is(0)),
            "apply", [func, Expr::List(args)], env:env, ctx:ctx => {
                ctx.eval(Expr::List(cons(func,args)), Some(env))?
            },
            "nth", [Expr::Int(i), Expr::List(c)] => {
                c.into_iter()
                 .nth(i.pipe(as_usize)?)
                 .ok_or(QxErr::NoArgs(None, ""))?
            },
        }
    }
}

fn as_i64(i: usize) -> Result<i64, QxErr> {
    i.try_into().map_err(|_| QxErr::IntOverflowErr)
}

fn as_usize(i: i64) -> Result<usize, QxErr> {
    i.try_into().map_err(|_| QxErr::IntOverflowErr)
}

fn builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident {
            "=", [lhs, rhs] => Expr::Bool(lhs == rhs),

            // loose type eq
            "t-eq?", [lhs, rhs] => Expr::Bool(
                // not null safe
                lhs.get_type() == ExprType::Nil
                || rhs.get_type() == ExprType::Nil
                || lhs.get_type() == rhs.get_type()
            ),
            // strict type eq
            "t-eq", [lhs, rhs] => Expr::Bool(lhs.get_type() == rhs.get_type()),

            "typeof", [expr] => Expr::String(expr.get_type().to_string().into()),

            "println", [expr] => { println!("{expr:#}"); Expr::Nil },
            "prn", [expr] => { println!("{expr}"); Expr::Nil },

            "read-string", [Expr::String(s)] => {
                read::Input(s.clone()).tokenize().try_into()?
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

            "eval", [ast], env:env, ctx:ctx => ctx.eval(ast, Some(env))?,
            "eval-noenv", [ast], env:_, ctx:ctx => ctx.eval(ast, None)?,
            "*ENV*", [], env:env => { println!("{env:#?}"); Expr::Nil },

            "throw", [err] => Err(QxErr::LispErr(err))?,
            "bye", [] => Err(QxErr::Stop)?,
            "fatal", [ex] => Err(QxErr::Fatal(Rc::new(QxErr::LispErr(ex))))?,

            "atom", [ex] => Expr::Atom(Rc::new(RefCell::new(ex))),
            "atom?", [ex] => Expr::Bool(matches!(ex, Expr::Atom(_))),
            "deref", [Expr::Atom(atom)] => {
                let inner = atom.borrow();
                inner.clone()
            },
            "reset!", [Expr::Atom(atom), new] => atom.replace(new),
            "swap!", [Expr::Atom(atom), cl @ Expr::Closure(_)] args @ ..,
                env:env, ctx:ctx => {
                    let res = ctx.eval(
                        Expr::List(
                            cons(cl, cons(atom.take(), args))
                        ),
                        Some(env)
                    )?;

                    *RefCell::borrow_mut(&atom) = res.clone();

                    res
            },
            "not", [Expr::Bool(b)] => Expr::Bool(!b),
            "time", [] => Expr::Int(
                FIRST_TIME
                    .elapsed()
                    .map(|it| it.as_millis())
                    .unwrap_or(0)
                    .try_into()
                    .map_err(|it: TryFromIntError| QxErr::Any(it.into()))?
            ),

            // move to the global env
            "export!", [Expr::List(l)], env:env, ctx:ctx => {
                if Rc::ptr_eq(&env.inner(), &ctx.env.inner()) {
                    Err(anyhow!("ExportError: Nowhere to Export"))?;
                }

                for ident in &l {
                    let Expr::Sym(s) = ident else {
                        Err(QxErr::TypeErr { expected: ExprType::Sym, found: ident.get_type() })?
                    };

                    let Some(ex) = env.get(&s) else {
                        Err(QxErr::NoDefErr(s))?
                    };

                    ctx.defenv(&s, &ex, None, Shadow::Yes).pipe(drop);
                }

                Expr::Nil
            },

            // get all defined symbols
            "reflect:defsym", [], env:env => {
                let y = env.data().borrow();
                y.keys().cloned().map(Expr::Sym).collect::<Cons>().pipe(Expr::List)
            },
        }
    }
}

fn str_builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident {
            "str:len", [Expr::String(s)] => Expr::Int(s.len().try_into().unwrap_or(0)),
            "str:at", [Expr::String(s), Expr::Int(i)] => {
                Expr::String(
                    s
                        .chars()
                        .nth(as_usize(i)?)
                        .map(|it| it.to_string().into())
                        .ok_or_else(|| anyhow!("str:at: index out of bounds"))?
                )
            },

            "str:substr", [Expr::String(string), Expr::Int(from), Expr::Int(to)] => {
                Expr::String(
                    string
                        .get(as_usize(from)?..as_usize(to)?)
                        .ok_or_else(|| anyhow!("str:substr: index out of bounds"))?
                        .into()
                )
            },

            "str:chars", [Expr::String(s)] => {
                Expr::List(s.chars().map(|it| Expr::String(it.to_string().into())).collect())
            },

            "str:splitby", [Expr::String(s), Expr::String(sep)] => Expr::List(
                s.split(&*sep)
                 .map(|it| Expr::String(it.into()))
                 .collect::<Cons>()
            ),
        }
    }
}

fn typeconvert(ident: &str) -> Option<Expr> {
    match ident {
        "int" => Func::new_expr("int", |args, _, _| {
            let Some(arg) = args.car() else {
                return Err(QxErr::NoArgs(None, "int"));
            };

            to_int(arg)
        }),
        "int?" => func! {"int?"; [it] => Expr::Bool(matches!(it, Expr::Int(_))) },
        "str" => func! {"str";
            [] any @ .. =>
                Expr::String(
                    any.iter()
                    .try_fold(String::new(), |mut acc , it| {
                            write!(acc, "{it:#}")
                            .map(|()| acc)
                        })
                        .map_err(|it| QxErr::Any(it.into()))?
                        .into()
                )
        },
        "str?" => func! {"str?"; [it] => Expr::Bool(matches!(it, Expr::String(_))) },
        "sym?" => func! {"sym?"; [it] => Expr::Bool(matches!(it, Expr::Sym(_))) },
        "list?" => func! {"str?"; [it] => Expr::Bool(matches!(it, Expr::List(_))) },
        "bool" => Func::new_expr("bool", |args, _, _| {
            let Some(arg) = args.car() else {
                return Err(QxErr::NoArgs(None, "bool"));
            };

            to_bool(arg)
        }),

        _ => None::<Expr>?,
    }
    .pipe(Some)
}

fn to_bool(arg: Expr) -> Result<Expr, QxErr> {
    match arg {
        Expr::Atom(a) => to_bool(a.borrow().clone()),
        Expr::Bool(_) => Ok(arg),
        Expr::Nil => Ok(Expr::Bool(false)),
        Expr::Int(it) => Ok(Expr::Bool(it != 0)),
        Expr::String(ref s) => s.parse::<bool>().map_or_else(
            |it| {
                Err(QxErr::TypeParseErr {
                    from: arg,
                    to: ExprType::Bool,
                    err: it.into(),
                })
            },
            |it| Ok(Expr::Bool(it)),
        ),
        _ => Err(QxErr::TypeConvErr {
            from: arg.get_type(),
            to: ExprType::Bool,
        }),
    }
}

fn to_int(arg: Expr) -> Result<Expr, QxErr> {
    match arg {
        Expr::Atom(a) => to_int(a.borrow().clone()),
        Expr::String(ref s) => s.parse::<i64>().map_or_else(
            |it| {
                Err(QxErr::TypeParseErr {
                    from: arg,
                    to: ExprType::Bool,
                    err: it.into(),
                })
            },
            |it| Ok(Expr::Int(it)),
        ),
        Expr::Int(_) => Ok(arg),
        Expr::Bool(b) => Ok(Expr::Int(i64::from(b))),
        Expr::Nil => Ok(Expr::Int(0)),

        _ => Err(QxErr::TypeConvErr {
            from: arg.get_type(),
            to: ExprType::Int,
        }),
    }
}
