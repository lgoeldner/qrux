use anyhow::anyhow;
use ecow::EcoString;
use tap::Pipe;

use crate::{
    eval::Shadow,
    expr,
    lazy::Lazy,
    read::{
        self,
        kw::Keyword,
        types::{cons, ExprType as Type, QxErr},
        Cons, Expr, QxResult,
    },
    Func,
};

use std::{cell::RefCell, fmt::Write, num::TryFromIntError, rc::Rc, time::SystemTime};

use super::Env;

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
        "rem",
        ">",
        "<",
        ">=",
        "<=",
        "concat",
        "into",
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
        // strings
        "str:len",
        "str:splitby",
        "str:_substr",
        "str:chars",
        // types
        "int",
        "str",
        "bool",
        "str?",
        "list?",
        "sym?",
        "key?",
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

        let first = iter.next().unwrap().to_int()?;

        iter.try_fold(first, |acc, el| {
            op(acc, el.to_int()?).ok_or(QxErr::IntOverflowErr)
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
        "rem" => Func::new_expr("rem", |args, _, _| int_op(i64::checked_rem, args)),

        ">" => func! { ">";  [lhs, rhs] => Expr::Bool(lhs.to_int()? > rhs.to_int()?) },
        "<" => func! { "<";  [lhs, rhs] => Expr::Bool(lhs.to_int()? < rhs.to_int()?) },
        ">=" => func! {">="; [lhs, rhs] => Expr::Bool(lhs.to_int()? >= rhs.to_int()?) },
        "<=" => func! {"<="; [lhs, rhs] => Expr::Bool(lhs.to_int()? <= rhs.to_int()?) },
        _ => None?,
    }
    .into()
}

static FIRST_TIME: Lazy<SystemTime> = Lazy::new(SystemTime::now);

fn insert_into_map(map: im::HashMap<Keyword, Expr>, args: Cons) -> QxResult<Expr> {
    let mut m = map;

    for [k, v] in args.pair_iter() {
        m = m.update(k.as_kw()?, v);
    }

    Ok(Expr::Map(m))
}

fn list_builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident {
            "into", [] args @ .. => {
                let coll = args.car().ok_or_else(|| QxErr::NoArgs(None, "into"))?;
                match coll {
                    Expr::Map(m) => insert_into_map(m, args.cdr())?,
                    _ => Expr::List(
                        args
                            .cdr()
                            .reversed()
                            .into_iter()
                            .fold(coll.to_list()?, flip(cons))
                    ),
                }
            },

            "concat", [] lists @ .. => {
                lists.into_iter()
                .map(|it| match it {
                    Expr::List(it) => it,
                    el => Cons::new(el),
                })
                .reduce(Cons::concat)
                .map_or(Expr::Nil, Expr::List)
            },

            "rev", [it] => Expr::List(it.to_list()?.reversed()),
            "cons", [el, lst] => {
                Expr::List(cons(el, lst.to_list()?))
            },
            "car", [lst] => lst.to_list()?.car().unwrap_or(Expr::Nil),
            "cdr", [lst] => Expr::List(lst.to_list()?.cdr()),
            "list", [] rest @ .. => Expr::List(rest),
            "count", [lst] => Expr::Int(lst.to_list()?.into_iter().count().pipe(as_i64)?),
            "empty?", [lst] => Expr::Bool(lst.to_list()?.len_is(0)),
            "apply", [func, args], env:env, ctx:ctx => {
                ctx.eval(Expr::List(cons(func,args.to_list()?)), Some(env))?
            },
            "nth", [i, c] => {
                c.to_list()?
                 .into_iter()
                 .nth(i.to_int()?.pipe(as_usize)?)
                 .ok_or(QxErr::NoArgs(None, "nth"))?
            },
        }
    }
}

fn map_builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident {

        }
    }
}

fn as_i64(i: usize) -> Result<i64, QxErr> {
    i.try_into().map_err(|_| QxErr::IntOverflowErr)
}

fn as_usize(i: i64) -> Result<usize, QxErr> {
    i.try_into().map_err(|_| QxErr::IntOverflowErr)
}

fn env_to_expr(env: &Env) -> Expr {
    let y = env.inner().outer_env().map_or_else(Cons::nil, |outer| {
        let outer_env_expr = env_to_expr(&Env(outer));
        Cons::from(&[Cons::from(&[expr!(kw ":outer"), outer_env_expr]).pipe(Expr::List)])
    });


    env.vals()
        .borrow()
        .iter()
        .map(|(k, v)| expr!(cons Expr::Sym(k.clone()), v.clone()))
        .fold(y, flip(cons))
        .pipe(Expr::List)
}

fn flip<A, B, R>(f: impl Fn(B, A) -> R) -> impl Fn(A, B) -> R {
    move |b, a| f(a, b)
}

fn builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident {
            "=", [lhs, rhs] => Expr::Bool(lhs == rhs),

            // loose type eq
            "t-eq?", [lhs, rhs] => Expr::Bool(
                // not null safe
                lhs.get_type() == Type::Nil
                || rhs.get_type() == Type::Nil
                || lhs.get_type() == rhs.get_type()
            ),
            // strict type eq
            "t-eq", [lhs, rhs] => Expr::Bool(lhs.get_type() == rhs.get_type()),

            "typeof", [expr] => Expr::Keyword(Keyword::new(expr.get_type().to_string().into())),

            "println", [expr] => { println!("{expr:#}"); Expr::Nil },
            "prn", [expr] => { println!("{expr}"); Expr::Nil },

            "read-string", [s] => {
                read::Input(s.as_strt(Type::String)?).tokenize().try_into()?
            },
            "slurp", [s] => {
                Expr::String(
                    std::fs::read_to_string(&s.as_strt(Type::String)? as &str)
                    .map_err(|err| QxErr::Any(err.into()))?
                    .into()
                )
            },
            "writef", [file_location, to_write] => {
                match std::fs::write(&*file_location.as_strt(Type::String)?, to_write.as_strt(Type::String)?.as_bytes()) {
                    Ok(()) => Expr::Nil,
                    Err(err) => Err(QxErr::Any(err.into()))?
                }
            },

            "eval", [ast], env:env, ctx:ctx => ctx.eval(ast, Some(env))?,
            "eval-noenv", [ast], env:_, ctx:ctx => ctx.eval(ast, None)?,
            "*ENV*", [], env:env => { env_to_expr(&env) },

            "throw", [err] => Err(QxErr::LispErr(err))?,
            "bye", [] => Err(QxErr::Stop)?,
            "fatal", [ex] => Err(QxErr::Fatal(Rc::new(QxErr::LispErr(ex))))?,

            "atom", [ex] => Expr::Atom(Rc::new(RefCell::new(ex))),
            "atom?", [ex] => Expr::Bool(matches!(ex, Expr::Atom(_))),
            "deref", [atom] => {
                match atom {
                    Expr::Atom(atom) => atom.borrow().clone(),
                    e => e,
                }
            },
            "reset!", [atom, new] => atom.to_atom()?.replace(new),
            "swap!", [atom, cl @ Expr::Closure(_)] args @ ..,
                env:env, ctx:ctx => {
                    let atom = atom.to_atom()?;
                    let res = ctx.eval(
                        Expr::List(
                            cons(cl, cons(atom.take(), args))
                        ),
                        Some(env)
                    )?;

                    *RefCell::borrow_mut(&atom) = res.clone();

                    res
            },
            "not", [b] => Expr::Bool(!b.to_bool()?),
            "time", [] => Expr::Int(
                FIRST_TIME
                    .elapsed()
                    .map(|it| it.as_millis())
                    .unwrap_or(0)
                    .try_into()
                    .map_err(|it: TryFromIntError| QxErr::Any(it.into()))?
            ),

            // move to the global env
            "export!", [l], env:env, ctx:ctx => {
                if Rc::ptr_eq(&env.inner(), &ctx.env.inner()) {
                    Err(anyhow!("ExportError: Nowhere to Export"))?;
                }

                for ident in &l.to_list()? {
                    let Expr::Sym(s) = ident else {
                        Err(QxErr::TypeErr { expected: Type::Sym, found: ident.get_type() })?
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
                let y = env.vals().borrow();
                y.keys().cloned().map(Expr::Sym).collect::<Cons>().pipe(Expr::List)
            },
        }
    }
}

fn str_builtins(ident: &str) -> Option<Expr> {
    funcmatch! {
        match ident {
            "str:len", [s] => Expr::Int(s.as_strt(Type::String)?.len().pipe(as_i64)?),
            "str:at", [Expr::String(s), Expr::Int(i)] => {
                Expr::String(
                    s
                        .chars()
                        .nth(as_usize(i)?)
                        .map(|it| it.to_string().into())
                        .ok_or_else(|| anyhow!("str:at: index out of bounds"))?
                )
            },

            "str:_substr", [string, Expr::Int(from), Expr::Int(to)] => {
                Expr::String(
                    string
                        .as_strt(Type::String)?
                        .get(as_usize(from)?..as_usize(to)?)
                        .ok_or_else(|| anyhow!("str:_substr: index out of bounds"))?
                        .into()
                )
            },

            "str:chars", [Expr::String(s)] => {
                Expr::List(s.chars().map(|it| Expr::String(it.to_string().into())).collect())
            },

            "str:splitby", [Expr::String(s), Expr::String(sep)] => Expr::List(
                s.split(&*sep)
                 .map(|it| Expr::String(it.into()))
                 .collect()
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

            arg.to_int().map(Expr::Int)
        }),
        "key:name" => func! {"key:name";
            [Expr::Keyword(k)] => Expr::String(k.inspect_inner(|it| EcoString::from(it)))
        },
        "key" => func! {"key";
            [any] => any.as_strt(Type::Keyword)?.pipe(Keyword::new).pipe(Expr::Keyword)
        },
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
        "sym" => func! {"sym"; [ref s] => to_sym(s)? },
        "bool" => Func::new_expr("bool", |args, _, _| {
            let Some(arg) = args.car() else {
                return Err(QxErr::NoArgs(None, "bool"));
            };

            arg.to_bool().map(Expr::Bool)
        }),

        "int?" => func! {"int?"; [it] => Expr::Bool(matches!(it, Expr::Int(_))) },
        "str?" => func! {"str?"; [it] => Expr::Bool(matches!(it, Expr::String(_))) },
        "sym?" => func! {"sym?"; [it] => Expr::Bool(matches!(it, Expr::Sym(_))) },
        "key?" => func! { "key?"; [it] => Expr::Bool(matches!(it, Expr::Keyword(_))) },
        "list?" => func! {"list?"; [it] => Expr::Bool(matches!(it, Expr::List(_))) },
        "fn?" => func! {"fn?"; [it] => Expr::Bool(matches!(it, Expr::Closure(_) | Expr::Func(_)))},

        _ => None::<Expr>?,
    }
    .pipe(Some)
}

fn to_sym(arg: &Expr) -> QxResult<Expr> {
    let s = arg.as_strt(Type::Sym)?;
    if s.chars().any(char::is_whitespace) {
        Err(QxErr::TypeConvErr {
            from: Type::String,
            to: Type::Sym,
        })
    } else {
        Ok(Expr::Sym(s))
    }
}
