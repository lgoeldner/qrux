use core::fmt;
use std::fmt::Display;

use crate::{
    read::{Closure, Cons, Expr},
    Func,
};
use colored::Colorize;

pub fn pp_ast(ast: &Expr) {
    println!(";; => {ast:?}",);
}

impl std::fmt::Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_joined(f, self.iter(), ["(", ")"], None)
        // write!(f, "(")?;

        // self.0
        //     .as_ref()
        //     .map(|it| write_cons_inner(f, it))
        //     .transpose()?;

        // write!(f, ")")
    }
}

impl std::fmt::Debug for Cons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

fn write_map<'a>(
    f: &mut fmt::Formatter,
    map: impl IntoIterator<Item = (impl Display, &'a Expr)>,
) -> fmt::Result {
    if f.alternate() {
        write!(f, "{{")?;
    } else {
        write!(f, "{}", "{".purple())?;
    }

    let mut iter = map.into_iter();
    if let Some((k, v)) = iter.next() {
        if f.alternate() {
            writeln!(f, "\n  {k:#} {v:#}")?;
        } else {
            writeln!(f, "\n  {k} {v}")?;
        }
    }

    for (k, v) in iter {
        if f.alternate() {
            writeln!(f, "  {k:#} {v:#}")?;
        } else {
            writeln!(f, "  {k} {v}")?;
        }
    }

    if f.alternate() {
        write!(f, "}}")
    } else {
        write!(f, "{}", "}".purple())
    }
}

fn write_joined(
    f: &mut fmt::Formatter<'_>,
    i: impl Iterator<Item = impl Display>,
    [open_delim, close_delim]: [&str; 2],
    len: Option<usize>,
) -> fmt::Result {
    const MULTILINE_THRESHOLD: usize = 10;
    let multiline = len.is_some_and(|it| it > MULTILINE_THRESHOLD);
    let mut p = i.peekable();
    let delim = if multiline { "\n" } else { " " };

    f.write_str(open_delim)?;
    if multiline {
        f.write_str("\n")?;
    }

    loop {
        let Some(next) = p.next() else { break };
        let is_last = p.peek().is_none();

        next.fmt(f)?;
        if !is_last {
            f.write_str(delim)?;
        }
    }

    if multiline {
        f.write_str("\n")?;
    }
    f.write_str(close_delim)?;
    Ok(())
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                Self::Vec(v) => write_joined(f, v.iter(), ["[", "]"], Some(v.len())),
                Self::MapLit(l) => writeln!(f, "MapLit{{{l:?}}}"),
                Self::Atom(it) => format!("{:#}", it.borrow()).fmt(f),
                Self::Int(int) => int.to_string().fmt(f),
                Self::Sym(sym) => write!(f, "{sym}"),
                Self::String(string) => write!(f, "{string}"),
                // Self::List(list) => write_list(f, list),
                Self::Nil => "nil".fmt(f),
                Self::Func(_) | Self::Closure(_) => Ok(()),
                Self::Bool(b) => b.to_string().fmt(f),
                Self::List(it) => {
                    write_joined(f, it.iter(), ["(", ")"], None)
                    // write!(f, "(")?;
                    // it.0.as_ref()
                    //     .map(|it| write_cons_inner(f, it))
                    //     .transpose()?;
                    // write!(f, ")")
                }
                Self::Keyword(kw) => write!(f, "{kw}"),
                Self::Map(map) => write_map(f, map),
            }
        } else {
            match self {
                Self::Vec(v) => write_joined(f, v.iter(), ["[", "]"], Some(v.len())),
                Self::MapLit(l) => write_map(f, l.chunks_exact(2).map(|it| (&it[0], &it[1]))),
                Self::Atom(it) => format!("<Atom ({})>", it.borrow()).fmt(f),
                Self::Int(int) => int.to_string().cyan().fmt(f),
                Self::Sym(sym) => sym.to_string().red().fmt(f),
                Self::String(string) => format!(r#""{string}""#).bright_green().fmt(f),
                Self::Nil => "nil".bold().blue().fmt(f),
                Self::Func(Func(name, _)) => format!("<Func \"{name}\">").red().fmt(f),
                Self::Closure(c) => c.to_string().red().fmt(f),
                Self::Bool(b) => b.to_string().bright_blue().fmt(f),
                Self::List(it) => it.fmt(f),
                Self::Keyword(kw) => kw.fmt(f),
                Self::Map(map) => write_map(f, map),
            }
        }
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            "".fmt(f)
        } else if self.is_macro {
            write!(f, "<macro ({})>", self.args)
        } else {
            write!(f, "<fn* ({})>", self.args)
        }
    }
}
