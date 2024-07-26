use core::fmt;
use std::rc::Rc;

use crate::read::{Closure, Cons, ConsCell, Expr};
use colored::Colorize;

pub fn pp_ast(ast: &Expr) {
    println!("{ast:?}",);
}

impl std::fmt::Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        self.0
            .as_ref()
            .map(|it| write_cons_inner(f, &it))
            .transpose()?;

        write!(f, ")")
    }
}

impl std::fmt::Debug for Cons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}

fn write_cons_inner(f: &mut fmt::Formatter, list: &Rc<ConsCell>) -> fmt::Result {
    match &**list {
        ConsCell {
            ref car,
            cdr: Cons(None),
        } => {
            if f.alternate() {
                write!(f, "{car:#}")
            } else {
                write!(f, "{car}")
            }
        }

        ConsCell {
            car,
            cdr: Cons(Some(ref cdr)),
        } => {
            if f.alternate() {
                write!(f, "{car:#}")?;
            } else {
                write!(f, "{car}")?;
            }

            write!(f, " ")?;

            write_cons_inner(f, cdr)
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                Self::Atom(it) => format!("{:#}", it.borrow()).fmt(f),
                Self::Int(int) => int.to_string().fmt(f),
                Self::Sym(sym) => write!(f, "{sym}"),
                Self::String(string) => write!(f, "{string}"),
                // Self::List(list) => write_list(f, list),
                Self::Nil => "nil".fmt(f),
                Self::Func(_) | Self::Closure(_) => Ok(()),
                Self::Bool(b) => b.to_string().fmt(f),
                Self::List(it) => {
                    write!(f, "(")?;
                    it.0.as_ref()
                        .map(|it| write_cons_inner(f, &it))
                        .transpose()?;
                    write!(f, ")")
                }
                Self::Keyword(kw) => write!(f, "{kw}"),
            }
        } else {
            match self {
                Self::Atom(it) => format!("<Atom ({})>", it.borrow()).fmt(f),
                Self::Int(int) => int.to_string().cyan().fmt(f),
                Self::Sym(sym) => sym.to_string().red().fmt(f),
                Self::String(string) => format!(r#""{string}""#).bright_green().fmt(f),
                // Self::List(list) => write_list(f, list),
                Self::Nil => "nil".bold().blue().fmt(f),
                Self::Func(_) => "<Func>".red().fmt(f),
                Self::Closure(c) => c.to_string().red().fmt(f),
                Self::Bool(b) => b.to_string().bright_blue().fmt(f),
                Self::List(it) => it.fmt(f),
                Self::Keyword(kw) => kw.to_string().green().fmt(f),
            }
        }
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            "".fmt(f)
        } else if self.is_macro {
            write!(f, "<macro ({})>", self.args_name.join(" "))
        } else {
            write!(f, "<fn* ({})>", self.args_name.join(" "))
        }
    }
}
