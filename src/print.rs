use crate::read::Expr;
use colored::Colorize;

pub fn pp_ast(ast: &Expr) {
    println!("{ast:?}",);
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_list(f: &mut std::fmt::Formatter, list: &[Expr]) -> Result<(), std::fmt::Error> {
            write!(f, "(")?;

            list.iter()
                .map(|it| format!("{it}"))
                .collect::<Vec<_>>()
                .join(" ")
                .fmt(f)?;

            write!(f, ")")
        }

        if f.alternate() {
            match self {
                Self::Atom(it) => format!("{:#}", it.borrow()).fmt(f),
                Self::Int(int) => int.to_string().fmt(f),
                Self::Sym(sym) => sym.fmt(f),
                Self::String(string) => string.fmt(f),
                Self::List(list) => write_list(f, list),
                Self::Nil => "nil".bold().blue().fmt(f),
                Self::Func(_) | Self::Closure(_) => Ok(()),
                Self::Bool(b) => b.to_string().fmt(f),
            }
        } else {
            match self {
                Self::Atom(it) => format!("&{{ {} }}", it.borrow()).fmt(f),
                Self::Int(int) => int.to_string().cyan().fmt(f),
                Self::Sym(sym) => sym.to_string().red().fmt(f),
                Self::String(string) => format!(r#""{string}""#).bright_green().fmt(f),
                Self::List(list) => write_list(f, list),
                Self::Nil => "nil".bold().blue().fmt(f),
                Self::Func(_) => "<Func>".red().fmt(f),
                Self::Closure(c) => format!(
                    "<Closure> ({})",
                    c.args_name
                        .iter()
                        .map(std::string::ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" "),
                )
                .red()
                .fmt(f),
                Self::Bool(b) => b.to_string().bright_blue().fmt(f),
            }
        }
    }
}
