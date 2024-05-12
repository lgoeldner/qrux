#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
// #![feature(try_trait_v2)]


use std::{path::PathBuf, rc::Rc};

use colored::Colorize;
use reedline::{DefaultPrompt, DefaultPromptSegment, FileBackedHistory, Reedline};

use env::Env;
use read::{Expr, QxErr};

type FuncT = Rc<dyn Fn(&mut Runtime, &[Expr]) -> Result<Expr, read::QxErr>>;

pub struct Runtime {
    repl: Term,
    env: Rc<Env>,
}

pub struct Func(FuncT);

impl Eq for Func {}
// No Closure/Func has the same type
impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

pub struct Term {
    prompt: DefaultPrompt,
    reedline: Reedline,
}

impl Func {
    pub fn apply(&self, ctx: &mut Runtime, args: &[Expr]) -> Result<Expr, read::QxErr> {
        self.0(ctx, args)
    }

    pub fn new_expr(
        f: impl Fn(&mut Runtime, &[Expr]) -> Result<Expr, read::QxErr> + 'static,
    ) -> Expr {
        Expr::Func(Self(Rc::new(f)))
    }
}

impl Clone for Func {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl Runtime {
    #[must_use]
    pub fn new(repl: Term) -> Self {
        Self {
            repl,
            env: Env::new(),
        }
    }

    pub fn read_from_stdin(&mut self) -> Result<read::AST, read::QxErr> {
        read::read_stdin(self)
    }

    #[inline]
    pub(crate) fn term(&mut self) -> &mut Term {
        &mut self.repl
    }
}

impl Default for Term {
    fn default() -> Self {
        Self::new()
    }
}

impl Term {
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn new() -> Self {
        let history =
            Box::new(FileBackedHistory::with_file(50, PathBuf::from(".qrux/history.txt")).unwrap());

        Self {
            prompt: DefaultPrompt {
                left_prompt: DefaultPromptSegment::Basic("User".to_owned()),
                ..Default::default()
            },
            reedline: Reedline::create().with_history(history),
        }
    }
}

pub mod env;
pub mod eval;
pub mod lazy;
pub mod print;
pub mod read;

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_list(f: &mut std::fmt::Formatter, list: &[Expr]) -> Result<(), std::fmt::Error> {
            write!(f, "(")?;

            list.iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
                .fmt(f)?;

            write!(f, ")")
        }

        match self {
            Self::Int(int) => int.to_string().cyan().fmt(f),
            Self::Sym(sym) => sym.to_string().red().fmt(f),
            Self::String(string) => format!(r#""{string}""#).bright_green().fmt(f),
            Self::List(list) => write_list(f, list),
            Self::Nil => "nil".bold().blue().fmt(f),
            Self::Func(_) | Self::Closure(_) => "<Func>".red().fmt(f),
            Self::Bool(b) => b.to_string().bright_blue().fmt(f),
        }
    }
}
