#![warn(clippy::pedantic, clippy::nursery)]

use std::{collections::HashMap, path::PathBuf, rc::Rc};

use colored::Colorize;
use reedline::{DefaultPrompt, FileBackedHistory, Reedline};

use env::{Env, EnvObj};
use read::{Expr, QxErr};

type FuncT = Rc<dyn Fn(&mut Runtime, &[Expr]) -> Result<Expr, read::QxErr>>;

pub struct Runtime {
    repl: Term,
    env: Rc<Env>,
}

pub struct Func(FuncT);

impl Func {
    pub fn apply(&self, ctx: &mut Runtime, args: &[Expr]) -> Result<Expr, read::QxErr> {
        self.0(ctx, args)
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
        read::read(self)
    }

    #[inline]
    pub(crate) fn repl(&mut self) -> &mut Term {
        &mut self.repl
    }
}

pub struct Term {
    prompt: DefaultPrompt,
    reedline: Reedline,
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
        let history = Box::new(
            FileBackedHistory::with_file(50, PathBuf::from("~/.qrux/history.txt")).unwrap(),
        );

        Self {
            prompt: DefaultPrompt::default(),
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
