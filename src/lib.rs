#![warn(clippy::pedantic, clippy::nursery)]

use std::{collections::HashMap, path::PathBuf, rc::Rc};

use colored::Colorize;
use reedline::{DefaultPrompt, FileBackedHistory, Reedline};

use read::{Expr, QxErr};
use env::{Env, EnvObj};

type FuncT = Rc<dyn Fn(&mut Runtime, &[Expr]) -> Result<Expr, read::QxErr>>;

pub struct Runtime {
    repl: Repl,
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
    pub fn new(repl: Repl) -> Self {
        Self {
            repl,
            env: Env::with_builtins(),
        }
    }
    
    pub fn read_from_stdin(&mut self) -> Result<read::AST, read::QxErr> {
        read::read(self)
    }

    #[inline]
    pub(crate) fn repl(&mut self) -> &mut Repl {
        &mut self.repl
    }
}

pub struct Repl {
    prompt: DefaultPrompt,
    reedline: Reedline,
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

impl Repl {
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

pub mod eval;
pub mod lazy;
pub mod print;
pub mod read;
pub mod env;

impl std::fmt::Display for Expr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_list(
            f: &mut std::fmt::Formatter,
            list: &[Expr],
        ) -> Result<(), std::fmt::Error> {
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
            Self::Func(_) => "<Func>".red().fmt(f),
        }
    }
}
