#![warn(clippy::pedantic, clippy::nursery)]

use std::path::PathBuf;

use read::TokenType;
use reedline::{DefaultPrompt, FileBackedHistory, Reedline};

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

use colored::Colorize;
impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => int.to_string().cyan().fmt(f),
            Self::Sym(sym) => sym.to_string().green().fmt(f),
            Self::String(string) => format!("'{string}'").bright_green().fmt(f),
            Self::List(list) => {
                write!(f, "(")?;

                list.iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
                    .fmt(f)?;

                write!(f, ")")
            }
        }
    }
}
