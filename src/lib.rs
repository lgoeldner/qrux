#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
// #![feature(try_trait_v2)]

use std::path::PathBuf;

use reedline::{DefaultPrompt, DefaultPromptSegment, FileBackedHistory, Reedline};

use env::{Env, Inner};
use read::{Expr, QxErr};

type FuncT = fn(&mut Runtime, &[Expr]) -> Result<Expr, read::QxErr>;

pub struct Runtime {
    repl: Term,
    env: Env,
}

impl std::fmt::Debug for Runtime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(Runtime))
            .field("env", &self.env)
            .finish_non_exhaustive()
    }
}

#[derive(Clone)]
pub struct Func(FuncT);

impl Eq for Func {}
// No Closure/Func has the same type
impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
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

    pub fn new_expr(f: fn(&mut Runtime, &[Expr]) -> Result<Expr, read::QxErr>) -> Expr {
        Expr::Func(Self(f))
    }
}

impl Runtime {
    #[must_use]
    pub fn new(repl: Term) -> Self {
        let mut prototype = Self {
            repl,
            env: Inner::new_env(None),
        };

        prototype
            .eval(
                include_str!("init.qx")
                    .parse()
                    .expect("builtin prelude failed"),
                None,
            )
            .expect("builtin prelude failed");

        prototype
    }

    #[must_use]
    /// # Safety
    /// Any Code evaluated with this runtime will not 
    /// have access to the prelude defined in the language itself
    pub unsafe fn without_prelude(repl: Term) -> Self {
        Self {
            repl,
            env: Inner::new_env(None),
        }
    }

    pub fn read_from_stdin(&mut self) -> Result<Expr, read::QxErr> {
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
        let mut dir = directories_next::ProjectDirs::from(
            //
            "io",
            "Linus Göldner",
            "qrux",
        )
        .map_or_else(
            || PathBuf::from(".qrux-history"),
            |it| it.data_dir().to_path_buf(),
        );

        dir.push("qrux-history.txt");

        let history = Box::new(FileBackedHistory::with_file(50, dir).unwrap());

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
