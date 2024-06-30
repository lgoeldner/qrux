#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
// #![feature(try_trait_v2)]

use std::path::PathBuf;

use reedline::{DefaultPrompt, DefaultPromptSegment, FileBackedHistory, Reedline};

use env::{Env, Inner};
use read::{Cons, Expr, PResult, QxErr};

type FuncT = fn(Cons, Env, &mut Runtime) -> Result<Expr, read::QxErr>;

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
pub struct Func(&'static str, FuncT);

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
    #[inline]
    pub fn apply(
        &self,
        ctx: &mut Runtime,
        args: Cons,
        env: Option<Env>,
    ) -> Result<Expr, read::QxErr> {
        self.1(args, env.unwrap_or(ctx.env.clone()), ctx)
    }

    #[inline]
    pub fn new_expr(label: &'static str, f: FuncT) -> Expr {
        Expr::Func(Self(label, f))
    }
}

impl Runtime {
    /// returns the new Runtime and the result of evaluating the prelude and,
    /// if supplied, the first `env::arg` as a file

    pub fn new(repl: Term) -> (Self, PResult<Expr>) {
        let mut prototype = Self {
            repl,
            env: Inner::new_env(None),
        };

        prototype.env.set(
            &"*ARGS*".into(),
            Expr::List(std::env::args().skip(1).map(|it| expr!(str it)).collect()),
        );

        let res = prototype.eval(
            include_str!("init.qx")
                .parse()
                .expect("builtin prelude failed"),
            None,
        );

        (prototype, res)
    }

    #[must_use]
    /// # Safety
    /// Any Code evaluated with this runtime will not
    /// have access to the prelude.
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

trait Apply {
    /// applies the function to self and returns the result
    fn apply<R>(self, f: impl FnOnce(Self) -> R) -> R
    where
        Self: Sized;
}

impl<T> Apply for T {
    fn apply<R>(self, f: impl FnOnce(Self) -> R) -> R
    where
        Self: Sized,
    {
        f(self)
    }
}
