#![warn(clippy::pedantic, clippy::nursery)]
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::must_use_candidate,
    clippy::use_self,
    clippy::option_if_let_else,
    clippy::similar_names
)]

use env::{core_map, Env};
use fxhash::FxHashSet;
use read::{Cons, Expr, QxResult};
use reedline::{DefaultPrompt, DefaultPromptSegment, FileBackedHistory, Reedline};
use std::path::PathBuf;
use tap::{Pipe, Tap};

type FuncT = fn(Cons, Env, &mut Runtime) -> Result<Expr, read::QxErr>;

/// An instance of a qx runtime. 
/// Use `Runtime::new()` to create
/// Use `Runtime::eval`
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

/// Represents the Terminal, used for highlighting and prompting
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
        self.1(args, env.unwrap_or_else(|| ctx.env.clone()), ctx)
    }

    #[inline]
    pub fn new_expr(label: &'static str, f: FuncT) -> Expr {
        Expr::Func(Self(label, f))
    }
}

impl Runtime {
    /// returns the new Runtime and the result of evaluating the prelude and,
    /// if supplied, the first `env::arg` as a file
    pub fn new() -> (Self, QxResult<Expr>) {
        let repl = Term::new();
        let mut prototype = Self::without_prelude(repl);

        prototype
            .env
            .set(
                "*ARGS*".into(),
                Expr::List(std::env::args().skip(1).map(|it| expr!(str it)).collect()),
                eval::Shadow::No,
            )
            .pipe(drop);

        let res = prototype.eval(
            include_str!("init.qx")
                .parse()
                .expect("builtin prelude failed"),
            None,
        );

        (prototype, res)
    }

    #[must_use]
    /// Any Code evaluated with this runtime will not
    /// have access to the prelude.
    pub fn without_prelude(repl: Term) -> Self {
        Self {
            repl,
            env: Env::core(),
        }
    }

    pub fn read_from_stdin(&mut self) -> Result<Expr, read::QxErr> {
        read::read_stdin(self)
    }

    #[inline]
    pub(crate) const fn term(&mut self) -> &mut Term {
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
        let dir = directories_next::ProjectDirs::from("io", "Linus Göldner", "qrux")
            .map_or_else(
                || PathBuf::from(".qrux-history"),
                |it| it.data_dir().to_path_buf(),
            )
            .with_file_name("qrux-history.txt");

        let history = Box::new(FileBackedHistory::with_file(50, dir).unwrap());

        let highlighter = highlighter::Lisp::new(
            core_map()
                .into_keys()
                .map(|it| it.to_string())
                .collect::<FxHashSet<String>>()
                .tap_mut(|it| {
                    it.extend(
                        [
                            "fn*",
                            "loop",
                            "do",
                            "val!",
                            "del!",
                            "defmacro!",
                            "mexp",
                            "try*",
                            "if",
                            "let*",
                            "quote",
                            "qqex",
                            "quasiquote",
                            "do",
                        ]
                        .as_slice()
                        .iter()
                        .map(ToString::to_string),
                    );
                }),
            None,
        );

        Self {
            prompt: DefaultPrompt {
                left_prompt: DefaultPromptSegment::Basic("User".to_owned()),
                right_prompt: DefaultPromptSegment::Empty,
            },

            reedline: Reedline::create()
                .with_history(history)
                .with_highlighter(Box::new(highlighter)),
        }
    }
}

pub mod env;
pub mod eval;
mod highlighter;
pub mod lazy;
pub mod print;
pub mod read;
