#![warn(clippy::pedantic, clippy::nursery)]
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::must_use_candidate
)]
// #![feature(try_trait_v2)]

use std::{path::PathBuf, vec};

use reedline::{
    Completer, DefaultCompleter, DefaultHinter, DefaultPrompt, DefaultPromptSegment,
    ExampleHighlighter, FileBackedHistory, Reedline,
};

use nu_ansi_term::{Color, Style};

use env::{Env, Inner};
use read::{Cons, Expr, PResult};
use tap::{Pipe, Tap};

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
pub struct Func(&'static str, FuncT, bool);

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
    pub const fn should_eval_args(&self) -> bool {
        self.2
    }

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
    pub fn new_expr(label: &'static str, f: FuncT, noeval: bool) -> Expr {
        Expr::Func(Self(label, f, noeval))
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

        prototype
            .env
            .set(
                &"*ARGS*".into(),
                Expr::Cons(std::env::args().skip(1).map(|it| expr!(str it)).collect()),
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
        let dir = directories_next::ProjectDirs::from("io", "Linus Göldner", "qrux")
            .map_or_else(
                || PathBuf::from(".qrux-history"),
                |it| it.data_dir().to_path_buf(),
            )
            .with_file_name("qrux-history.txt");

        let history = Box::new(FileBackedHistory::with_file(50, dir).unwrap());
        let commands = env::core_func_names();

        // TODO: Write custom highlighter
        let highlighter = exh::ExampleHighlighter::new(commands)
            .tap_mut(|it| it.change_colors(Color::LightPurple, Color::Cyan, Color::Cyan));

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
pub mod lazy;
pub mod print;
pub mod read;

mod exh {
    use nu_ansi_term::{Color, Style};
    use reedline::Highlighter;
    use reedline::StyledText;

    pub static DEFAULT_BUFFER_MATCH_COLOR: Color = Color::Green;
    pub static DEFAULT_BUFFER_NEUTRAL_COLOR: Color = Color::White;
    pub static DEFAULT_BUFFER_NOT_MATCH_COLOR: Color = Color::Red;

    /// A simple, example highlighter that shows how to highlight keywords
    pub struct ExampleHighlighter {
        external_commands: Vec<&'static str>,
        match_color: Color,
        not_match_color: Color,
        neutral_color: Color,
    }

    impl Highlighter for ExampleHighlighter {
        fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
            let mut styled_text = StyledText::new();

            if self.external_commands.iter().any(|x| line.contains(x)) {
                let matches: Vec<&str> = self
                    .external_commands
                    .iter()
                    .filter(|c| line.contains(*c))
                    .map(std::ops::Deref::deref)
                    .collect();

                let longest_match = matches
                    .iter()
                    .map(|it| (it, it.len()))
                    .max_by_key(|(_, k)| *k)
                    .map_or("", |(it, _)| it);

                let buffer_split: Vec<&str> = line.splitn(2, &longest_match).collect();

                styled_text.push((
                    Style::new().fg(self.neutral_color),
                    buffer_split[0].to_string(),
                ));

                styled_text.push((Style::new().fg(self.match_color), longest_match.to_string()));

                styled_text.push((
                    Style::new().bold().fg(self.neutral_color),
                    buffer_split[1].to_string(),
                ));
            } else if self.external_commands.is_empty() {
                styled_text.push((Style::new().fg(self.neutral_color), line.to_string()));
            } else {
                styled_text.push((Style::new().fg(self.not_match_color), line.to_string()));
            }

            styled_text
        }
    }

    impl ExampleHighlighter {
        /// Construct the default highlighter with a given set of extern commands/keywords to detect and highlight
        pub fn new(external_commands: Vec<&'static str>) -> Self {
            Self {
                external_commands,
                match_color: DEFAULT_BUFFER_MATCH_COLOR,
                not_match_color: DEFAULT_BUFFER_NOT_MATCH_COLOR,
                neutral_color: DEFAULT_BUFFER_NEUTRAL_COLOR,
            }
        }

        /// Configure the highlighter to use different colors
        pub fn change_colors(
            &mut self,
            match_color: Color,
            notmatch_color: Color,
            neutral_color: Color,
        ) {
            self.match_color = match_color;
            self.not_match_color = notmatch_color;
            self.neutral_color = neutral_color;
        }
    }
    impl Default for ExampleHighlighter {
        fn default() -> Self {
            Self::new(vec![])
        }
    }
}
