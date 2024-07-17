#![warn(clippy::pedantic, clippy::nursery)]
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::must_use_candidate
)]
// #![feature(try_trait_v2)]

use std::{collections::HashSet, path::PathBuf, vec};

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

    pub fn new(repl: Term) -> (Self, PResult<Expr>) {
        let mut prototype = Self {
            repl,
            env: Inner::new_env(None),
        };

        prototype
            .env
            .set(
                "*ARGS*".into(),
                Expr::Cons(std::env::args().skip(1).map(|it| expr!(str it)).collect()),
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

        let highlighter = highlighter::Lisp::new(commands);

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

mod highlighter {
    use std::collections::HashSet;

    use crate::read;
    use nu_ansi_term::{Color, Style};
    use reedline::Highlighter;
    use reedline::StyledText;
    use tap::prelude::*;

    pub static DEFAULT_BUFFER_MATCH_COLOR: Color = Color::Green;
    pub static DEFAULT_BUFFER_NEUTRAL_COLOR: Color = Color::White;
    pub static DEFAULT_BUFFER_NOT_MATCH_COLOR: Color = Color::Red;

    /// A simple, example highlighter that shows how to highlight keywords
    pub struct Lisp {
        keywords: HashSet<&'static str>,
    }

    impl Highlighter for Lisp {
        fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
            let paren_color = Color::LightGreen;
            let kw_color = Color::Blue;
            let fn_color = Color::Red;
            let neutral_color = Color::Yellow;
            let num_color = Color::Magenta;
            let str_color = Color::Green;
            let bool_color = Color::Blue;
            let nil_color = Color::LightBlue;
            let mut styled_text = StyledText::new();
            let mut tokens = read::tokenize_with_whitespace(line);
            let paren_colors = [Color::Yellow, Color::Purple, Color::Blue, Color::Yellow];
            
            let mut paren_depth: i32 = 0;

            loop {
                let Some(token) = tokens.next() else {
                    break;
                };
                
                let trimmed = token.trim();
                
                match trimmed {
                    "(" => paren_depth += 1,
                    ")" => paren_depth -= 1,
                    _ => {}
                }
                
                match trimmed {
                    // skip a quoted list
                    "(" if matches!(tokens.prev().map(str::trim), Some("'")) => {
                        styled_text.push((Style::from(paren_colors[paren_depth.unsigned_abs() as usize % 3]), token.to_string()));
                        let mut stack = 1;
                        loop {
                            let Some(t) = tokens.next() else { break };

                            match t.trim() {
                                ")" => stack -= 1,
                                "(" => stack += 1,
                                _ => (),
                            }

                            if stack <= 0 {
                                break;
                            }

                            styled_text.push((Style::from(neutral_color), t.to_string()));
                        }

                        tokens.back();
                    }

                    "(" | ")" => styled_text.push((Style::from(paren_colors[paren_depth.unsigned_abs() as usize % 3]), token.to_string())),
                    
                    string if string.starts_with('"') => styled_text.push((Style::from(str_color), token.to_string())),
                    num if num.parse::<i64>().is_ok() => {
                        styled_text.push((Style::from(num_color), token.to_string()));
                    }
                    "true" | "false" => styled_text.push((Style::from(bool_color), token.to_string())),
                    "nil" => styled_text.push((Style::from(nil_color).bold(), token.to_string())),
                    kw if self.keywords.contains(kw) => styled_text.push((Style::from(kw_color).bold(), token.to_string())),

                    _ if matches!(tokens.prev().map(str::trim), Some("(")) => {
                        styled_text.push((Style::from(fn_color), token.to_string()));
                    }
                    _ => styled_text.push((Style::from(neutral_color), token.to_string())),
                }
            }

            styled_text
        }
    }

    impl Lisp {
        /// Construct the default highlighter with a given set of extern commands/keywords to detect and highlight
        pub fn new(external_commands: Vec<&'static str>) -> Self {
            Self {
                keywords: HashSet::from_iter(external_commands),
            }
        }
    }

    impl Default for Lisp {
        fn default() -> Self {
            Self::new(vec![])
        }
    }
}
