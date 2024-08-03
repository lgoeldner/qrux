use crate::{
    env::{Env, EnvMap},
    read,
};
use ecow::EcoString;
use fxhash::FxHashSet;
use nu_ansi_term::{Color, Style};
use reedline::{Highlighter, StyledText};
use std::{cell::RefCell, collections::HashSet, sync::Arc};

pub struct Lisp {
    funcs: FxHashSet<String>,
    colors: ColorTheme,
}

pub struct ColorTheme {
    pub func: Color,
    pub closure: Color,
    pub neutral: Color,
    pub num: Color,
    pub str: Color,
    pub bool: Color,
    pub nil: Color,
    pub parens: [Color; 3],
    pub comment: Color,
    pub kw: Color,
    pub quoted: Color,
}

impl Default for ColorTheme {
    fn default() -> Self {
        use Color as C;
        Self {
            func: C::LightRed,
            closure: C::LightMagenta,
            neutral: C::Purple,
            num: C::Cyan,
            str: C::LightGreen,
            bool: C::Blue,
            nil: C::LightBlue,
            parens: [
                Color::Rgb(255, 198, 124),
                Color::Rgb(228, 124, 255),
                Color::LightBlue,
            ],
            comment: C::DarkGray,
            kw: Color::Rgb(138, 206, 0),
            quoted: C::LightGray,
        }
    }
}

impl Highlighter for Lisp {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let colors = &self.colors;

        let mut styled_text = StyledText::new();
        let mut tokens = read::tokenize_with_whitespace(line);

        let mut paren_depth: i32 = 0;

        loop {
            let Some(token) = tokens.next() else {
                break;
            };

            let trimmed = token.trim();

            match trimmed {
                // skip a quoted list
                "(" if tokens.prev().is_some_and(|it| it.ends_with('\'')) => {
                    styled_text.push((
                        Style::from(colors.parens[paren_depth.unsigned_abs() as usize % 3]),
                        token.to_string(),
                    ));

                    let Some(_) = tokens.peek() else {
                        continue;
                    };

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

                        styled_text.push((Style::from(colors.quoted), t.to_string()));
                    }

                    tokens.back();
                }

                // parentheses
                "(" => styled_text.push((
                    Style::from(colors.parens[paren_depth.unsigned_abs() as usize % 3]),
                    token.to_string(),
                )),

                ")" => styled_text.push((
                    Style::from(colors.parens[(paren_depth + 2).unsigned_abs() as usize % 3]),
                    token.to_string(),
                )),

                kw if kw.starts_with(':') => {
                    styled_text.push((Style::from(colors.kw), token.to_string()));
                }

                string if string.starts_with('"') => {
                    styled_text.push((Style::from(colors.str), token.to_string()));
                }

                num if num.parse::<i64>().is_ok() => {
                    styled_text.push((Style::from(colors.num), token.to_string()));
                }

                "true" | "false" => {
                    styled_text.push((Style::from(colors.bool), token.to_string()));
                }

                "nil" => styled_text.push((Style::from(colors.nil).bold(), token.to_string())),

                func if self.funcs.contains(func) => {
                    styled_text.push((Style::from(colors.func).bold(), token.to_string()));
                }

                comment if comment.starts_with(';') => {
                    styled_text.push((Style::from(colors.comment).bold(), token.to_string()));
                }

                // non-keyword functions
                _ if matches!(tokens.prev().map(str::trim), Some("(")) => {
                    styled_text.push((Style::from(colors.closure), token.to_string()));
                }
                // other data
                _ => styled_text.push((Style::from(colors.neutral), token.to_string())),
            }

            match trimmed {
                "(" => paren_depth += 1,
                ")" => paren_depth -= 1,
                _ => {}
            }
        }

        styled_text
    }
}

impl Lisp {
    /// Construct the default highlighter with a given set of extern commands/keywords to detect and highlight
    pub fn new(funcs: FxHashSet<String>, colors: impl Into<Option<ColorTheme>>) -> Self {
        Self {
            colors: colors.into().unwrap_or_default(),
            funcs,
        }
    }
}

impl Default for Lisp {
    fn default() -> Self {
        Self::new(Default::default(), None)
    }
}
