use std::collections::HashSet;

use crate::read;
use nu_ansi_term::{Color, Style};
use reedline::Highlighter;
use reedline::StyledText;

pub struct Lisp {
    funcs: HashSet<&'static str>,
}

impl Highlighter for Lisp {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let func_color = Color::LightRed;
        let fn_color = Color::LightMagenta;
        let neutral_color = Color::Purple;
        let num_color = Color::Cyan;
        let str_color = Color::LightGreen;
        let bool_color = Color::Blue;
        let nil_color = Color::LightBlue;
        let paren_colors = [
            Color::Rgb(255, 198, 124),
            Color::Rgb(228, 124, 255),
            Color::LightBlue,
        ];
        let comment_color = Color::DarkGray;
        let kw_color = Color::Rgb(138, 206, 0);

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
                        Style::from(paren_colors[paren_depth.unsigned_abs() as usize % 3]),
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

                        styled_text.push((Style::from(Color::LightGray), t.to_string()));
                    }

                    tokens.back();
                }

                // parentheses
                "(" => styled_text.push((
                    Style::from(paren_colors[paren_depth.unsigned_abs() as usize % 3]),
                    token.to_string(),
                )),

                ")" => styled_text.push((
                    Style::from(paren_colors[(paren_depth + 2).unsigned_abs() as usize % 3]),
                    token.to_string(),
                )),

                kw if kw.starts_with(':') => {
                    styled_text.push((Style::from(kw_color), token.to_string()));
                }

                string if string.starts_with('"') => {
                    styled_text.push((Style::from(str_color), token.to_string()));
                }

                num if num.parse::<i64>().is_ok() => {
                    styled_text.push((Style::from(num_color), token.to_string()));
                }

                "true" | "false" => {
                    styled_text.push((Style::from(bool_color), token.to_string()));
                }

                "nil" => styled_text.push((Style::from(nil_color).bold(), token.to_string())),

                kw if self.funcs.contains(kw) => {
                    styled_text.push((Style::from(func_color).bold(), token.to_string()));
                }

                comment if comment.starts_with(';') => {
                    styled_text.push((Style::from(comment_color).bold(), token.to_string()));
                }

                // non-keyword functions
                _ if matches!(tokens.prev().map(str::trim), Some("(")) => {
                    styled_text.push((Style::from(fn_color), token.to_string()));
                }
                // other data
                _ => styled_text.push((Style::from(neutral_color), token.to_string())),
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
    pub fn new(external_commands: Vec<&'static str>) -> Self {
        Self {
            funcs: HashSet::from_iter(external_commands),
        }
    }
}

impl Default for Lisp {
    fn default() -> Self {
        Self::new(vec![])
    }
}
