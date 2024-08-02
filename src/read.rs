use ecow::EcoString;
pub use types::*;

use anyhow::{anyhow, Context};
use reedline::Signal;
use regex::Regex;

use crate::{expr, lazy::Lazy, Runtime, Term};

use self::stream::TokenStream;

mod stream;

pub mod expr_macro;
pub mod types;

static RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r#"[\s]*((!!)|~@|[\[\]{}()'`~^@\\#,]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
        .unwrap()
});

/// Split input into tokens
/// for reader macros:
///   - !! : creates an atom
///   - @ : dereference an atom
///   - \ : do infix notation, similar to haskell, see `parse_list`
///   - ' : quote
///   - ` : quasiquoting:
///     - ~ : splice-unquote
///     - , : unquote
pub fn tokenize(input: &str) -> TokenStream {
    RE.find_iter(input)
        .map(|it| it.as_str().trim())
        .filter(|it| !it.is_empty() && !it.starts_with(';'))
        .collect()
}

pub fn tokenize_with_whitespace(input: &str) -> TokenStream {
    RE.find_iter(input).map(|it| it.as_str()).collect()
}

fn get_inp(ctx: &mut Term) -> QxResult<String> {
    match ctx.reedline.read_line(&ctx.prompt) {
        Ok(Signal::Success(line)) => Ok(line),
        Ok(Signal::CtrlD | Signal::CtrlC) => Err(QxErr::Stop)?,
        any => Err(anyhow!("REPL Err: {any:?}"))?,
    }
}

pub(crate) fn read_stdin(runtime: &mut Runtime) -> QxResult<Expr> {
    Input::get(runtime.term())?.tokenize().try_into()
}

#[allow(dead_code)]
pub(crate) fn from_string(s: EcoString) -> QxResult<Expr> {
    Input(s).tokenize().try_into()
}

impl TryFrom<TokenStream<'_>> for Expr {
    type Error = QxErr;
    fn try_from(value: TokenStream<'_>) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl core::str::FromStr for Expr {
    type Err = QxErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Input(s.into()).tokenize().try_into()
    }
}

impl TokenStream<'_> {
    fn parse(mut self) -> Result<Expr, QxErr> {
        self.parse_atom().and_then(|it| {
            if self.is_eof() {
                Ok(it)
            } else {
                Err(QxErr::MismatchedParen(ParenType::Open))
            }
        })
    }

    fn parse_atom(&mut self) -> Result<Expr, QxErr> {
        let raw_token = self.next().context("Didnt expect EOF")?;

        Ok(match raw_token {
            "(" => self.parse_list()?,
            // if this is encountered, it's a syntax error,
            // because it should be consumed in `parse_list`
            ")" => Err(QxErr::MismatchedParen(ParenType::Close))?,

            // reader macros //
            "'" => expr!(cons expr!(quote), self.parse_atom()?),
            "@" => expr!(cons expr!(deref), self.parse_atom()?),
            "!!" => expr!(cons expr!(atom), self.parse_atom()?),

            "#" => {
                let next = self.parse_list()?;
                // .map_err(|_| QxErr::MissingToken(anyhow!("List for Anonymous Function")))?;

                // (fn* (% ($ :nodef)) <body>)
                expr![cons
                    expr!(sym "fn*"),
                    expr![cons
                        expr!(sym "%"),
                        expr![cons expr!(sym "$"),expr!(kw "nodef")]
                    ],
                    next
                ]
            }

            // quasiquoting
            "`" => expr!(cons expr!(quasiquote), self.parse_atom()?),
            "~" => expr!(cons expr!(sym "splice-unquote"), self.parse_atom()?),
            "," => expr!(cons expr!(unquote), self.parse_atom()?),
            //--//
            "nil" => Expr::Nil,
            "true" => expr!(bool true),
            "false" => expr!(bool false),

            int if int.parse::<i64>().is_ok() => {
                Expr::Int(
                    // SAFETY: already checked if it can parse into an int
                    unsafe { int.parse::<i64>().unwrap_unchecked() },
                )
            }

            string if string.starts_with('"') => {
                if string.len() == 1 || !string.ends_with('"') {
                    return Err(QxErr::MissingToken(anyhow!("Second String delimiter")));
                }

                Expr::String(
                    unescaper::unescape(&string[1..string.len() - 1])
                        .map_err(|err| {
                            QxErr::Any(anyhow!("Failed to unescape string: {string:?}, Err: {err}"))
                        })?
                        .into(),
                )
            }

            kw if kw.starts_with(':') => expr!(kw kw),
            sym => expr!(sym sym),
        })
    }

    fn parse_list(&mut self) -> Result<Expr, QxErr> {
        let mut list = Vec::new();

        loop {
            if self.peek() == Some(")") {
                self.next();
                break;
            }

            // infix operator, swaps the order of the last and next expressions
            // expressions to allow things like (10 \+ 10)
            if self.peek() == Some(r"\") {
                self.next();
                let op = self.parse_atom()?;

                list.push(op);

                let len = list.len();

                if len < 2 {
                    return Err(QxErr::Any(anyhow!("Invalid infix operator use!")));
                }

                list.swap(len - 1, len - 2);

                continue;
            }

            list.push(self.parse_atom()?);
        }

        // Ok(Expr::List(list.into()))
        Ok(Expr::List(list.into()))
    }
}
