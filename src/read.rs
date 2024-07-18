use ecow::EcoString;
use tap::Pipe;
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
    Regex::new(
        r#"[\s]*((!!)|~@|[\[\]{}()'`~^@\\]|"(?:\\.|[^\\"])*"?|;.*|,|[^\s\[\]{}('"`,;)]*)"#,
    )
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
    RE.find_iter(input)
        .map(|it| it.as_str())
        .collect()
}

fn get_inp(ctx: &mut Term) -> PResult<String> {
    match ctx.reedline.read_line(&ctx.prompt) {
        Ok(Signal::Success(line)) => Ok(line),
        Ok(Signal::CtrlD | Signal::CtrlC) => Err(QxErr::Stop)?,
        any => Err(anyhow!("REPL Err: {any:?}"))?,
    }
}

pub(crate) fn read_stdin(runtime: &mut Runtime) -> PResult<Expr> {
    Input::get(runtime.term())?.tokenize().try_into()
}

#[allow(dead_code)]
pub(crate) fn from_string(s: EcoString) -> PResult<Expr> {
    Input(s).tokenize().try_into()
}

impl TryFrom<TokenStream<'_>> for Expr {
    type Error = QxErr;
    fn try_from(value: TokenStream<'_>) -> Result<Self, Self::Error> {
        parse(value)
    }
}

impl core::str::FromStr for Expr {
    type Err = QxErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Input(s.into())
            .tokenize()
            .try_into()
    }
}

fn parse(mut tokens: TokenStream) -> Result<Expr, QxErr> {
    parse_atom(&mut tokens).and_then(|it| {
        if tokens.is_eof() {
            Ok(it)
        } else {
            Err(QxErr::MismatchedParen(ParenType::Open))
        }
    })
}

fn parse_atom(stream: &mut TokenStream) -> Result<Expr, QxErr> {
    let raw_token = stream.next().context("Didnt expect EOF")?;

    Ok(match raw_token {
        "(" => parse_list(stream)?,
        // if this is encountered, it's a syntax error,
        // because it should be consumed in `parse_list`
        ")" => Err(QxErr::MismatchedParen(ParenType::Close))?,

        // reader macros //
        "'" => expr!(cons expr!(quote), parse_atom(stream)?),
        "@" => expr!(cons expr!(deref), parse_atom(stream)?),
        "!!" => expr!(cons expr!(atom), parse_atom(stream)?),

        // quasiquoting
        "`" => expr!(cons expr!(quasiquote), parse_atom(stream)?),
        "~" => expr!(cons expr!(sym "splice-unquote"), parse_atom(stream)?),
        "," => expr!(cons expr!(unquote), parse_atom(stream)?),
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
        sym => expr!(sym sym),
    })
}

fn parse_list(stream: &mut TokenStream) -> Result<Expr, QxErr> {
    let mut list = Vec::new();

    loop {
        if stream.peek() == Some(")") {
            stream.next();
            break;
        }

        // infix operator, swaps the order of the last and next expressions
        // expressions to allow things like (10 \+ 10)
        if stream.peek() == Some(r"\") {
            stream.next();
            let op = parse_atom(stream)?;

            list.push(op);

            let len = list.len();

            if len < 2 {
                return Err(QxErr::Any(anyhow!("Invalid infix operator use!")));
            }

            list.swap(len - 1, len - 2);

            continue;
        }

        list.push(parse_atom(stream)?);
    }

    // Ok(Expr::List(list.into()))
    Ok(Expr::List(list.into()))
}
