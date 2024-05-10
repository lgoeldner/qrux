#![warn(clippy::pedantic, clippy::nursery)]

use std::vec;

use anyhow::{anyhow, bail, Context};
use reedline::Signal;
use regex::Regex;
use thiserror::Error;

use crate::{lazy::Lazy, Repl, Runtime};

use self::stream::TokenStream;

mod stream;

#[derive(Error, Debug)]
pub enum QxErr {
    #[error("Interrupted, Stop")]
    Stop,
    #[error("Fatal Error: {0}")]
    Fatal(#[from] Box<QxErr>),
    #[error(transparent)]
    Any(#[from] anyhow::Error),

    #[error("Mismatched Paren {0}")]
    MismatchedParen(ParenType),

    #[error("Missing Token: {0}")]
    MissingToken(anyhow::Error),

    #[error("Missing argument, received: {0:?}")]
    NoArgs(Option<Vec<TokenType>>),
}

#[derive(Debug)]
pub enum ParenType {
    Open,
    Close,
}

impl std::fmt::Display for ParenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Open => write!(f, "("),
            Self::Close => write!(f, ")"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Int(i64),
    String(String),
    Sym(String),
    List(Vec<TokenType>),
    Nil,
}

type PResult<T> = Result<T, QxErr>;

pub fn tokenize(input: &str) -> TokenStream {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
            .unwrap()
    });

    RE.find_iter(input)
        .map(|it| it.as_str().trim())
        .filter(|it| !it.is_empty())
        .collect()
}

fn get_inp(ctx: &mut Repl) -> PResult<String> {
    match ctx.reedline.read_line(&ctx.prompt) {
        Ok(Signal::Success(line)) => Ok(line),
        Ok(Signal::CtrlD | Signal::CtrlC) => Err(QxErr::Stop)?,
        any => Err(anyhow!("REPL Err: {any:?}"))?,
    }
}

pub(crate) fn read(runtime: &mut Runtime) -> PResult<AST> {
    Input::get(runtime.repl())?.tokenize().try_into()
}

impl TryFrom<TokenStream<'_>> for Vec<TokenType> {
    type Error = QxErr;
    fn try_from(value: TokenStream<'_>) -> Result<Self, Self::Error> {
        parse(value)
    }
}

struct Input(String);
impl Input {
    pub fn get(ctx: &mut Repl) -> PResult<Self> {
        Ok(Self(get_inp(ctx)?))
    }

    pub fn tokenize(&self) -> TokenStream {
        tokenize(&self.0)
    }
}

pub type AST = Vec<TokenType>;

fn parse(mut tokens: TokenStream) -> Result<AST, QxErr> {
    let mut ast = vec![];

    while let Some(token) = tokens.peek() {
        // let token = tokens.peek().context("Missing token")?;

        let t = parse_atom(&mut tokens)?;

        ast.push(t);
    }

    Ok(ast)
}

fn parse_atom(stream: &mut TokenStream) -> Result<TokenType, QxErr> {
    let raw_token = stream.next().context("Missing token")?;

    Ok(match raw_token {
        "(" => {
            // stream.back();
            parse_list(stream)?
        }
        ")" => Err(QxErr::MismatchedParen(ParenType::Close))?,

        "nil" => TokenType::Nil,

        int if int.parse::<i64>().is_ok() => {
            TokenType::Int(
                // SAFETY: already checked if it can parse into an int
                unsafe { int.parse::<i64>().unwrap_unchecked() },
            )
        }

        string if (string.starts_with('"') && string.ends_with('"')) => {
            if string.len() == 1 {
                return Err(QxErr::MissingToken(anyhow!("Second String delimiter")));
            }

            TokenType::String(string[1..string.len() - 1].to_owned())
        }
        sym => TokenType::Sym(sym.to_string()),
    })
}

fn parse_list(stream: &mut TokenStream) -> Result<TokenType, anyhow::Error> {
    // debug_assert_eq!(stream.next().context("Missing \"(\"")?, "(");

    let mut list = Vec::new();

    loop {
        if stream.peek() == Some(")") {
            stream.next();
            break;
        }

        list.push(parse_atom(stream)?);
    }

    Ok(TokenType::List(list))
}
