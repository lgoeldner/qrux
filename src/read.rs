#![warn(clippy::pedantic, clippy::nursery)]

use std::vec;

use anyhow::{anyhow, bail, Context};
use reedline::Signal;
use regex::Regex;
use thiserror::Error;

use crate::{lazy::Lazy, Repl};

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
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Int(i64),
    String(String),
    Sym(String),
    List(Vec<TokenType>),
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

pub fn read() -> PResult<AST> {
    let inp = get_inp(&mut Repl::new())?;
    let tokens = tokenize(&inp);
    let ast = parse(tokens)?;

    Ok(ast)
}

pub type AST = Vec<TokenType>;

fn parse(mut tokens: TokenStream) -> Result<AST, anyhow::Error> {
    let mut ast = vec![];

    while let Some(token) = tokens.peek() {
        let token = tokens.peek().context("Missing token")?;

        let t = parse_atom(&mut tokens)?;

        ast.push(t);
    }

    Ok(ast)
}

fn parse_atom(stream: &mut TokenStream) -> Result<TokenType, anyhow::Error> {
    let token = stream.next().context("Missing token")?;

    Ok(match token {
        "(" => {
            stream.back();
            parse_list(stream)?
        }
        int if int.parse::<i64>().is_ok() => TokenType::Int(int.parse()?),
        string if string.starts_with('\"') && string.ends_with('\"') => {
            TokenType::String(string[1..string.len() - 1].to_owned())
        }
        sym => TokenType::Sym(sym.to_string()),
    })
}

fn parse_list(stream: &mut TokenStream) -> Result<TokenType, anyhow::Error> {
    debug_assert_eq!(stream.next().context("Missing \"(\"")?, "(");

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
