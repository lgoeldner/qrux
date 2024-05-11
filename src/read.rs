#![warn(clippy::pedantic, clippy::nursery)]

use std::{rc::Rc, vec};

use anyhow::{anyhow, bail, Context};
use colored::Colorize;
use reedline::Signal;
use regex::Regex;
use thiserror::Error;

use crate::{env::Env, lazy::Lazy, Func, Runtime, Term};

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

    #[error("Wrong / Missing argument, received: {0:?}")]
    NoArgs(Option<Vec<Expr>>),

    #[error("Type error, expected: {expected:?}, found: {found:?}")]
    TypeErr { expected: Expr, found: Expr },
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

#[derive(Clone, Debug)]
pub struct Closure {
    args_name: Vec<String>,
    body: Box<Expr>,
    captured: Rc<Env>
}

impl Closure {
    pub fn new(args_name: Vec<String>, body: Expr, captured: Rc<Env>) -> Self {
        Self {
            args_name,
            body: Box::new(body),
            captured,
        }
    }

    pub fn apply(&self, ctx: &mut Runtime, args: &[Expr]) -> Result<Expr, QxErr> {

        // define new env
        let old_env = Rc::clone(&ctx.env);

        ctx.env = Env::with_outer_args(Rc::clone(&self.captured), args, &self.args_name);
        let res = ctx.eval(*self.body.clone())?;

        // restore
        ctx.env = old_env;
        Ok(res)
    }
}

#[derive(Clone)]
pub enum Expr {
    Closure(Closure),
    Func(Func),
    Int(i64),
    String(String),
    Sym(String),
    List(Vec<Expr>),
    Bool(bool),
    Nil,
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
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

fn get_inp(ctx: &mut Term) -> PResult<String> {
    match ctx.reedline.read_line(&ctx.prompt) {
        Ok(Signal::Success(line)) => Ok(line),
        Ok(Signal::CtrlD | Signal::CtrlC) => Err(QxErr::Stop)?,
        any => Err(anyhow!("REPL Err: {any:?}"))?,
    }
}

pub(crate) fn read(runtime: &mut Runtime) -> PResult<AST> {
    Input::get(runtime.repl())?.tokenize().try_into()
}

impl TryFrom<TokenStream<'_>> for Vec<Expr> {
    type Error = QxErr;
    fn try_from(value: TokenStream<'_>) -> Result<Self, Self::Error> {
        parse(value)
    }
}

struct Input(String);
impl Input {
    pub fn get(ctx: &mut Term) -> PResult<Self> {
        Ok(Self(get_inp(ctx)?))
    }

    pub fn tokenize(&self) -> TokenStream {
        tokenize(&self.0)
    }
}

pub type AST = Vec<Expr>;

fn parse(mut tokens: TokenStream) -> Result<AST, QxErr> {
    let mut ast = vec![];

    while let Some(token) = tokens.peek() {
        // let token = tokens.peek().context("Missing token")?;

        let t = parse_atom(&mut tokens)?;

        ast.push(t);
    }

    Ok(ast)
}

fn parse_atom(stream: &mut TokenStream) -> Result<Expr, QxErr> {
    let raw_token = stream.next().context("Missing token")?;

    Ok(match raw_token {
        "(" => {
            // stream.back();
            parse_list(stream)?
        }
        ")" => Err(QxErr::MismatchedParen(ParenType::Close))?,

        "nil" => Expr::Nil,
        "true" => Expr::Bool(true),
        "false" => Expr::Bool(false),

        int if int.parse::<i64>().is_ok() => {
            Expr::Int(
                // SAFETY: already checked if it can parse into an int
                unsafe { int.parse::<i64>().unwrap_unchecked() },
            )
        }

        string if (string.starts_with('"') && string.ends_with('"')) => {
            if string.len() == 1 {
                return Err(QxErr::MissingToken(anyhow!("Second String delimiter")));
            }

            Expr::String(string[1..string.len() - 1].to_owned())
        }
        sym => Expr::Sym(sym.to_string()),
    })
}

fn parse_list(stream: &mut TokenStream) -> Result<Expr, QxErr> {
    // debug_assert_eq!(stream.next().context("Missing \"(\"")?, "(");

    let mut list = Vec::new();

    loop {
        if stream.peek() == Some(")") {
            stream.next();
            break;
        }

        list.push(parse_atom(stream)?);
    }

    Ok(Expr::List(list))
}
