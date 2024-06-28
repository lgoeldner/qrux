use std::{cell::RefCell, rc::Rc};

use thiserror::Error;

use crate::Func;

pub type PResult<T> = Result<T, QxErr>;

pub mod closure;
pub mod typing;
pub use closure::Closure;

#[derive(Clone, Eq, PartialEq, Default)]
pub enum Expr {
    Atom(Rc<RefCell<Expr>>),
    Closure(Rc<Closure>),
    Func(Func),
    Int(i64),
    String(Rc<str>),
    Sym(Rc<str>),
    List(Rc<[Expr]>),
    Bool(bool),
    Cons(Option<Rc<ConsCell>>),
    #[default]
    Nil,
}

#[derive(Clone, Eq, PartialEq, Default)]
pub struct ConsCell {
    pub car: Expr,
    pub cdr: Option<Rc<ConsCell>>,
}


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
    TypeErr {
        expected: Box<Expr>,
        found: Box<Expr>,
    },

    #[error("{0}")]
    LispErr(Expr),
}

impl Expr {
    #[must_use]
    pub fn contains_sym(&self, sym: &str) -> bool {
        match self {
            Self::Sym(s) => &**s == sym,
            Self::List(l) => l.iter().any(|ex| ex.contains_sym(sym)),
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
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

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

pub struct Input(pub Rc<str>);
impl Input {
    pub fn get(ctx: &mut crate::Term) -> PResult<Self> {
        Ok(Self(super::get_inp(ctx)?.into()))
    }

    #[must_use]
    pub fn tokenize(&self) -> super::stream::TokenStream {
        super::tokenize(&self.0)
    }
}
