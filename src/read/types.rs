use std::{cell::RefCell, default, rc::Rc};

use thiserror::Error;

use crate::Func;

pub type PResult<T> = Result<T, QxErr>;

pub mod closure;
pub mod typing;
pub use closure::Closure;

/// cheap to clone, only contains small values (with copy)
/// or `Rc`s
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
    Cons(Cons),
    #[default]
    Nil,
}

#[derive(Clone, Copy, Eq, PartialEq, Default, Debug)]
pub enum ExprType {
    Atom,
    Closure,
    Func,
    Int,
    String,
    Sym,
    List,
    Bool,
    Cons,
    #[default]
    Nil,
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
    TypeErr { expected: ExprType, found: ExprType },

    #[error("{0}")]
    LispErr(Expr),

    #[error("Integer Operation Failed!")]
    IntOverflowErr,
}

#[derive(Clone, Eq, PartialEq, Default)]
pub struct Cons(pub Option<Rc<ConsCell>>);

#[derive(Clone, Eq, PartialEq, Default)]
pub struct ConsCell {
    pub car: Expr,
    pub cdr: Cons,
}

#[derive(Clone)]
pub struct ConsIter {
    head: Option<Rc<ConsCell>>,
}

impl ConsIter {
    pub fn rest(self) -> Cons {
        Cons(self.head)
    }
}

impl Cons {
    pub fn collect<I: Iterator<Item = Expr> + DoubleEndedIterator>(iter: I) -> Cons {
        let inner = iter.rev().fold(None, |acc, it| {
            Some(Rc::new(ConsCell {
                car: it,
                cdr: Cons(acc),
            }))
        });

        Cons(inner)
    }

    pub fn car(&self) -> Option<Expr> {
        self.0.as_ref().map(|it| it.car.clone())
    }

    pub fn cdr_opt(&self) -> Option<Cons> {
        self.0.as_ref().map(|it| it.cdr.clone())
    }

    pub fn nth(&self, n: usize) -> Option<Expr> {
        match n {
            0 => self.car(),
            _ => self.cdr().nth(n - 1),
        }
    }

    /// cdr except it returns an empty Cons instead of an Option::None
    pub fn cdr(&self) -> Cons {
        match self.0 {
            Some(ref it) => it.cdr.clone(),
            None => Cons(None),
        }
    }

    pub fn len_is(self, n: usize) -> bool {
        self.into_iter().take(n).count() == n
    }
}

impl<T: AsRef<[Expr]>> From<T> for Cons {
    fn from(list: T) -> Self {
        Self(list.as_ref().iter().rev().fold(None, |acc, it| {
            Some(Rc::new(ConsCell {
                car: it.clone(),
                cdr: Cons(acc),
            }))
        }))
    }
}

impl Iterator for ConsIter {
    type Item = Expr;

    fn next(&mut self) -> Option<Self::Item> {
        let old_head = self.head.take();

        self.head = old_head.as_ref().map(|it| it.cdr.0.clone()).flatten();

        old_head.map(|it| it.car.clone())
    }
}

impl FromIterator<Expr> for Cons {
    fn from_iter<T: IntoIterator<Item = Expr>>(iter: T) -> Self {
        let v = iter.into_iter().collect::<Vec<_>>();

        Self::from(v)
    }
}

impl IntoIterator for &Cons {
    type Item = Expr;
    type IntoIter = ConsIter;

    fn into_iter(self) -> Self::IntoIter {
        ConsIter {
            head: self.0.clone(),
        }
    }
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

    pub fn get_type(&self) -> ExprType {
        match self {
            Self::Int(_) => ExprType::Int,
            Self::String(_) => ExprType::String,
            Self::Sym(_) => ExprType::Sym,
            Self::List(_) => ExprType::List,
            Self::Bool(_) => ExprType::Bool,
            Self::Cons(_) => ExprType::Cons,
            Self::Nil => ExprType::Nil,
            Self::Atom(_) => ExprType::Atom,
            Self::Closure(_) => ExprType::Closure,
            Self::Func(_) => ExprType::Func,
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
