use crate::{env::Env, Func};
use ecow::EcoString;
use im::HashMap;
use kw::Keyword;
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

pub type QxResult<T> = Result<T, QxErr>;

pub mod closure;
pub mod typing;
pub use closure::Closure;
pub mod cast;
pub mod kw;

pub type Atom = Rc<RefCell<Expr>>;

/// cheap to clone, only contains `Copy` or `Rc`s
#[derive(Clone, Eq, PartialEq, Default)]
pub enum Expr {
    #[default]
    Nil,
    Atom(Atom),
    Closure(Rc<Closure>),
    Func(Func),
    Int(i64),
    String(EcoString),
    Sym(EcoString),
    Bool(bool),
    List(Cons),
    Keyword(kw::Keyword),
    Vec(im::Vector<Expr>),   
    Map(HashMap<Keyword, Expr>),
    MapLit(Rc<[Expr]>),
}

#[derive(Clone, Copy, Eq, PartialEq, Default, Debug)]
pub enum ExprType {
    #[default]
    Nil,
    Atom,
    Closure,
    Func,
    Int,
    String,
    Sym,
    Bool,
    List,
    Keyword,
    Vec,
    Map,
}

impl std::fmt::Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

use colored::Colorize;

#[derive(Error, Debug)]
pub enum QxErr {
    #[error("RecurErr: Missing Loop, Args: {0}")]
    Recur(Cons),

    #[error("Interrupted, Stop")]
    Stop,
    #[error("FatalErr: {0:#}")]
    Fatal(#[from] Rc<QxErr>),

    #[error(transparent)]
    Any(#[from] anyhow::Error),

    #[error("MismatchedParen {0}")]
    MismatchedParen(ParenType),

    #[error("MissingToken: {0}")]
    MissingToken(anyhow::Error),

    #[error("in {1}: ArgumentError, received: {}",
		.0.as_ref().map_or_else(|| "None".on_red().to_string(), ToString::to_string).red())
	]
    NoArgs(Option<Cons>, &'static str),

    #[error("TypeError, expected: {expected}, found: {found}")]
    TypeErr { expected: ExprType, found: ExprType },

    #[error("{0:#}")]
    LispErr(Expr),

    #[error("IntOpError")]
    IntOverflowErr,

    #[error("TypeConvertErr from {from} to {to}")]
    TypeConvErr { from: ExprType, to: ExprType },

    #[error("TypeConvertErr from {val} ({from}) to {to}")]
    TypeConvValErr {
        from: ExprType,
        val: Expr,
        to: ExprType,
    },

    #[error("TypeParseErr from {from} to {to}: {err}")]
    TypeParseErr {
        from: Expr,
        to: ExprType,
        err: anyhow::Error,
    },

    #[error("ShadowErr: tried to shadow `{0}`, use Atom instead!")]
    ShadowErr(EcoString),

    #[error("NoDefErr: `{0}` is not defined!")]
    NoDef(EcoString),

	#[error("MatchErr: `{0}` does not match `{1}`")]
	NoMatch(Expr, Expr)
}

#[derive(Clone, Eq, PartialEq, Default)]
pub struct Cons(pub Option<Rc<ConsCell>>);

#[derive(Clone, Eq, PartialEq, Default)]
pub struct ConsCell {
    pub car: Expr,
    pub cdr: Cons,
}

pub struct ConsExactPairIter {
    iter: ConsIter,
}

impl Iterator for ConsExactPairIter {
    type Item = [Expr; 2];

    fn next(&mut self) -> Option<Self::Item> {
        Some([self.iter.next()?, self.iter.next()?])
    }
}

#[derive(Clone)]
pub struct ConsIter {
    head: Cons,
}

impl ConsIter {
    #[must_use]
    pub const fn inner(&self) -> &Cons {
        &self.head
    }
}

impl ConsIter {
    pub fn rest(&mut self) -> Cons {
        Cons(self.head.0.take())
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.head.0.is_none()
    }
}

impl Cons {
    #[must_use]
    pub const fn nil() -> Self {
        Self(None)
    }

    #[must_use]
    pub fn cdar(&self) -> Option<Expr> {
        self.cdr().car()
    }

    pub fn iter(&self) -> ConsIter {
        self.into_iter()
    }

    pub fn collect<I: Iterator<Item = Expr> + DoubleEndedIterator>(iter: I) -> Self {
        let inner = iter.rev().fold(None, |acc, it| {
            Some(Rc::new(ConsCell {
                car: it,
                cdr: Self(acc),
            }))
        });

        Self(inner)
    }

    #[must_use]
    pub fn pair_iter(self) -> ConsExactPairIter {
        ConsExactPairIter {
            iter: self.into_iter(),
        }
    }

    #[must_use]
    pub fn concat(self, other: Self) -> Self {
        let mut result = other;
        let mut current = self.reversed();

        while let Some(cell) = current.0 {
            result = Self(Some(Rc::new(ConsCell {
                car: cell.car.clone(),
                cdr: result,
            })));
            current = cell.cdr.clone();
        }

        result
    }

    #[must_use]
    pub fn reversed(self) -> Self {
        let mut acc = Self(None);
        let mut current = self;

        while let Cons(Some(cell)) = current {
            acc = cons(cell.car.clone(), acc);
            current = cell.cdr.clone();
        }

        acc
    }

    #[must_use]
    pub fn car(&self) -> Option<Expr> {
        self.0.as_ref().map(|it| it.car.clone())
    }

    #[must_use]
    pub fn cdr_opt(&self) -> Option<Self> {
        self.0.as_ref().map(|it| it.cdr.clone())
    }

    #[must_use]
    pub fn nth(&self, n: usize) -> Option<Expr> {
        match n {
            0 => self.car(),
            _ => self.cdr().nth(n - 1),
        }
    }

    /// `cdr_opt` except it returns an empty Cons instead of an `Option::None`
    #[must_use]
    pub fn cdr(&self) -> Self {
        self.0
            .as_ref()
            .map_or_else(|| Self(None), |it| it.cdr.clone())
    }

    #[must_use]
    pub fn len_is_atleast(&self, n: usize) -> bool {
        self.into_iter().take(n).count() == n
    }

    #[must_use]
    pub fn len_is(&self, n: usize) -> bool {
        self.into_iter().take(n + 1).count() == n
    }

    #[inline]
    pub fn new(e: impl Into<Option<Expr>>) -> Self {
        e.into().map_or_else(Self::nil, |e| cons(e, Self::nil()))
    }
}

#[must_use]
pub fn cons(car: Expr, cdr: Cons) -> Cons {
    Cons(Some(Rc::new(ConsCell { car, cdr })))
}

impl<T: AsRef<[Expr]>> From<T> for Cons {
    fn from(list: T) -> Self {
        Self(list.as_ref().iter().rev().fold(None, |acc, it| {
            Some(Rc::new(ConsCell {
                car: it.clone(),
                cdr: Self(acc),
            }))
        }))
    }
}

impl Iterator for ConsIter {
    type Item = Expr;

    fn next(&mut self) -> Option<Self::Item> {
        let old_head = self.head.0.take();

        self.head = Cons(old_head.as_ref().and_then(|it| it.cdr.0.clone()));

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
        ConsIter { head: self.clone() }
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

    #[must_use]
    pub const fn get_type(&self) -> ExprType {
        match self {
            Self::Vec(_) => ExprType::Vec,
            Self::Int(_) => ExprType::Int,
            Self::String(_) => ExprType::String,
            Self::Sym(_) => ExprType::Sym,
            Self::Bool(_) => ExprType::Bool,
            Self::List(_) => ExprType::List,
            Self::Nil => ExprType::Nil,
            Self::Atom(_) => ExprType::Atom,
            Self::Closure(_) => ExprType::Closure,
            Self::Func(_) => ExprType::Func,
            Self::Map(_) | Self::MapLit(_) => ExprType::Map,
            Self::Keyword(_) => ExprType::Keyword,
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

pub struct Input(pub EcoString);

impl Input {
    pub fn get(ctx: &mut crate::Term) -> QxResult<Self> {
        Ok(Self(super::get_inp(ctx)?.into()))
    }

    #[must_use]
    pub fn tokenize(&self) -> super::stream::TokenStream {
        super::tokenize(&self.0)
    }
}

#[cfg(test)]
mod test;
