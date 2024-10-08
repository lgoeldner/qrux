use ecow::EcoString;
use tap::Pipe;

use super::{kw::{self, Keyword}, Atom, Cons, Expr, ExprType, QxErr, QxResult};

impl Expr {
    pub fn as_map(&self) -> QxResult<im::HashMap<Keyword, Expr>> {
        match self {
            Expr::Atom(a) => a.borrow().as_map(),
            Expr::Map(v) => Ok(v.clone()),
            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to: ExprType::Map,
            }),
        }
    }
    
    pub fn as_vec(&self) -> QxResult<im::Vector<Expr>> {
        match self {
            Expr::Atom(a) => a.borrow().as_vec(),
            Expr::Vec(v) => Ok(v.clone()),
            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to: ExprType::Vec,
            }),
        }
    }
    
    pub fn to_atom(&self) -> QxResult<Atom> {
        match self {
            Expr::Atom(a) => Ok(a.clone()),
            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to: ExprType::Atom,
            }),
        }
    }

    pub fn to_list(&self) -> QxResult<Cons> {
        match self {
            Expr::List(l) => Ok(l.clone()),
            Expr::Atom(l) => l.borrow().to_list(),

            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to: ExprType::List,
            }),
        }
    }

    pub fn as_kw(&self) -> QxResult<kw::Keyword> {
        match self {
            Expr::Keyword(k) => Ok(*k),
            Expr::Atom(a) => a.borrow().as_kw(),
            Expr::String(s) | Expr::Sym(s) => Ok(kw::Keyword::new(s.clone())),
            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to: ExprType::Keyword,
            }),
        }
    }

    pub fn to_bool(&self) -> QxResult<bool> {
        match self {
            Expr::Bool(b) => Ok(*b),
            Expr::Nil => Ok(false),
            Expr::Int(i) => Ok(*i != 0),
            Expr::Atom(a) => a.borrow().to_bool(),
            Expr::Sym(s) | Expr::String(s) => s.parse::<bool>().map_or_else(
                |_| {
                    Err(QxErr::TypeConvValErr {
                        from: self.get_type(),
                        val: self.clone(),
                        to: ExprType::Bool,
                    })
                },
                Ok,
            ),
            Expr::List(l) => Ok(l.0.is_some()),

            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to: ExprType::Bool,
            }),
        }
    }

    pub fn as_strt(&self, to: ExprType) -> QxResult<EcoString> {
        match self {
            Expr::Sym(s) | Expr::String(s) => s.clone().pipe(Ok),
            Expr::Atom(a) => a.borrow().as_strt(to),
            Expr::Keyword(k) => k.inspect_inner(|it| EcoString::from(it)).pipe(Ok),

            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                val: self.clone(),
                to,
            }),
        }
    }

    pub fn to_int(&self) -> QxResult<i64> {
        match self {
            Expr::Atom(a) => a.borrow().to_int(),
            Expr::String(ref s) => s.parse::<i64>().map_or_else(
                |it| {
                    Err(QxErr::TypeParseErr {
                        from: self.clone(),
                        to: ExprType::Int,
                        err: it.into(),
                    })
                },
                Ok,
            ),
            Expr::Int(i) => Ok(*i),
            Expr::Bool(b) => Ok(i64::from(*b)),
            Expr::Nil => Ok(0),

            _ => Err(QxErr::TypeConvValErr {
                from: self.get_type(),
                to: ExprType::Int,
                val: self.clone(),
            }),
        }
    }
}
