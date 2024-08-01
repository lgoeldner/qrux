use ecow::EcoString;
use tap::Pipe;

use super::{Cons, Expr, ExprType, QxErr, QxResult};

impl Expr {
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
                        to: ExprType::Bool,
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
