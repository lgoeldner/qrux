use ecow::EcoString;
use tap::Pipe;

use super::{Expr, ExprType, QxErr, QxResult};

impl Expr {
    pub fn to_strt(&self, to: ExprType) -> QxResult<EcoString> {
        match self {
            Expr::Bool(_) | Expr::Nil => self.to_string().pipe(EcoString::from).pipe(Ok),
            Expr::Sym(s) | Expr::String(s) => s.clone().pipe(Ok),
            Expr::Atom(a) => a.borrow().to_strt(to),
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

            _ => Err(QxErr::TypeConvErr {
                from: self.get_type(),
                to: ExprType::Int,
            }),
        }
    }
}
