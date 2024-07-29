use super::{Cons, Expr, Keyword, QxErr, QxResult};
use anyhow::anyhow;
use ecow::EcoString;
use std::fmt::Display;
use tap::Pipe;

pub struct Args {
    /// sorted by `[Arg].kw` to do a binary search for lookup
    args: Box<[Arg]>,
}

pub struct Arg {
    is_vararg: bool,
    name: EcoString,
    kw: Keyword,
    default: Option<Expr>,
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.default {
            Some(default) => write!(f, "({} {} {})", self.kw, self.name, default),
            None => write!(f, "{} {}", self.kw, self.name),
        }
    }
}

impl TryFrom<Expr> for Arg {
    type Error = QxErr;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match &value {
        Expr::List(ref cons) if cons.len_is(2) => 'err: {
				let Some(Expr::Sym(sym)) = cons.car() else {
					break 'err None;
				};

				let Some(default) = cons.cdr().car() else {
					break 'err None;
				};

				Self {
					is_vararg: sym == "&rest",
					default: Some(default),
					kw: Keyword::new(sym.clone()),
					name: sym,
				}.pipe(Some)
			},

			Expr::Sym(name) => {
				Self {
					is_vararg: name == "&rest",
					default: None,
					kw: Keyword::new(name.clone()),
					name: name.clone(),
				}.pipe(Some)
			}
        _ => None,
    }.ok_or_else(|| {
			QxErr::Any(anyhow!("ArgErr: Cannot create Closure Argument with {value}.\nValid Forms: (<sym> <default expr>) | <sym>"))
		})
    }
}

impl Args {
    pub fn new(args: Cons) -> QxResult<Self> {
        let mut is_vararg = false;

        let mut proto = args
            .into_iter()
            .map(Arg::try_from)
            .map(|it| {
				// if it is a &rest, there must only be one
                it.and_then(|it| {
                    if it.is_vararg {
                        if is_vararg {
                            Err(QxErr::Any(anyhow!("ArgsErr: Multiple varargs.")))?;
                        } else {
                            is_vararg = true;
                        }
                    }
                    Ok(it)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        proto.sort_by_key(|it| it.kw);

        Self { args: proto.into() }.pipe(Ok)
    }
}

impl Display for Args {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Args (")?;

        if let Some(first) = self.args.first() {
            write!(f, "{first}")?;
        }

        for arg in self.args.iter().skip(1) {
            write!(f, " {arg}")?;
        }

        write!(f, ")")
    }
}
