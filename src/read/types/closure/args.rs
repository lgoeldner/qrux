use super::{Cons, Expr, Keyword, QxErr, QxResult};
use anyhow::anyhow;
use ecow::{EcoString, EcoVec};
use std::fmt::{Debug, Display};
use tap::Pipe;

#[derive(Debug, Clone)]
pub struct Args {
    args: EcoVec<Arg>,
}

#[derive(Debug, Clone)]
pub struct Arg {
    is_vararg: bool,
    name: EcoString,
    kw: Keyword,
    default: Option<Expr>,
}

impl Arg {
    pub const fn is_vararg(&self) -> bool {
        self.is_vararg
    }

    pub fn name(&self) -> EcoString {
        self.name.clone()
    }

    pub const fn kw(&self) -> Keyword {
        self.kw
    }

    pub const fn default(&self) -> &Option<Expr> {
        &self.default
    }
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.default {
            Some(_) => write!(f, "{}?", &self.name),
            None => write!(f, "{}", &self.name),
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
    pub const fn args(&self) -> &EcoVec<Arg> {
        &self.args
    }

    pub fn new(args: Cons) -> QxResult<Self> {
        let mut has_vararg = false;

        let proto = args
            .into_iter()
            .map(Arg::try_from)
            .map(|it| {
                // if it is a &rest, there must only be one
                it.and_then(|it| {
                    if it.is_vararg {
                        if has_vararg {
                            Err(QxErr::Any(anyhow!("ArgsErr: Multiple varargs.")))?;
                        } else {
                            has_vararg = true;
                        }
                    }
                    Ok(it)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Self { args: proto.into() }.pipe(Ok)
    }
}

impl Display for Args {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "Args (")?;

        if let Some(first) = self.args.first() {
            write!(f, "{first}")?;
        }

        for arg in self.args.iter().skip(1) {
            write!(f, " {arg}")?;
        }

        // write!(f, ")")
        Ok(())
    }
}
