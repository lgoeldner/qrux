use crate::{
    env::Env,
    read::{Expr, QxErr},
    Runtime,
};

use super::Shadow;

impl Runtime {
    pub fn pat_match(&mut self, pat: &Expr, val: &Expr, env: Env) -> Result<Env, QxErr> {
        let val = self.eval(val.clone(), Some(env.clone()))?;

        match (pat, &val) {
            (Expr::Keyword(kw), Expr::Keyword(kw2)) if kw == kw2 => Ok(env),
            (Expr::List(l), Expr::List(l2)) => {
                let mut env = env;
                let (mut i1, mut i2) = (l.into_iter(), l2.into_iter());

                while let (Some(pat), Some(val)) = (i1.next(), i2.next()) {
                    env = self.pat_match(&pat, &val, env)?;
                }

                if i1.next().is_some() || i2.next().is_some() {
                    Err(QxErr::NoMatch(format!("{pat} does not match {val}").into()))
                } else {
                    Ok(env)
                }
            }
            (Expr::Sym(s), expr) => {
                env.set(s.clone(), expr.clone(), Shadow::No)?;
                Ok(env)
            }
            (Expr::String(s), Expr::String(s2)) if s == s2 => Ok(env),
            (Expr::Vec(v), Expr::Vec(v2)) => {
                let mut env = env;

                let (mut i1, mut i2) = (v.into_iter(), v2.into_iter());

                while let (Some(pat), Some(val)) = (i1.next(), i2.next()) {
                    env = self.pat_match(pat, val, env)?;
                }

                if i1.next().is_some() || i2.next().is_some() {
                    Err(QxErr::NoMatch(format!("{pat} does not match {val}").into()))
                } else {
                    Ok(env)
                }
            }
            (Expr::Int(i), Expr::Int(i2)) if i == i2 => Ok(env),

            _ => Err(QxErr::NoMatch(format!("{pat} does not match {val}").into())),
        }
    }
}
