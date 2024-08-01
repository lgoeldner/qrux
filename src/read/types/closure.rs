use super::{kw::Keyword, Cons, Expr, QxErr, QxResult};
use crate::{env::Env, Runtime};
use args::Args;
use ecow::EcoString;
use tap::Pipe;

pub mod args;

/// TODO: Add Keyword Arguments, Finish this

#[derive(Debug)]
pub struct Closure {
    // pub args_name: Box<[EcoString]>,
    pub body: Expr,
    pub captured: Env,
    pub is_macro: bool,
    pub args: args::Args,
}

impl Clone for Closure {
    fn clone(&self) -> Self {
        unreachable!("Closures Should always live in an `Rc<_>`")
    }
}

impl Eq for Closure {}

impl PartialEq for Closure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Closure {
    pub fn new(args: Cons, body: Expr, captured: Env, is_macro: bool) -> QxResult<Self> {
        Self {
            body,
            captured,
            is_macro,
            args: Args::new(args)?,
        }
        .pipe(Ok)
    }

    pub fn create_env(&self, inp: Cons, r: &mut Runtime) -> QxResult<Env> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum IsBound {
            Yes,
            No,
        }

        let env = Env::with_outer(self.captured.clone());

        let mut bound_idents = self
            .args
            .args()
            .iter()
            .map(|it| (IsBound::No, it))
            .collect::<Vec<_>>();

        let insert_kw = |it: Keyword, to| {
            env.inner()
                .data
                .borrow_mut()
                .insert(it.inspect_inner(|it| EcoString::from(it)), to)
        };

        let insert = |it: EcoString, to| env.inner().data.borrow_mut().insert(it, to);

        let (kws, rest_args) = split_kw_args(inp)?;

        // load keyword args
        for (kw, val) in kws {
            bound_idents
                .iter_mut()
                .find(|(i, a)| *i == IsBound::No && a.kw() == kw)
                .ok_or_else(|| QxErr::Any(anyhow::anyhow!("Keyword {kw} is not required!")))?
                .0 = IsBound::Yes;

            insert_kw(kw, val);
        }

        // load remaining args
        let mut args = rest_args.iter();
        let mut rem_idents_iter = bound_idents
            .clone()
            .into_iter()
            .filter(|&(i, _)| i == IsBound::No)
            .map(|(_, a)| a);

		// load the "normal" arguments and varargs
		// if there are no more idents to bind to or there are varargs, `None` remaining args are returned.
        let remaining_args = loop {
            let Some(ident) = rem_idents_iter.next() else {
                break Some(args);
            };

            // load varargs
            if ident.is_vararg() {
                for a in rem_idents_iter.rev() {
                    let ex = args.next_back().unwrap();
                    insert(a.name(), ex.clone());
                }

                let rest = args.cloned().collect::<Cons>();
                insert(EcoString::from("rest"), dbg!(Expr::List(rest)));
                break None;
            }

            let Some(ex) = args.next() else {
                break None;
            };

            bound_idents
                .iter_mut()
                .find(|(i, a)| *i == IsBound::No && a.kw() == ident.kw())
                .ok_or_else(|| QxErr::Any(anyhow::anyhow!("{ident} is not required!")))?
                .0 = IsBound::Yes;

            insert(ident.name(), ex.clone());
        };

		// insert defaults
        for (k, v) in bound_idents
            .iter_mut()
            .filter(|(i, _)| *i == IsBound::No)
            .filter_map(|(i, a)| {
                a.default().as_ref().map(|default| {
                    *i = IsBound::Yes;
                    (a.name(), default)
                })
            })
        {
            insert(k, r.eval(v.clone(), Some(env.clone()))?);
        }

		// check for missing args
        if bound_idents
            .iter()
            .any(|(i, a)| !a.is_vararg() && *i == IsBound::No)
        {
            let unbound_args = bound_idents
                .iter()
                .filter(|&(i, a)| !a.is_vararg() && *i == IsBound::No)
                .map(|(_, a)| Expr::Sym(a.name()))
                .collect::<Cons>();

            return Err(QxErr::Any(anyhow::anyhow!(
                "ArgsErr: Missing args: {}",
                unbound_args
            )));
        }

		// check for leftover args
        if let Some(mut i) = remaining_args.map(Iterator::peekable) {
            if i.peek().is_some() {
                return Err(QxErr::Any(anyhow::anyhow!(
                    "ArgsErr: Too many args: {}",
                    i.cloned().collect::<Cons>()
                )));
            }
        }

        Ok(env)
    }
}

type KWArgs = Vec<(Keyword, Expr)>;

fn split_kw_args(args: Cons) -> QxResult<(KWArgs, Vec<Expr>)> {
    let mut args_iter = args.iter();
    let mut kws = vec![];
    let mut rest = vec![];

    loop {
        let Some(a) = args_iter.next() else {
            break;
        };

        if let Expr::Keyword(kw) = a {
            let Some(val) = args_iter.next() else {
                return Err(QxErr::NoArgs(Some(args), "creating keyword args"));
            };

            kws.push((kw, val));
        } else {
            rest.push(a);
        }
    }

    Ok((kws, rest))
}
