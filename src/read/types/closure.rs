use super::{kw::Keyword, Cons, Expr, QxErr, QxResult};
use crate::env::Env;
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

    pub fn create_env(&self, inp: Cons) -> QxResult<Env> {
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

        let (kws, rest) = split_kw_args(inp)?;

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
        let mut rest_iter = rest.iter();
        let mut rem_idents_iter = bound_idents
            .clone()
            .into_iter()
            .filter(|&(i, _)| i == IsBound::No)
            .map(|(_, a)| a);

        loop {
            let Some(ident) = rem_idents_iter.next() else {
                break;
            };

            // load varargs
            if ident.is_vararg() {
                for a in rem_idents_iter.rev() {
                    let ex = rest_iter.next_back().unwrap();
                    insert(a.name(), ex.clone());
                }

                let rest = rest_iter.cloned().collect::<Cons>();
                insert(EcoString::from("rest"), Expr::List(rest));

                break;
            }

            let Some(ex) = rest_iter.next() else {
                break;
            };

            bound_idents
                .iter_mut()
                .find(|(i, a)| *i == IsBound::No && a.kw() == ident.kw())
                .ok_or_else(|| QxErr::Any(anyhow::anyhow!("{ident} is not required!")))?
                .0 = IsBound::Yes;

            insert(ident.name(), ex.clone());
        }

        // load the remaining default args
        for (ident, default) in bound_idents
            .iter()
            .filter(|(i, _)| *i == IsBound::No)
            .filter_map(|(_, a)| a.default().as_ref().map(|default| (a.name(), default)))
        {
            insert(ident, default.clone());
        }

        Ok(env)
    }

    // pub fn create_env(&self, args: Cons) -> Result<Env, QxErr> {
    //     // match self.load_args(args.clone()) {
    //     //     Ok(e) => {
    //     //         dbg!(e.data());
    //     //     }
    //     //     Err(e) => {
    //     //         dbg!(e);
    //     //     }
    //     // }

    //     self.load_args(args)

    //     // let env = Env::with_outer(self.captured.clone());
    //     // let has_varargs = matches!(self.args_name.last(), Some(s) if s.starts_with('&'));

    //     // let mut names = self.args_name.iter().peekable();
    //     // let mut args = args;

    //     // let insert_env = |it, to| env.inner().data.borrow_mut().insert(it, to);

    //     // while let Some(name) = names.next() {
    //     //     if names.peek().is_none() && has_varargs {
    //     //         insert_env(name[1..].into(), Expr::List(args));

    //     //         break;
    //     //     }

    //     //     insert_env(
    //     //         name.clone(),
    //     //         match args.car() {
    //     //             Some(arg) => arg,
    //     //             None => return Err(QxErr::NoArgs(Some(args), "create env")),
    //     //         },
    //     //     );

    //     //     args = match args.cdr_opt() {
    //     //         Some(args) => args,
    //     //         None => return Err(QxErr::NoArgs(None, "create env")),
    //     //     };
    //     // }

    //     // Ok(env)
    // }
}

fn split_kw_args(args: Cons) -> QxResult<(Vec<(Keyword, Expr)>, Vec<Expr>)> {
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
