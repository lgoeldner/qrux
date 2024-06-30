use std::cell::RefCell;
use std::num::TryFromIntError;
use std::ops;
use std::rc::Rc;
use std::time::SystemTime;

use anyhow::{anyhow, Context};

use crate::lazy::Lazy;
use crate::read::Expr;
use crate::{expr, read, Func, QxErr};

use super::Env;

pub fn core_map(sym: &str) -> Option<Expr> {
    Default::default()
}

fn int_ops(ident: &str) -> Option<Expr> {
    match ident {
		

		"+" => Func::new_expr(|_, _, _| todo!()),

        _ => None?,
    }
    .into()
}
