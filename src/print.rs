use std::fmt::Debug;

use crate::read::{Expr, AST};
use colored::Colorize;

pub fn pp_ast(ast: &AST) {
    println!(
        "{}",
        ast.iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
    );
}
