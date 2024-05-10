#![warn(clippy::pedantic, clippy::nursery)]

use std::{collections::HashMap, path::PathBuf, rc::Rc};

use read::{QxErr, TokenType};
use reedline::{DefaultPrompt, FileBackedHistory, Reedline};

type Func = Rc<dyn Fn(&mut Runtime, &[TokenType]) -> Result<TokenType, read::QxErr>>;
pub struct Runtime {
    repl: Repl,
    env: HashMap<String, Func>,
}

impl Runtime {
    #[must_use]
    pub fn new(repl: Repl) -> Self {
        Self {
            repl,
            env: Self::default_env_map(),
        }
    }

    fn default_env_map() -> HashMap<String, Func> {
        // let add: Func = Box::new(|ctx, args: &[TokenType]| {
        //     if let [TokenType::Int(lhs), TokenType::Int(rhs), ..] = args {
        //         Ok(TokenType::Int(lhs + rhs))
        //     } else {
        //         Err(QxErr::NoArgs(Some(args.to_vec())))
        //     }
        // });

        macro_rules! int_op_apply2 {
            ($($op:tt)+) => {
                [
                    $((
                        stringify!($op).to_string(),
                        Rc::new((|_ctx, args: &[TokenType]| {
                            if let [TokenType::Int(lhs), TokenType::Int(rhs), ..] = args {
                                Ok(TokenType::Int(lhs $op rhs))
                            } else {
                                Err(QxErr::NoArgs(Some(args.to_vec())))
                            }
                        }) ),
                    ),)+
                ]
            };
        }

        let list: &[(String, Func)] = &int_op_apply2!(+ - * / %);

        let mut map = HashMap::new();

        for (k, v) in list {
            map.insert(k.to_string(), v.clone());
        }

        map
    }

    pub fn read_from_stdin(&mut self) -> Result<read::AST, read::QxErr> {
        read::read(self)
    }

    #[inline]
    pub(crate) fn repl(&mut self) -> &mut Repl {
        &mut self.repl
    }
}

pub struct Repl {
    prompt: DefaultPrompt,
    reedline: Reedline,
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

impl Repl {
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn new() -> Self {
        let history = Box::new(
            FileBackedHistory::with_file(50, PathBuf::from("~/.qrux/history.txt")).unwrap(),
        );

        Self {
            prompt: DefaultPrompt::default(),
            reedline: Reedline::create().with_history(history),
        }
    }
}

pub mod eval;
pub mod lazy;
pub mod print;
pub mod read;

use colored::Colorize;
impl std::fmt::Display for TokenType {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_list(
            f: &mut std::fmt::Formatter,
            list: &[TokenType],
        ) -> Result<(), std::fmt::Error> {
            write!(f, "(")?;

            list.iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(" ")
                .fmt(f)?;

            write!(f, ")")
        }

        match self {
            Self::Int(int) => int.to_string().cyan().fmt(f),
            Self::Sym(sym) => sym.to_string().red().fmt(f),
            Self::String(string) => format!(r#""{string}""#).bright_green().fmt(f),
            Self::List(list) => write_list(f, list),
            Self::Nil => "nil".bold().blue().fmt(f),
        }
    }
}
