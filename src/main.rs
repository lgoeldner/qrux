#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]

use colored::Colorize;
use qrux::{print::pp_ast, read::QxErr, Runtime, Term};

fn main() {
    let exception_string = "Exception".on_red();
    let mut runtime = match Runtime::new(Term::new()) {
        (_, Err(QxErr::Stop)) => return,
        (_, Err(QxErr::Fatal(e))) => {
            eprintln!("Fatal Exception before REPL: {e:#}");
            return;
        }

        (r, Ok(_)) => r,
        (r, Err(e)) => {
            eprintln!("{exception_string}: {e}, continuing");
            r
        }
    };

    loop {
        match rep(&mut runtime) {
            Ok(()) => {}
            Err(QxErr::Stop) => return,
            Err(QxErr::Fatal(e)) => {
                eprintln!("Fatal Exception: {e:#}");
                return;
            }
            Err(e) => eprintln!("{exception_string}: {e:#}"),
        }
    }
}

fn rep(runtime: &mut Runtime) -> Result<(), QxErr> {
    let inp = runtime.read_from_stdin()?;
    let result = runtime.eval(inp, None)?;
    pp_ast(&result);

    Ok(())
}
