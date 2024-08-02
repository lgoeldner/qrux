use colored::Colorize;
use qrux::{print::pp_ast, read::QxErr, Runtime, Term};

fn main() -> Result<(), ()> {
    let exception = "Exception".red().on_black();
    let mut runtime = match Runtime::new(Term::new()) {
        (_, Err(QxErr::Stop)) => return Ok(()),
        (_, Err(QxErr::Fatal(e))) => {
            let fatal_exception = "Fatal Exception".on_red();
            eprintln!("{fatal_exception} before REPL: {e:#}");
            return Err(());
        }

        (r, Ok(_)) => r,
        (r, Err(e)) => {
            eprintln!("{exception}: {e}, continuing");
            r
        }
    };

    loop {
        match rep(&mut runtime) {
            Ok(()) => {}
            Err(QxErr::Stop) => return Ok(()),
            Err(QxErr::Fatal(e)) => {
                eprintln!("Fatal Exception: {e:#}");
                return Err(());
            }
            Err(e) => eprintln!("{exception}: {e:#}"),
        }
    }
}

fn rep(runtime: &mut Runtime) -> Result<(), QxErr> {
    let inp = runtime.read_from_stdin()?;
    let result = runtime.eval(inp, None, None)?;
    pp_ast(&result);

    Ok(())
}
