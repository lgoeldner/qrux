//! Directly starts a REPL.
//! Specify a file as the first argument to run it, and open a REPL.
//! Use (bye) to exit
use colored::Colorize;
use qrux::{print::pp_ast, read::QxErr, Runtime};

fn main() -> Result<(), ()> {
    // presave red Exception label
    let exception = "Exception".red().on_black();

    // create a new runtime. for the first argument,
    let mut runtime = match Runtime::new() {
        // all good
        (r, Ok(_)) => r,
        // all good and exit without REPL
        (_, Err(QxErr::Stop)) => return Ok(()),

        // fatal exception in user file, exit
        (_, Err(QxErr::Fatal(e))) => {
            let fatal_exception = "Fatal Exception".on_red();
            eprintln!("{fatal_exception} before REPL: {e:#}");
            return Err(());
        }

        // nonfatal exception in user file, continue to REPL
        (r, Err(e)) => {
            eprintln!("{exception}: {e}, continuing");
            r
        }
    };

    loop {
        // read, evaluate and print
        match rep(&mut runtime) {
            Ok(()) => {}
            // exit normally
            Err(QxErr::Stop) => return Ok(()),

            // handle exceptions
            Err(QxErr::Fatal(e)) => {
                eprintln!("Fatal Exception: {e:#}");
                return Err(());
            }
            Err(e) => eprintln!("{exception}: {e:#}"),
        }
    }
}

/// One repetition of the REPL
fn rep(runtime: &mut Runtime) -> Result<(), QxErr> {
    // read an expression from the terminal
    let inp = runtime.read_from_stdin()?;
    // evaluate it
    let result = runtime.eval(inp, None)?;
    // and pretty-print the result
    pp_ast(&result);

    Ok(())
}
