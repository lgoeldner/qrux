use qrux::{print::pp_ast, read::QxErr, Runtime, Term};

fn main() {
    let mut runtime = match Runtime::new(Term::new()) {
        (r, Ok(_)) => r,
        (_, Err(QxErr::Stop)) => return,
        (_, Err(QxErr::Fatal(e))) => {
            eprintln!("Fatal Exception before REPL: {e:#}");
            return;
        }
        (r, Err(e)) => {eprintln!("Exception before REPL: {e}"); r},
    };

    loop {
        match rep(&mut runtime) {
            Ok(_) => {}
            Err(QxErr::Stop) => return,
            Err(QxErr::Fatal(e)) => {
                eprintln!("Fatal Exception: {e:#}");
                return;
            }
            Err(e) => eprintln!("Exception: {e}"),
        }
    }
}

fn rep(runtime: &mut Runtime) -> Result<(), QxErr> {
    let inp = runtime.read_from_stdin()?;
    let result = runtime.eval(inp, None)?;
    pp_ast(&result);

    Ok(())
}
