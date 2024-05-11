use qrux::{eval, print::pp_ast, read::QxErr, Runtime, Term};

fn main() {
    let mut runtime = Runtime::new(Term::new());

    loop {
        match rep(&mut runtime) {
            Ok(_) => {}
            Err(QxErr::Stop | QxErr::Fatal(_)) => break,
            Err(e) => eprintln!("Exception: {e}"),
        }
    }
}

fn rep(runtime: &mut Runtime) -> Result<(), QxErr> {
    let inp = runtime.read_from_stdin()?;
    let result = runtime.eval_mult(inp)?;
    pp_ast(&result);

    Ok(())
}
