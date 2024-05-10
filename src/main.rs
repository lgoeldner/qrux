use qrux::{eval, print::pp_ast, read::QxErr, Repl, Runtime};

fn main() {
    let mut runtime = Runtime::new(Repl::new());

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
    let result = runtime.eval(inp)?;

    pp_ast(&result);

    Ok(())
}
