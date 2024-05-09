use std::convert::Infallible;

use qrux::{eval::eval, print::pp_ast, read::read};

fn main() -> anyhow::Result<Infallible> {
    loop {
        match rep() {
            Ok(_) => {}
            Err(e) => eprintln!("Exception: {e}"),
        }
    }
}

fn rep() -> Result<(), anyhow::Error> {
    let ast = read()?;
    let result = eval(ast)?;
    println!("{result:?}");
    pp_ast(&result);

    Ok(())
}
