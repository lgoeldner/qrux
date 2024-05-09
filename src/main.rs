use qrux::{
    eval::eval,
    print::pp_ast,
    read::{read, QxErr},
};

fn main() {
    loop {
        match rep() {
            Ok(_) => {}
            Err(QxErr::Stop | QxErr::Fatal(_)) => break,
            Err(e) => eprintln!("Exception: {e}"),
        }
    }
}

fn rep() -> Result<(), QxErr> {
    let ast = read()?;
    let result = eval(ast)?;
    println!("{result:?}");
    pp_ast(&result);

    Ok(())
}
