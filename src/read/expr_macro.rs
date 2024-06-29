pub use super::Expr;

#[macro_export]
macro_rules! expr {
    ($it:ident) => {
        $crate::Expr::Sym(stringify!($it).into())
    };

    (str $it:expr) => {
        $crate::Expr::String($it.into())
    };

    (int $it:literal) => {
        $crate::Expr::Int($it)
    };

    (sym $it:expr) => {
        $crate::Expr::Sym($it.into())
    };

    (list $($it:expr),*) => {
        $crate::Expr::List(::std::rc::Rc::new([$($it), *]))
    };

    (cons $($it:expr),*) => {
        Expr::Cons($crate::read::types::Cons::from(&[$($it), *]))
    };

    (nil) => {
        $crate::Expr::Nil
    };

    (bool $it:literal) => {
        $crate::Expr::Bool($it)
    };

    (atom $it:expr) => {
        $crate::Expr::Atom(::std::rc::Rc::new(::std::cell::RefCell::new($it)))
    };
}
