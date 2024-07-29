pub use super::Expr;

#[macro_export]
macro_rules! expr {


    (str $it:expr) => {
        $crate::Expr::String($it.into())
    };

    (int $it:literal) => {
        $crate::Expr::Int($it)
    };

    (sym $it:expr) => {
        $crate::Expr::Sym($it.into())
    };

    (cons $($it:expr),*) => {
        $crate::Expr::List($crate::read::types::Cons::from(&[$($it), *]))
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

	(kw $it:expr)  => {
		$crate::Expr::Keyword($crate::read::types::kw::Keyword::new($it.into()))
	};

	($it:ident) => {
        $crate::Expr::Sym(stringify!($it).into())
    };
}

#[macro_export]
macro_rules! special_form {
	($args:ident, $form:literal; [ $($p:pat),* ] $(..$rest:pat)? => $e:expr) => {
		{
			let mut _args = $args;

			$(
				let Some($p) = _args.next() else {
					return ControlFlow::Break(Err(QxErr::Any(anyhow!(concat!("Correct Form: ", $form)))))
				};
			)*

			$( let $rest = _args.rest(); )?

			$e
		}
	};
}
