use crate::{
    expr,
    read::{Cons, Expr},
};

#[test]
fn test_cdr_car() {
    let c = Cons::from(&[expr!(sym "test"), expr!(nil)]);

    assert_eq!(c.car(), Some(expr!(sym "test")));
    assert_eq!(c.cdr(), Cons::new(expr!(nil)));
    assert_eq!(c.cdr().car(), Some(expr!(nil)));
    assert_eq!(c.cdr().cdr().car(), None);
}
