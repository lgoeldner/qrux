use crate::{read::{QxErr, TokenType, AST}, Runtime};


impl Runtime {
    pub fn eval(&mut self, ast: AST) -> anyhow::Result<AST> {
        Ok(ast.into_iter().map(|it| self.eval_expr(it)).collect::<Result<_, _>>()?)
        
    }

    fn _eval() {

    }

    pub fn eval_ast(&mut self, ast: TokenType) -> Result<TokenType, QxErr> {

        match ast {
            _ => todo!()
        }
    }
}