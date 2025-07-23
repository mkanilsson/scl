use crate::{
    ast::parsed::{Expr, Stmt},
    new_lexer::Lexer,
};
use std::{fs, path::Path};

use lalrpop_util::lalrpop_mod;

lalrpop_mod! {
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
}

pub struct Parser;

impl Parser {
    pub fn parse_expr(expr: &str) -> Box<Expr> {
        let lexer = Lexer::new(&expr);
        grammar::ExprParser::new().parse(lexer).unwrap()
    }

    pub fn parse_stmt(stmt: &str) -> Stmt {
        let lexer = Lexer::new(&stmt);
        grammar::StmtParser::new().parse(lexer).unwrap()
    }
}

#[macro_export]
macro_rules! expr {
    ($s:ident, $e:ident, $kind:expr) => {
        Box::new(parsed::Expr::new(($s..$e).into(), $kind))
    };
}

#[macro_export]
macro_rules! stmt {
    ($s:ident, $e:ident, $kind:expr) => {
        parsed::Stmt::new(($s..$e).into(), $kind)
    };
}
