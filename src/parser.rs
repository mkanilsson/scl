use std::{fs, path::Path};

use crate::{
    ast::parsed::{Expr, Stmt, TranslationUnit},
    error::{Error, Result},
    helpers,
    lexer::Lexer,
};

use lalrpop_util::lalrpop_mod;
use miette::NamedSource;

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

    pub fn parse_translation_unit(path: &Path) -> Result<TranslationUnit> {
        let source = fs::read_to_string(&path).unwrap();

        let lexer = Lexer::new(&source);
        let namned_source = NamedSource::new(helpers::relative_path(path), source.to_string());

        match grammar::TranslationUnitParser::new().parse(lexer) {
            Ok(mut unit) => {
                unit.source = namned_source;
                Ok(unit)
            }
            Err(error) => Err(match error {
                lalrpop_util::ParseError::InvalidToken { location } => todo!(),
                lalrpop_util::ParseError::UnrecognizedEof { location, expected } => todo!(),
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                    Error::UnexpectedToken {
                        src: namned_source,
                        span: (token.0..token.2).into(),
                        expected: helpers::string_join_with_or(
                            expected
                                .iter()
                                .map(|f| f.as_str())
                                .collect::<Vec<_>>()
                                .as_slice(),
                        ),
                    }
                }
                lalrpop_util::ParseError::ExtraToken { token } => todo!(),
                lalrpop_util::ParseError::User { error } => todo!(),
            }),
        }
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

#[macro_export]
macro_rules! tajp {
    ($s:ident, $e:ident, $kind:expr) => {
        tajp::Type::new(($s..$e).into(), $kind)
    };
}
