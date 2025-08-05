use crate::{
    ast::parsed::TranslationUnit,
    error::{Error, Result},
    helpers,
    lexer::Lexer,
};

#[cfg(test)]
use crate::ast::parsed::{Expr, Stmt};

use lalrpop_util::lalrpop_mod;
use miette::NamedSource;

lalrpop_mod! {
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    grammar
}

pub struct Parser;

impl Parser {
    #[cfg(test)]
    pub fn parse_expr<'input>(expr: &'input str) -> Box<Expr> {
        let lexer = Lexer::new(expr);
        grammar::ExprParser::new().parse(lexer).unwrap()
    }

    #[cfg(test)]
    pub fn parse_stmt(stmt: &str) -> Stmt {
        let lexer = Lexer::new(stmt);
        grammar::StmtParser::new().parse(lexer).unwrap()
    }

    pub fn parse_translation_unit(source: &NamedSource<String>) -> Result<TranslationUnit> {
        let lexer = Lexer::new(source.inner());

        match grammar::TranslationUnitParser::new().parse(lexer) {
            Ok(unit) => Ok(unit),
            Err(error) => Err(match error {
                lalrpop_util::ParseError::InvalidToken { location } => Error::InvalidToken {
                    src: source.clone(),
                    span: (location..location).into(),
                },
                lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                    Error::ExpectedOneOf {
                        src: source.clone(),
                        span: (location..location).into(),
                        expected: helpers::string_join_with_or(
                            &expected.iter().map(|s| s.as_str()).collect::<Vec<_>>(),
                        ),
                    }
                }
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                    Error::UnexpectedToken {
                        src: source.clone(),
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
                lalrpop_util::ParseError::ExtraToken { token } => Error::ExtraToken {
                    src: source.clone(),
                    span: (token.0..token.2).into(),
                },
                lalrpop_util::ParseError::User { .. } => todo!(),
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
