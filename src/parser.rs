use std::collections::HashMap;

use miette::{NamedSource, SourceSpan};

use crate::ast::parsed::{Expr, ExprKind, Ident, Stmt};
use crate::error::{Error, Result};
use crate::helpers;
use crate::lexer::Lexer;
use crate::pratt::Pratt;
use crate::token::{Token, TokenKind};

#[repr(u8)]
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum BindingPower {
    Default = 0,
    Comma,
    Assignment,
    Logical,
    Relational,
    Additive,
    Multiplicative,
    Unary,
    Call,
    Member,
    Primary,
}

pub struct Parser {
    file_name: String,
    content: String,
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            file_name: lexer.file_name.clone(),
            content: lexer.content.clone(),
            tokens: lexer.collect(),
            i: 0,
        }
    }

    fn named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.file_name.clone(), self.content.clone())
    }

    fn span_from_first_and_last(first: SourceSpan, last: SourceSpan) -> SourceSpan {
        (first.offset()..last.offset() + last.len()).into()
    }

    fn new_expr(first: SourceSpan, last: SourceSpan, kind: ExprKind) -> Expr {
        Expr::new(Self::span_from_first_and_last(first, last), kind)
    }

    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.i)
    }

    pub fn parse(&mut self) -> Result<Stmt> {
        let mut stmts = vec![];

        while self.current().is_some() {
            stmts.push(self.parse_stmt()?);
        }

        Ok(Stmt::Block { stmts })
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let Some(stmt_fn) = self.peek().stmt_handler() else {
            todo!("Good error reporting for token not being a stmt beginner token thingy");
        };

        let stmt = stmt_fn(self)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(stmt)
    }

    pub fn parse_primary(&mut self) -> Result<Expr> {
        let token = self.next();
        println!("Parse primary {token:#?}");
        let kind = match token.kind {
            TokenKind::Number(n) => ExprKind::Number(n),
            TokenKind::Identifier(name) => ExprKind::Identifier(name),
            TokenKind::OpenParen => {
                let expr = self.parse_expr(BindingPower::Default)?;
                let last = self.expect(TokenKind::CloseParen)?;
                return Ok(Self::new_expr(token.span, last.span, expr.kind));
            }
            _ => unreachable!(),
        };

        Ok(Self::new_expr(token.span, token.span, kind))
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::Let)?;
        let ident = self.expect_ident()?;

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr(BindingPower::Logical)?;

        Ok(Stmt::VariableDeclaration {
            name: ident,
            value: expr,
        })
    }

    pub fn parse_binary_expr(&mut self, lhs: Expr, bp: BindingPower) -> Result<Expr> {
        let op = self.next();
        println!("Parse binop {op:#?}");
        let rhs = self.parse_expr(bp)?;

        Ok(Self::new_expr(lhs.span, rhs.span, ExprKind::BinOp {
            lhs: Box::new(lhs),
            op: op.binop().unwrap(),
            rhs: Box::new(rhs),
        }))
    }
    pub fn parse_expr(&mut self, bp: BindingPower) -> Result<Expr> {
        let current = self.peek();

        let Some(nud_fn) = current.nud_handler() else {
            panic!(
                "Expected one of {} but got {:#?}",
                helpers::string_join_with_or(TokenKind::nud_names().as_slice()),
                current.kind,
            );
        };

        let mut lhs = nud_fn(self)?;

        loop {
            // EOF
            let Some(current) = self.current().cloned() else {
                break;
            };

            // Not an operator
            let Some(led_handler) = current.led_handler() else {
                break;
            };

            if current.binding_power().unwrap() <= bp {
                break;
            }

            lhs = led_handler(self, lhs, current.binding_power().unwrap())?;
        }

        Ok(lhs)
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token> {
        let token = self.next();
        println!("{token:#?}");

        if std::mem::discriminant(&token.kind) != std::mem::discriminant(&expected) {
            return Err(Error::UnexpectedToken {
                src: self.named_source(),
                span: token.span,
                expected: expected.name().to_string(),
            });
        }

        Ok(token)
    }

    fn expect_ident(&mut self) -> Result<Ident> {
        let token = self.next();
        match token.kind {
            TokenKind::Identifier(ident) => Ok(Ident::new(ident, token.span)),
            _ => Err(Error::ExpectedButGot {
                src: self.named_source(),
                span: token.span,
                expected: "identifier".to_string(),
                got: match token.kind {
                    TokenKind::Number(n) => n.to_string(),
                    kind => kind.name().to_string(),
                },
            }),
        }
    }

    fn next(&mut self) -> Token {
        let token = self.peek().clone();
        self.i += 1;
        token
    }

    fn peek(&mut self) -> &Token {
        self.current().expect("Handle unexpected EOF")
    }

    pub fn parse_struct_instantation(&mut self, lhs: Expr, bp: BindingPower) -> Result<Expr> {
        let ident = match lhs.kind {
            ExprKind::Identifier(ident) => ident,
            _ => {
                return Err(Error::IdentBeforeStructInstantation {
                    src: self.named_source(),
                    span: lhs.span,
                });
            }
        };

        let mut members = HashMap::new();

        self.expect(TokenKind::OpenCurly)?;

        loop {
            if self.peek().kind == TokenKind::CloseCurly {
                break;
            }

            let value = self.parse_struct_instantation_value()?;
            members.insert(value.0, value.1);

            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.expect(TokenKind::Comma)?;
        }

        let last = self.expect(TokenKind::CloseCurly)?;

        Ok(Self::new_expr(
            lhs.span,
            last.span,
            ExprKind::StructInstantiation {
                name: ident,
                members,
            },
        ))
    }

    fn parse_struct_instantation_value(&mut self) -> Result<(Ident, Expr)> {
        let name = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let expr = self.parse_expr(BindingPower::Logical)?;
        Ok((name, expr))
    }

    pub fn parse_member_expr(&mut self, lhs: Expr, _: BindingPower) -> Result<Expr> {
        self.expect(TokenKind::Dot)?;
        let ident = self.expect_ident()?;

        Ok(Self::new_expr(
            lhs.span,
            ident.span,
            ExprKind::MemberAccess {
                lhs: Box::new(lhs),
                member: ident,
            },
        ))
    }
}
