use std::collections::HashMap;

use miette::{NamedSource, SourceSpan};

use crate::ast::parsed::{
    Expr, ExprKind, ExternProcDefinition, Ident, ProcDefinition, Stmt, StmtKind, TranslationUnit,
};
use crate::ast::tajp::{Type, TypeKind};
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

    fn new_stmt(first: SourceSpan, last: SourceSpan, kind: StmtKind) -> Stmt {
        Stmt::new(Self::span_from_first_and_last(first, last), kind)
    }

    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.i)
    }

    pub fn parse(self) -> Result<TranslationUnit> {
        self.parse_translation_unit()
    }

    fn parse_translation_unit(mut self) -> Result<TranslationUnit> {
        let mut procs = vec![];
        let mut extern_procs = vec![];

        while let Some(c) = self.current() {
            match &c.kind {
                TokenKind::Proc => procs.push(self.parse_proc_definition()?),
                TokenKind::Extern => extern_procs.push(self.parse_extern_proc_definition()?),
                _ => {
                    return Err(Error::ExpectedProcStructExtern {
                        src: self.named_source(),
                        span: c.span,
                    });
                }
            }
        }

        Ok(TranslationUnit {
            procs,
            extern_procs,
            source: NamedSource::new(self.file_name, self.content),
        })
    }

    fn parse_proc_definition(&mut self) -> Result<ProcDefinition> {
        self.expect(TokenKind::Proc)?;
        let ident = self.expect_ident()?;
        self.expect(TokenKind::OpenParen)?;
        let mut params = vec![];

        while let Some(c) = self.current() {
            if c.kind == TokenKind::CloseParen {
                break;
            }

            params.push(self.parse_proc_definition_param()?);

            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.expect(TokenKind::Comma)?;
        }

        self.expect(TokenKind::CloseParen)?;

        let return_type = self.parse_type()?;
        let stmts = self.parse_block()?;

        Ok(ProcDefinition {
            ident,
            params,
            return_type,
            stmts,
        })
    }

    fn parse_extern_proc_definition(&mut self) -> Result<ExternProcDefinition> {
        self.expect(TokenKind::Extern)?;
        self.expect(TokenKind::Proc)?;
        let ident = self.expect_ident()?;
        self.expect(TokenKind::OpenParen)?;
        let mut params = vec![];

        let mut variadic = false;
        while let Some(c) = self.current() {
            if c.kind == TokenKind::CloseParen {
                break;
            }

            if self.peek().kind == TokenKind::DotDotDot {
                self.next();
                variadic = true;
                if self.peek().kind == TokenKind::Comma {
                    self.expect(TokenKind::Comma)?;
                }
                break;
            }

            params.push(self.parse_type()?);

            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.expect(TokenKind::Comma)?;
        }

        self.expect(TokenKind::CloseParen)?;

        let return_type = self.parse_type()?;

        Ok(ExternProcDefinition {
            ident,
            params,
            return_type,
            variadic,
        })
    }

    fn parse_proc_definition_param(&mut self) -> Result<(Ident, Type)> {
        let ident = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let t = self.parse_type()?;

        Ok((ident, t))
    }

    fn parse_type(&mut self) -> Result<Type> {
        let ident = self.expect_ident()?;

        Ok(Type::new(ident.span, TypeKind::Named(ident)))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>> {
        self.expect(TokenKind::OpenCurly)?;

        let mut stmts = vec![];

        while let Some(c) = self.current() {
            if c.kind == TokenKind::CloseCurly {
                break;
            }

            stmts.push(self.parse_stmt()?);
        }

        self.expect(TokenKind::CloseCurly)?;

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let stmt = if let Some(stmt_fn) = self.peek().stmt_handler() {
            stmt_fn(self)?
        } else {
            let expr = self.parse_expr(BindingPower::Default)?;
            println!("{:#?}", expr);
            Stmt::new(expr.span, StmtKind::Expr(expr))
        };

        self.expect(TokenKind::Semicolon)?;
        Ok(stmt)
    }

    pub fn parse_primary(&mut self) -> Result<Expr> {
        let token = self.next();
        let kind = match token.kind {
            TokenKind::Number(n) => ExprKind::Number(n),
            TokenKind::Identifier(name) => ExprKind::Identifier(name),
            TokenKind::String(value) => ExprKind::String(value),
            TokenKind::True => ExprKind::Bool(true),
            TokenKind::False => ExprKind::Bool(false),
            TokenKind::OpenParen => {
                let expr = self.parse_expr(BindingPower::Default)?;
                let last = self.expect(TokenKind::CloseParen)?;
                return Ok(Self::new_expr(token.span, last.span, expr.kind));
            }
            TokenKind::Builtin(name) => {
                self.expect(TokenKind::OpenParen)?;
                let params =
                    self.parse_commma_separated_exprs(TokenKind::OpenParen, BindingPower::Logical)?;
                let last = self.expect(TokenKind::CloseParen)?;

                return Ok(Self::new_expr(
                    token.span,
                    last.span,
                    ExprKind::Builtin(name, params),
                ));
            }
            _ => unreachable!(),
        };

        Ok(Self::new_expr(token.span, token.span, kind))
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Stmt> {
        let start = self.expect(TokenKind::Let)?;
        let ident = self.expect_ident()?;

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr(BindingPower::Logical)?;

        Ok(Self::new_stmt(
            start.span,
            expr.span,
            StmtKind::VariableDeclaration {
                name: ident,
                value: expr,
            },
        ))
    }

    pub fn parse_return(&mut self) -> Result<Stmt> {
        let start = self.expect(TokenKind::Ret)?;
        let expr = match self.peek().nud_handler() {
            Some(_) => Some(self.parse_expr(BindingPower::Default)?),
            None => None,
        };

        let last = if let Some(expr) = &expr {
            expr.span
        } else {
            start.span
        };

        Ok(Self::new_stmt(
            start.span,
            last,
            StmtKind::Return { value: expr },
        ))
    }

    pub fn parse_binary_expr(&mut self, lhs: Expr, bp: BindingPower) -> Result<Expr> {
        let op = self.next();
        let rhs = self.parse_expr(bp)?;

        Ok(Self::new_expr(
            lhs.span,
            rhs.span,
            ExprKind::BinOp {
                lhs: Box::new(lhs),
                op: op.binop().unwrap(),
                rhs: Box::new(rhs),
            },
        ))
    }

    pub fn parse_call(&mut self, lhs: Expr, bp: BindingPower) -> Result<Expr> {
        self.expect(TokenKind::OpenParen)?;

        let params =
            self.parse_commma_separated_exprs(TokenKind::CloseParen, BindingPower::Logical)?;
        let last = self.expect(TokenKind::CloseParen)?;

        Ok(Self::new_expr(
            lhs.span,
            last.span,
            ExprKind::Call {
                expr: Box::new(lhs),
                params,
            },
        ))
    }

    fn parse_commma_separated_exprs(
        &mut self,
        wrapper: TokenKind,
        min_bp: BindingPower,
    ) -> Result<Vec<Expr>> {
        self.parse_commma_separated(wrapper, |parser| parser.parse_expr(min_bp))
    }

    fn parse_commma_separated<T, F>(&mut self, wrapper: TokenKind, mut func: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut values = vec![];

        loop {
            if self.peek().kind == wrapper {
                break;
            }

            values.push(func(self)?);

            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.expect(TokenKind::Comma)?;
        }

        Ok(values)
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

        self.expect(TokenKind::OpenCurly)?;

        let members = self.parse_commma_separated(TokenKind::CloseCurly, |parser| {
            parser.parse_struct_instantation_value()
        })?;

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
