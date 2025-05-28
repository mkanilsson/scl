use std::collections::HashMap;
use std::iter::Peekable;

use crate::ast::parsed::{Expr, Stmt};
use crate::helpers;
use crate::lexer::{self, Lexer};
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
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            tokens: lexer.collect(),
            i: 0,
        };

        parser
    }

    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.i)
    }

    pub fn parse(&mut self) -> Stmt {
        let mut stmts = vec![];

        while self.current().is_some() {
            stmts.push(self.parse_stmt());
        }

        Stmt::Block { stmts }
    }

    fn parse_stmt(&mut self) -> Stmt {
        let Some(stmt_fn) = self.peek().stmt_handler() else {
            todo!("Good error reporting for token not being a stmt beginner token thingy");
        };

        let stmt = stmt_fn(self);
        self.expect(TokenKind::Semicolon);
        stmt
    }

    pub fn parse_primary(&mut self) -> Expr {
        let token = self.next();
        println!("Parse primary {token:#?}");
        match token.kind {
            TokenKind::Number(n) => Expr::Number(n),
            TokenKind::Identifier(name) => Expr::Identifier(name),
            _ => unreachable!(),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Stmt {
        self.expect(TokenKind::Let);
        let ident = self.expect_ident();

        self.expect(TokenKind::Equal);
        let expr = self.parse_expr(BindingPower::Logical);

        Stmt::VariableDeclaration {
            name: ident,
            value: expr,
        }
    }

    pub fn parse_binary_expr(&mut self, lhs: Expr, bp: BindingPower) -> Expr {
        let op = self.next();
        println!("Parse binop {op:#?}");
        let rhs = self.parse_expr(bp);
        return Expr::BinOp {
            lhs: Box::new(lhs),
            op: op.binop().unwrap(),
            rhs: Box::new(rhs),
        };
    }
    pub fn parse_expr(&mut self, bp: BindingPower) -> Expr {
        let current = self.peek();

        let Some(nud_fn) = current.nud_handler() else {
            panic!(
                "Expected one of {} but got {:#?}",
                helpers::string_join_with_or(TokenKind::nud_names().as_slice()),
                current.kind,
            );
        };

        let mut lhs = nud_fn(self);

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

            lhs = led_handler(self, lhs, current.binding_power().unwrap());
        }

        lhs
    }

    fn expect(&mut self, expected: TokenKind) {
        let token = self.next();

        if std::mem::discriminant(&token.kind) != std::mem::discriminant(&expected) {
            let got = match token.kind {
                TokenKind::Number(n) => n.to_string(),
                TokenKind::Identifier(ident) => ident.to_string(),
                _ => token.kind.name().to_string(),
            };

            panic!("Expected '{}' but got '{}'", expected.name(), got);
        }
    }

    fn expect_ident(&mut self) -> String {
        let token = self.next();
        match token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => panic!("Expected identifier but got '{}'", match token.kind {
                TokenKind::Number(n) => n.to_string(),
                kind => kind.name().to_string(),
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

    pub fn parse_struct_instantation(&mut self, lhs: Expr, bp: BindingPower) -> Expr {
        let ident = match lhs {
            Expr::Identifier(ident) => ident,
            _ => panic!("Expected struct name in struct instantation"),
        };

        let mut members = HashMap::new();

        self.expect(TokenKind::OpenCurly);

        loop {
            if self.peek().kind == TokenKind::CloseCurly {
                break;
            }

            let value = self.parse_struct_instantation_value();
            members.insert(value.0, value.1);

            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.expect(TokenKind::Comma);
        }

        self.expect(TokenKind::CloseCurly);

        Expr::StructInstantiation {
            name: ident,
            members,
        }
    }

    fn parse_struct_instantation_value(&mut self) -> (String, Expr) {
        let name = self.expect_ident();
        self.expect(TokenKind::Colon);
        let expr = self.parse_expr(BindingPower::Logical);
        (name, expr)
    }

    pub fn parse_member_expr(&mut self, lhs: Expr, _: BindingPower) -> Expr {
        self.expect(TokenKind::Dot);
        let ident = self.expect_ident();

        Expr::MemberAccess {
            lhs: Box::new(lhs),
            member: ident,
        }
    }
}
