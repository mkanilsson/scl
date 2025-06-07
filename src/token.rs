use miette::SourceSpan;
use strum::Display;

use crate::ast::parsed::{BinOp, Expr, Stmt};
use crate::error::Result;
use crate::parser::{BindingPower, Parser};
use crate::pratt::Pratt;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

impl Token {
    pub fn binop(&self) -> Option<BinOp> {
        self.kind.binop()
    }
}

impl Pratt for Token {
    fn is_nud(&self) -> bool {
        self.kind.is_nud()
    }

    fn nud_handler(&self) -> Option<fn(&mut Parser) -> Result<Expr>> {
        self.kind.nud_handler()
    }

    fn is_led(&self) -> bool {
        self.kind.is_led()
    }

    fn led_handler(&self) -> Option<fn(&mut Parser, lhs: Expr, bp: BindingPower) -> Result<Expr>> {
        self.kind.led_handler()
    }

    fn stmt_handler(&self) -> Option<fn(&mut Parser) -> Result<Stmt>> {
        self.kind.stmt_handler()
    }

    fn binding_power(&self) -> Option<BindingPower> {
        self.kind.binding_power()
    }

    fn nud_names() -> Vec<&'static str> {
        TokenKind::nud_names()
    }

    fn name(&self) -> &'static str {
        self.kind.name()
    }
}

impl Token {
    pub fn new(span: SourceSpan, kind: TokenKind) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Pratt, Display)]
pub enum TokenKind {
    #[name("number")]
    #[nud(Parser::parse_primary)]
    Number(String),

    #[nud(Parser::parse_primary)]
    #[name("identifier")]
    Identifier(String),

    #[nud(Parser::parse_primary)]
    #[name("string")]
    String(String),

    #[nud(Parser::parse_primary)]
    #[led(BindingPower::Call, Parser::parse_call)]
    #[name("(")]
    OpenParen,
    #[name(")")]
    CloseParen,

    #[stmt(Parser::parse_variable_declaration)]
    Let,

    #[name("+")]
    #[led(BindingPower::Additive, Parser::parse_binary_expr)]
    Plus,

    #[name("-")]
    #[led(BindingPower::Additive, Parser::parse_binary_expr)]
    Minus,

    #[name("*")]
    #[led(BindingPower::Multiplicative, Parser::parse_binary_expr)]
    Star,
    #[name("/")]
    #[led(BindingPower::Multiplicative, Parser::parse_binary_expr)]
    Slash,

    #[name("=")]
    Equal,
    #[name(";")]
    Semicolon,

    #[name("{")]
    #[led(BindingPower::Multiplicative, Parser::parse_struct_instantation)]
    OpenCurly,
    #[name("}")]
    CloseCurly,

    #[name(":")]
    Colon,
    #[name(",")]
    Comma,

    #[name(".")]
    #[led(BindingPower::Member, Parser::parse_member_expr)]
    Dot,

    #[name("...")]
    DotDotDot,

    #[stmt(Parser::parse_return)]
    Ret,

    Proc,
    Extern,
}

impl TokenKind {
    pub fn binop(&self) -> Option<BinOp> {
        Some(match self {
            TokenKind::Plus => BinOp::Add,
            TokenKind::Minus => BinOp::Subtract,
            TokenKind::Star => BinOp::Multiply,
            TokenKind::Slash => BinOp::Divide,
            _ => return None,
        })
    }
}
