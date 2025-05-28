use crate::ast::parsed::{BinOp, Expr, Stmt};
use crate::parser::{BindingPower, Parser};
use crate::pratt::Pratt;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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

    fn nud_handler(&self) -> Option<fn(&mut Parser) -> Expr> {
        self.kind.nud_handler()
    }

    fn is_led(&self) -> bool {
        self.kind.is_led()
    }

    fn led_handler(&self) -> Option<fn(&mut Parser, lhs: Expr, bp: BindingPower) -> Expr> {
        self.kind.led_handler()
    }

    fn stmt_handler(&self) -> Option<fn(&mut Parser) -> crate::ast::parsed::Stmt> {
        self.kind.stmt_handler()
    }

    fn binding_power(&self) -> Option<BindingPower> {
        self.kind.binding_power()
    }

    fn nud_names() -> Vec<&'static str> {
        TokenKind::nud_names()
    }
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Pratt)]
pub enum TokenKind {
    #[nud(Parser::parse_primary)]
    Number(u128),

    #[nud(Parser::parse_primary)]
    Identifier(String),

    #[stmt(Parser::parse_variable_declaration)]
    Let,

    #[led(BindingPower::Additive, Parser::parse_binary_expr)]
    Plus,
    #[led(BindingPower::Additive, Parser::parse_binary_expr)]
    Minus,

    #[led(BindingPower::Multiplicative, Parser::parse_binary_expr)]
    Star,
    #[led(BindingPower::Multiplicative, Parser::parse_binary_expr)]
    Slash,

    Equal,
    Semicolon,

    #[led(BindingPower::Multiplicative, Parser::parse_struct_instantation)]
    OpenCurly,
    CloseCurly,

    Colon,
    Comma,

    #[led(BindingPower::Member, Parser::parse_member_expr)]
    Dot,
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
