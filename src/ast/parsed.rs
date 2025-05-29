use std::collections::HashMap;

use miette::SourceSpan;

use super::tajp::Type;

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: SourceSpan,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: SourceSpan, kind: ExprKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
    pub span: SourceSpan,
}

impl Ident {
    pub fn new(name: impl Into<String>, span: SourceSpan) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(u128),
    Identifier(String),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    StructInstantiation {
        name: String,
        members: HashMap<Ident, Expr>,
    },
    MemberAccess {
        lhs: Box<Expr>,
        member: Ident,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block { stmts: Vec<Stmt> },
    VariableDeclaration { name: Ident, value: Expr },
    Return { value: Option<Expr> },
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub procs: Vec<ProcDefinition>,
}

#[derive(Debug, Clone)]
pub struct ProcDefinition {
    pub ident: Ident,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Type,
    pub stmts: Vec<Stmt>,
}
