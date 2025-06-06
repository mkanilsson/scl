use std::{collections::HashMap, hash::Hash};

use miette::{NamedSource, SourceSpan};
use strum::Display;

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

#[derive(Debug, Clone)]
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

    pub fn from_string(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            span: (0..0).into(),
        }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Clone, Display)]
pub enum ExprKind {
    Number(String),
    Identifier(String),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    StructInstantiation {
        name: String,
        // TODO: Change to a vec
        members: HashMap<Ident, Expr>,
    },
    MemberAccess {
        lhs: Box<Expr>,
        member: Ident,
    },
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub span: SourceSpan,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: SourceSpan, kind: StmtKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Display)]
pub enum StmtKind {
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
    pub source: NamedSource<String>,
    pub procs: Vec<ProcDefinition>,
}

#[derive(Debug, Clone)]
pub struct ProcDefinition {
    pub ident: Ident,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Type,
    pub stmts: Vec<Stmt>,
}
