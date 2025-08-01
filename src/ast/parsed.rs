use std::{hash::Hash, rc::Rc};

use miette::SourceSpan;
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
    Identifier(Ident),
    String(String),
    Bool(bool),
    Builtin(Builtin),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    StructInstantiation {
        name: Ident,
        members: Vec<(Ident, Expr)>,
    },
    MemberAccess {
        lhs: Box<Expr>,
        member: Ident,
    },
    Cast {
        lhs: Box<Expr>,
        tajp: Type,
    },
    Assignment {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        expr: Box<Expr>,
        params: Vec<Expr>,
        generic_params: Vec<Type>,
    },
    If {
        condition: Box<Expr>,
        true_block: Box<Block>,
        false_block: Box<Block>,
    },
    AddressOf(Box<Expr>),
    Deref(Box<Expr>),
    Block(Box<Block>),
}

#[derive(Debug, Clone)]
pub struct Builtin {
    pub name: String,
    pub params: Vec<Expr>,
    pub span: SourceSpan,
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
    VariableDeclaration { name: Ident, value: Expr },
    Return { value: Option<Expr> },
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, Display)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub procs: Vec<Rc<ProcDefinition>>,
    pub extern_procs: Vec<ExternProcDefinition>,
    pub structs: Vec<StructDefinition>,
    pub imports: Vec<Import>,
}

#[derive(Debug, Clone)]
pub enum Import {
    Part(Box<Import>, Ident),
    Final(Ident),
}

#[derive(Debug, Clone)]
pub struct ProcDefinition {
    pub ident: Ident,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Type,
    pub body: Block,
    pub type_params: Vec<Ident>,
}

#[derive(Debug, Clone)]
pub struct ExternProcDefinition {
    pub ident: Ident,
    pub params: Vec<Type>,
    pub return_type: Type,
    pub variadic: bool,
    pub type_params: Vec<Ident>,
    pub attributes: Vec<Builtin>,
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub ident: Ident,
    pub fields: Vec<(Ident, Type)>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub last: Option<Expr>,
    pub span: SourceSpan,
}
