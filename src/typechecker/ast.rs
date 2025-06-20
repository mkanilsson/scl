use strum::Display;

use crate::ast::parsed::BinOp;

use super::tajp::TypeId;

#[derive(Debug, Clone)]
pub struct CheckedTranslationUnit {
    pub procs: Vec<CheckedProc>,
}

#[derive(Debug, Clone)]
pub struct CheckedProc {
    pub name: String,
    pub type_id: TypeId,
    pub body: CheckedBlock,
    pub params: Vec<(String, TypeId)>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, Display)]
pub enum CheckedStmt {
    Return { value: Option<CheckedExpr> },
    VariableDeclaration { name: String, value: CheckedExpr },
    Expr(CheckedExpr),
}

#[derive(Debug, Clone)]
pub struct CheckedExpr {
    pub type_id: TypeId,
    pub kind: CheckedExprKind,
}

#[derive(Debug, Clone, Display)]
pub enum CheckedExprKind {
    Identifier(String),
    Number(u64),
    String(String),
    BinOp {
        lhs: Box<CheckedExpr>,
        op: BinOp,
        rhs: Box<CheckedExpr>,
    },
    DirectCall {
        name: String,
        params: Vec<CheckedExpr>,
        variadic_after: Option<u64>,
    },
    StructInstantiation {
        name: String,
        fields: Vec<(String, CheckedExpr)>,
    },
    MemberAccess {
        lhs: Box<CheckedExpr>,
        name: String,
    },
    If {
        condition: Box<CheckedExpr>,
        false_block: Box<CheckedBlock>,
        true_block: Box<CheckedBlock>,
    },
}

#[derive(Debug, Clone)]
pub struct CheckedBlock {
    pub stmts: Vec<CheckedStmt>,
    pub last: Option<CheckedExpr>,
}
