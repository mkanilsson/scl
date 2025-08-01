use strum::Display;

use crate::ast::parsed::BinOp;

use super::{
    proc::ProcId,
    stack::{StackSlotId, StackSlots},
    tajp::TypeId,
};

#[derive(Debug, Clone)]
pub struct CheckedTranslationUnit {
    pub procs: Vec<CheckedProc>,
}

#[derive(Debug, Clone)]
pub struct CheckedProc {
    pub proc_id: ProcId,
    pub body: CheckedBlock,
    pub params: Vec<(String, StackSlotId)>,
    pub return_type: TypeId,
    pub stack_slots: StackSlots,
}

#[derive(Debug, Clone, Display)]
pub enum CheckedStmt {
    Return {
        value: Option<CheckedExpr>,
    },
    VariableDeclaration {
        stack_slot: StackSlotId,
        value: CheckedExpr,
    },
    Expr(CheckedExpr),
    While {
        condition: CheckedExpr,
        body: CheckedBlock,
    },
}

#[derive(Debug, Clone)]
pub struct CheckedExpr {
    pub type_id: TypeId,
    pub lvalue: bool,
    pub kind: CheckedExprKind,
}

#[derive(Debug, Clone, Display)]
pub enum CheckedExprKind {
    Identifier(String),
    StackValue(StackSlotId),
    Number(u64),
    String(String),
    BinOp {
        lhs: Box<CheckedExpr>,
        op: BinOp,
        rhs: Box<CheckedExpr>,
    },
    DirectCall {
        proc_id: ProcId,
        params: Vec<CheckedExpr>,
        variadic_after: Option<u64>,
        stack_slot: StackSlotId,
    },
    StructInstantiation {
        name: String,
        stack_slot: StackSlotId,
        fields: Vec<(String, CheckedExpr)>,
    },
    MemberAccess {
        lhs: Box<CheckedExpr>,
        name: String,
    },
    If {
        stack_slot: StackSlotId,
        condition: Box<CheckedExpr>,
        false_block: Box<CheckedBlock>,
        true_block: Box<CheckedBlock>,
    },
    Assignment {
        lhs: Box<CheckedExpr>,
        rhs: Box<CheckedExpr>,
    },
    AddressOf {
        expr: Box<CheckedExpr>,
    },
    Deref {
        type_id: TypeId,
        expr: Box<CheckedExpr>,
        stack_slot: StackSlotId,
    },
    Block(Box<CheckedBlock>),
    Store {
        expr: Box<CheckedExpr>,
        stack_slot: StackSlotId,
    },
}

#[derive(Debug, Clone)]
pub struct CheckedBlock {
    pub stmts: Vec<CheckedStmt>,
    pub last: Option<CheckedExpr>,
    pub type_id: TypeId,
}

pub enum CheckedBuiltin {
    LinkName(String),
}
