use strum::{Display, EnumIs};

use crate::{ast::parsed::BinOp, typechecker::implementations::InterfaceId};

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
    pub has_this: bool,
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
    None,
}

#[derive(Debug, Clone)]
pub struct CheckedExpr {
    pub type_id: TypeId,
    pub lvalue: bool,
    pub kind: CheckedExprKind,
}

#[derive(Debug, Clone, Display, EnumIs)]
pub enum CheckedExprKind {
    StackValue(StackSlotId),
    Identifier(String),
    Number(u64),
    String(String),
    Proc {
        proc_id: ProcId,
        lhs: Option<Box<CheckedExpr>>,
    },
    InterfaceProc {
        for_type_id: TypeId,
        interface_id: InterfaceId,
        proc_id: ProcId,
        lhs: Option<Box<CheckedExpr>>,
    },
    This,
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
    InterfaceCall {
        for_type_id: TypeId,
        proc_id: ProcId,
        interface_id: InterfaceId,
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
    ArrayInstantiation {
        exprs: Vec<CheckedExpr>,
        stack_slot: StackSlotId,
    },
    ArrayAccess {
        lhs: Box<CheckedExpr>,
        index: Box<CheckedExpr>,
    },
    SizeOf(TypeId),

    F64ToF32(Box<CheckedExpr>),
    F32ToF64(Box<CheckedExpr>),

    F32ToSigned(Box<CheckedExpr>),
    F32ToUnsigned(Box<CheckedExpr>),
    F64ToSigned(Box<CheckedExpr>),
    F64ToUnsigned(Box<CheckedExpr>),

    I32ToFloat(Box<CheckedExpr>),
    U32ToFloat(Box<CheckedExpr>),
    I64ToFloat(Box<CheckedExpr>),
    U64ToFloat(Box<CheckedExpr>),
    SignExtend8(Box<CheckedExpr>),
    ZeroExtend8(Box<CheckedExpr>),
    SignExtend16(Box<CheckedExpr>),
    ZeroExtend16(Box<CheckedExpr>),
    SignExtend32(Box<CheckedExpr>),
    ZeroExtend32(Box<CheckedExpr>),
}

#[derive(Debug, Clone)]
pub struct CheckedBlock {
    pub stmts: Vec<CheckedStmt>,
    pub last: Option<CheckedExpr>,
    pub type_id: TypeId,
    pub deferred: Vec<Vec<CheckedExpr>>,
}

pub enum CheckedBuiltin {
    LinkName(String),
}
