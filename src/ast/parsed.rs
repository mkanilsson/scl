use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expr {
    Number(u128),
    Identifier(String),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    StructInstantiation {
        name: String,
        members: HashMap<String, Expr>,
    },
    MemberAccess {
        lhs: Box<Expr>,
        member: String,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block { stmts: Vec<Stmt> },
    VariableDeclaration { name: String, value: Expr },
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}
