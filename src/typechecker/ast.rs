use super::tajp::TypeId;

#[derive(Debug, Clone)]
pub struct CheckedTranslationUnit {
    pub procs: Vec<CheckedProc>,
}

#[derive(Debug, Clone)]
pub struct CheckedProc {
    pub type_id: TypeId,
    pub stmts: Vec<CheckedStmt>,
}

#[derive(Debug, Clone)]
pub enum CheckedStmt {
    Return { value: Option<CheckedExpr> },
}

#[derive(Debug, Clone)]
pub struct CheckedExpr {
    pub type_id: TypeId,
    pub kind: CheckedExprKind,
}

#[derive(Debug, Clone)]
pub enum CheckedExprKind {
    Identifier(String),
}
