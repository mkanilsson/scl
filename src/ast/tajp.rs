use miette::SourceSpan;

use crate::ast::parsed::Ident;

#[derive(Debug, Clone)]
pub struct Type {
    pub span: SourceSpan,
    pub kind: TypeKind,
}

impl Type {
    pub fn new(span: SourceSpan, kind: TypeKind) -> Type {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Named(Ident),
}
