use std::hash::Hash;

use miette::SourceSpan;

use crate::ast::parsed::Ident;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub span: SourceSpan,
    pub kind: TypeKind,
}

impl Type {
    pub fn new(span: SourceSpan, kind: TypeKind) -> Type {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Named(Ident),
    Never,
}

impl ToString for TypeKind {
    fn to_string(&self) -> String {
        match self {
            TypeKind::Named(ident) => ident.name.to_string(),
            TypeKind::Never => "!".to_string(),
        }
    }
}
