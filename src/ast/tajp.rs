use std::{fmt::Display, hash::Hash};

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

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            TypeKind::Named(ident) => &ident.name,
            TypeKind::Never => "!",
        };

        write!(f, "{text}")
    }
}
