use std::collections::HashMap;

use miette::{NamedSource, SourceSpan};

use crate::{
    ast::parsed::Ident,
    error::{Error, Result},
};

use super::tajp::TypeId;

#[derive(Debug)]
pub struct Scope {
    scope: Vec<HashMap<Ident, TypeId>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            scope: vec![HashMap::new()],
        }
    }

    pub fn add_to_scope(&mut self, ident: &Ident, type_id: TypeId) {
        let len = self.scope.len();
        self.scope[len - 1].insert(ident.clone(), type_id);
    }

    pub fn enter(&mut self) {
        self.scope.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.scope.pop().unwrap();
    }

    pub fn find(&self, ident: &Ident) -> Option<TypeId> {
        self.find_with_original_span(ident).map(|v| v.0)
    }

    pub fn force_find(&self, source: &NamedSource<String>, ident: &Ident) -> Result<TypeId> {
        self.find_with_original_span(ident)
            .map(|v| v.0)
            .ok_or(Error::UnknownIdent {
                src: source.clone(),
                span: ident.span,
                ident: ident.name.clone(),
            })
    }

    pub fn find_with_original_span(&self, ident: &Ident) -> Option<(TypeId, SourceSpan)> {
        for scope in self.scope.iter().rev() {
            if scope.contains_key(ident) {
                let kv = scope.get_key_value(ident).unwrap();
                return Some((*kv.1, kv.0.span));
            }
        }

        None
    }
}
