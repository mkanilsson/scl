use std::collections::HashMap;

use miette::SourceSpan;

use crate::ast::parsed::Ident;

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

    pub fn enter_scope(&mut self) -> ScopeGuard<'_> {
        self.scope.push(HashMap::new());
        ScopeGuard { scope: self }
    }

    pub fn find(&self, ident: &Ident) -> Option<TypeId> {
        self.find_with_original_span(ident).map(|v| v.0)
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

pub struct ScopeGuard<'a> {
    scope: &'a mut Scope,
}

impl Drop for ScopeGuard<'_> {
    fn drop(&mut self) {
        self.scope.scope.pop();
    }
}
