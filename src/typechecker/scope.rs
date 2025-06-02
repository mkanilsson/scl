use std::collections::HashMap;

use crate::ast::parsed::Ident;

use super::tajp::TypeId;

pub struct Scope {
    scope: Vec<HashMap<Ident, TypeId>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            scope: vec![HashMap::new()],
        }
    }

    pub fn add_to_scope(&mut self, ident: Ident, type_id: TypeId) {
        let len = self.scope.len();
        self.scope[len - 1].insert(ident, type_id);
    }

    pub fn enter_scope(&mut self) -> ScopeGuard<'_> {
        self.scope.push(HashMap::new());
        ScopeGuard { scope: self }
    }

    pub fn find(&self, ident: &Ident) -> Option<TypeId> {
        for scope in self.scope.iter().rev() {
            if scope.contains_key(ident) {
                return Some(scope[ident]);
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
