use std::collections::HashMap;

use miette::{NamedSource, SourceSpan};

use crate::{
    ast::parsed::Ident,
    error::{Error, Result},
};

use super::{Checker, module::ModuleId, proc::ProcId, stack::StackSlotId, tajp::TypeId};

#[derive(Debug, Clone, Copy)]
pub struct ScopeData {
    pub type_id: TypeId,
    pub stack_slot: Option<StackSlotId>,
    pub proc_id: Option<ProcId>,
}

#[derive(Debug)]
pub struct Scope {
    scope: Vec<HashMap<Ident, ScopeData>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            scope: vec![HashMap::new()],
        }
    }

    pub fn add_to_scope(
        &mut self,
        ident: &Ident,
        type_id: TypeId,
        stack_slot: Option<StackSlotId>,
        proc_id: Option<ProcId>,
    ) {
        let len = self.scope.len();
        self.scope[len - 1].insert(ident.clone(), ScopeData {
            type_id,
            stack_slot,
            proc_id,
        });
    }

    pub fn enter(&mut self) {
        self.scope.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.scope.pop().unwrap();
    }

    pub fn find(&self, ident: &Ident, module_id: ModuleId, checker: &Checker) -> Option<ScopeData> {
        self.find_with_original_span(ident, module_id, checker)
            .map(|v| v.0)
    }

    pub fn force_find(
        &self,
        source: &NamedSource<String>,
        ident: &Ident,
        module_id: ModuleId,
        checker: &Checker,
    ) -> Result<ScopeData> {
        self.find_with_original_span(ident, module_id, checker)
            .map(|v| v.0)
            .ok_or(Error::UnknownIdent {
                src: source.clone(),
                span: ident.span,
                ident: ident.name.clone(),
            })
    }

    pub fn force_find_from_string(
        &self,
        source: &NamedSource<String>,
        ident: &str,
        module_id: ModuleId,
        checker: &Checker,
    ) -> Result<ScopeData> {
        self.force_find(
            source,
            &Ident::new(ident.to_string(), (0..0).into()),
            module_id,
            checker,
        )
    }

    pub fn find_with_original_span(
        &self,
        ident: &Ident,
        module_id: ModuleId,
        checker: &Checker,
    ) -> Option<(ScopeData, SourceSpan)> {
        for scope in self.scope.iter().rev() {
            if scope.contains_key(ident) {
                let kv = scope.get_key_value(ident).unwrap();
                return Some((*kv.1, kv.0.span));
            }
        }

        for proc in checker.procs.for_scope(module_id) {
            if proc.0 == *ident {
                return Some((
                    ScopeData {
                        proc_id: Some(proc.2),
                        stack_slot: None,
                        type_id: proc.1,
                    },
                    proc.0.span,
                ));
            }
        }

        None
    }
}
