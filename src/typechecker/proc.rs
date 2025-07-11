use std::collections::HashMap;

use miette::NamedSource;

use crate::ast::parsed::Ident;
use crate::error::{Error, Result};

use super::{module::ModuleId, tajp::TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProcId(pub usize);

impl From<usize> for ProcId {
    fn from(value: usize) -> Self {
        ProcId(value)
    }
}

#[derive(Debug)]
pub struct ProcCollection {
    pub procs: Vec<Proc>,
    pub parsed: HashMap<ModuleId, HashMap<Ident, ProcId>>,
}

impl ProcCollection {
    pub fn new() -> Self {
        Self {
            parsed: HashMap::new(),
            procs: vec![],
        }
    }

    pub fn add(&mut self, module_id: ModuleId, proc: Proc) -> ProcId {
        let id = self.procs.len();
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(proc.name.clone(), id.into());
        self.procs.push(proc);
        id.into()
    }

    pub fn add_to_module(&mut self, module_id: ModuleId, proc_id: ProcId, ident: &Ident) {
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(ident.clone(), proc_id);
    }

    pub fn force_find(
        &self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        ident: &Ident,
    ) -> Result<ProcId> {
        if let Some(found) = self.find(module_id, ident) {
            Ok(found)
        } else {
            Err(Error::UnknownProc {
                src: src.clone(),
                span: ident.span,
                proc_name: ident.name.clone(),
            })
        }
    }

    pub fn force_find_type_of(
        &self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        ident: &Ident,
    ) -> Result<TypeId> {
        let proc_id = self.force_find(src, module_id, ident)?;
        Ok(self.procs[proc_id.0].type_id)
    }

    pub fn find(&self, module_id: ModuleId, ident: &Ident) -> Option<ProcId> {
        self.parsed.get(&module_id)?.get(ident).copied()
    }

    pub fn for_scope(&self, module_id: ModuleId) -> Vec<(Ident, TypeId)> {
        let Some(module) = self.parsed.get(&module_id) else {
            return vec![];
        };

        module
            .iter()
            .map(|(k, v)| (k.clone(), self.procs[v.0].type_id))
            .collect()
    }
}

#[derive(Debug)]
pub struct Proc {
    pub name: Ident,
    pub type_id: TypeId,
}
