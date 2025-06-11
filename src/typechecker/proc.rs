use std::collections::HashMap;

use crate::ast::parsed::Ident;

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
    parsed: HashMap<ModuleId, HashMap<Ident, ProcId>>,
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
        let parsed_for_module = self.parsed.entry(module_id).or_insert_with(HashMap::new);
        parsed_for_module.insert(proc.name.clone(), id.into());
        self.procs.push(proc);
        id.into()
    }
}

#[derive(Debug)]
pub struct Proc {
    pub name: Ident,
    pub type_id: TypeId,
}
