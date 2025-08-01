use std::collections::HashMap;
use std::fmt::Display;

use miette::{NamedSource, SourceSpan};

use crate::ast::parsed::Ident;
use crate::error::{Error, Result};

use super::Checker;
use super::{module::ModuleId, tajp::TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProcId(pub usize);

impl From<usize> for ProcId {
    fn from(value: usize) -> Self {
        ProcId(value)
    }
}

impl Display for ProcId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

    pub fn add_generic(
        &mut self,
        original: ProcId,
        type_id: TypeId,
        generics: Vec<TypeId>,
    ) -> ProcId {
        let id = self.procs.len();

        let generic = &self.procs[original.0];
        self.procs.push(Proc {
            generic: false,
            external: false,
            module_id: generic.module_id,
            type_id,
            name: generic.name.clone(),
            generic_instances: generics,
            link_name: None,
        });

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

    pub fn for_scope(&self, module_id: ModuleId) -> Vec<(Ident, TypeId, ProcId)> {
        let Some(module) = self.parsed.get(&module_id) else {
            return vec![];
        };

        module
            .iter()
            .map(|(k, v)| (k.clone(), self.procs[v.0].type_id, *v))
            .collect()
    }

    pub fn find_original_span(&self, module_id: ModuleId, ident: &Ident) -> Option<SourceSpan> {
        let module = self.parsed.get(&module_id)?;
        Some(module.get_key_value(ident)?.0.span)
    }

    pub fn mangled_name_of(&self, proc_id: ProcId, checker: &Checker) -> String {
        let proc = &self.procs[proc_id.0];

        if let Some(link_name) = &proc.link_name {
            return link_name.clone();
        }

        if proc.external {
            proc.name.name.clone()
        } else {
            if proc.name.name == "main" {
                return "main".to_string();
            }

            let base = format!(
                "{}.{}",
                checker.modules.mangled_name_of(proc.module_id),
                proc.name.name
            );

            if !proc.generic_instances.is_empty() {
                let types = proc
                    .generic_instances
                    .iter()
                    .map(|t| checker.types.mangled_name_of(*t, checker))
                    .collect::<Vec<_>>()
                    .join("..");

                format!("{base}..{}..{types}", proc.generic_instances.len())
            } else {
                base
            }
        }
    }

    pub fn is_generic(&self, proc_id: ProcId) -> bool {
        self.procs[proc_id.0].generic
    }

    pub fn module_for(&self, proc_id: ProcId) -> ModuleId {
        self.procs[proc_id.0].module_id
    }
}

#[derive(Debug)]
pub struct Proc {
    pub name: Ident,
    pub type_id: TypeId,
    pub module_id: ModuleId,
    pub external: bool,
    pub generic: bool,
    pub generic_instances: Vec<TypeId>,
    pub link_name: Option<String>,
}
