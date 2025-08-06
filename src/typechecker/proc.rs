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
    pub impls: HashMap<TypeId, HashMap<Ident, ProcId>>,
}

impl ProcCollection {
    pub fn new() -> Self {
        Self {
            parsed: HashMap::new(),
            procs: vec![],
            impls: HashMap::new(),
        }
    }

    fn generate_proc_id(&mut self, proc: Proc) -> ProcId {
        let id = self.procs.len();
        self.procs.push(proc);
        id.into()
    }

    pub fn add(&mut self, module_id: ModuleId, proc: Proc) -> ProcId {
        let name = proc.name.clone();
        let id = self.generate_proc_id(proc);
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(name, id);
        id
    }

    pub fn add_impl(&mut self, for_type_id: TypeId, proc: Proc) -> ProcId {
        let name = proc.name.clone();
        let id = self.generate_proc_id(proc);
        let impl_for_type_id = self.impls.entry(for_type_id).or_default();
        impl_for_type_id.insert(name, id);
        id
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
            impl_for: None,
        });

        id.into()
    }

    pub fn add_to_module(&mut self, module_id: ModuleId, proc_id: ProcId, ident: &Ident) {
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(ident.clone(), proc_id);
    }

    pub fn force_find_for_module(
        &self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        ident: &Ident,
    ) -> Result<ProcId> {
        if let Some(found) = self.find_for_module(module_id, ident) {
            Ok(found)
        } else {
            Err(Error::UnknownProc {
                src: src.clone(),
                span: ident.span,
                proc_name: ident.name.clone(),
            })
        }
    }

    pub fn force_find_for_impl(
        &self,
        src: &NamedSource<String>,
        for_type_id: TypeId,
        ident: &Ident,
    ) -> Result<ProcId> {
        if let Some(found) = self.find_for_impl(for_type_id, ident) {
            Ok(found)
        } else {
            Err(Error::UnknownProc {
                src: src.clone(),
                span: ident.span,
                proc_name: ident.name.clone(),
            })
        }
    }

    pub fn force_find_for_module_type_of(
        &self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        ident: &Ident,
    ) -> Result<TypeId> {
        let proc_id = self.force_find_for_module(src, module_id, ident)?;
        Ok(self.procs[proc_id.0].type_id)
    }

    pub fn force_find_for_impl_type_of(
        &self,
        src: &NamedSource<String>,
        for_type_id: TypeId,
        ident: &Ident,
    ) -> Result<TypeId> {
        let proc_id = self.force_find_for_impl(src, for_type_id, ident)?;
        Ok(self.procs[proc_id.0].type_id)
    }
    pub fn find_for_module(&self, module_id: ModuleId, ident: &Ident) -> Option<ProcId> {
        self.parsed.get(&module_id)?.get(ident).copied()
    }

    pub fn find_for_impl(&self, for_type_id: TypeId, ident: &Ident) -> Option<ProcId> {
        self.impls.get(&for_type_id)?.get(ident).copied()
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
            let module_name = checker.modules.mangled_name_of(proc.module_id);
            let base = match &proc.impl_for {
                Some(data) => {
                    format!(
                        "{}..{}.{}",
                        module_name,
                        checker.types.mangled_name_of(data.for_type_id, checker),
                        proc.name.name
                    )
                }
                None => {
                    format!("{}.{}", module_name, proc.name.name)
                }
            };

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

    pub fn type_id_for(&self, proc_id: ProcId) -> TypeId {
        self.procs[proc_id.0].type_id
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
    pub impl_for: Option<ImplData>,
}

#[derive(Debug)]
pub struct ImplData {
    for_type_id: TypeId,
}

impl ImplData {
    pub fn new(for_type_id: TypeId) -> Self {
        Self { for_type_id }
    }
}
