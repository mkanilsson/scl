use std::collections::HashMap;

use crate::{
    ast::{
        parsed::Ident,
        tajp::{Type, TypeKind},
    },
    error::Result,
    typechecker::{Checker, module::ModuleId, proc::ProcId, tajp::TypeId},
};

#[derive(Debug)]
pub struct Implementation {
    for_type_id: TypeId,
    procs: Vec<ProcId>,
    interface: Option<InterfaceId>,
}

#[derive(Debug)]
pub struct Interface {
    pub ident: Ident,
    pub module_id: ModuleId,
}

#[derive(Debug)]
pub struct ImplementationCollection {
    implementations: Vec<Implementation>,
    interfaces: Vec<Option<Interface>>,
    parsed: HashMap<ModuleId, HashMap<TypeKind, InterfaceId>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InterfaceId(pub usize);

impl From<usize> for InterfaceId {
    fn from(value: usize) -> Self {
        InterfaceId(value)
    }
}

impl ImplementationCollection {
    pub fn new() -> Self {
        Self {
            implementations: Vec::new(),
            interfaces: Vec::new(),
            parsed: HashMap::new(),
        }
    }

    pub fn add(&mut self, for_type_id: TypeId, procs: Vec<ProcId>, interface: Option<InterfaceId>) {
        self.implementations.push(Implementation {
            for_type_id,
            procs,
            interface,
        });
    }

    pub fn register_interface(&mut self, module_id: ModuleId, ident: &Ident) -> InterfaceId {
        let type_id = self.interfaces.len();
        self.interfaces.push(None);
        let type_id = type_id.into();
        self.insert_parsed_for_module(module_id, TypeKind::Named(ident.clone()), type_id);
        type_id
    }

    fn insert_parsed_for_module(&mut self, module_id: ModuleId, t: TypeKind, type_id: InterfaceId) {
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(t, type_id);
    }

    pub fn find_by_type_id_and_name(
        &self,
        for_type_id: TypeId,
        name: &Ident,
        checker: &Checker,
    ) -> Option<ProcId> {
        // TODO: Return vec of all matches as there might be more than one

        for implementation in &self.implementations {
            if checker
                .types
                .matches(implementation.for_type_id, for_type_id, checker)
            {
                for proc in &implementation.procs {
                    if name == checker.procs.name_for(*proc) {
                        return Some(*proc);
                    }
                }
            }
        }

        None
    }

    pub fn find_by_exact_type_id_and_name(
        &self,
        for_type_id: TypeId,
        name: &Ident,
        checker: &Checker,
    ) -> Option<ProcId> {
        for implementation in &self.implementations {
            if implementation.for_type_id == for_type_id {
                for proc in &implementation.procs {
                    if name == checker.procs.name_for(*proc) {
                        return Some(*proc);
                    }
                }
            }
        }

        None
    }

    pub fn is_impl_for(&self, proc_id: ProcId) -> Option<TypeId> {
        for implementation in &self.implementations {
            for proc in &implementation.procs {
                if *proc == proc_id {
                    return Some(implementation.for_type_id);
                }
            }
        }

        None
    }

    pub fn force_find_interface(&mut self, module_id: ModuleId, t: &Type) -> Result<InterfaceId> {
        if let Some(found) = self.find_interface(module_id, t) {
            Ok(found)
        } else {
            todo!("Nice error message about interface not being found")
        }
    }

    pub fn find_interface(&self, module_id: ModuleId, t: &Type) -> Option<InterfaceId> {
        self.parsed.get(&module_id)?.get(&t.kind).copied()
    }

    pub fn type_implements(
        &self,
        for_type_id: TypeId,
        interface_id: InterfaceId,
        checker: &Checker,
    ) -> bool {
        for implementation in &self.implementations {
            if let Some(interface_id_for_impl) = implementation.interface {
                if interface_id_for_impl == interface_id
                    && checker
                        .types
                        .matches(implementation.for_type_id, for_type_id, checker)
                {
                    return true;
                }
            }
        }

        false
    }
}
