use std::collections::HashMap;

use crate::{
    ast::{self, parsed::Ident, tajp::TypeKind},
    error::Result,
    typechecker::{Checker, module::ModuleId, proc::ProcId, tajp::TypeId},
};

#[derive(Debug)]
pub struct Implementation {
    for_type_id: TypeId,
    kind: ImplementationKind,
}

#[derive(Debug)]
pub enum ImplementationKind {
    Interface {
        interface_id: InterfaceId,
        procs: HashMap<ProcId, ProcId>,
    },
    Normal(Vec<ProcId>),
}

#[derive(Debug)]
pub struct Interface {
    pub ident: Ident,
    pub module_id: ModuleId,
    pub procs: Vec<ProcId>,
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

    pub fn add(&mut self, for_type_id: TypeId, procs: Vec<ProcId>) {
        self.implementations.push(Implementation {
            for_type_id,
            kind: ImplementationKind::Normal(procs),
        });
    }

    pub fn map_proc_via_interface_and_type(
        &self,
        proc_id: ProcId,
        for_type_id: TypeId,
        interface_id: InterfaceId,
    ) -> ProcId {
        for implementation in &self.implementations {
            if implementation.for_type_id != for_type_id {
                continue;
            }

            match &implementation.kind {
                ImplementationKind::Interface {
                    interface_id: interface_id_for_impl,
                    procs,
                } => {
                    if *interface_id_for_impl == interface_id {
                        return *procs.get(&proc_id).unwrap();
                    }
                }
                ImplementationKind::Normal(_) => {}
            }
        }

        unreachable!()
    }

    pub fn add_for_interface(
        &mut self,
        for_type_id: TypeId,
        procs: HashMap<ProcId, ProcId>,
        interface_id: InterfaceId,
    ) {
        self.implementations.push(Implementation {
            for_type_id,
            kind: ImplementationKind::Interface {
                interface_id,
                procs,
            },
        });
    }

    pub fn register_interface(&mut self, module_id: ModuleId, ident: &Ident) -> InterfaceId {
        let type_id = self.interfaces.len();
        self.interfaces.push(None);
        let type_id = type_id.into();
        self.insert_parsed_for_module(module_id, TypeKind::Named(ident.clone()), type_id);
        type_id
    }

    pub fn define_interface(&mut self, interface_id: InterfaceId, interface: Interface) {
        assert!(self.interfaces[interface_id.0].is_none());
        self.interfaces[interface_id.0] = Some(interface);
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
                match &implementation.kind {
                    ImplementationKind::Normal(procs) => {
                        for proc in procs {
                            if name == checker.procs.name_for(*proc) {
                                return Some(*proc);
                            }
                        }
                    }
                    ImplementationKind::Interface { procs, .. } => {
                        for proc in procs {
                            if name == checker.procs.name_for(*proc.1) {
                                return Some(*proc.1);
                            }
                        }
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
                match &implementation.kind {
                    ImplementationKind::Normal(procs) => {
                        for proc in procs {
                            if name == checker.procs.name_for(*proc) {
                                return Some(*proc);
                            }
                        }
                    }
                    ImplementationKind::Interface { procs, .. } => {
                        for proc in procs {
                            if name == checker.procs.name_for(*proc.1) {
                                return Some(*proc.1);
                            }
                        }
                    }
                }
            }
        }

        None
    }

    pub fn is_impl_for(&self, proc_id: ProcId) -> Option<TypeId> {
        for implementation in &self.implementations {
            match &implementation.kind {
                ImplementationKind::Normal(procs) => {
                    for proc in procs {
                        if *proc == proc_id {
                            return Some(implementation.for_type_id);
                        }
                    }
                }
                ImplementationKind::Interface { procs, .. } => {
                    for proc in procs {
                        if *proc.1 == proc_id {
                            return Some(implementation.for_type_id);
                        }
                    }
                }
            }
        }

        None
    }

    pub fn force_find_interface(
        &mut self,
        module_id: ModuleId,
        t: &TypeKind,
    ) -> Result<InterfaceId> {
        if let Some(found) = self.find_interface(module_id, t) {
            Ok(found)
        } else {
            todo!("Nice error message about interface not being found")
        }
    }

    pub fn force_find_interface_by_name(
        &mut self,
        module_id: ModuleId,
        ident: &Ident,
    ) -> Result<InterfaceId> {
        self.force_find_interface(module_id, &ast::tajp::TypeKind::Named(ident.clone()))
    }

    pub fn find_interface(&self, module_id: ModuleId, t: &TypeKind) -> Option<InterfaceId> {
        self.parsed.get(&module_id)?.get(&t).copied()
    }

    pub fn type_implements(
        &self,
        for_type_id: TypeId,
        interface_id: InterfaceId,
        checker: &Checker,
    ) -> bool {
        for implementation in &self.implementations {
            match &implementation.kind {
                ImplementationKind::Interface {
                    interface_id: interface_id_for_impl,
                    ..
                } => {
                    if *interface_id_for_impl == interface_id
                        && checker
                            .types
                            .matches(implementation.for_type_id, for_type_id, checker)
                    {
                        return true;
                    }
                }
                ImplementationKind::Normal(_) => {}
            }
        }

        false
    }

    pub fn find_by_interface_id_and_name(
        &self,
        interface_id: InterfaceId,
        name: &Ident,
        checker: &Checker,
    ) -> Option<ProcId> {
        let interface = self.interfaces[interface_id.0].as_ref().unwrap();

        for proc_id in &interface.procs {
            if checker.procs.name_for(*proc_id) == name {
                return Some(*proc_id);
            }
        }

        None
    }

    pub fn get_interface(&self, interface_id: InterfaceId) -> &Interface {
        self.interfaces
            .get(interface_id.0)
            .as_ref()
            .unwrap()
            .as_ref()
            .unwrap()
    }

    pub fn name_of(&self, interface_id: InterfaceId, checker: &Checker) -> String {
        let interface = self.interfaces[interface_id.0].as_ref().unwrap();
        let module = checker.modules.mangled_name_of(interface.module_id);

        format!("{module}.{}", interface.ident.name)
    }

    pub fn get_interface_for_proc(&self, proc_id: ProcId) -> Option<InterfaceId> {
        for implementation in &self.implementations {
            match &implementation.kind {
                ImplementationKind::Interface {
                    interface_id,
                    procs,
                } => {
                    for proc in procs {
                        if *proc.1 == proc_id {
                            return Some(*interface_id);
                        }
                    }
                }
                ImplementationKind::Normal(_) => continue,
            }
        }

        None
    }
}
