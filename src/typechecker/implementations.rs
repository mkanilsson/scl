use crate::{
    ast::parsed::Ident,
    typechecker::{Checker, proc::ProcId, tajp::TypeId},
};

#[derive(Debug)]
pub struct Implementation {
    for_type_id: TypeId,
    procs: Vec<ProcId>,
}

#[derive(Debug)]
pub struct ImplementationCollection {
    implementations: Vec<Implementation>,
}

impl ImplementationCollection {
    pub fn new() -> Self {
        Self {
            implementations: Vec::new(),
        }
    }

    pub fn add(&mut self, for_type_id: TypeId, procs: Vec<ProcId>) {
        self.implementations
            .push(Implementation { for_type_id, procs });
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
                .matches(implementation.for_type_id, for_type_id)
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
}
