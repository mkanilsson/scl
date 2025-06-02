use ast::CheckedTranslationUnit;
use scope::Scope;
use tajp::TypeCollection;

use crate::{
    ast::parsed::{ProcDefinition, TranslationUnit},
    error::Result,
};

mod ast;
mod scope;
mod tajp;

pub struct Checker {
    types: TypeCollection,
    unit: TranslationUnit,
    scope: Scope,
}

impl Checker {
    pub fn new(unit: TranslationUnit) -> Self {
        Self {
            types: TypeCollection::new(),
            unit,
            scope: Scope::new(),
        }
    }

    pub fn check(&mut self) -> Result<CheckedTranslationUnit> {
        let procs = self.unit.procs.clone();

        for proc in procs {
            self.add_proc_types(&proc);
        }

        todo!()
    }

    fn add_proc_types(&mut self, definition: &ProcDefinition) {
        // let params = vec![];
        for param in &definition.params {
            todo!()
        }

        todo!()
        // self.scope.add_to_scope(definition.ident, type_id);
    }
}
