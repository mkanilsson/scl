use std::fmt::Debug;

use ast::CheckedTranslationUnit;
use scope::Scope;
use tajp::{Type, TypeCollection};

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
            self.add_proc_types(&proc)?;
        }

        Ok(CheckedTranslationUnit { procs: vec![] })
    }

    fn add_proc_types(&mut self, definition: &ProcDefinition) -> Result<()> {
        let mut params = vec![];
        for param in &definition.params {
            params.push(self.types.force_find(&self.unit.source, &param.1)?);
        }

        let return_type = self
            .types
            .force_find(&self.unit.source, &definition.return_type)?;
        let type_id = self.types.register_type(Type::Proc {
            params,
            return_type,
        });

        self.scope.add_to_scope(definition.ident.clone(), type_id);
        Ok(())
    }
}

impl Debug for Checker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Checker")
            .field("types", &self.types)
            .field("scope", &self.scope)
            .finish()
    }
}
