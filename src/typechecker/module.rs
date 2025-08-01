use miette::NamedSource;

use crate::{
    ast::parsed::{Ident, TranslationUnit},
    error::{Error, Result},
};
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

impl From<usize> for ModuleId {
    fn from(value: usize) -> Self {
        ModuleId(value)
    }
}

#[derive(Debug)]
pub struct ModuleCollection {
    pub modules: Vec<Option<Module>>,
}

impl ModuleCollection {
    pub fn new() -> Self {
        Self { modules: vec![] }
    }

    pub fn allocate(&mut self) -> ModuleId {
        let id = self.modules.len();
        self.modules.push(None);
        id.into()
    }

    pub fn write(&mut self, module_id: ModuleId, module: Module) -> ModuleId {
        assert!(self.modules[module_id.0].is_none());
        self.modules[module_id.0] = Some(module);
        module_id
    }

    pub fn force_find_in(
        &self,
        current: ModuleId,
        haystack: ModuleId,
        name: &Ident,
    ) -> Result<ModuleId> {
        for child in self.modules[haystack.0].as_ref().unwrap().children.iter() {
            if self.modules[child.0].as_ref().unwrap().name == name.name {
                return Ok(*child);
            }
        }

        Err(Error::ModuleNotFound {
            src: self.modules[current.0].as_ref().unwrap().source.clone(),
            span: name.span,
            module_name: name.name.clone(),
            base_name: self.mangled_name_of(haystack),
        })
    }

    pub fn source_for(&self, module_id: ModuleId) -> &NamedSource<String> {
        &self.modules[module_id.0].as_ref().unwrap().source
    }

    pub fn children_for(&self, module_id: ModuleId) -> Rc<Vec<ModuleId>> {
        Rc::clone(&self.modules[module_id.0].as_ref().unwrap().children)
    }

    pub fn unit_for(&self, module_id: ModuleId) -> Rc<TranslationUnit> {
        Rc::clone(&self.modules[module_id.0].as_ref().unwrap().unit)
    }

    pub fn mangled_name_of(&self, module_id: ModuleId) -> String {
        let module = self.modules[module_id.0].as_ref().unwrap();

        if let Some(parent_id) = module.parent {
            let parent_name = self.mangled_name_of(parent_id);
            format!("{parent_name}.{}", module.name)
        } else {
            module.name.clone()
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub children: Rc<Vec<ModuleId>>,
    pub source: NamedSource<String>,
    pub unit: Rc<TranslationUnit>,
    pub parent: Option<ModuleId>,
}
