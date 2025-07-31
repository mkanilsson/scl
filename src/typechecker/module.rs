use miette::NamedSource;

use crate::{
    ast::parsed::{Ident, TranslationUnit},
    error::{Error, Result},
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

impl From<usize> for ModuleId {
    fn from(value: usize) -> Self {
        ModuleId(value)
    }
}

#[derive(Debug)]
pub struct ModuleCollection {
    pub modules: Vec<Module>,
    pub paths: HashMap<PathBuf, ModuleId>,
}

impl ModuleCollection {
    pub fn new() -> Self {
        Self {
            modules: vec![],
            paths: HashMap::new(),
        }
    }

    pub fn add(&mut self, module: Module) -> ModuleId {
        assert!(self.find_from_path(&module.path).is_none());

        let id = self.modules.len();
        self.paths.insert(module.path.clone(), id.into());
        self.modules.push(module);
        id.into()
    }

    pub fn find_from_path(&self, path: &Path) -> Option<ModuleId> {
        self.paths.get(path).copied()
    }

    pub fn force_find_in(
        &self,
        current: ModuleId,
        haystack: ModuleId,
        name: &Ident,
    ) -> Result<ModuleId> {
        for child in self.modules[haystack.0].children.iter() {
            if self.modules[child.0].name == name.name {
                return Ok(*child);
            }
        }

        Err(Error::ModuleNotFound {
            src: self.modules[current.0].source.clone(),
            span: name.span,
            module_name: "TODO".to_string(),
            base_name: name.name.clone(),
        })
    }

    pub fn source_for(&self, module_id: ModuleId) -> &NamedSource<String> {
        &self.modules[module_id.0].source
    }

    pub fn children_for(&self, module_id: ModuleId) -> Rc<Vec<ModuleId>> {
        Rc::clone(&self.modules[module_id.0].children)
    }

    pub fn unit_for(&self, module_id: ModuleId) -> Rc<TranslationUnit> {
        Rc::clone(&self.modules[module_id.0].unit)
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub path: PathBuf,
    pub children: Rc<Vec<ModuleId>>,
    pub source: NamedSource<String>,
    pub unit: Rc<TranslationUnit>,
}
