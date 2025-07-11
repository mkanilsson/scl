use miette::NamedSource;

use crate::{
    ast::parsed::Ident,
    error::{Error, Result},
};
use std::{collections::HashMap, path::PathBuf};

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

    pub fn find_from_path(&self, path: &PathBuf) -> Option<ModuleId> {
        self.paths.get(path).copied()
    }

    pub fn force_find_in(
        &self,
        source: &NamedSource<String>,
        module_id: ModuleId,
        name: &Ident,
    ) -> Result<ModuleId> {
        for child in &self.modules[module_id.0].children {
            if self.modules[child.0].name == name.name {
                return Ok(*child);
            }
        }

        Err(Error::ModuleNotFound {
            src: source.clone(),
            span: name.span,
            module_name: "TODO".to_string(),
            base_name: name.name.clone(),
        })
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub path: PathBuf,
    pub children: Vec<ModuleId>,
}
