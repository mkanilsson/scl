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
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub path: PathBuf,
    pub children: Vec<ModuleId>,
}
