use std::collections::HashMap;

use super::{Result, module::ModuleId};

#[derive(Debug)]
pub struct PackageCollection {
    dependencies: HashMap<ModuleId, HashMap<String, ModuleId>>,
}

impl PackageCollection {
    pub fn new() -> Self {
        Self {
            dependencies: HashMap::new(),
        }
    }

    pub fn register_dependency(&mut self, package_id: ModuleId, dependency: &(String, ModuleId)) {
        let dependencies_for_package = self.dependencies.entry(package_id).or_default();

        if dependencies_for_package.contains_key(&dependency.0) {
            todo!("Error message for duplicate dependencies");
        }

        dependencies_for_package.insert(dependency.0.clone(), dependency.1);
    }

    pub fn force_find_dependency(
        &self,
        package_id: ModuleId,
        dependency_name: &str,
    ) -> Result<ModuleId> {
        Ok(self
            .find_dependency(package_id, dependency_name)
            .expect("TODO: Nice error message"))
    }

    pub fn find_dependency(&self, package_id: ModuleId, dependency_name: &str) -> Option<ModuleId> {
        self.dependencies
            .get(&package_id)?
            .get(dependency_name)
            .copied()
    }
}
