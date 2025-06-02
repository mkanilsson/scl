use std::any::TypeId;

#[derive(Debug, Clone)]
pub struct CheckedTranslationUnit {
    pub procs: Vec<CheckedProcDefinition>,
}

#[derive(Debug, Clone)]
pub struct CheckedProcDefinition {
    pub type_id: TypeId,
}
