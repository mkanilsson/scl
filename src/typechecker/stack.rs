use std::fmt::Display;

use super::tajp::TypeId;

#[derive(Debug, Clone, Copy)]
pub struct StackSlotId(pub usize);

impl StackSlotId {
    pub fn qbe_name(&self) -> String {
        format!(".ss.{}", self)
    }
}

impl From<usize> for StackSlotId {
    fn from(value: usize) -> Self {
        StackSlotId(value)
    }
}

impl Display for StackSlotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct StackSlots {
    pub slots: Vec<TypeId>,
}

impl StackSlots {
    pub fn new() -> Self {
        Self { slots: vec![] }
    }

    pub fn allocate(&mut self, tajp: TypeId) -> StackSlotId {
        let id = self.slots.len().into();
        self.slots.push(tajp);
        id
    }

    pub fn type_of(&self, id: StackSlotId) -> TypeId {
        self.slots[id.0]
    }
}
