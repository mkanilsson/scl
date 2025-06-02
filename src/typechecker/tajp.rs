use std::collections::HashMap;

use miette::{NamedSource, SourceSpan};

use crate::error::{Error, Result};

pub const BOOL_TYPE_ID: TypeId = TypeId(0);
pub const I32_TYPE_ID: TypeId = TypeId(1);
pub const U32_TYPE_ID: TypeId = TypeId(2);

pub enum Type {
    Bool,
    I32,
    U32,
    Fn {
        params: Vec<TypeId>,
        return_type: Option<TypeId>,
    },
}

#[derive(Clone, Copy)]
pub struct TypeId(pub usize);

pub struct TypeCollection {
    types: Vec<Type>,
    named: HashMap<String, TypeId>,
}

impl TypeCollection {
    pub fn new() -> Self {
        Self {
            types: vec![Type::Bool, Type::I32, Type::U32],
            named: HashMap::new(),
        }
    }

    pub fn force_find(
        &self,
        src: NamedSource<String>,
        span: SourceSpan,
        name: &str,
    ) -> Result<TypeId> {
        if let Some(found) = self.find(name) {
            Ok(found)
        } else {
            Err(Error::UnknownType {
                src,
                span,
                type_name: name.to_string(),
            })
        }
    }

    pub fn find(&self, name: &str) -> Option<TypeId> {
        if let Some(primative) = self.find_primative(name) {
            return Some(primative);
        }

        self.named.get(name).copied()
    }

    fn find_primative(&self, name: &str) -> Option<TypeId> {
        Some(match name {
            "bool" => BOOL_TYPE_ID,
            "i32" => I32_TYPE_ID,
            "u32" => U32_TYPE_ID,
            _ => return None,
        })
    }
}
