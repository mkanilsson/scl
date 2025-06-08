use std::collections::HashMap;

use miette::NamedSource;
use strum::EnumIs;

use crate::{
    ast::{self, parsed::Ident},
    error::{Error, Result},
};

pub const VOID_TYPE_ID: TypeId = TypeId(0);
pub const BOOL_TYPE_ID: TypeId = TypeId(1);
pub const I32_TYPE_ID: TypeId = TypeId(2);
pub const U32_TYPE_ID: TypeId = TypeId(3);
pub const STRING_TYPE_ID: TypeId = TypeId(4);

#[derive(Debug, Clone, PartialEq, Eq, EnumIs)]
pub enum Type {
    UndefinedStruct,
    Void,
    Bool,
    I32,
    U32,
    String,
    Proc {
        params: Vec<TypeId>,
        return_type: TypeId,
        variadic: bool,
    },
    Struct {
        name: Ident,
        fields: Vec<(Ident, TypeId)>,
    },
}

impl Type {
    pub fn as_proc(self) -> (Vec<TypeId>, TypeId, bool) {
        match self {
            Type::Proc {
                params,
                return_type,
                variadic,
            } => (params, return_type, variadic),
            _ => unreachable!(),
        }
    }

    pub fn as_struct(self) -> (Ident, Vec<(Ident, TypeId)>) {
        match self {
            Type::Struct { name, fields } => (name, fields),
            _ => unreachable!(),
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Type::I32 | Type::U32 => true,
            _ => false,
        }
    }

    pub fn to_string(&self, collection: &TypeCollection) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Bool => "bool".into(),
            Type::I32 => "i32".into(),
            Type::U32 => "u32".into(),
            Type::String => "string".into(),
            Type::Proc {
                params,
                return_type,
                variadic,
            } => {
                let mut params = params
                    .iter()
                    .map(|param| collection.name_of(*param))
                    .collect::<Vec<_>>();

                if *variadic {
                    params.push("...".to_string());
                }

                let params = params.join(", ");

                let return_type = collection.name_of(*return_type);

                format!("proc ({params}) {return_type}")
            }
            Type::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| format!("{}: {}", field.0.name, collection.name_of(field.1)))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("struct {} {{ {fields} }}", name.name)
            }
            Type::UndefinedStruct => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub usize);

impl From<usize> for TypeId {
    fn from(value: usize) -> Self {
        TypeId(value)
    }
}

#[derive(Debug)]
pub struct TypeCollection {
    types: Vec<Type>,
    parsed: HashMap<ast::tajp::TypeKind, TypeId>,
}

impl TypeCollection {
    pub fn new() -> Self {
        Self {
            types: vec![Type::Void, Type::Bool, Type::I32, Type::U32, Type::String],
            parsed: HashMap::new(),
        }
    }

    pub fn register_type(&mut self, t: Type) -> TypeId {
        if let Some(existing) = self
            .types
            .iter()
            .enumerate()
            .find_map(|(i, existing)| if t == *existing { Some(i) } else { None })
        {
            return existing.into();
        }

        let id = self.types.len();
        self.types.push(t);
        id.into()
    }

    pub fn register_undefined_struct(&mut self, ident: &Ident) -> TypeId {
        let type_id = self.register_type(Type::UndefinedStruct);
        self.parsed
            .insert(ast::tajp::TypeKind::Named(ident.clone()), type_id);

        type_id
    }

    pub fn force_find(&self, src: &NamedSource<String>, t: &ast::tajp::Type) -> Result<TypeId> {
        if let Some(found) = self.find(t) {
            Ok(found)
        } else {
            Err(Error::UnknownType {
                src: src.clone(),
                span: t.span,
                type_name: t.kind.to_string(),
            })
        }
    }

    pub fn find(&self, t: &ast::tajp::Type) -> Option<TypeId> {
        if let Some(primative) = self.find_primative(t) {
            return Some(primative);
        }

        self.parsed.get(&t.kind).copied()
    }

    pub fn force_find_by_name(&self, src: &NamedSource<String>, name: &Ident) -> Result<TypeId> {
        self.force_find(
            src,
            &ast::tajp::Type::new(name.span, ast::tajp::TypeKind::Named(name.clone())),
        )
    }

    fn find_primative(&self, t: &ast::tajp::Type) -> Option<TypeId> {
        match &t.kind {
            ast::tajp::TypeKind::Named(ident) => Some(match ident.name.as_str() {
                "void" => VOID_TYPE_ID,
                "bool" => BOOL_TYPE_ID,
                "i32" => I32_TYPE_ID,
                "u32" => U32_TYPE_ID,
                "string" => STRING_TYPE_ID,
                _ => return None,
            }),
        }
    }

    pub fn get_definition(&self, type_id: TypeId) -> Type {
        self.types.get(type_id.0).unwrap().clone()
    }

    pub fn name_of(&self, type_id: TypeId) -> String {
        self.types.get(type_id.0).unwrap().to_string(self)
    }

    pub fn is_number(&self, type_id: TypeId) -> bool {
        self.get_definition(type_id).is_number()
    }

    pub fn define_struct(&mut self, type_id: TypeId, s: Type) {
        assert_eq!(self.types[type_id.0], Type::UndefinedStruct);
        self.types[type_id.0] = s;
    }

    pub fn qbe_type_of<'a>(&self, type_id: TypeId) -> qbe::Type<'a> {
        let definition = self.get_definition(type_id);

        match definition {
            Type::Bool => qbe::Type::Word,
            Type::I32 | Type::U32 => qbe::Type::Word,
            Type::String => qbe::Type::Long,
            Type::Proc { .. } => qbe::Type::Long,
            Type::Struct { .. } => todo!(),
            Type::Void => unreachable!(),
            Type::UndefinedStruct => unreachable!(),
        }
    }
}
