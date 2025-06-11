use std::collections::HashMap;

use miette::NamedSource;
use strum::EnumIs;

use crate::{
    ast::{self, parsed::Ident},
    error::{Error, Result},
};

use super::module::ModuleId;

pub const VOID_TYPE_ID: TypeId = TypeId(0);
pub const BOOL_TYPE_ID: TypeId = TypeId(1);
pub const I32_TYPE_ID: TypeId = TypeId(2);
pub const U32_TYPE_ID: TypeId = TypeId(3);
pub const STRING_TYPE_ID: TypeId = TypeId(4);

#[derive(Debug, Clone, PartialEq, Eq, EnumIs)]
pub enum Type {
    UndefinedStruct,
    UndefinedProc,
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
        module_id: ModuleId,
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
            Type::Struct {
                name,
                fields,
                module_id: _,
            } => (name, fields),
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
            Type::Struct { name, .. } => name.name.clone(),
            Type::UndefinedStruct | Type::UndefinedProc => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

impl From<usize> for TypeId {
    fn from(value: usize) -> Self {
        TypeId(value)
    }
}

#[derive(Debug)]
pub struct TypeCollection {
    types: Vec<Type>,
    parsed: HashMap<ModuleId, HashMap<ast::tajp::TypeKind, TypeId>>,
    pub structs: Vec<TypeId>,
}

impl TypeCollection {
    pub fn new() -> Self {
        Self {
            types: vec![Type::Void, Type::Bool, Type::I32, Type::U32, Type::String],
            parsed: HashMap::new(),
            structs: vec![],
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

    pub fn register_undefined_struct(&mut self, module_id: ModuleId, ident: &Ident) -> TypeId {
        let type_id = self.types.len();
        self.types.push(Type::UndefinedStruct);
        let type_id = type_id.into();
        self.insert_parsed_for_module(
            module_id,
            ast::tajp::TypeKind::Named(ident.clone()),
            type_id,
        );
        type_id
    }

    pub fn register_undefined_proc(&mut self) -> TypeId {
        let type_id = self.types.len();
        self.types.push(Type::UndefinedProc);
        type_id.into()
    }

    fn insert_parsed_for_module(
        &mut self,
        module_id: ModuleId,
        t: ast::tajp::TypeKind,
        type_id: TypeId,
    ) {
        let parsed_for_module = self.parsed.entry(module_id).or_insert_with(HashMap::new);
        parsed_for_module.insert(t, type_id);
    }

    pub fn force_find(
        &self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        t: &ast::tajp::Type,
    ) -> Result<TypeId> {
        if let Some(found) = self.find(module_id, t) {
            Ok(found)
        } else {
            Err(Error::UnknownType {
                src: src.clone(),
                span: t.span,
                type_name: t.kind.to_string(),
            })
        }
    }

    pub fn find(&self, module_id: ModuleId, t: &ast::tajp::Type) -> Option<TypeId> {
        if let Some(primative) = self.find_primative(t) {
            return Some(primative);
        }

        self.parsed.get(&module_id)?.get(&t.kind).copied()
    }

    pub fn force_find_by_name(
        &self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        name: &Ident,
    ) -> Result<TypeId> {
        self.force_find(
            src,
            module_id,
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
        self.structs.push(type_id);
    }

    pub fn define_proc(&mut self, type_id: TypeId, proc: Type) {
        assert_eq!(self.types[type_id.0], Type::UndefinedProc);
        self.types[type_id.0] = proc;
    }

    pub fn qbe_type_def_of<'a>(&self, type_id: TypeId) -> &'static qbe::TypeDef<'a> {
        let definition = self.get_definition(type_id);
        self.qbe_type_def_of_definition(&definition)
    }
    pub fn qbe_type_def_of_definition<'a>(&self, definition: &Type) -> &'static qbe::TypeDef<'a> {
        // What am i suppose to do here when the reference needs to point into the module
        // but then module gets locked becuase it's borrowed as mutable
        Box::leak(Box::new(match definition {
            Type::Struct {
                name,
                fields,
                module_id: _,
            } => qbe::TypeDef {
                align: None,
                items: fields.iter().map(|f| (self.qbe_type_of(f.1), 0)).collect(),
                name: name.name.clone(),
            },
            _ => unreachable!(),
        }))
    }

    pub fn qbe_type_of<'a>(&self, type_id: TypeId) -> qbe::Type<'a> {
        let definition = self.get_definition(type_id);
        self.qbe_type_of_definition(&definition)
    }

    fn qbe_type_of_definition<'a>(&self, definition: &Type) -> qbe::Type<'a> {
        match definition {
            Type::Bool => qbe::Type::Word,
            Type::I32 | Type::U32 => qbe::Type::Word,
            Type::String => qbe::Type::Long,
            Type::Proc { .. } => qbe::Type::Long,
            Type::Struct { .. } => {
                qbe::Type::Aggregate(self.qbe_type_def_of_definition(definition))
            }
            Type::Void => unreachable!(),
            Type::UndefinedStruct | Type::UndefinedProc => unreachable!(),
        }
    }

    pub fn alignment_of(&self, type_id: TypeId) -> usize {
        let definition = self.get_definition(type_id);
        self.alignment_of_definition(&definition)
    }

    fn alignment_of_definition(&self, definition: &Type) -> usize {
        self.qbe_type_of_definition(definition).align() as usize
    }

    pub fn size_of(&self, type_id: TypeId) -> usize {
        let definition = self.get_definition(type_id);
        self.size_of_definition(&definition)
    }

    fn size_of_definition(&self, definition: &Type) -> usize {
        self.qbe_type_of_definition(definition).size() as usize
    }

    fn size_of_struct(&self, fields: &Vec<(Ident, TypeId)>) -> usize {
        self.memory_layout_of_struct(fields).size
    }

    pub fn memory_layout_of(&self, type_id: TypeId) -> MemoryLayout {
        let definition = self.get_definition(type_id);
        self.memory_layout_of_definition(&definition)
    }

    pub fn memory_layout_of_definition(&self, definition: &Type) -> MemoryLayout {
        match definition {
            Type::I32 | Type::Bool | Type::U32 | Type::String | Type::Proc { .. } => {
                MemoryLayout::new(
                    self.size_of_definition(definition),
                    self.alignment_of_definition(definition),
                    None,
                )
            }
            Type::Struct {
                name: _,
                fields,
                module_id: _,
            } => self.memory_layout_of_struct(fields),
            Type::Void | Type::UndefinedStruct | Type::UndefinedProc => unreachable!(),
        }
    }

    fn memory_layout_of_struct(&self, fields: &Vec<(Ident, TypeId)>) -> MemoryLayout {
        let mut offset = 0;

        let mut largest_alignment = 0;

        let mut layout_fields = HashMap::new();

        for field in fields {
            let layout = self.memory_layout_of(field.1);

            offset = self.round_up_to_alignment(offset, layout.alignment);

            layout_fields.insert(
                field.0.name.clone(),
                FieldLayout::new(offset, layout.size, layout.alignment),
            );

            offset += layout.size;

            if layout.alignment > largest_alignment {
                largest_alignment = layout.alignment;
            }
        }

        MemoryLayout::new(
            self.round_up_to_alignment(offset, largest_alignment),
            largest_alignment,
            Some(layout_fields),
        )
    }

    fn round_up_to_alignment(&self, value: usize, alignment: usize) -> usize {
        (value + (alignment - 1)) & !(alignment - 1)
    }

    pub fn add_to_module(&mut self, module_id: ModuleId, type_id: TypeId, ident: &Ident) {
        let parsed_for_module = self.parsed.entry(module_id).or_insert_with(HashMap::new);
        parsed_for_module.insert(ast::tajp::TypeKind::Named(ident.clone()), type_id);
    }
}

#[derive(Clone)]
pub struct MemoryLayout {
    pub size: usize,
    pub alignment: usize,
    // TODO: Handle nested structs
    pub fields: Option<HashMap<String, FieldLayout>>,
}

impl MemoryLayout {
    pub fn new(
        size: usize,
        alignment: usize,
        fields: Option<HashMap<String, FieldLayout>>,
    ) -> Self {
        Self {
            size,
            alignment,
            fields,
        }
    }
}

#[derive(Clone)]
pub struct FieldLayout {
    pub offset: usize,
    pub size: usize,
    pub alignment: usize,
}

impl FieldLayout {
    pub fn new(offset: usize, size: usize, alignment: usize) -> Self {
        Self {
            offset,
            size,
            alignment,
        }
    }
}
