use std::collections::HashMap;

use miette::{NamedSource, SourceSpan};
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
pub const NEVER_TYPE_ID: TypeId = TypeId(5);

#[derive(Debug, Clone, PartialEq, Eq, EnumIs)]
pub enum Type {
    UndefinedStruct,
    UndefinedProc,
    Void,
    Bool,
    I32,
    U32,
    String,
    Never,
    Proc(ProcStructure),
    Struct(StructStructure),
    Ptr(TypeId),

    Generic(GenericId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcStructure {
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructStructure {
    pub ident: Ident,
    pub fields: Vec<IdentTypeId>,
    pub module_id: ModuleId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentTypeId {
    pub ident: Ident,
    pub type_id: TypeId,
}

impl Type {
    #[allow(clippy::wrong_self_convention)]
    pub fn as_proc(self) -> ProcStructure {
        match self {
            Type::Proc(structure) => structure,
            _ => unreachable!(),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn as_struct(self) -> StructStructure {
        match self {
            Type::Struct(structure) => structure,
            _ => unreachable!(),
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Type::I32 | Type::U32)
    }

    pub fn to_string(&self, collection: &TypeCollection) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Bool => "bool".into(),
            Type::I32 => "i32".into(),
            Type::U32 => "u32".into(),
            Type::String => "string".into(),
            Type::Never => "!".into(),
            Type::Proc(structure) => {
                let mut params = structure
                    .params
                    .iter()
                    .map(|param| collection.name_of(*param))
                    .collect::<Vec<_>>();

                if structure.variadic {
                    params.push("...".to_string());
                }

                let params = params.join(", ");

                let return_type = collection.name_of(structure.return_type);

                format!("proc ({params}) {return_type}")
            }
            Type::Struct(structure) => structure.ident.name.clone(),
            Type::Ptr(inner) => format!("*{}", collection.name_of(*inner)),
            Type::Generic(generic_id) => {
                format!("(TODO: Generic args in {}:{})", file!(), line!())
            }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericId(pub usize);

impl From<usize> for GenericId {
    fn from(value: usize) -> Self {
        GenericId(value)
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
            types: vec![
                Type::Void,
                Type::Bool,
                Type::I32,
                Type::U32,
                Type::String,
                Type::Never,
            ],
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
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(t, type_id);
    }

    pub fn force_find(
        &mut self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        t: &ast::tajp::Type,
    ) -> Result<TypeId> {
        self.force_find_with_generics(src, module_id, t, &[])
    }

    pub fn force_find_with_generics(
        &mut self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        t: &ast::tajp::Type,
        generics: &[Ident],
    ) -> Result<TypeId> {
        if let Some(found) = self.find(module_id, t) {
            Ok(found)
        } else {
            self.try_add(src, module_id, t, generics)
        }
    }

    fn try_add(
        &mut self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        t: &ast::tajp::Type,
        generics: &[Ident],
    ) -> Result<TypeId> {
        match &t.kind {
            ast::tajp::TypeKind::Named(name) => {
                if let Some(generic_id) = generics.iter().position(|t| t == name) {
                    Ok(self.register_type(Type::Generic(GenericId(generic_id))))
                } else {
                    Err(Error::UnknownType {
                        src: src.clone(),
                        span: t.span,
                        type_name: t.kind.to_string(),
                    })
                }
            }
            ast::tajp::TypeKind::Ptr(inner) => {
                let inner = self.force_find_with_generics(src, module_id, inner, generics)?;
                Ok(self.register_type(Type::Ptr(inner)))
            }
            ast::tajp::TypeKind::Never => panic!("This should be caught by parser"),
        }
    }

    pub fn find(&self, module_id: ModuleId, t: &ast::tajp::Type) -> Option<TypeId> {
        if let Some(primative) = self.find_primative(t) {
            return Some(primative);
        }

        self.parsed.get(&module_id)?.get(&t.kind).copied()
    }

    pub fn force_find_by_name(
        &mut self,
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
            ast::tajp::TypeKind::Never => Some(NEVER_TYPE_ID),
            ast::tajp::TypeKind::Ptr(_) => None,
        }
    }

    pub fn get_definition(&self, type_id: TypeId) -> Type {
        self.types.get(type_id.0).unwrap().clone()
    }

    pub fn name_of(&self, type_id: TypeId) -> String {
        self.types.get(type_id.0).unwrap().to_string(self)
    }

    pub fn inner_of(&self, type_id: TypeId) -> TypeId {
        match self.types.get(type_id.0).unwrap() {
            Type::Ptr(type_id) => *type_id,
            _ => panic!("inner_of on non-ptr"),
        }
    }

    pub fn is_number(&self, type_id: TypeId) -> bool {
        self.get_definition(type_id).is_number()
    }

    pub fn is_ptr(&self, type_id: TypeId) -> bool {
        self.get_definition(type_id).is_ptr()
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
            Type::Struct(structure) => qbe::TypeDef {
                align: None,
                items: structure
                    .fields
                    .iter()
                    .map(|f| (self.qbe_type_of(f.type_id), 1))
                    .collect(),
                name: structure.ident.name.clone(),
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
            Type::Never => qbe::Type::Word,
            Type::Ptr(_) => qbe::Type::Long,
            Type::Void => qbe::Type::Word,
            Type::UndefinedStruct | Type::UndefinedProc => unreachable!(),
            Type::Generic(_) => {
                unreachable!("Should've been resolved to a real type")
            }
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

    fn size_of_struct(&self, fields: &Vec<IdentTypeId>) -> usize {
        self.memory_layout_of_struct(fields).size
    }

    pub fn memory_layout_of(&self, type_id: TypeId) -> MemoryLayout {
        let definition = self.get_definition(type_id);
        self.memory_layout_of_definition(&definition)
    }

    pub fn memory_layout_of_definition(&self, definition: &Type) -> MemoryLayout {
        match definition {
            Type::I32
            | Type::Bool
            | Type::U32
            | Type::String
            | Type::Proc { .. }
            | Type::Never
            | Type::Void
            | Type::Ptr(_) => MemoryLayout::new(
                self.size_of_definition(definition),
                self.alignment_of_definition(definition),
                None,
            ),
            Type::Struct(structure) => self.memory_layout_of_struct(&structure.fields),
            Type::UndefinedStruct | Type::UndefinedProc => unreachable!(),
            Type::Generic(_) => {
                unreachable!("Should've been resolved to a real type")
            }
        }
    }

    fn memory_layout_of_struct(&self, fields: &Vec<IdentTypeId>) -> MemoryLayout {
        let mut offset = 0;

        let mut largest_alignment = 0;

        let mut layout_fields = HashMap::new();

        for field in fields {
            let layout = self.memory_layout_of(field.type_id);

            offset = self.round_up_to_alignment(offset, layout.alignment);

            layout_fields.insert(
                field.ident.name.clone(),
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
        let parsed_for_module = self.parsed.entry(module_id).or_default();
        parsed_for_module.insert(ast::tajp::TypeKind::Named(ident.clone()), type_id);
    }

    pub fn is_generic(&self, type_id: TypeId) -> bool {
        match self.get_definition(type_id) {
            Type::Ptr(type_id) => self.is_generic(type_id),
            Type::Generic(_) => true,
            Type::Proc(definition) => {
                self.is_generic(definition.return_type)
                    || definition.params.iter().any(|p| self.is_generic(*p))
            }
            Type::Struct(_) => todo!(),
            _ => false,
        }
    }

    pub fn resolve_generic_type(
        &mut self,
        type_id: TypeId,
        resolved_generics: &HashMap<GenericId, Spanned<TypeId>>,
    ) -> Result<TypeId> {
        let definition = match self.get_definition(type_id) {
            Type::Ptr(inner) => Type::Ptr(self.resolve_generic_type(inner, resolved_generics)?),
            Type::Proc(_) => todo!(),
            Type::Struct(_) => todo!(),
            Type::Generic(generic_id) => {
                return Ok(resolved_generics
                    .get(&generic_id)
                    .expect("TODO: nice error about unresolved type_id")
                    .value);
            }
            other => other,
        };

        Ok(self.register_type(definition))
    }

    pub fn infer_generic_types(
        &mut self,
        src: &NamedSource<String>,
        expr_span: SourceSpan,
        generic_type_id: TypeId,
        real_type_id: TypeId,
        resolved_generics: &mut HashMap<GenericId, Spanned<TypeId>>,
    ) -> Result<()> {
        if generic_type_id == real_type_id {
            return Ok(());
        }

        let generic_definition = self.get_definition(generic_type_id);
        let real_definition = self.get_definition(real_type_id);
        match (generic_definition, real_definition) {
            (_, Type::Generic(_)) => panic!("Real is generic..."),

            (Type::UndefinedStruct, Type::UndefinedStruct) => todo!(),
            (Type::UndefinedProc, Type::UndefinedProc) => todo!(),
            (Type::Void, Type::Void) => todo!(),
            (Type::Bool, Type::Bool) => todo!(),
            (Type::I32, Type::I32) => todo!(),
            (Type::U32, Type::U32) => todo!(),
            (Type::String, Type::String) => todo!(),
            (Type::Never, Type::Never) => todo!(),
            (Type::Proc(_), Type::Proc(_)) => todo!(),
            (Type::Struct(_), Type::Struct(_)) => todo!(),
            (Type::Ptr(generic_inner_type_id), Type::Ptr(real_inner_type_id)) => self
                .infer_generic_types(
                    src,
                    expr_span,
                    generic_inner_type_id,
                    real_inner_type_id,
                    resolved_generics,
                )?,
            (Type::Generic(generic_id), _) => {
                if let Some(resolved_type) = resolved_generics.get(&generic_id) {
                    if resolved_type.value != real_type_id {
                        return Err(Error::GenericAlreadyDefinedWithAnotherType {
                            src: src.clone(),
                            infered_span: expr_span,
                            infered_name: self.name_of(real_type_id),
                            defined_span: resolved_type.span,
                            defined_name: self.name_of(resolved_type.value),
                        });
                    }
                } else {
                    resolved_generics.insert(generic_id, Spanned::new(real_type_id, expr_span));
                }
            }

            (a, b) if a != b => todo!("Not same structure"),
            (generic, real) => panic!("Generic: {:#?}\nReal: {:#?}", generic, real),
        }

        Ok(())
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

pub struct Spanned<T> {
    value: T,
    span: SourceSpan,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: SourceSpan) -> Self {
        Self { value, span }
    }
}
