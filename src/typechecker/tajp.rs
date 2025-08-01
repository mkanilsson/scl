use std::{collections::HashMap, hash::Hash};

use miette::{NamedSource, SourceSpan};
use strum::EnumIs;

use crate::{
    ast::{self, parsed::Ident},
    error::{Error, Result},
};

use super::{Checker, module::ModuleId};

pub const VOID_TYPE_ID: TypeId = TypeId(0);
pub const BOOL_TYPE_ID: TypeId = TypeId(1);
pub const I8_TYPE_ID: TypeId = TypeId(2);
pub const U8_TYPE_ID: TypeId = TypeId(3);
pub const I16_TYPE_ID: TypeId = TypeId(4);
pub const U16_TYPE_ID: TypeId = TypeId(5);
pub const I32_TYPE_ID: TypeId = TypeId(6);
pub const U32_TYPE_ID: TypeId = TypeId(7);
pub const I64_TYPE_ID: TypeId = TypeId(8);
pub const U64_TYPE_ID: TypeId = TypeId(9);
pub const ISIZE_TYPE_ID: TypeId = TypeId(10);
pub const USIZE_TYPE_ID: TypeId = TypeId(11);
pub const STRING_TYPE_ID: TypeId = TypeId(12);
pub const NEVER_TYPE_ID: TypeId = TypeId(14);

#[derive(Debug, Clone, PartialEq, Eq, EnumIs)]
pub enum Type {
    UndefinedStruct,
    UndefinedProc,
    Void,
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    ISIZE,
    USIZE,
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

    pub fn is_unsigned(&self) -> bool {
        matches!(self, Type::U32)
    }

    pub fn to_string(&self, collection: &TypeCollection, checker: &Checker) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Bool => "bool".into(),
            Type::I8 => "i8".into(),
            Type::U8 => "u8".into(),
            Type::I16 => "i16".into(),
            Type::U16 => "u16".into(),
            Type::I32 => "i32".into(),
            Type::U32 => "u32".into(),
            Type::I64 => "i64".into(),
            Type::U64 => "u64".into(),
            Type::ISIZE => "isize".into(),
            Type::USIZE => "usize".into(),
            Type::String => "string".into(),
            Type::Never => "!".into(),
            Type::Proc(structure) => {
                let mut params = structure
                    .params
                    .iter()
                    .map(|param| collection.name_of(*param, checker))
                    .collect::<Vec<_>>();

                if structure.variadic {
                    params.push("...".to_string());
                }

                let params = params.join(", ");

                let return_type = collection.name_of(structure.return_type, checker);

                format!("proc ({params}) {return_type}")
            }
            Type::Struct(structure) => {
                format!(
                    "{}.{}",
                    checker.modules.mangled_name_of(structure.module_id),
                    structure.ident.name
                )
            }
            Type::Ptr(inner) => format!("*{}", collection.name_of(*inner, checker)),
            Type::Generic(_) => {
                format!("(TODO: Generic args in {}:{})", file!(), line!())
            }
            Type::UndefinedStruct | Type::UndefinedProc => {
                unreachable!()
            }
        }
    }

    pub fn to_mangled_string(&self, collection: &TypeCollection, checker: &Checker) -> String {
        match self {
            Type::Void
            | Type::Bool
            | Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::ISIZE
            | Type::USIZE
            | Type::String
            | Type::Never
            | Type::Struct(_) => self.to_string(collection, checker),
            Type::Proc(_) => {
                todo!()
            }
            Type::Ptr(inner) => format!("P{}", collection.mangled_name_of(*inner, checker)),
            Type::Generic(_) => {
                unreachable!()
            }
            Type::UndefinedStruct | Type::UndefinedProc => {
                unreachable!()
            }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
                Type::I8,
                Type::U8,
                Type::I16,
                Type::U16,
                Type::I32,
                Type::U32,
                Type::I64,
                Type::U64,
                Type::ISIZE,
                Type::USIZE,
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

    pub fn force_find_with_resolved_generics(
        &mut self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        t: &ast::tajp::Type,
        generics: &[Ident],
        resolved_generics: &HashMap<GenericId, Spanned<TypeId>>,
    ) -> Result<TypeId> {
        let type_id = self.force_find_with_generics(src, module_id, t, generics)?;
        self.resolve_generic_type(type_id, resolved_generics)
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

    pub fn force_find_by_name_with_generics(
        &mut self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        name: &Ident,
        generics: &[Ident],
    ) -> Result<TypeId> {
        self.force_find_with_generics(
            src,
            module_id,
            &ast::tajp::Type::new(name.span, ast::tajp::TypeKind::Named(name.clone())),
            generics,
        )
    }

    pub fn force_find_by_name_with_resolved_generics(
        &mut self,
        src: &NamedSource<String>,
        module_id: ModuleId,
        name: &Ident,
        generics: &[Ident],
        resolved_generics: &HashMap<GenericId, Spanned<TypeId>>,
    ) -> Result<TypeId> {
        let type_id = self.force_find_with_generics(
            src,
            module_id,
            &ast::tajp::Type::new(name.span, ast::tajp::TypeKind::Named(name.clone())),
            generics,
        )?;
        self.resolve_generic_type(type_id, resolved_generics)
    }

    fn find_primative(&self, t: &ast::tajp::Type) -> Option<TypeId> {
        match &t.kind {
            ast::tajp::TypeKind::Named(ident) => Some(match ident.name.as_str() {
                "void" => VOID_TYPE_ID,
                "bool" => BOOL_TYPE_ID,
                "i8" => I8_TYPE_ID,
                "u8" => U8_TYPE_ID,
                "i16" => I16_TYPE_ID,
                "u16" => U16_TYPE_ID,
                "i32" => I32_TYPE_ID,
                "u32" => U32_TYPE_ID,
                "i64" => I64_TYPE_ID,
                "u64" => U64_TYPE_ID,
                "isize" => ISIZE_TYPE_ID,
                "usize" => USIZE_TYPE_ID,
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

    pub fn name_of(&self, type_id: TypeId, checker: &Checker) -> String {
        self.types.get(type_id.0).unwrap().to_string(self, checker)
    }

    pub fn mangled_name_of(&self, type_id: TypeId, checker: &Checker) -> String {
        self.types
            .get(type_id.0)
            .unwrap()
            .to_mangled_string(self, checker)
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

    pub fn is_unsigned(&self, type_id: TypeId) -> bool {
        self.get_definition(type_id).is_unsigned()
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

    pub fn qbe_type_def_of<'a>(
        &self,
        type_id: TypeId,
        checker: &Checker,
    ) -> &'static qbe::TypeDef<'a> {
        let definition = self.get_definition(type_id);
        self.qbe_type_def_of_definition(&definition, checker)
    }
    pub fn qbe_type_def_of_definition<'a>(
        &self,
        definition: &Type,
        checker: &Checker,
    ) -> &'static qbe::TypeDef<'a> {
        // What am i suppose to do here when the reference needs to point into the module
        // but then module gets locked becuase it's borrowed as mutable
        Box::leak(Box::new(match definition {
            Type::Struct(structure) => qbe::TypeDef {
                align: None,
                items: structure
                    .fields
                    .iter()
                    .map(|f| (self.qbe_type_of(f.type_id, checker), 1))
                    .collect(),
                name: definition.to_mangled_string(self, checker),
            },
            _ => unreachable!(),
        }))
    }

    pub fn qbe_type_of<'a>(&self, type_id: TypeId, checker: &Checker) -> qbe::Type<'a> {
        let definition = self.get_definition(type_id);
        self.qbe_type_of_definition(&definition, checker)
    }

    fn qbe_type_of_definition<'a>(&self, definition: &Type, checker: &Checker) -> qbe::Type<'a> {
        match definition {
            Type::Bool => qbe::Type::Word,
            Type::I8 => qbe::Type::SignedByte,
            Type::U8 => qbe::Type::UnsignedByte,
            Type::I16 => qbe::Type::SignedHalfword,
            Type::U16 => qbe::Type::UnsignedHalfword,
            Type::I32 | Type::U32 => qbe::Type::Word,
            Type::I64 | Type::U64 => qbe::Type::Long,
            Type::ISIZE | Type::USIZE => qbe::Type::Long,
            Type::String => qbe::Type::Long,
            Type::Proc { .. } => qbe::Type::Long,
            Type::Struct { .. } => {
                qbe::Type::Aggregate(self.qbe_type_def_of_definition(definition, checker))
            }
            Type::Never => qbe::Type::Word,
            Type::Ptr(_) => qbe::Type::Long,
            Type::Void => qbe::Type::Word,
            Type::UndefinedStruct | Type::UndefinedProc => {
                unreachable!()
            }
            Type::Generic(_) => {
                unreachable!("Should've been resolved to a real type")
            }
        }
    }

    fn alignment_of_definition(&self, definition: &Type, checker: &Checker) -> usize {
        self.qbe_type_of_definition(definition, checker).align() as usize
    }

    fn size_of_definition(&self, definition: &Type, checker: &Checker) -> usize {
        self.qbe_type_of_definition(definition, checker).size() as usize
    }

    pub fn memory_layout_of(&self, type_id: TypeId, checker: &Checker) -> MemoryLayout {
        let definition = self.get_definition(type_id);
        self.memory_layout_of_definition(&definition, checker)
    }

    pub fn memory_layout_of_definition(
        &self,
        definition: &Type,
        checker: &Checker,
    ) -> MemoryLayout {
        match definition {
            Type::Bool
            | Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::ISIZE
            | Type::USIZE
            | Type::String
            | Type::Proc { .. }
            | Type::Never
            | Type::Void
            | Type::Ptr(_) => MemoryLayout::new(
                self.size_of_definition(definition, checker),
                self.alignment_of_definition(definition, checker),
                None,
            ),
            Type::Struct(structure) => self.memory_layout_of_struct(&structure.fields, checker),
            Type::UndefinedStruct | Type::UndefinedProc => {
                unreachable!()
            }
            Type::Generic(_) => {
                unreachable!("Should've been resolved to a real type")
            }
        }
    }

    fn memory_layout_of_struct(
        &self,
        fields: &Vec<IdentTypeId>,
        checker: &Checker,
    ) -> MemoryLayout {
        let mut offset = 0;

        let mut largest_alignment = 0;

        let mut layout_fields = HashMap::new();

        for field in fields {
            let layout = self.memory_layout_of(field.type_id, checker);

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
            Type::Proc(proc) => {
                let return_type = self.resolve_generic_type(proc.return_type, resolved_generics)?;

                let mut params = vec![];
                for param in proc.params {
                    params.push(self.resolve_generic_type(param, resolved_generics)?);
                }

                Type::Proc(ProcStructure {
                    params,
                    return_type,
                    variadic: proc.variadic,
                })
            }
            Type::Struct(structure) => {
                let mut fields = vec![];

                for field in structure.fields {
                    fields.push(IdentTypeId {
                        ident: field.ident.clone(),
                        type_id: self.resolve_generic_type(field.type_id, resolved_generics)?,
                    });
                }
                Type::Struct(StructStructure {
                    ident: structure.ident,
                    fields,
                    module_id: structure.module_id,
                })
            }
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
        &self,
        src: &NamedSource<String>,
        expr_span: SourceSpan,
        generic_type_id: TypeId,
        real_type_id: TypeId,
        resolved_generics: &mut HashMap<GenericId, Spanned<TypeId>>,
        checker: &Checker,
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
                    checker,
                )?,
            (Type::Generic(generic_id), _) => {
                if let Some(resolved_type) = resolved_generics.get(&generic_id) {
                    if resolved_type.value != real_type_id {
                        return Err(Error::GenericAlreadyDefinedWithAnotherType {
                            src: src.clone(),
                            infered_span: expr_span,
                            infered_name: self.name_of(real_type_id, checker),
                            defined_span: resolved_type.span,
                            defined_name: self.name_of(resolved_type.value, checker),
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
#[allow(dead_code)] // NOTE: These will be useful when I add llvm support
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

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: SourceSpan,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: SourceSpan) -> Self {
        Self { value, span }
    }
}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Eq> Eq for Spanned<T> {}
