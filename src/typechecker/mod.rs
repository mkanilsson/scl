use std::{collections::HashMap, rc::Rc};

use ast::{
    CheckedBlock, CheckedBuiltin, CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt,
    CheckedTranslationUnit,
};
use indexmap::IndexMap;
use miette::SourceSpan;
use module::{Module, ModuleCollection, ModuleId};
use package::PackageCollection;
use proc::{Proc, ProcCollection, ProcId};
use scope::Scope;
use stack::StackSlots;
use tajp::{
    BOOL_TYPE_ID, F32_TYPE_ID, F64_TYPE_ID, GenericId, I8_TYPE_ID, I16_TYPE_ID, I32_TYPE_ID,
    I64_TYPE_ID, ISIZE_TYPE_ID, IdentTypeId, NEVER_TYPE_ID, ProcStructure, STRING_TYPE_ID, Spanned,
    StructStructure, Type, TypeCollection, TypeId, U8_TYPE_ID, U16_TYPE_ID, U32_TYPE_ID,
    U64_TYPE_ID, USIZE_TYPE_ID, VOID_TYPE_ID,
};

use crate::{
    ast::parsed::{
        BinOp, Block, Builtin, Expr, ExprKind, ExternProcDefinition, Ident, Impl, Import,
        Interface, ProcDefinition, Stmt, StmtKind, StructDefinition,
    },
    error::{Error, Result},
    helpers::string_join_with_and,
    package::{CheckedPackage, ParsedModule, ParsedPackage},
    typechecker::implementations::{ImplementationCollection, InterfaceId},
};

pub mod ast;
mod implementations;
pub mod module;
mod package;
pub mod proc;
mod scope;
pub mod stack;
pub mod tajp;

macro_rules! verify_number {
    ($value:ident, $t:ident, $type_id:expr, $expr:ident, $ctx:ident, $self:ident) => {
        if let Ok(value) = $value.parse::<$t>() {
            value
        } else {
            return Err(Error::InvalidNumber {
                src: $self.modules.source_for($ctx.module_id).clone(),
                span: $expr.span,
                value: $value.to_string(),
                type_name: $self.types.name_of($type_id, $self),
            });
        }
    };
}

macro_rules! parse_number {
    ($value:ident, $t:ident, $type_id:expr, $expr:ident, $ctx:ident, $self:ident) => {{
        let value = verify_number!($value, $t, $type_id, $expr, $ctx, $self);

        Ok(HasNever::new(
            CheckedExpr {
                type_id: $type_id,
                kind: ast::CheckedExprKind::Number(value as u64),
                lvalue: false,
            },
            false,
        ))
    }};
}

macro_rules! parse_float {
    ($value:ident, $t:ident, $type_id:ident, $expr:ident, $ctx:ident, $self:ident) => {{
        let value = verify_number!($value, $t, $type_id, $expr, $ctx, $self);

        Ok(HasNever::new(
            CheckedExpr {
                type_id: $type_id,
                kind: ast::CheckedExprKind::Number(value.to_bits() as u64),
                lvalue: false,
            },
            false,
        ))
    }};
}

#[derive(Debug)]
pub struct Checker {
    pub types: TypeCollection,
    scope: Scope,
    modules: ModuleCollection,
    pub procs: ProcCollection,
    packages: PackageCollection,
    implementations: ImplementationCollection,
    pub instantiated_generic_procs: IndexMap<(ProcId, Vec<TypeId>), (ProcId, CheckedProc)>,
    pub generic_procs: HashMap<ProcId, ProcDefinition>,
}

struct CheckerContext {
    module_id: ModuleId,
}

struct ProcContext<'a> {
    return_type: (TypeId, SourceSpan),
    stack_slots: &'a mut StackSlots,
    generics: &'a [Ident],
    resolved_generics: &'a HashMap<GenericId, Spanned<TypeId>>,
    this: Option<TypeId>,
}

struct BlockContext {
    deferred: Vec<Vec<CheckedExpr>>,
}

impl BlockContext {
    pub fn new() -> Self {
        Self { deferred: vec![] }
    }

    pub fn enter(&mut self) {
        self.deferred.push(vec![]);
    }

    pub fn exit(&mut self) {
        self.deferred.pop();
    }

    pub fn push(&mut self, expr: CheckedExpr) {
        let i = self.deferred.len() - 1;
        self.deferred[i].push(expr);
    }
}

impl Checker {
    pub fn new() -> Self {
        Self {
            types: TypeCollection::new(),
            scope: Scope::new(),
            modules: ModuleCollection::new(),
            procs: ProcCollection::new(),
            packages: PackageCollection::new(),
            instantiated_generic_procs: IndexMap::new(),
            generic_procs: HashMap::new(),
            implementations: ImplementationCollection::new(),
        }
    }

    pub fn add_package(
        &mut self,
        package: ParsedPackage,
        dependencies: &[(String, ModuleId)],
    ) -> Result<CheckedPackage> {
        let package_id = self.create_module_ids(package.base_module, None)?;

        // TODO: Find a way to limit struct and proc lookups in these functions
        self.declare_interfaces(package_id)?;
        self.declare_structs(package_id)?;
        let generic_procs = self
            .declare_procs(package_id)?
            .iter()
            .map(|p| (*p.0, (**p.1).clone()))
            .collect::<HashMap<_, _>>();

        self.generic_procs.extend(generic_procs);

        for dependency in dependencies {
            self.add_dependency(package_id, dependency);
        }

        self.resolve_imports(package_id, package_id)?;
        self.define_structs(package_id)?;
        self.declare_impls(package_id)?;

        self.define_procs(package_id)?;
        let units = self.check_package(package_id)?;

        Ok(CheckedPackage::new(package_id, units))
    }

    fn add_dependency(&mut self, package_id: ModuleId, dependency: &(String, ModuleId)) {
        self.packages.register_dependency(package_id, dependency);
    }

    fn resolve_imports(&mut self, package_id: ModuleId, module_id: ModuleId) -> Result<()> {
        let ctx = CheckerContext {
            module_id: package_id,
        };
        for import in &self.modules.unit_for(module_id).imports {
            self.resolve_import_part(ctx.module_id, package_id, import, true, &ctx)?;
        }

        for module in self.modules.children_for(module_id).iter() {
            self.resolve_imports(package_id, *module)?;
        }

        Ok(())
    }

    fn resolve_import_part(
        &mut self,
        import_to: ModuleId,
        module_id: ModuleId,
        import: &Import,
        first: bool,
        ctx: &CheckerContext,
    ) -> Result<()> {
        match import {
            Import::Part(import, ident) => {
                if first {
                    if ident.name == "package" {
                        self.resolve_import_part(import_to, module_id, import, false, ctx)
                    } else {
                        self.resolve_import_part(
                            import_to,
                            self.packages
                                .force_find_dependency(module_id, &ident.name)?,
                            import,
                            false,
                            ctx,
                        )
                    }
                } else {
                    let module_id = self
                        .modules
                        .force_find_in(ctx.module_id, module_id, ident)?;
                    self.resolve_import_part(import_to, module_id, import, false, ctx)
                }
            }
            Import::Final(ident) => {
                if first {
                    todo!("This should show an error message");
                }

                if let Ok(type_id) = self.types.force_find_by_name_with_generics(
                    self.modules.source_for(ctx.module_id),
                    module_id,
                    ident,
                    &[],
                ) {
                    // TODO: Verify that the name is unique
                    self.types.add_to_module(import_to, type_id, ident);
                    return Ok(());
                }

                if let Ok(proc_id) = self.procs.force_find_for_module(
                    self.modules.source_for(module_id),
                    module_id,
                    ident,
                ) {
                    // TODO: Verify that the name is unique
                    self.procs.add_to_module(import_to, proc_id, ident);
                    return Ok(());
                }

                Err(Error::ProcOrStructNotFound {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    span: ident.span,
                    wanted_name: ident.name.clone(),
                    module_name: self.modules.mangled_name_of(module_id),
                })
            }
            Import::Group(imports) => {
                if first {
                    todo!("This should show an error message");
                }

                for import in imports {
                    self.resolve_import_part(import_to, module_id, import, false, ctx)?;
                }

                Ok(())
            }
        }
    }

    fn declare_impls<'a>(&mut self, module_id: ModuleId) -> Result<()> {
        let ctx = CheckerContext { module_id };

        for child_id in self.modules.children_for(module_id).iter() {
            self.declare_impls(*child_id)?;
        }

        let unit = self.modules.unit_for(module_id);

        // FIXME: Procs with the same structure will have different TypeId's when they should have
        //        the same
        for impl_block in &unit.impls {
            self.add_impl(impl_block, &ctx)?;
        }

        Ok(())
    }

    fn add_impl(&mut self, impl_block: &Impl, ctx: &CheckerContext) -> Result<()> {
        let for_type_id = self.types.force_find_with_generics(
            self.modules.source_for(ctx.module_id),
            ctx.module_id,
            &impl_block.tajp,
            &impl_block.type_params,
        )?;

        let mut procs = vec![];

        for proc in &impl_block.procs {
            let type_id = self.types.register_undefined_proc();
            let has_this = proc.signature.params.len() > 0 && proc.signature.params[0].is_this();

            procs.push(self.procs.add(
                ctx.module_id,
                Proc {
                    type_id,
                    name: proc.signature.ident.clone(),
                    module_id: ctx.module_id,
                    external: false,
                    generic: false,
                    generic_instances: Vec::new(),
                    link_name: None,
                    has_this,
                },
            ));
        }

        let interface_type_id = if let Some(interface) = &impl_block.interface {
            Some(
                self.implementations
                    .force_find_interface(ctx.module_id, &interface)?,
            )
        } else {
            None
        };

        self.implementations
            .add(for_type_id, procs, interface_type_id);

        Ok(())
    }

    fn declare_procs<'a>(
        &mut self,
        module_id: ModuleId,
    ) -> Result<HashMap<ProcId, Rc<ProcDefinition>>> {
        let ctx = CheckerContext { module_id };

        let mut generic_procs = HashMap::new();

        for child_id in self.modules.children_for(module_id).iter() {
            generic_procs.extend(self.declare_procs(*child_id)?);
        }

        let unit = self.modules.unit_for(module_id);

        // FIXME: Procs with the same structure will have different TypeId's when they should have
        //        the same
        for proc in &unit.procs {
            let proc_id = self.add_proc_name(proc, &ctx)?;
            if !proc.signature.type_params.is_empty() {
                generic_procs.insert(proc_id, Rc::clone(proc));
            }
        }

        for proc in &unit.extern_procs {
            self.add_extern_proc_name(proc, &ctx)?;
        }

        Ok(generic_procs)
    }

    fn define_structs(&mut self, module_id: ModuleId) -> Result<()> {
        let ctx = CheckerContext { module_id };

        for child_id in self.modules.children_for(module_id).iter() {
            self.define_structs(*child_id)?;
        }

        for s in &self.modules.unit_for(module_id).structs {
            let struct_type_id = self.types.force_find_by_name_with_generics(
                self.modules.source_for(module_id),
                module_id,
                &s.ident,
                &[],
            )?;

            self.define_struct(s, struct_type_id, &ctx)?;
        }

        Ok(())
    }

    fn define_procs(&mut self, module_id: ModuleId) -> Result<()> {
        let ctx = CheckerContext { module_id };

        for child_id in self.modules.children_for(module_id).iter() {
            self.define_procs(*child_id)?;
        }

        let unit = self.modules.unit_for(module_id);

        for proc in &unit.procs {
            self.define_proc(
                proc,
                self.procs.force_find_for_module_type_of(
                    self.modules.source_for(ctx.module_id),
                    ctx.module_id,
                    &proc.signature.ident,
                )?,
                false,
                &ctx,
            )?;
        }

        for impl_block in &unit.impls {
            let for_type_id = self.types.force_find_with_generics(
                self.modules.source_for(ctx.module_id),
                ctx.module_id,
                &impl_block.tajp,
                &impl_block.type_params,
            )?;

            for proc in &impl_block.procs {
                let proc_id = self
                    .implementations
                    .find_by_exact_type_id_and_name(for_type_id, &proc.signature.ident, &self)
                    .expect("To exist");

                self.define_proc(proc, self.procs.type_id_for(proc_id), true, &ctx)?;
            }
        }

        for extern_proc in &unit.extern_procs {
            self.define_extern_proc(
                extern_proc,
                self.procs.force_find_for_module_type_of(
                    self.modules.source_for(ctx.module_id),
                    ctx.module_id,
                    &extern_proc.signature.ident,
                )?,
                &ctx,
            )?;
        }

        Ok(())
    }

    fn check_package(&mut self, module_id: ModuleId) -> Result<Vec<CheckedTranslationUnit>> {
        let ctx = CheckerContext { module_id };

        let mut units = vec![];

        for child_id in self.modules.children_for(module_id).iter() {
            units.extend(self.check_package(*child_id)?);
        }

        units.push(self.check_unit(&ctx)?);

        Ok(units)
    }

    fn declare_structs(&mut self, module_id: ModuleId) -> Result<()> {
        let ctx = CheckerContext { module_id };

        for child_id in self.modules.children_for(module_id).iter() {
            self.declare_structs(*child_id)?;
        }

        for s in &self.modules.unit_for(module_id).structs {
            self.add_struct_name(s, &ctx)?;
        }

        Ok(())
    }

    fn declare_interfaces(&mut self, module_id: ModuleId) -> Result<()> {
        let ctx = CheckerContext { module_id };

        for child_id in self.modules.children_for(module_id).iter() {
            self.declare_interfaces(*child_id)?;
        }

        for s in &self.modules.unit_for(module_id).interfaces {
            self.add_interface_name(s, &ctx)?;
        }

        Ok(())
    }

    fn create_module_ids(
        &mut self,
        module: ParsedModule,
        parent: Option<ModuleId>,
    ) -> Result<ModuleId> {
        let module_id = self.modules.allocate();

        let mut children = vec![];
        for child in module.children {
            children.push(self.create_module_ids(child, Some(module_id))?);
        }

        Ok(self.modules.write(
            module_id,
            Module {
                children: Rc::new(children),
                name: module.name,
                source: module.source,
                unit: Rc::new(module.unit),
                parent,
            },
        ))
    }

    fn check_unit(&mut self, ctx: &CheckerContext) -> Result<CheckedTranslationUnit> {
        self.scope.enter();

        let mut checked_procs = vec![];
        let unit = self.modules.unit_for(ctx.module_id);
        for proc in &unit.procs {
            if !proc.signature.type_params.is_empty() {
                continue;
            }

            checked_procs.push(self.typecheck_proc(proc, ctx)?);
        }

        for impl_block in &unit.impls {
            let for_type_id = self.types.force_find_with_generics(
                self.modules.source_for(ctx.module_id),
                ctx.module_id,
                &impl_block.tajp,
                &[],
            )?;

            for proc in &impl_block.procs {
                let proc_id = self
                    .implementations
                    .find_by_type_id_and_name(for_type_id, &proc.signature.ident, &self)
                    .expect("To exist");

                // FIXME: Impl procs should be generated some other way
                checked_procs.push(self.typecheck_proc_with_type_id(
                    proc,
                    self.procs.type_id_for(proc_id),
                    proc_id,
                    &[],
                    &HashMap::new(),
                    Some(for_type_id),
                    ctx,
                )?);
            }
        }

        self.scope.exit();

        Ok(CheckedTranslationUnit {
            procs: checked_procs,
        })
    }

    fn define_proc(
        &mut self,
        definition: &ProcDefinition,
        type_id: TypeId,
        is_impl: bool,
        ctx: &CheckerContext,
    ) -> Result<()> {
        let mut params: Vec<(Ident, TypeId)> = vec![];

        let source = self.modules.source_for(ctx.module_id);

        for (i, param) in definition.signature.params.iter().enumerate() {
            let (ident, tajp) = match param {
                crate::ast::parsed::ProcParam::This(span) => {
                    if !is_impl {
                        return Err(Error::ThisOnImplBlock {
                            src: self.modules.source_for(ctx.module_id).clone(),
                            span: *span,
                        });
                    }

                    if i != 0 {
                        return Err(Error::ThisNotFirst {
                            src: self.modules.source_for(ctx.module_id).clone(),
                            span: *span,
                        });
                    }

                    continue;
                }
                crate::ast::parsed::ProcParam::Normal { ident, tajp } => (ident, tajp),
            };

            if let Some(original) = params.iter().find(|p| p.0.name == ident.name) {
                return Err(Error::ProcParmNameCollision {
                    src: source.clone(),
                    original_span: original.0.span,
                    redefined_span: ident.span,
                    name: ident.name.clone(),
                });
            }

            let type_id = self.types.force_find_with_generics(
                source,
                ctx.module_id,
                &tajp,
                &definition.signature.type_params,
            )?;
            params.push((ident.clone(), type_id));
        }

        let return_type = self.types.force_find_with_generics(
            source,
            ctx.module_id,
            &definition.signature.return_type,
            &definition.signature.type_params,
        )?;

        self.types.define_proc(
            type_id,
            Type::Proc(ProcStructure {
                params: params.iter().map(|p| p.1).collect::<Vec<_>>(),
                return_type,
                variadic: false,
            }),
        );

        Ok(())
    }

    fn add_struct_name(&mut self, s: &StructDefinition, ctx: &CheckerContext) -> Result<TypeId> {
        // TODO: Verify that the name is unique
        let type_id = self
            .types
            .register_undefined_struct(ctx.module_id, &s.ident);
        Ok(type_id)
    }

    fn add_interface_name(&mut self, s: &Interface, ctx: &CheckerContext) -> Result<InterfaceId> {
        // TODO: Verify that the name is unique
        let type_id = self
            .implementations
            .register_interface(ctx.module_id, &s.ident);
        Ok(type_id)
    }

    fn add_proc(
        &mut self,
        ident: Ident,
        external: bool,
        generic: bool,
        link_name: Option<String>,
        ctx: &CheckerContext,
    ) -> Result<ProcId> {
        if let Some(span) = self.procs.find_original_span(ctx.module_id, &ident) {
            return Err(Error::ProcNameCollision {
                src: self.modules.source_for(ctx.module_id).clone(),
                original_span: span,
                redefined_span: ident.span,
                name: ident.name,
            });
        }

        let type_id = self.types.register_undefined_proc();
        Ok(self.procs.add(
            ctx.module_id,
            Proc {
                type_id,
                name: ident,
                module_id: ctx.module_id,
                external,
                generic,
                generic_instances: Vec::new(),
                link_name,
                has_this: false,
            },
        ))
    }

    fn add_extern_proc_name(
        &mut self,
        proc: &ExternProcDefinition,
        ctx: &CheckerContext,
    ) -> Result<ProcId> {
        let mut link_name = None;

        for attr in &proc.signature.attributes {
            match self.typecheck_builtin_as_proc_attribute(attr, ctx)? {
                CheckedBuiltin::LinkName(name) => {
                    if link_name.is_some() {
                        todo!("Warn about multiple link_name attributes");
                    }

                    link_name = Some(name)
                }
            }
        }

        self.add_proc(proc.signature.ident.clone(), true, false, link_name, ctx)
    }

    fn add_proc_name(&mut self, proc: &ProcDefinition, ctx: &CheckerContext) -> Result<ProcId> {
        self.add_proc(
            proc.signature.ident.clone(),
            false,
            !proc.signature.type_params.is_empty(),
            None,
            ctx,
        )
    }

    fn define_struct(
        &mut self,
        s: &StructDefinition,
        type_id: TypeId,
        ctx: &CheckerContext,
    ) -> Result<()> {
        let mut fields: Vec<IdentTypeId> = vec![];
        let source = self.modules.source_for(ctx.module_id);
        for field in &s.fields {
            if let Some(original) = fields.iter().find(|p| p.ident.name == field.0.name) {
                return Err(Error::StructFieldNameCollision {
                    src: source.clone(),
                    original_span: original.ident.span,
                    redefined_span: field.0.span,
                    name: field.0.name.clone(),
                });
            }

            let type_id = self.types.force_find_with_generics(
                source,
                ctx.module_id,
                &field.1,
                &s.type_params,
            )?;
            fields.push(IdentTypeId {
                ident: field.0.clone(),
                type_id,
            });
        }

        self.types.define_struct(
            type_id,
            Type::Struct(StructStructure {
                module_id: ctx.module_id,
                ident: s.ident.clone(),
                fields,
                generic_instances: vec![],
            }),
        );

        Ok(())
    }

    fn define_extern_proc(
        &mut self,
        definition: &ExternProcDefinition,
        type_id: TypeId,
        ctx: &CheckerContext,
    ) -> Result<()> {
        let source = self.modules.source_for(ctx.module_id);
        let mut params: Vec<TypeId> = vec![];
        for param_type in definition.signature.params.iter().map(|p| p.as_normal().1) {
            let type_id = self.types.force_find_with_generics(
                source,
                ctx.module_id,
                param_type,
                &definition.signature.type_params,
            )?;

            params.push(type_id);
        }

        let return_type = self.types.force_find_with_generics(
            source,
            ctx.module_id,
            &definition.signature.return_type,
            &definition.signature.type_params,
        )?;

        self.types.define_proc(
            type_id,
            Type::Proc(ProcStructure {
                params: params.clone(),
                return_type,
                variadic: definition.variadic,
            }),
        );

        Ok(())
    }

    fn typecheck_proc_with_type_id(
        &mut self,
        proc: &ProcDefinition,
        type_id: TypeId,
        proc_id: ProcId,
        generics: &[Ident],
        resolved_generics: &HashMap<GenericId, Spanned<TypeId>>,
        this_type_id: Option<TypeId>,
        ctx: &CheckerContext,
    ) -> Result<CheckedProc> {
        let definition = self.types.get_definition(type_id).as_proc();

        self.scope.enter();

        let mut ss = StackSlots::new();

        let mut params = vec![];
        for param in proc
            .signature
            .params
            .iter()
            .filter(|p| p.is_normal())
            .map(|p| p.as_normal().0)
            .zip(definition.params)
        {
            let stack_slot = ss.allocate(param.1);
            self.scope
                .add_to_scope(param.0, param.1, Some(stack_slot), None);
            params.push((param.0.name.clone(), stack_slot));
        }

        let return_type = (definition.return_type, proc.signature.return_type.span);

        let mut proc_ctx = ProcContext {
            return_type: return_type,
            stack_slots: &mut ss,
            generics,
            resolved_generics,
            this: this_type_id,
        };

        let body = self.typecheck_block(
            &proc.body,
            Some(return_type),
            return_type.0 != VOID_TYPE_ID,
            &mut BlockContext::new(),
            &mut proc_ctx,
            ctx,
        )?;

        if body.value.type_id != return_type.0 && !body.never {
            return Err(Error::ReturnValueDoesntMatch {
                src: self.modules.source_for(ctx.module_id).clone(),
                return_type_span: return_type.1,
                expr_span: if let Some(last) = &proc.body.last {
                    last.span
                } else {
                    proc.body.span
                },
                return_type: self.types.name_of(return_type.0, self),
                actual_type: self.types.name_of(body.value.type_id, self),
            });
        }

        self.scope.exit();

        Ok(CheckedProc {
            proc_id,
            body: body.value,
            params,
            return_type: definition.return_type,
            stack_slots: ss,
            has_this: this_type_id.is_some(),
        })
    }

    fn typecheck_proc(
        &mut self,
        proc: &ProcDefinition,
        ctx: &CheckerContext,
    ) -> Result<CheckedProc> {
        let scope_data = self
            .scope
            .find(&proc.signature.ident, ctx.module_id, self)
            .expect("Proc to have been added to scope");

        self.typecheck_proc_with_type_id(
            proc,
            scope_data.type_id,
            scope_data.proc_id.unwrap(),
            &[],
            &HashMap::new(),
            None,
            ctx,
        )
    }

    fn typecheck_block(
        &mut self,
        block: &Block,
        wanted: Option<(TypeId, SourceSpan)>,
        requires_value: bool,
        mut block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedBlock>> {
        block_ctx.enter();

        let mut checked_stmts = vec![];
        let mut has_encountered_never = false;
        for stmt in &block.stmts {
            let stmt = self.typecheck_stmt(stmt, &mut block_ctx, proc_ctx, ctx)?;
            has_encountered_never |= stmt.never;
            checked_stmts.push(stmt.value);
        }

        let last = if let Some(expr) = &block.last {
            let expr = self.typecheck_expr(
                expr,
                wanted,
                requires_value,
                false,
                &mut block_ctx,
                proc_ctx,
                ctx,
            )?;
            has_encountered_never |= expr.never;
            Some(expr.value)
        } else {
            if requires_value && !has_encountered_never {
                return Err(Error::BlockRequiresValue {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    span: block.span,
                });
            }

            None
        };

        let type_id = if let Some(last) = &last {
            last.type_id
        } else {
            VOID_TYPE_ID
        };

        let deferred = block_ctx.deferred.clone();
        block_ctx.exit();

        Ok(HasNever::new(
            CheckedBlock {
                stmts: checked_stmts,
                last,
                type_id,
                deferred,
            },
            has_encountered_never,
        ))
    }

    fn typecheck_stmt(
        &mut self,
        stmt: &Stmt,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedStmt>> {
        #[allow(unreachable_patterns)]
        match &stmt.kind {
            StmtKind::Return { value } => {
                self.typecheck_return_stmt(stmt.span, value, block_ctx, proc_ctx, ctx)
            }
            StmtKind::VariableDeclaration { name, value } => {
                self.typecheck_variable_declaration_stmt(name, value, block_ctx, proc_ctx, ctx)
            }
            StmtKind::Expr(expr) => {
                let expr =
                    self.typecheck_expr(expr, None, false, false, block_ctx, proc_ctx, ctx)?;
                Ok(HasNever::new(CheckedStmt::Expr(expr.value), expr.never))
            }
            StmtKind::While { condition, body } => {
                self.typecheck_while_stmt(condition, body, block_ctx, proc_ctx, ctx)
            }
            StmtKind::Defer { expr } => self.typecheck_defer_stmt(expr, block_ctx, proc_ctx, ctx),
            stmt => todo!("typecheck_stmt: {}", stmt),
        }
    }

    fn typecheck_return_stmt(
        &mut self,
        span: SourceSpan,
        value: &Option<Expr>,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedStmt>> {
        match value {
            Some(value) => {
                if proc_ctx.return_type.0 == VOID_TYPE_ID {
                    return Err(Error::ReturnShouldntHaveValue {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: value.span,
                    });
                }

                let expr = self.typecheck_expr(
                    value,
                    Some(proc_ctx.return_type),
                    true,
                    false,
                    block_ctx,
                    proc_ctx,
                    ctx,
                )?;

                if expr.value.type_id != proc_ctx.return_type.0 && !expr.never {
                    return Err(Error::ReturnValueDoesntMatch {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        return_type_span: proc_ctx.return_type.1,
                        expr_span: value.span,
                        return_type: self.types.name_of(proc_ctx.return_type.0, self),
                        actual_type: self.types.name_of(expr.value.type_id, self),
                    });
                }
                Ok(HasNever::new(
                    CheckedStmt::Return {
                        value: Some(expr.value),
                    },
                    true,
                ))
            }
            None => {
                if proc_ctx.return_type.0 != VOID_TYPE_ID {
                    return Err(Error::ReturnShouldHaveValue {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span,
                        name: self.types.name_of(proc_ctx.return_type.0, self),
                    });
                }

                Ok(HasNever::new(CheckedStmt::Return { value: None }, true))
            }
        }
    }

    fn typecheck_variable_declaration_stmt(
        &mut self,
        ident: &Ident,
        expr: &Expr,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedStmt>> {
        let expr = self.typecheck_expr(expr, None, true, false, block_ctx, proc_ctx, ctx)?;
        let stack_slot = proc_ctx.stack_slots.allocate(expr.value.type_id);
        self.scope
            .add_to_scope(ident, expr.value.type_id, Some(stack_slot), None);

        Ok(HasNever::new(
            CheckedStmt::VariableDeclaration {
                stack_slot,
                value: expr.value,
            },
            expr.never,
        ))
    }

    fn typecheck_while_stmt(
        &mut self,
        condition: &Expr,
        body: &Block,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedStmt>> {
        let checked_condition = self.typecheck_expr(
            condition,
            Some((BOOL_TYPE_ID, (0..0).into())),
            false,
            false,
            block_ctx,
            proc_ctx,
            ctx,
        )?;

        if checked_condition.value.type_id != BOOL_TYPE_ID && !checked_condition.never {
            return Err(Error::ExpectedButGot {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: condition.span,
                expected: "bool".to_string(),
                got: self.types.name_of(checked_condition.value.type_id, self),
            });
        }

        let checked_body = self.typecheck_block(body, None, false, block_ctx, proc_ctx, ctx)?;

        Ok(HasNever::new(
            CheckedStmt::While {
                condition: checked_condition.value,
                body: checked_body.value,
            },
            checked_condition.never || checked_body.never,
        ))
    }

    fn typecheck_defer_stmt(
        &mut self,
        expr: &Expr,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedStmt>> {
        let checked_expr =
            self.typecheck_expr(expr, None, false, false, block_ctx, proc_ctx, ctx)?;

        block_ctx.push(checked_expr.value);

        Ok(HasNever::new(CheckedStmt::None, false))
    }

    fn typecheck_expr(
        &mut self,
        expr: &Expr,
        wanted: Option<(TypeId, SourceSpan)>,
        requires_value: bool,
        for_call: bool,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        #[allow(unreachable_patterns)]
        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let scope_data = self.scope.force_find(
                    self.modules.source_for(ctx.module_id),
                    ident,
                    ctx.module_id,
                    self,
                )?;

                Ok(HasNever::new(
                    CheckedExpr {
                        type_id: scope_data.type_id,
                        kind: if let Some(stack_slot) = scope_data.stack_slot {
                            ast::CheckedExprKind::StackValue(stack_slot)
                        } else {
                            if let Some(proc_id) = scope_data.proc_id {
                                ast::CheckedExprKind::Proc { lhs: None, proc_id }
                            } else {
                                ast::CheckedExprKind::Identifier(ident.name.clone())
                            }
                        },
                        lvalue: true,
                    },
                    false,
                ))
            }
            ExprKind::Number(value) => {
                if let Some(wanted) = wanted {
                    match wanted.0 {
                        I8_TYPE_ID => {
                            parse_number!(value, i8, I8_TYPE_ID, expr, ctx, self)
                        }
                        U8_TYPE_ID => {
                            parse_number!(value, u8, U8_TYPE_ID, expr, ctx, self)
                        }
                        I16_TYPE_ID => {
                            parse_number!(value, i16, I16_TYPE_ID, expr, ctx, self)
                        }
                        U16_TYPE_ID => {
                            parse_number!(value, u16, U16_TYPE_ID, expr, ctx, self)
                        }
                        I32_TYPE_ID => {
                            parse_number!(value, i32, I32_TYPE_ID, expr, ctx, self)
                        }
                        U32_TYPE_ID => {
                            parse_number!(value, u32, U32_TYPE_ID, expr, ctx, self)
                        }
                        I64_TYPE_ID | ISIZE_TYPE_ID => {
                            parse_number!(value, i64, wanted.0, expr, ctx, self)
                        }
                        U64_TYPE_ID | USIZE_TYPE_ID => {
                            parse_number!(value, u64, wanted.0, expr, ctx, self)
                        }
                        F32_TYPE_ID => {
                            parse_float!(value, f32, F32_TYPE_ID, expr, ctx, self)
                        }
                        F64_TYPE_ID => {
                            parse_float!(value, f64, F64_TYPE_ID, expr, ctx, self)
                        }
                        _ => {
                            if value.contains(".") {
                                parse_float!(value, f32, F32_TYPE_ID, expr, ctx, self)
                            } else {
                                parse_number!(value, i32, I32_TYPE_ID, expr, ctx, self)
                            }
                        }
                    }
                } else {
                    if value.contains(".") {
                        parse_float!(value, f32, F32_TYPE_ID, expr, ctx, self)
                    } else {
                        parse_number!(value, i32, I32_TYPE_ID, expr, ctx, self)
                    }
                }
            }
            ExprKind::BinOp { lhs, op, rhs } => {
                self.typecheck_binop_expr(lhs, *op, rhs, wanted, block_ctx, proc_ctx, ctx)
            }
            ExprKind::String(value) => Ok(HasNever::new(
                CheckedExpr {
                    type_id: STRING_TYPE_ID,
                    kind: ast::CheckedExprKind::String(value.clone()),
                    lvalue: false,
                },
                false,
            )),
            ExprKind::Call {
                expr,
                params,
                generic_params,
            } => self.typecheck_call_expr(
                expr,
                params,
                generic_params,
                wanted,
                block_ctx,
                proc_ctx,
                ctx,
            ),
            ExprKind::Bool(value) => Ok(HasNever::new(
                CheckedExpr {
                    type_id: BOOL_TYPE_ID,
                    kind: ast::CheckedExprKind::Number(if *value { 1 } else { 0 }),
                    lvalue: false,
                },
                false,
            )),
            ExprKind::Builtin(builtin) => Ok(HasNever::new(
                self.typecheck_builtin_expr(expr, builtin, block_ctx, proc_ctx, ctx)?,
                false,
            )),
            ExprKind::StructInstantiation { name, members } => self
                .typecheck_struct_instantiation_expr(expr, name, members, block_ctx, proc_ctx, ctx),
            ExprKind::MemberAccess { lhs, member } => Ok(HasNever::new(
                self.typecheck_member_access_expr(lhs, member, for_call, block_ctx, proc_ctx, ctx)?,
                false,
            )),
            ExprKind::If {
                condition,
                true_block,
                false_block,
            } => self.typecheck_if_expr(
                condition,
                true_block,
                false_block,
                requires_value,
                block_ctx,
                proc_ctx,
                ctx,
            ),
            ExprKind::Cast { lhs, tajp } => {
                self.typecheck_cast_expr(lhs, tajp, expr.span, block_ctx, proc_ctx, ctx)
            }
            ExprKind::Assignment { lhs, rhs } => {
                self.typecheck_assignment_expr(lhs, rhs, block_ctx, proc_ctx, ctx)
            }
            ExprKind::AddressOf(expr) => {
                self.typecheck_address_of_expr(expr, block_ctx, proc_ctx, ctx)
            }
            ExprKind::Block(block) => {
                let block =
                    self.typecheck_block(block, wanted, requires_value, block_ctx, proc_ctx, ctx)?;
                Ok(HasNever::new(
                    CheckedExpr {
                        type_id: block.value.type_id,
                        lvalue: false,
                        kind: CheckedExprKind::Block(Box::new(block.value)),
                    },
                    block.never,
                ))
            }
            ExprKind::Deref(expr) => self.typecheck_deref_expr(expr, block_ctx, proc_ctx, ctx),
            ExprKind::ArrayInstantiation(exprs) => {
                self.typecheck_array_instantiation_expr(exprs, block_ctx, proc_ctx, ctx)
            }
            ExprKind::ArrayAccess { lhs, index } => {
                self.typecheck_array_access_expr(lhs, index, block_ctx, proc_ctx, ctx)
            }
            ExprKind::This => {
                if proc_ctx.this.is_none() {
                    return Err(Error::ThisOutsideImplProc {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: expr.span,
                    });
                }

                Ok(HasNever::new(
                    CheckedExpr {
                        type_id: proc_ctx.this.unwrap(),
                        // TODO: Is this lvalue??
                        lvalue: false,
                        kind: CheckedExprKind::This,
                    },
                    false,
                ))
            }
            kind => todo!("typecheck_expr: {}", kind),
        }
    }

    fn typecheck_binop_expr(
        &mut self,
        lhs: &Expr,
        op: BinOp,
        rhs: &Expr,
        wanted: Option<(TypeId, SourceSpan)>,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_lhs =
            self.typecheck_expr(lhs, wanted, true, false, block_ctx, proc_ctx, ctx)?;
        let checked_rhs =
            self.typecheck_expr(rhs, wanted, true, false, block_ctx, proc_ctx, ctx)?;

        let has_encountered_never = checked_lhs.never || checked_rhs.never;

        if checked_lhs.value.type_id != checked_rhs.value.type_id && !has_encountered_never {
            return Err(Error::BinOpSidesMismatch {
                src: self.modules.source_for(ctx.module_id).clone(),
                lhs_span: lhs.span,
                rhs_span: rhs.span,
                lhs_type_name: self.types.name_of(checked_lhs.value.type_id, self),
                rhs_type_name: self.types.name_of(checked_rhs.value.type_id, self),
            });
        }

        Ok(HasNever::new(
            match op {
                BinOp::Divide | BinOp::Multiply | BinOp::Add | BinOp::Subtract => self
                    .typecheck_other_binop_expr(
                        lhs,
                        checked_lhs.value,
                        op,
                        rhs,
                        checked_rhs.value,
                        ctx,
                    )?,
                BinOp::Equal
                | BinOp::NotEqual
                | BinOp::LessThan
                | BinOp::LessThanOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanOrEqual => self.typecheck_boolable_binop_expr(
                    lhs,
                    checked_lhs.value,
                    op,
                    rhs,
                    checked_rhs.value,
                    ctx,
                )?,
            },
            has_encountered_never,
        ))
    }

    fn typecheck_other_binop_expr(
        &mut self,
        lhs: &Expr,
        checked_lhs: CheckedExpr,
        op: BinOp,
        rhs: &Expr,
        checked_rhs: CheckedExpr,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        self.expect_number_or_ptr(&checked_lhs, lhs.span, ctx)?;
        self.expect_number_or_ptr(&checked_rhs, rhs.span, ctx)?;

        Ok(CheckedExpr {
            type_id: checked_lhs.type_id,
            kind: ast::CheckedExprKind::BinOp {
                lhs: Box::new(checked_lhs),
                op,
                rhs: Box::new(checked_rhs),
            },
            lvalue: false,
        })
    }

    fn typecheck_boolable_binop_expr(
        &mut self,
        lhs: &Expr,
        checked_lhs: CheckedExpr,
        op: BinOp,
        rhs: &Expr,
        checked_rhs: CheckedExpr,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        self.expect_number_or_ptr(&checked_lhs, lhs.span, ctx)?;
        self.expect_number_or_ptr(&checked_rhs, rhs.span, ctx)?;

        Ok(CheckedExpr {
            type_id: BOOL_TYPE_ID,
            kind: ast::CheckedExprKind::BinOp {
                lhs: Box::new(checked_lhs),
                op,
                rhs: Box::new(checked_rhs),
            },
            lvalue: false,
        })
    }

    fn typecheck_call_expr(
        &mut self,
        expr: &Expr,
        params: &[Expr],
        generic_params: &[crate::ast::tajp::Type],
        wanted: Option<(TypeId, SourceSpan)>,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_expr =
            self.typecheck_expr(expr, wanted, true, true, block_ctx, proc_ctx, ctx)?;

        let (mut proc_id, lhs) = match checked_expr.value.kind {
            CheckedExprKind::Proc { proc_id, lhs } => (proc_id, lhs),
            _ => todo!("Indirect calls"),
        };

        let mut type_id = self.procs.type_id_for(proc_id);

        let proc_type = self.types.get_definition(type_id).as_proc();

        let mut resolved_generics = HashMap::new();

        for (id, generic_param) in generic_params.iter().enumerate() {
            resolved_generics.insert(
                GenericId(id),
                Spanned::new(
                    self.types.force_find_with_resolved_generics(
                        self.modules.source_for(ctx.module_id),
                        ctx.module_id,
                        generic_param,
                        proc_ctx.generics,
                        proc_ctx.resolved_generics,
                    )?,
                    generic_param.span,
                ),
            );
        }

        if params.len() < proc_type.params.len()
            || (params.len() > proc_type.params.len() && !proc_type.variadic)
        {
            return Err(Error::ProcCallParamCountMismatch {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: expr.span,
                expected: proc_type.params.len(),
                got: params.len(),
                variadic: proc_type.variadic,
            });
        }

        let mut params = params.to_owned();
        let non_variadic_params = params
            .drain(0..proc_type.params.len())
            .zip(&proc_type.params);

        let mut checked_params = vec![];
        let mut has_encountered_never = false;
        for (param, expected_type) in non_variadic_params {
            let wanted = if self.types.is_generic(*expected_type) {
                // TODO: Check if the GenericId has already been confirmed
                None
            } else {
                Some((*expected_type, (0..0).into()))
            };

            // TODO: Get the location of the suspected span or allow the span to be optional
            let checked_expr =
                self.typecheck_expr(&param, wanted, true, false, block_ctx, proc_ctx, ctx)?;

            self.types.infer_generic_types(
                self.modules.source_for(ctx.module_id),
                param.span,
                *expected_type,
                checked_expr.value.type_id,
                &mut resolved_generics,
                self,
            )?;

            has_encountered_never |= checked_expr.never;

            let expected_type = self
                .types
                .resolve_generic_type(*expected_type, &resolved_generics)?;

            if checked_expr.value.type_id != expected_type && !checked_expr.never {
                return Err(Error::ProcCallParamTypeMismatch {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    span: param.span,
                    expected: self.types.name_of(expected_type, self),
                    got: self.types.name_of(checked_expr.value.type_id, self),
                });
            }

            checked_params.push(checked_expr.value);
        }

        for param in &params {
            let checked_expr =
                self.typecheck_expr(param, None, true, false, block_ctx, proc_ctx, ctx)?;

            has_encountered_never |= checked_expr.never;

            checked_params.push(checked_expr.value);
        }

        let return_type_id = self
            .types
            .resolve_generic_type(proc_type.return_type, &resolved_generics)?;

        if self.procs.is_generic(proc_id) {
            let mut generic_types = resolved_generics.iter().collect::<Vec<_>>();
            generic_types.sort_by(|a, b| a.0.cmp(b.0));
            let generic_types: Vec<TypeId> = generic_types.into_iter().map(|t| t.1.value).collect();

            if let Some((nongeneric_proc_id, _)) = self
                .instantiated_generic_procs
                .get(&(proc_id, generic_types.clone()))
            {
                proc_id = *nongeneric_proc_id;
            } else {
                type_id = self
                    .types
                    .resolve_generic_type(type_id, &resolved_generics)?;

                let proc = self.generic_procs.get(&proc_id).unwrap().clone();
                let nongeneric_proc_id = proc_id;
                proc_id = self
                    .procs
                    .add_generic(proc_id, type_id, generic_types.clone());

                let checked = self.typecheck_proc_with_type_id(
                    &proc,
                    type_id,
                    proc_id,
                    &proc.signature.type_params,
                    &resolved_generics,
                    None,
                    &CheckerContext {
                        module_id: self.procs.module_for(nongeneric_proc_id),
                    },
                )?;

                self.instantiated_generic_procs
                    .insert((nongeneric_proc_id, generic_types), (proc_id, checked));
            }
        }

        if self.procs.has_this_for(proc_id) {
            let lhs = *lhs.unwrap();
            let inner_type_id = lhs.type_id;

            checked_params.insert(
                0,
                CheckedExpr {
                    type_id: self.types.register_type(Type::Ptr(inner_type_id)),
                    lvalue: false,
                    kind: self.get_ptr_to_checked_expr(lhs, proc_ctx),
                },
            );
        }

        Ok(HasNever::new(
            CheckedExpr {
                type_id: return_type_id,
                kind: CheckedExprKind::DirectCall {
                    proc_id,
                    params: checked_params,
                    variadic_after: if proc_type.variadic {
                        Some(proc_type.params.len() as u64)
                    } else {
                        None
                    },
                    stack_slot: proc_ctx.stack_slots.allocate(return_type_id),
                },
                lvalue: true,
            },
            has_encountered_never || proc_type.return_type == NEVER_TYPE_ID,
        ))
    }

    fn typecheck_builtin_expr(
        &mut self,
        expr: &Expr,
        Builtin {
            name,
            params,
            generic_params,
            span: _,
        }: &Builtin,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        match name.as_str() {
            "type_name" => {
                let type_id = if params.len() == 1 {
                    let checked_param = self
                        .typecheck_expr(&params[0], None, false, false, block_ctx, proc_ctx, ctx)?;
                    checked_param.value.type_id
                } else if generic_params.len() == 1 {
                    self.types.force_find_with_resolved_generics(
                        self.modules.source_for(ctx.module_id),
                        ctx.module_id,
                        &generic_params[0],
                        proc_ctx.generics,
                        proc_ctx.resolved_generics,
                    )?
                } else {
                    return Err(Error::BuiltinGenericOrParamCountMismatch {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: expr.span,
                        name: name.to_string(),

                        expected_generic: 1,
                        got_generic: generic_params.len(),
                        variadic_generic: true,

                        expected_params: 1,
                        got_params: params.len(),
                        variadic_params: false,
                    });
                };

                Ok(CheckedExpr {
                    type_id: STRING_TYPE_ID,
                    kind: CheckedExprKind::String(self.types.name_of(type_id, self)),
                    lvalue: false,
                })
            }
            "len" => {
                if params.len() != 1 {
                    return Err(Error::BuiltinParamCountMismatch {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: expr.span,
                        name: name.to_string(),
                        expected: 1,
                        got: params.len(),
                        variadic: false,
                    });
                }

                let checked_param =
                    self.typecheck_expr(&params[0], None, false, false, block_ctx, proc_ctx, ctx)?;

                let size = match self.types.get_definition(checked_param.value.type_id) {
                    Type::Array(_, size) => size,
                    _ => {
                        return Err(Error::ExpectedButGot {
                            src: self.modules.source_for(ctx.module_id).clone(),
                            span: params[0].span,
                            expected: "[_]any".to_string(),
                            got: self.types.name_of(checked_param.value.type_id, self),
                        });
                    }
                };

                Ok(CheckedExpr {
                    type_id: USIZE_TYPE_ID,
                    kind: CheckedExprKind::Number(size as u64),
                    lvalue: false,
                })
            }
            "size_of" => {
                if generic_params.len() != 1 {
                    return Err(Error::BuiltinParamCountMismatch {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: expr.span,
                        name: name.to_string(),
                        expected: 1,
                        got: generic_params.len(),
                        variadic: false,
                    });
                }

                let t = self.types.force_find_with_resolved_generics(
                    self.modules.source_for(ctx.module_id),
                    ctx.module_id,
                    &generic_params[0],
                    proc_ctx.generics,
                    proc_ctx.resolved_generics,
                )?;

                let size = self.types.memory_layout_of(t, self).size;

                Ok(CheckedExpr {
                    type_id: U32_TYPE_ID,
                    kind: CheckedExprKind::Number(size as u64),
                    lvalue: false,
                })
            }
            name => Err(Error::UnknownBuiltin {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: expr.span,
                name: name.to_string(),
            }),
        }
    }

    fn typecheck_builtin_as_proc_attribute(
        &self,
        Builtin {
            name,
            params,
            span,
            generic_params: _,
        }: &Builtin,
        ctx: &CheckerContext,
    ) -> Result<CheckedBuiltin> {
        match name.as_str() {
            "link_name" => {
                if params.len() != 1 {
                    return Err(Error::BuiltinParamCountMismatch {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: *span,
                        name: name.to_string(),
                        expected: 1,
                        got: params.len(),
                        variadic: false,
                    });
                }

                let param = &params[0];

                match &param.kind {
                    ExprKind::String(name) => Ok(CheckedBuiltin::LinkName(name.clone())),
                    _ => Err(Error::BuiltinExpectsArgAtToBe {
                        src: self.modules.source_for(ctx.module_id).clone(),
                        span: *span,
                        name: "link_name",
                        arg_type: "string",
                        arg_index: 0,
                    }),
                }
            }
            name => Err(Error::UnknownBuiltin {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: *span,
                name: name.to_string(),
            }),
        }
    }

    fn typecheck_struct_instantiation_expr(
        &mut self,
        expr: &Expr,
        name: &Ident,
        fields: &[(Ident, Expr)],
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let struct_type_id = self.types.force_find_by_name_with_generics(
            self.modules.source_for(ctx.module_id),
            ctx.module_id,
            name,
            &[],
        )?;

        let definition = self.types.get_definition(struct_type_id);
        if !definition.is_struct() {
            return Err(Error::StructInstantiationOnNonStruct {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: name.span,
            });
        }

        let s = definition.as_struct();

        let mut checked_fields: Vec<(Ident, CheckedExpr)> = vec![];

        let mut has_encountered_never = false;

        let mut resolved_generics = HashMap::new();

        for field in fields {
            let defined_field = s.fields.iter().find(|f| f.ident == field.0);
            let Some(defined_field) = defined_field else {
                return Err(Error::StructInstantiationFieldDoesntExist {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    span: field.0.span,
                    struct_name: name.name.clone(),
                    field_name: field.0.name.clone(),
                });
            };

            if let Some(existing_field) = checked_fields.iter().find(|f| f.0 == field.0) {
                return Err(Error::StructInstantiationFieldAlreadyDeclared {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    original_span: existing_field.0.span,
                    redefined_span: field.0.span,
                    field_name: field.0.name.clone(),
                });
            }

            let wanted = if self.types.is_generic(defined_field.type_id) {
                // TODO: Check if the GenericId has already been confirmed
                None
            } else {
                Some((defined_field.type_id, (0..0).into()))
            };

            let checked_expr =
                self.typecheck_expr(&field.1, wanted, true, false, block_ctx, proc_ctx, ctx)?;

            self.types.infer_generic_types(
                self.modules.source_for(ctx.module_id),
                field.1.span,
                defined_field.type_id,
                checked_expr.value.type_id,
                &mut resolved_generics,
                self,
            )?;

            has_encountered_never |= checked_expr.never;

            if checked_expr.value.type_id
                != self
                    .types
                    .resolve_generic_type(defined_field.type_id, &resolved_generics)?
                && !checked_expr.never
            {
                return Err(Error::StructInstantiationFieldTypeMismatch {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    span: field.1.span,
                    struct_name: name.name.clone(),
                    field_name: field.0.name.clone(),
                    expected: self.types.name_of(defined_field.type_id, self),
                    got: self.types.name_of(checked_expr.value.type_id, self),
                });
            }

            checked_fields.push((field.0.clone(), checked_expr.value));
        }

        if checked_fields.len() != s.fields.len() {
            let mut missing_fields = vec![];

            for known_fields in &s.fields {
                if !checked_fields.iter().any(|c| c.0 == known_fields.ident) {
                    missing_fields.push(format!("'{}'", known_fields.ident.name));
                }
            }

            return Err(Error::StructInstantiationMissingFields {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: expr.span,
                struct_name: name.name.clone(),
                fields: string_join_with_and(
                    missing_fields
                        .iter()
                        .map(|f| f.as_str())
                        .collect::<Vec<_>>()
                        .as_slice(),
                ),
                multiple: missing_fields.len() > 1,
            });
        }

        let struct_type_id = self
            .types
            .resolve_generic_type(struct_type_id, &resolved_generics)?;

        Ok(HasNever::new(
            CheckedExpr {
                type_id: struct_type_id,
                kind: CheckedExprKind::StructInstantiation {
                    name: name.name.clone(),
                    fields: checked_fields
                        .drain(0..)
                        .map(|f| (f.0.name.clone(), f.1))
                        .collect(),
                    stack_slot: proc_ctx.stack_slots.allocate(struct_type_id),
                },
                lvalue: false,
            },
            has_encountered_never,
        ))
    }

    fn typecheck_member_access_expr(
        &mut self,
        lhs: &Expr,
        member: &Ident,
        for_call: bool,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        let checked_lhs = self.typecheck_expr(lhs, None, true, false, block_ctx, proc_ctx, ctx)?;

        if for_call {
            // Prioritise impl procs over members
            if let Some(expr) = self.typecheck_member_access_expr_for_procs(&checked_lhs, member) {
                return Ok(expr);
            }

            if let Some(expr) =
                self.typecheck_member_access_expr_for_struct_members(&checked_lhs, member)
            {
                return Ok(expr);
            }
        } else {
            // Prioritise memebers over impl procs
            if let Some(expr) =
                self.typecheck_member_access_expr_for_struct_members(&checked_lhs, member)
            {
                return Ok(expr);
            }

            if let Some(expr) = self.typecheck_member_access_expr_for_procs(&checked_lhs, member) {
                return Ok(expr);
            }
        }

        Err(Error::NoImplProcOrMemberForType {
            src: self.modules.source_for(ctx.module_id).clone(),
            span: member.span,
            name: member.name.clone(),
            type_name: self.types.name_of(checked_lhs.value.type_id, self),
        })
    }

    fn typecheck_member_access_expr_for_procs(
        &mut self,
        checked_lhs: &HasNever<CheckedExpr>,
        member: &Ident,
    ) -> Option<CheckedExpr> {
        if let Some(proc_id) =
            self.implementations
                .find_by_type_id_and_name(checked_lhs.value.type_id, member, self)
        {
            Some(CheckedExpr {
                type_id: self.procs.type_id_for(proc_id),
                kind: CheckedExprKind::Proc {
                    lhs: Some(Box::new(checked_lhs.value.clone())),
                    proc_id: proc_id,
                },
                lvalue: false,
            })
        } else {
            None
        }
    }

    fn typecheck_member_access_expr_for_struct_members(
        &mut self,
        checked_lhs: &HasNever<CheckedExpr>,
        member: &Ident,
    ) -> Option<CheckedExpr> {
        let definition = self.types.get_definition(checked_lhs.value.type_id);
        if !definition.is_struct() {
            return None;
        }

        let definition = definition.as_struct();

        let Some(field) = definition.fields.iter().find(|f| f.ident == *member) else {
            return None;
        };

        // A member expression is an lvalue if the lhs is also an lvalue
        // or the lhs is a "this" expr
        let lvalue = checked_lhs.value.lvalue || checked_lhs.value.kind.is_this();

        Some(CheckedExpr {
            type_id: field.type_id,
            kind: CheckedExprKind::MemberAccess {
                lhs: Box::new(checked_lhs.value.clone()),
                name: field.ident.name.clone(),
            },
            lvalue,
        })
    }

    // TODO: Make it act as an expression,
    //       Currently it's checked as a statement
    fn typecheck_if_expr(
        &mut self,
        condition: &Expr,
        true_block: &Block,
        false_block: &Block,
        requires_value: bool,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_condition = self.typecheck_expr(
            condition,
            Some((BOOL_TYPE_ID, (0..0).into())),
            true,
            false,
            block_ctx,
            proc_ctx,
            ctx,
        )?;
        let checked_true_block =
            self.typecheck_block(true_block, None, requires_value, block_ctx, proc_ctx, ctx)?;

        let checked_false_block = self.typecheck_block(
            false_block,
            Some((checked_true_block.value.type_id, true_block.span)),
            requires_value,
            block_ctx,
            proc_ctx,
            ctx,
        )?;

        if checked_condition.value.type_id != BOOL_TYPE_ID && !checked_condition.never {
            return Err(Error::ExpectedButGot {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: condition.span,
                expected: "bool".to_string(),
                got: self.types.name_of(checked_condition.value.type_id, self),
            });
        }

        if requires_value
            && checked_true_block.value.type_id != checked_false_block.value.type_id
            && !(checked_true_block.never || checked_false_block.never)
        {
            return Err(Error::IfTypeMismatch {
                src: self.modules.source_for(ctx.module_id).clone(),
                true_block_span: true_block.span,
                true_block_type: self.types.name_of(checked_true_block.value.type_id, self),
                false_block_span: false_block.span,
                false_block_type: self.types.name_of(checked_false_block.value.type_id, self),
            });
        }

        let type_id = if requires_value {
            checked_true_block.value.type_id
        } else {
            VOID_TYPE_ID
        };

        Ok(HasNever::new(
            CheckedExpr {
                kind: CheckedExprKind::If {
                    stack_slot: proc_ctx.stack_slots.allocate(type_id),
                    condition: Box::new(checked_condition.value),
                    true_block: Box::new(checked_true_block.value),
                    false_block: Box::new(checked_false_block.value),
                },
                type_id,
                lvalue: false,
            },
            checked_condition.never || (checked_true_block.never && checked_false_block.never),
        ))
    }

    fn typecheck_cast_expr(
        &mut self,
        lhs: &Expr,
        t: &crate::ast::tajp::Type,
        span: SourceSpan,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let wanted = self.types.force_find_with_resolved_generics(
            self.modules.source_for(ctx.module_id),
            ctx.module_id,
            t,
            proc_ctx.generics,
            proc_ctx.resolved_generics,
        )?;
        let checked_lhs = self.typecheck_expr(
            lhs,
            Some((wanted, t.span)),
            true,
            false,
            block_ctx,
            proc_ctx,
            ctx,
        )?;

        if checked_lhs.never {
            return Ok(checked_lhs);
        }

        let kind = match (checked_lhs.value.type_id, wanted) {
            // Change sign, or bool to i32
            (U8_TYPE_ID, I8_TYPE_ID)
            | (I8_TYPE_ID, U8_TYPE_ID)
            | (U16_TYPE_ID, I16_TYPE_ID)
            | (I16_TYPE_ID, U16_TYPE_ID)
            | (U32_TYPE_ID, I32_TYPE_ID)
            | (I32_TYPE_ID, U32_TYPE_ID)
            | (U64_TYPE_ID, I64_TYPE_ID)
            | (I64_TYPE_ID, U64_TYPE_ID)
            | (USIZE_TYPE_ID, ISIZE_TYPE_ID)
            | (ISIZE_TYPE_ID, USIZE_TYPE_ID)
            | (BOOL_TYPE_ID, I32_TYPE_ID)
            | (BOOL_TYPE_ID, U32_TYPE_ID) => checked_lhs.value.kind,
            // Integers to bool
            (U8_TYPE_ID, BOOL_TYPE_ID)
            | (I8_TYPE_ID, BOOL_TYPE_ID)
            | (U16_TYPE_ID, BOOL_TYPE_ID)
            | (I16_TYPE_ID, BOOL_TYPE_ID)
            | (U32_TYPE_ID, BOOL_TYPE_ID)
            | (I32_TYPE_ID, BOOL_TYPE_ID)
            | (U64_TYPE_ID, BOOL_TYPE_ID)
            | (I64_TYPE_ID, BOOL_TYPE_ID)
            | (USIZE_TYPE_ID, BOOL_TYPE_ID)
            | (ISIZE_TYPE_ID, BOOL_TYPE_ID) => CheckedExprKind::BinOp {
                lhs: Box::new(checked_lhs.value),
                op: BinOp::NotEqual,
                rhs: Box::new(CheckedExpr {
                    type_id: BOOL_TYPE_ID,
                    kind: CheckedExprKind::Number(0),
                    lvalue: false,
                }),
            },
            // Float and Doubles
            (F64_TYPE_ID, F32_TYPE_ID) => CheckedExprKind::F64ToF32(Box::new(checked_lhs.value)),
            (F32_TYPE_ID, F64_TYPE_ID) => CheckedExprKind::F32ToF64(Box::new(checked_lhs.value)),
            (F32_TYPE_ID, I8_TYPE_ID)
            | (F32_TYPE_ID, I16_TYPE_ID)
            | (F32_TYPE_ID, I32_TYPE_ID)
            | (F32_TYPE_ID, I64_TYPE_ID)
            | (F32_TYPE_ID, ISIZE_TYPE_ID) => {
                CheckedExprKind::F32ToSigned(Box::new(checked_lhs.value))
            }
            (F32_TYPE_ID, U8_TYPE_ID)
            | (F32_TYPE_ID, U16_TYPE_ID)
            | (F32_TYPE_ID, U32_TYPE_ID)
            | (F32_TYPE_ID, U64_TYPE_ID)
            | (F32_TYPE_ID, USIZE_TYPE_ID) => {
                CheckedExprKind::F32ToUnsigned(Box::new(checked_lhs.value))
            }
            (F64_TYPE_ID, I8_TYPE_ID)
            | (F64_TYPE_ID, I16_TYPE_ID)
            | (F64_TYPE_ID, I32_TYPE_ID)
            | (F64_TYPE_ID, I64_TYPE_ID)
            | (F64_TYPE_ID, ISIZE_TYPE_ID) => {
                CheckedExprKind::F64ToSigned(Box::new(checked_lhs.value))
            }
            (F64_TYPE_ID, U8_TYPE_ID)
            | (F64_TYPE_ID, U16_TYPE_ID)
            | (F64_TYPE_ID, U32_TYPE_ID)
            | (F64_TYPE_ID, U64_TYPE_ID)
            | (F64_TYPE_ID, USIZE_TYPE_ID) => {
                CheckedExprKind::F64ToUnsigned(Box::new(checked_lhs.value))
            }
            (I32_TYPE_ID, F32_TYPE_ID | F64_TYPE_ID) => {
                CheckedExprKind::I32ToFloat(Box::new(checked_lhs.value))
            }
            (U32_TYPE_ID, F32_TYPE_ID | F64_TYPE_ID) => {
                CheckedExprKind::U32ToFloat(Box::new(checked_lhs.value))
            }
            (I64_TYPE_ID, F32_TYPE_ID | F64_TYPE_ID) => {
                CheckedExprKind::I64ToFloat(Box::new(checked_lhs.value))
            }
            (U64_TYPE_ID, F32_TYPE_ID | F64_TYPE_ID) => {
                CheckedExprKind::U64ToFloat(Box::new(checked_lhs.value))
            }
            // Sign extend
            (I8_TYPE_ID | U8_TYPE_ID, I16_TYPE_ID | I32_TYPE_ID | I64_TYPE_ID | ISIZE_TYPE_ID) => {
                CheckedExprKind::SignExtend8(Box::new(checked_lhs.value))
            }
            (I16_TYPE_ID | U16_TYPE_ID, I32_TYPE_ID | I64_TYPE_ID | ISIZE_TYPE_ID) => {
                CheckedExprKind::SignExtend16(Box::new(checked_lhs.value))
            }
            (I32_TYPE_ID | U32_TYPE_ID, I64_TYPE_ID | ISIZE_TYPE_ID) => {
                CheckedExprKind::SignExtend32(Box::new(checked_lhs.value))
            }
            // Zero extend
            (I8_TYPE_ID | U8_TYPE_ID, U16_TYPE_ID | U32_TYPE_ID | U64_TYPE_ID | USIZE_TYPE_ID) => {
                CheckedExprKind::ZeroExtend8(Box::new(checked_lhs.value))
            }
            (I16_TYPE_ID | U16_TYPE_ID, U32_TYPE_ID | U64_TYPE_ID | USIZE_TYPE_ID) => {
                CheckedExprKind::ZeroExtend16(Box::new(checked_lhs.value))
            }
            (I32_TYPE_ID | U32_TYPE_ID, U64_TYPE_ID | USIZE_TYPE_ID) => {
                CheckedExprKind::ZeroExtend32(Box::new(checked_lhs.value))
            }
            // Subtype (downcast integer)
            (
                USIZE_TYPE_ID | ISIZE_TYPE_ID | I64_TYPE_ID | U64_TYPE_ID,
                I8_TYPE_ID | U8_TYPE_ID | I16_TYPE_ID | U16_TYPE_ID | I32_TYPE_ID | U32_TYPE_ID,
            )
            | (I32_TYPE_ID | U32_TYPE_ID, I8_TYPE_ID | U8_TYPE_ID | I16_TYPE_ID | U16_TYPE_ID)
            | (I16_TYPE_ID | U16_TYPE_ID, I8_TYPE_ID | U8_TYPE_ID) => checked_lhs.value.kind,
            (got, wanted) if got == wanted => checked_lhs.value.kind,
            (VOID_TYPE_ID, _) | (_, VOID_TYPE_ID) | _ => {
                return Err(Error::InvalidCast {
                    src: self.modules.source_for(ctx.module_id).clone(),
                    span,
                    got: self.types.name_of(checked_lhs.value.type_id, self),
                    wanted: self.types.name_of(wanted, self),
                });
            }
        };

        Ok(HasNever::new(
            CheckedExpr {
                type_id: wanted,
                kind,
                lvalue: false,
            },
            false,
        ))
    }

    fn typecheck_address_of_expr(
        &mut self,
        expr: &Expr,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        // TODO: Wanted inner of ptr
        let checked_expr =
            self.typecheck_expr(expr, None, true, false, block_ctx, proc_ctx, ctx)?;
        let type_id = self
            .types
            .register_type(Type::Ptr(checked_expr.value.type_id));

        let never = checked_expr.never;
        let kind = self.get_ptr_to_checked_expr(checked_expr.value, proc_ctx);

        Ok(HasNever::new(
            CheckedExpr {
                type_id,
                kind,
                lvalue: false,
            },
            never,
        ))
    }

    fn get_ptr_to_checked_expr(
        &mut self,
        checked_expr: CheckedExpr,
        proc_ctx: &mut ProcContext,
    ) -> CheckedExprKind {
        if checked_expr.lvalue {
            CheckedExprKind::AddressOf {
                expr: Box::new(checked_expr),
            }
        } else {
            CheckedExprKind::AddressOf {
                expr: Box::new(CheckedExpr {
                    type_id: checked_expr.type_id,
                    lvalue: true,
                    kind: CheckedExprKind::Store {
                        stack_slot: proc_ctx.stack_slots.allocate(checked_expr.type_id),
                        expr: Box::new(checked_expr),
                    },
                }),
            }
        }
    }

    fn typecheck_deref_expr(
        &mut self,
        expr: &Expr,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        // TODO: Wanted of ptr of whatever is wanted here
        let checked_expr =
            self.typecheck_expr(expr, None, true, false, block_ctx, proc_ctx, ctx)?;

        if !self.types.is_ptr(checked_expr.value.type_id) {
            return Err(Error::DerefNonPtr {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: expr.span,
                type_name: self.types.name_of(checked_expr.value.type_id, self),
            });
        }

        let type_id = self.types.inner_of(checked_expr.value.type_id);

        let kind = if checked_expr.value.lvalue {
            CheckedExprKind::Deref {
                type_id: checked_expr.value.type_id,
                stack_slot: proc_ctx.stack_slots.allocate(checked_expr.value.type_id),
                expr: Box::new(checked_expr.value),
            }
        } else {
            CheckedExprKind::Deref {
                type_id: checked_expr.value.type_id,
                stack_slot: proc_ctx.stack_slots.allocate(checked_expr.value.type_id),
                expr: Box::new(CheckedExpr {
                    type_id: checked_expr.value.type_id,
                    lvalue: true,
                    kind: CheckedExprKind::Store {
                        stack_slot: proc_ctx.stack_slots.allocate(checked_expr.value.type_id),
                        expr: Box::new(checked_expr.value),
                    },
                }),
            }
        };

        Ok(HasNever::new(
            CheckedExpr {
                type_id,
                kind,
                lvalue: true,
            },
            checked_expr.never,
        ))
    }

    fn typecheck_array_instantiation_expr(
        &mut self,
        exprs: &[Expr],
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        // TODO: Wanted, unwrap inner
        let mut inferred_type_id = None;

        let mut checked_exprs = vec![];
        let mut has_never = false;

        for expr in exprs {
            let checked_expr = self.typecheck_expr(
                expr,
                inferred_type_id,
                true,
                false,
                block_ctx,
                proc_ctx,
                ctx,
            )?;

            if !checked_expr.never {
                if let Some(inferred_type_id) = inferred_type_id {
                    if checked_expr.value.type_id != inferred_type_id.0 {
                        return Err(Error::ArrayInstantiationDifferentTypes {
                            src: self.modules.source_for(ctx.module_id).clone(),
                            first_span: inferred_type_id.1,
                            first_type_name: self.types.name_of(inferred_type_id.0, self),
                            second_span: expr.span,
                            second_type_name: self.types.name_of(checked_expr.value.type_id, self),
                        });
                    }
                } else {
                    inferred_type_id = Some((checked_expr.value.type_id, expr.span));
                }
            }

            has_never |= checked_expr.never;

            checked_exprs.push(checked_expr);
        }

        // TODO: Handle the case where it's an empty array
        let type_id = self.types.register_type(Type::Array(
            inferred_type_id.unwrap().0,
            checked_exprs.len(),
        ));

        Ok(HasNever::new(
            CheckedExpr {
                type_id: type_id,
                lvalue: false,
                kind: CheckedExprKind::ArrayInstantiation {
                    exprs: checked_exprs.into_iter().map(|expr| expr.value).collect(),
                    stack_slot: proc_ctx.stack_slots.allocate(type_id),
                },
            },
            has_never,
        ))
    }

    fn typecheck_array_access_expr(
        &mut self,
        lhs: &Expr,
        index: &Expr,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_lhs = self.typecheck_expr(lhs, None, true, false, block_ctx, proc_ctx, ctx)?;
        if !self.types.is_array(checked_lhs.value.type_id) && !checked_lhs.never {
            return Err(Error::ArrayAccessOnNonArray {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: lhs.span,
                type_name: self.types.name_of(checked_lhs.value.type_id, self),
            });
        }

        let checked_index = self.typecheck_expr(
            index,
            Some((USIZE_TYPE_ID, (0..0).into())),
            true,
            false,
            block_ctx,
            proc_ctx,
            ctx,
        )?;

        if !self.types.is_unsigned_integer(checked_index.value.type_id) && !checked_lhs.never {
            return Err(Error::ArrayAccessWithNonIntegerIndex {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: index.span,
                type_name: self.types.name_of(checked_index.value.type_id, self),
            });
        }

        let inner_type_id = self.types.inner_of(checked_lhs.value.type_id);
        Ok(HasNever::new(
            CheckedExpr {
                type_id: inner_type_id,
                lvalue: true,
                kind: CheckedExprKind::ArrayAccess {
                    lhs: Box::new(checked_lhs.value),
                    index: Box::new(checked_index.value),
                },
            },
            checked_lhs.never || checked_index.never,
        ))
    }

    fn typecheck_assignment_expr(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        block_ctx: &mut BlockContext,
        proc_ctx: &mut ProcContext,
        ctx: &CheckerContext,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_lhs = self.typecheck_expr(lhs, None, false, false, block_ctx, proc_ctx, ctx)?;
        let checked_rhs = self.typecheck_expr(
            rhs,
            Some((checked_lhs.value.type_id, lhs.span)),
            true,
            false,
            block_ctx,
            proc_ctx,
            ctx,
        )?;

        if !checked_lhs.value.lvalue {
            return Err(Error::CantAssignToRValue {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: lhs.span,
            });
        }

        if checked_lhs.value.type_id != checked_rhs.value.type_id && !checked_rhs.never {
            return Err(Error::ExpectedButGot {
                src: self.modules.source_for(ctx.module_id).clone(),
                span: rhs.span,
                expected: self.types.name_of(checked_lhs.value.type_id, self),
                got: self.types.name_of(checked_rhs.value.type_id, self),
            });
        }

        Ok(HasNever::new(
            CheckedExpr {
                type_id: checked_lhs.value.type_id,
                kind: CheckedExprKind::Assignment {
                    lhs: Box::new(checked_lhs.value),
                    rhs: Box::new(checked_rhs.value),
                },
                lvalue: false,
            },
            checked_rhs.never,
        ))
    }

    fn expect_number_or_ptr(
        &self,
        expr: &CheckedExpr,
        span: SourceSpan,
        ctx: &CheckerContext,
    ) -> Result<()> {
        if !(self.types.is_number(expr.type_id) || self.types.is_ptr(expr.type_id)) {
            Err(Error::ExpectedButGot {
                src: self.modules.source_for(ctx.module_id).clone(),
                span,
                expected: "number or ptr".to_string(),
                got: self.types.name_of(expr.type_id, self),
            })
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
struct HasNever<T> {
    value: T,
    never: bool,
}

impl<T> HasNever<T> {
    pub fn new(value: T, never: bool) -> Self {
        Self { value, never }
    }
}
