use std::{collections::HashMap, path::Path, str::FromStr};

use ast::{
    CheckedBlock, CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt, CheckedTranslationUnit,
};
use miette::{NamedSource, SourceSpan};
use module::{Module, ModuleCollection, ModuleId};
use package::PackageCollection;
use proc::{Proc, ProcCollection};
use scope::Scope;
use stack::{StackSlotId, StackSlots};
use tajp::{
    BOOL_TYPE_ID, GenericId, I32_TYPE_ID, IdentTypeId, NEVER_TYPE_ID, ProcStructure,
    STRING_TYPE_ID, Spanned, StructStructure, Type, TypeCollection, TypeId, U32_TYPE_ID,
    VOID_TYPE_ID,
};

use crate::{
    ast::parsed::{
        BinOp, Block, Expr, ExprKind, ExternProcDefinition, Ident, Import, ProcDefinition, Stmt,
        StmtKind, StructDefinition, TranslationUnit,
    },
    error::{Error, Result},
    helpers::string_join_with_and,
    package::{CheckedPackage, ParsedModule, ParsedPackage},
};

pub mod ast;
pub mod module;
mod package;
mod proc;
mod scope;
pub mod stack;
pub mod tajp;

#[derive(Debug)]
pub struct Checker {
    pub types: TypeCollection,
    scope: Scope,
    modules: ModuleCollection,
    procs: ProcCollection,
    packages: PackageCollection,
}

struct CheckerContext<'a> {
    unit: &'a TranslationUnit,
    source: &'a NamedSource<String>,
    module_id: ModuleId,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            types: TypeCollection::new(),
            scope: Scope::new(),
            modules: ModuleCollection::new(),
            procs: ProcCollection::new(),
            packages: PackageCollection::new(),
        }
    }

    pub fn add_package(
        &mut self,
        package: &ParsedPackage,
        dependencies: &[(String, ModuleId)],
    ) -> Result<CheckedPackage> {
        let package_id = self.create_module_ids(&package.name, &package.path, &package.modules)?;

        // TODO: Find a way to limit struct and proc lookups in these functions
        self.declare_structs(&package.path, &package.unit, &package.modules)?;
        self.declare_procs(&package.path, &package.unit, &package.modules)?;

        for dependency in dependencies {
            self.add_dependency(package_id, dependency);
        }

        self.resolve_imports(package_id, &package.path, &package.unit, &package.modules)?;
        self.define_structs(&package.path, &package.unit, &package.modules)?;
        self.define_procs(&package.path, &package.unit, &package.modules)?;
        let units = self.check_package(&package.path, &package.unit, &package.modules)?;

        Ok(CheckedPackage::new(package_id, units))
    }

    fn add_dependency(&mut self, package_id: ModuleId, dependency: &(String, ModuleId)) {
        self.packages.register_dependency(package_id, dependency);
    }

    fn resolve_imports(
        &mut self,
        package_id: ModuleId,
        path: &Path,
        unit: &TranslationUnit,
        modules: &[ParsedModule],
    ) -> Result<()> {
        let ctx = CheckerContext {
            module_id: self.modules.find_from_path(path).unwrap(),
            source: &unit.source,
            unit,
        };
        for import in &unit.imports {
            self.resolve_import_part(ctx.module_id, package_id, import, true, &ctx)?;
        }

        for module in modules {
            self.resolve_imports(package_id, &module.path, &module.unit, &module.children)?;
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
                    let module_id = self.modules.force_find_in(ctx.source, module_id, ident)?;
                    self.resolve_import_part(import_to, module_id, import, false, ctx)
                }
            }
            Import::Final(ident) => {
                if first {
                    todo!("This should show an error message");
                }

                if let Ok(type_id) = self.types.force_find_by_name(ctx.source, module_id, ident) {
                    // TODO: Verify that the name is unique
                    self.types.add_to_module(import_to, type_id, ident);
                    return Ok(());
                }

                if let Ok(proc_id) = self.procs.force_find(ctx.source, module_id, ident) {
                    // TODO: Verify that the name is unique
                    self.procs.add_to_module(import_to, proc_id, ident);
                    return Ok(());
                }

                Err(Error::ProcOrStructNotFound {
                    src: ctx.source.clone(),
                    span: ident.span,
                    wanted_name: ident.name.clone(),
                    module_name: "TODO".to_string(),
                })
            }
        }
    }

    fn declare_procs(
        &mut self,
        path: &Path,
        unit: &TranslationUnit,
        modules: &[ParsedModule],
    ) -> Result<()> {
        let ctx = CheckerContext {
            module_id: self.modules.find_from_path(path).unwrap(),
            unit,
            source: &unit.source,
        };

        for child in modules {
            self.declare_procs(&child.path, &child.unit, &child.children)?;
        }

        // FIXME: Procs with the same structure will have different TypeId's when they should have
        //        the same
        for proc in &unit.procs {
            self.add_proc_name(proc, &ctx)?;
        }

        for proc in &unit.extern_procs {
            self.add_extern_proc_name(proc, &ctx)?;
        }

        Ok(())
    }

    fn define_structs(
        &mut self,
        path: &Path,
        unit: &TranslationUnit,
        modules: &[ParsedModule],
    ) -> Result<()> {
        let ctx = CheckerContext {
            module_id: self.modules.find_from_path(path).unwrap(),
            unit,
            source: &unit.source,
        };

        for child in modules {
            self.define_structs(&child.path, &child.unit, &child.children)?;
        }

        for s in &unit.structs {
            let struct_type_id =
                self.types
                    .force_find_by_name(&unit.source, ctx.module_id, &s.ident)?;

            self.define_struct(s, struct_type_id, &ctx)?;
        }

        Ok(())
    }

    fn define_procs(
        &mut self,
        path: &Path,
        unit: &TranslationUnit,
        modules: &[ParsedModule],
    ) -> Result<()> {
        let ctx = CheckerContext {
            module_id: self.modules.find_from_path(path).unwrap(),
            unit,
            source: &unit.source,
        };

        for child in modules {
            self.define_procs(&child.path, &child.unit, &child.children)?;
        }

        for proc in &unit.procs {
            self.define_proc(
                proc,
                self.procs
                    .force_find_type_of(ctx.source, ctx.module_id, &proc.ident)?,
                &ctx,
            )?;
        }

        for extern_proc in &unit.extern_procs {
            self.define_extern_proc(
                extern_proc,
                self.procs
                    .force_find_type_of(ctx.source, ctx.module_id, &extern_proc.ident)?,
                &ctx,
            )?;
        }

        Ok(())
    }

    fn check_package(
        &mut self,
        path: &Path,
        unit: &TranslationUnit,
        modules: &[ParsedModule],
    ) -> Result<Vec<CheckedTranslationUnit>> {
        let ctx = CheckerContext {
            module_id: self.modules.find_from_path(path).unwrap(),
            unit,
            source: &unit.source,
        };

        let mut units = vec![];

        for child in modules {
            units.extend(self.check_package(&child.path, &child.unit, &child.children)?);
        }

        units.push(self.check_unit(unit, &ctx)?);

        Ok(units)
    }

    fn declare_structs(
        &mut self,
        path: &Path,
        unit: &TranslationUnit,
        modules: &[ParsedModule],
    ) -> Result<()> {
        let ctx = CheckerContext {
            module_id: self.modules.find_from_path(path).unwrap(),
            unit,
            source: &unit.source,
        };

        for child in modules {
            self.declare_structs(&child.path, &child.unit, &child.children)?;
        }

        for s in &unit.structs {
            self.add_struct_name(s, &ctx)?;
        }

        Ok(())
    }

    fn create_module_ids(
        &mut self,
        name: &str,
        path: &Path,
        modules: &[ParsedModule],
    ) -> Result<ModuleId> {
        let mut children = vec![];
        for child in modules {
            children.push(self.create_module_ids(&child.name, &child.path, &child.children)?);
        }

        Ok(self.modules.add(Module {
            children,
            path: path.to_owned(),
            name: name.to_string(),
        }))
    }

    fn check_unit(
        &mut self,
        unit: &TranslationUnit,
        ctx: &CheckerContext,
    ) -> Result<CheckedTranslationUnit> {
        self.scope.enter();

        for proc in self.procs.for_scope(ctx.module_id) {
            self.scope.add_to_scope(&proc.0, proc.1, None);
        }

        let mut checked_procs = vec![];
        for proc in &unit.procs {
            checked_procs.push(self.typecheck_proc(proc, ctx)?);
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
        ctx: &CheckerContext,
    ) -> Result<()> {
        let mut params: Vec<(Ident, TypeId)> = vec![];
        for param in &definition.params {
            if let Some(original) = params.iter().find(|p| p.0.name == param.0.name) {
                return Err(Error::ProcParmNameCollision {
                    src: ctx.source.clone(),
                    original_span: original.0.span,
                    redefined_span: param.0.span,
                    name: param.0.name.clone(),
                });
            }

            let type_id = self.types.force_find(ctx.source, ctx.module_id, &param.1)?;
            params.push((param.0.clone(), type_id));
        }

        let return_type =
            self.types
                .force_find(ctx.source, ctx.module_id, &definition.return_type)?;

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

    fn add_proc(&mut self, ident: Ident, ctx: &CheckerContext) -> Result<TypeId> {
        if let Some(span) = self.procs.find_original_span(ctx.module_id, &ident) {
            return Err(Error::ProcNameCollision {
                src: ctx.source.clone(),
                original_span: span,
                redefined_span: ident.span,
                name: ident.name,
            });
        }

        let type_id = self.types.register_undefined_proc();
        self.procs.add(
            ctx.module_id,
            Proc {
                type_id,
                name: ident,
            },
        );

        Ok(type_id)
    }

    fn add_extern_proc_name(
        &mut self,
        proc: &ExternProcDefinition,
        ctx: &CheckerContext,
    ) -> Result<TypeId> {
        self.add_proc(proc.ident.clone(), ctx)
    }

    fn add_proc_name(&mut self, proc: &ProcDefinition, ctx: &CheckerContext) -> Result<TypeId> {
        self.add_proc(proc.ident.clone(), ctx)
    }

    fn define_struct(
        &mut self,
        s: &StructDefinition,
        type_id: TypeId,
        ctx: &CheckerContext,
    ) -> Result<()> {
        let mut fields: Vec<IdentTypeId> = vec![];
        for field in &s.fields {
            if let Some(original) = fields.iter().find(|p| p.ident.name == field.0.name) {
                return Err(Error::StructFieldNameCollision {
                    src: ctx.source.clone(),
                    original_span: original.ident.span,
                    redefined_span: field.0.span,
                    name: field.0.name.clone(),
                });
            }

            let type_id = self.types.force_find(ctx.source, ctx.module_id, &field.1)?;
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
        let mut params: Vec<TypeId> = vec![];
        for param_type in &definition.params {
            let type_id = self.types.force_find_with_generics(
                ctx.source,
                ctx.module_id,
                param_type,
                &definition.type_params,
            )?;

            params.push(type_id);
        }

        let return_type = self.types.force_find_with_generics(
            ctx.source,
            ctx.module_id,
            &definition.return_type,
            &definition.type_params,
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

    fn typecheck_proc(
        &mut self,
        proc: &ProcDefinition,
        ctx: &CheckerContext,
    ) -> Result<CheckedProc> {
        let scope_data = self
            .scope
            .find(&proc.ident)
            .expect("Proc to have been added to scope");

        let definition = self.types.get_definition(scope_data.type_id).as_proc();

        self.scope.enter();

        let mut ss = StackSlots::new();

        let mut params = vec![];
        for param in proc.params.iter().map(|p| &p.0).zip(definition.params) {
            let stack_slot = ss.allocate(param.1);
            self.scope.add_to_scope(param.0, param.1, Some(stack_slot));
            params.push((param.0.name.clone(), stack_slot));
        }

        let return_type = (definition.return_type, proc.return_type.span);

        let body = self.typecheck_block(
            &proc.body,
            Some(return_type),
            return_type,
            ctx,
            return_type.0 != VOID_TYPE_ID,
            &mut ss,
        )?;

        if body.value.type_id != return_type.0 && !body.never {
            return Err(Error::ReturnValueDoesntMatch {
                src: ctx.source.clone(),
                return_type_span: return_type.1,
                expr_span: if let Some(last) = &proc.body.last {
                    last.span
                } else {
                    proc.body.span
                },
                return_type: self.types.name_of(return_type.0),
                actual_type: self.types.name_of(body.value.type_id),
            });
        }

        self.scope.exit();

        Ok(CheckedProc {
            type_id: scope_data.type_id,
            body: body.value,
            name: proc.ident.name.clone(),
            params,
            return_type: definition.return_type,
            stack_slots: ss,
        })
    }

    fn typecheck_block(
        &mut self,
        block: &Block,
        wanted: Option<(TypeId, SourceSpan)>,
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        requires_value: bool,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedBlock>> {
        let mut checked_stmts = vec![];
        let mut has_encountered_never = false;
        for stmt in &block.stmts {
            let stmt = self.typecheck_stmt(return_type, stmt, ctx, ss)?;
            has_encountered_never |= stmt.never;
            checked_stmts.push(stmt.value);
        }

        let last = if let Some(expr) = &block.last {
            let expr = self.typecheck_expr(expr, wanted, return_type, ctx, requires_value, ss)?;
            has_encountered_never |= expr.never;
            Some(expr.value)
        } else {
            if requires_value && !has_encountered_never {
                return Err(Error::BlockRequiresValue {
                    src: ctx.source.clone(),
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

        Ok(HasNever::new(
            CheckedBlock {
                stmts: checked_stmts,
                last,
                type_id,
            },
            has_encountered_never,
        ))
    }

    fn typecheck_stmt(
        &mut self,
        return_type: (TypeId, SourceSpan),
        stmt: &Stmt,
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedStmt>> {
        #[allow(unreachable_patterns)]
        match &stmt.kind {
            StmtKind::Return { value } => {
                self.typecheck_return_stmt(return_type, stmt.span, value, ctx, ss)
            }
            StmtKind::VariableDeclaration { name, value } => {
                self.typecheck_variable_declaration_stmt(name, value, return_type, ctx, ss)
            }
            StmtKind::Expr(expr) => {
                let expr = self.typecheck_expr(expr, None, return_type, ctx, false, ss)?;
                Ok(HasNever::new(CheckedStmt::Expr(expr.value), expr.never))
            }
            stmt => todo!("typecheck_stmt: {}", stmt),
        }
    }

    fn typecheck_return_stmt(
        &mut self,
        return_type: (TypeId, SourceSpan),
        span: SourceSpan,
        value: &Option<Expr>,
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedStmt>> {
        match value {
            Some(value) => {
                if return_type.0 == VOID_TYPE_ID {
                    return Err(Error::ReturnShouldntHaveValue {
                        src: ctx.source.clone(),
                        span: value.span,
                    });
                }

                let expr =
                    self.typecheck_expr(value, Some(return_type), return_type, ctx, true, ss)?;

                if expr.value.type_id != return_type.0 && !expr.never {
                    return Err(Error::ReturnValueDoesntMatch {
                        src: ctx.source.clone(),
                        return_type_span: return_type.1,
                        expr_span: value.span,
                        return_type: self.types.name_of(return_type.0),
                        actual_type: self.types.name_of(expr.value.type_id),
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
                if return_type.0 != VOID_TYPE_ID {
                    return Err(Error::ReturnShouldHaveValue {
                        src: ctx.source.clone(),
                        span,
                        name: self.types.name_of(return_type.0),
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
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedStmt>> {
        let expr = self.typecheck_expr(expr, None, return_type, ctx, true, ss)?;
        let stack_slot = ss.allocate(expr.value.type_id);
        self.scope
            .add_to_scope(ident, expr.value.type_id, Some(stack_slot));

        Ok(HasNever::new(
            CheckedStmt::VariableDeclaration {
                stack_slot,
                value: expr.value,
            },
            expr.never,
        ))
    }

    fn typecheck_expr(
        &mut self,
        expr: &Expr,
        wanted: Option<(TypeId, SourceSpan)>,
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        requires_value: bool,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        #[allow(unreachable_patterns)]
        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let scope_data = self.scope.force_find(ctx.source, ident)?;

                Ok(HasNever::new(
                    CheckedExpr {
                        type_id: scope_data.type_id,
                        kind: if let Some(stack_slot) = scope_data.stack_slot {
                            ast::CheckedExprKind::StackValue(stack_slot)
                        } else {
                            ast::CheckedExprKind::Identifier(ident.name.clone())
                        },
                        lvalue: true,
                    },
                    false,
                ))
            }
            ExprKind::Number(value) => {
                if let Some(wanted) = wanted {
                    match wanted.0 {
                        U32_TYPE_ID => {
                            let value = self.verify_number::<u32>(
                                value.as_str(),
                                expr.span,
                                U32_TYPE_ID,
                                ctx,
                            )?;
                            Ok(HasNever::new(
                                CheckedExpr {
                                    type_id: U32_TYPE_ID,
                                    kind: ast::CheckedExprKind::Number(value as u64),
                                    lvalue: false,
                                },
                                false,
                            ))
                        }
                        I32_TYPE_ID | _ => {
                            let value = self.verify_number::<i32>(
                                value.as_str(),
                                expr.span,
                                I32_TYPE_ID,
                                ctx,
                            )?;
                            Ok(HasNever::new(
                                CheckedExpr {
                                    type_id: I32_TYPE_ID,
                                    kind: ast::CheckedExprKind::Number(value as u64),
                                    lvalue: false,
                                },
                                false,
                            ))
                        }
                    }
                } else {
                    let value =
                        self.verify_number::<i32>(value.as_str(), expr.span, U32_TYPE_ID, ctx)?;
                    Ok(HasNever::new(
                        CheckedExpr {
                            type_id: I32_TYPE_ID,
                            kind: ast::CheckedExprKind::Number(value as u64),
                            lvalue: false,
                        },
                        false,
                    ))
                }
            }
            ExprKind::BinOp { lhs, op, rhs } => {
                self.typecheck_binop_expr(lhs, *op, rhs, wanted, return_type, ctx, ss)
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
            } => {
                self.typecheck_call_expr(expr, params, generic_params, wanted, return_type, ctx, ss)
            }
            ExprKind::Bool(value) => Ok(HasNever::new(
                CheckedExpr {
                    type_id: BOOL_TYPE_ID,
                    kind: ast::CheckedExprKind::Number(if *value { 1 } else { 0 }),
                    lvalue: false,
                },
                false,
            )),
            ExprKind::Builtin(name, params) => Ok(HasNever::new(
                self.typecheck_builtin_expr(expr, name, params, return_type, ctx, ss)?,
                false,
            )),
            ExprKind::StructInstantiation { name, members } => {
                self.typecheck_struct_instantiation_expr(expr, name, members, return_type, ctx, ss)
            }
            ExprKind::MemberAccess { lhs, member } => Ok(HasNever::new(
                self.typecheck_member_access_expr(lhs, member, return_type, ctx, ss)?,
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
                return_type,
                ctx,
                requires_value,
                ss,
            ),
            ExprKind::Cast { lhs, tajp } => {
                self.typecheck_cast_expr(lhs, tajp, expr.span, return_type, ctx, ss)
            }
            ExprKind::Assignment { lhs, rhs } => {
                self.typecheck_assignment_expr(lhs, rhs, return_type, ctx, ss)
            }
            ExprKind::AddressOf(expr) => self.typecheck_address_of_expr(expr, return_type, ctx, ss),
            ExprKind::Block(block) => {
                let block = self.typecheck_block(block, wanted, return_type, ctx, true, ss)?;
                Ok(HasNever::new(
                    CheckedExpr {
                        type_id: block.value.type_id,
                        lvalue: false,
                        kind: CheckedExprKind::Block(Box::new(block.value)),
                    },
                    block.never,
                ))
            }
            ExprKind::Deref(expr) => self.typecheck_deref_expr(&expr, return_type, ctx, ss),
            kind => todo!("typecheck_expr: {}", kind),
        }
    }

    fn typecheck_binop_expr(
        &mut self,
        lhs: &Expr,
        op: BinOp,
        rhs: &Expr,
        wanted: Option<(TypeId, SourceSpan)>,
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_lhs = self.typecheck_expr(lhs, wanted, return_type, ctx, true, ss)?;
        let checked_rhs = self.typecheck_expr(rhs, wanted, return_type, ctx, true, ss)?;

        let has_encountered_never = checked_lhs.never || checked_rhs.never;

        if checked_lhs.value.type_id != checked_rhs.value.type_id && !has_encountered_never {
            return Err(Error::BinOpSidesMismatch {
                src: ctx.source.clone(),
                lhs_span: lhs.span,
                rhs_span: rhs.span,
                lhs_type_name: self.types.name_of(checked_lhs.value.type_id),
                rhs_type_name: self.types.name_of(checked_rhs.value.type_id),
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
                BinOp::Equal | BinOp::NotEqual => {
                    self.typecheck_boolable_binop_expr(checked_lhs.value, op, checked_rhs.value)?
                }
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
        self.expect_number(&checked_lhs, lhs.span, ctx)?;
        self.expect_number(&checked_rhs, rhs.span, ctx)?;

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
        checked_lhs: CheckedExpr,
        op: BinOp,
        checked_rhs: CheckedExpr,
    ) -> Result<CheckedExpr> {
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
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_expr = self.typecheck_expr(expr, wanted, return_type, ctx, true, ss)?;

        let ident = match checked_expr.value.kind {
            CheckedExprKind::Identifier(name) => name,
            _ => todo!("Indirect calls"),
        };

        let proc_type = self
            .types
            .get_definition(
                self.scope
                    .force_find_from_string(ctx.source, &ident)?
                    .type_id,
            )
            .as_proc();

        let mut resolved_generics = HashMap::new();

        for (id, generic_param) in generic_params.iter().enumerate() {
            resolved_generics.insert(
                GenericId(id),
                Spanned::new(
                    self.types
                        .force_find(ctx.source, ctx.module_id, generic_param)?,
                    generic_param.span,
                ),
            );
        }

        if params.len() < proc_type.params.len()
            || (params.len() > proc_type.params.len() && !proc_type.variadic)
        {
            return Err(Error::ProcCallParamCountMismatch {
                src: ctx.source.clone(),
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
            let checked_expr = self.typecheck_expr(&param, wanted, return_type, ctx, true, ss)?;

            self.types.infer_generic_types(
                ctx.source,
                param.span,
                *expected_type,
                checked_expr.value.type_id,
                &mut resolved_generics,
            )?;

            has_encountered_never |= checked_expr.never;

            if checked_expr.value.type_id
                != self
                    .types
                    .resolve_generic_type(*expected_type, &resolved_generics)?
                && !checked_expr.never
            {
                return Err(Error::ProcCallParamTypeMismatch {
                    src: ctx.source.clone(),
                    span: param.span,
                    expected: self.types.name_of(*expected_type),
                    got: self.types.name_of(checked_expr.value.type_id),
                });
            }

            checked_params.push(checked_expr.value);
        }

        for param in &params {
            let checked_expr = self.typecheck_expr(param, None, return_type, ctx, true, ss)?;

            has_encountered_never |= checked_expr.never;

            checked_params.push(checked_expr.value);
        }

        Ok(HasNever::new(
            CheckedExpr {
                type_id: self
                    .types
                    .resolve_generic_type(proc_type.return_type, &resolved_generics)?,
                kind: CheckedExprKind::DirectCall {
                    name: ident,
                    params: checked_params,
                    variadic_after: if proc_type.variadic {
                        Some(proc_type.params.len() as u64)
                    } else {
                        None
                    },
                    stack_slot: ss.allocate(proc_type.return_type),
                },
                lvalue: true,
            },
            has_encountered_never || proc_type.return_type == NEVER_TYPE_ID,
        ))
    }

    fn typecheck_builtin_expr(
        &mut self,
        expr: &Expr,
        name: &str,
        params: &[Expr],
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<CheckedExpr> {
        match name {
            "type_name" => {
                if params.len() != 1 {
                    return Err(Error::BuiltinParamCountMismatch {
                        src: ctx.source.clone(),
                        span: expr.span,
                        name: name.to_string(),
                        expected: 1,
                        got: params.len(),
                        variadic: false,
                    });
                }

                let checked_param =
                    self.typecheck_expr(&params[0], None, return_type, ctx, false, ss)?;

                Ok(CheckedExpr {
                    type_id: STRING_TYPE_ID,
                    kind: CheckedExprKind::String(self.types.name_of(checked_param.value.type_id)),
                    lvalue: false,
                })
            }
            name => Err(Error::UnknownBuiltin {
                src: ctx.source.clone(),
                span: expr.span,
                name: name.to_string(),
            }),
        }
    }

    fn typecheck_struct_instantiation_expr(
        &mut self,
        expr: &Expr,
        name: &Ident,
        fields: &[(Ident, Expr)],
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        let struct_type_id = self
            .types
            .force_find_by_name(ctx.source, ctx.module_id, name)?;

        let definition = self.types.get_definition(struct_type_id);
        if !definition.is_struct() {
            return Err(Error::StructInstantiationOnNonStruct {
                src: ctx.source.clone(),
                span: name.span,
            });
        }

        let s = definition.as_struct();

        let mut checked_fields: Vec<(Ident, CheckedExpr)> = vec![];

        let mut has_encountered_never = false;

        for field in fields {
            let defined_field = s.fields.iter().find(|f| f.ident == field.0);
            let Some(defined_field) = defined_field else {
                return Err(Error::StructInstantiationFieldDoesntExist {
                    src: ctx.source.clone(),
                    span: field.0.span,
                    struct_name: name.name.clone(),
                    field_name: field.0.name.clone(),
                });
            };

            if let Some(existing_field) = checked_fields.iter().find(|f| f.0 == field.0) {
                return Err(Error::StructInstantiationFieldAlreadyDeclared {
                    src: ctx.source.clone(),
                    original_span: existing_field.0.span,
                    redefined_span: field.0.span,
                    field_name: field.0.name.clone(),
                });
            }

            let checked_expr = self.typecheck_expr(
                &field.1,
                Some((defined_field.type_id, defined_field.ident.span)),
                return_type,
                ctx,
                true,
                ss,
            )?;

            has_encountered_never |= checked_expr.never;

            if checked_expr.value.type_id != defined_field.type_id && !checked_expr.never {
                return Err(Error::StructInstantiationFieldTypeMismatch {
                    src: ctx.source.clone(),
                    span: field.1.span,
                    struct_name: name.name.clone(),
                    field_name: field.0.name.clone(),
                    expected: self.types.name_of(defined_field.type_id),
                    got: self.types.name_of(checked_expr.value.type_id),
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
                src: ctx.source.clone(),
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

        Ok(HasNever::new(
            CheckedExpr {
                type_id: struct_type_id,
                kind: CheckedExprKind::StructInstantiation {
                    name: name.name.clone(),
                    fields: checked_fields
                        .drain(0..)
                        .map(|f| (f.0.name.clone(), f.1))
                        .collect(),
                    stack_slot: ss.allocate(struct_type_id),
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
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<CheckedExpr> {
        let checked_lhs = self.typecheck_expr(lhs, None, return_type, ctx, true, ss)?;

        let definition = self.types.get_definition(checked_lhs.value.type_id);
        if !definition.is_struct() {
            return Err(Error::MemberAccessNotAStruct {
                src: ctx.source.clone(),
                span: lhs.span,
                got: self.types.name_of(checked_lhs.value.type_id),
            });
        }

        let definition = definition.as_struct();

        let Some(field) = definition.fields.iter().find(|f| f.ident == *member) else {
            return Err(Error::MemberAccessUnknownField {
                src: ctx.source.clone(),
                span: member.span,
                struct_name: self.types.name_of(checked_lhs.value.type_id),
                field_name: member.name.clone(),
            });
        };

        // A member expression is an lvalue if the lhs is also an lvalue
        let lvalue = checked_lhs.value.lvalue;

        Ok(CheckedExpr {
            type_id: field.type_id,
            kind: CheckedExprKind::MemberAccess {
                lhs: Box::new(checked_lhs.value),
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
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        requires_value: bool,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_condition = self.typecheck_expr(
            condition,
            Some((BOOL_TYPE_ID, (0..0).into())),
            return_type,
            ctx,
            true,
            ss,
        )?;
        let checked_true_block =
            self.typecheck_block(true_block, None, return_type, ctx, requires_value, ss)?;

        let checked_false_block = self.typecheck_block(
            false_block,
            Some((checked_true_block.value.type_id, true_block.span)),
            return_type,
            ctx,
            requires_value,
            ss,
        )?;

        if checked_condition.value.type_id != BOOL_TYPE_ID && !checked_condition.never {
            return Err(Error::ExpectedButGot {
                src: ctx.source.clone(),
                span: condition.span,
                expected: "bool".to_string(),
                got: self.types.name_of(checked_condition.value.type_id),
            });
        }

        if requires_value
            && checked_true_block.value.type_id != checked_false_block.value.type_id
            && !(checked_true_block.never || checked_false_block.never)
        {
            return Err(Error::IfTypeMismatch {
                src: ctx.source.clone(),
                true_block_span: true_block.span,
                true_block_type: self.types.name_of(checked_true_block.value.type_id),
                false_block_span: false_block.span,
                false_block_type: self.types.name_of(checked_false_block.value.type_id),
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
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        let wanted = self.types.force_find(ctx.source, ctx.module_id, t)?;
        let checked_lhs =
            self.typecheck_expr(lhs, Some((wanted, t.span)), return_type, ctx, true, ss)?;

        if checked_lhs.never {
            return Ok(checked_lhs);
        }

        let kind = match (checked_lhs.value.type_id, wanted) {
            (U32_TYPE_ID, I32_TYPE_ID)
            | (I32_TYPE_ID, U32_TYPE_ID)
            | (BOOL_TYPE_ID, I32_TYPE_ID)
            | (BOOL_TYPE_ID, U32_TYPE_ID) => checked_lhs.value.kind,
            (U32_TYPE_ID, BOOL_TYPE_ID) | (I32_TYPE_ID, BOOL_TYPE_ID) => CheckedExprKind::BinOp {
                lhs: Box::new(checked_lhs.value),
                op: BinOp::NotEqual,
                rhs: Box::new(CheckedExpr {
                    type_id: BOOL_TYPE_ID,
                    kind: CheckedExprKind::Number(0),
                    lvalue: false,
                }),
            },
            (got, wanted) if got == wanted => checked_lhs.value.kind,
            (VOID_TYPE_ID, _) | (_, VOID_TYPE_ID) | _ => {
                return Err(Error::InvalidCast {
                    src: ctx.source.clone(),
                    span,
                    got: self.types.name_of(checked_lhs.value.type_id),
                    wanted: self.types.name_of(wanted),
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
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        // TODO: Wanted inner of ptr
        let checked_expr = self.typecheck_expr(expr, None, return_type, ctx, true, ss)?;
        let type_id = self
            .types
            .register_type(Type::Ptr(checked_expr.value.type_id));

        let kind = if checked_expr.value.lvalue {
            CheckedExprKind::AddressOf {
                expr: Box::new(checked_expr.value),
            }
        } else {
            CheckedExprKind::AddressOf {
                expr: Box::new(CheckedExpr {
                    type_id: checked_expr.value.type_id,
                    lvalue: true,
                    kind: CheckedExprKind::Store {
                        stack_slot: ss.allocate(checked_expr.value.type_id),
                        expr: Box::new(checked_expr.value),
                    },
                }),
            }
        };

        Ok(HasNever::new(
            CheckedExpr {
                type_id,
                kind,
                lvalue: false,
            },
            checked_expr.never,
        ))
    }

    fn typecheck_deref_expr(
        &mut self,
        expr: &Expr,
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        // TODO: Wanted of ptr of whatever is wanted here
        let checked_expr = self.typecheck_expr(expr, None, return_type, ctx, true, ss)?;

        if !self.types.is_ptr(checked_expr.value.type_id) {
            return Err(Error::DerefNonPtr {
                src: ctx.source.clone(),
                span: expr.span,
                type_name: self.types.name_of(checked_expr.value.type_id),
            });
        }

        let type_id = self.types.inner_of(checked_expr.value.type_id);

        let kind = if checked_expr.value.lvalue {
            CheckedExprKind::Deref {
                type_id: checked_expr.value.type_id,
                stack_slot: ss.allocate(checked_expr.value.type_id),
                expr: Box::new(checked_expr.value),
            }
        } else {
            CheckedExprKind::Deref {
                type_id: checked_expr.value.type_id,
                stack_slot: ss.allocate(checked_expr.value.type_id),
                expr: Box::new(CheckedExpr {
                    type_id: checked_expr.value.type_id,
                    lvalue: true,
                    kind: CheckedExprKind::Store {
                        stack_slot: ss.allocate(checked_expr.value.type_id),
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

    fn typecheck_assignment_expr(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        return_type: (TypeId, SourceSpan),
        ctx: &CheckerContext,
        ss: &mut StackSlots,
    ) -> Result<HasNever<CheckedExpr>> {
        let checked_lhs = self.typecheck_expr(lhs, None, return_type, ctx, false, ss)?;
        let checked_rhs = self.typecheck_expr(
            rhs,
            Some((checked_lhs.value.type_id, lhs.span)),
            return_type,
            ctx,
            true,
            ss,
        )?;

        if !checked_lhs.value.lvalue {
            return Err(Error::CantAssignToRValue {
                src: ctx.source.clone(),
                span: lhs.span,
            });
        }

        if checked_lhs.value.type_id != checked_rhs.value.type_id && !checked_rhs.never {
            return Err(Error::ExpectedButGot {
                src: ctx.source.clone(),
                span: rhs.span,
                expected: self.types.name_of(checked_lhs.value.type_id),
                got: self.types.name_of(checked_rhs.value.type_id),
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

    fn find_stack_slot_and_offset_for_assignment(&self, expr: &CheckedExpr) -> (StackSlotId, u64) {
        match &expr.kind {
            CheckedExprKind::StackValue(stack_slot_id) => (*stack_slot_id, 0),
            CheckedExprKind::DirectCall { stack_slot, .. } => (*stack_slot, 0),
            CheckedExprKind::MemberAccess { lhs, name } => {
                let memory_layout = self.types.memory_layout_of(lhs.type_id);
                let fields = memory_layout.fields.unwrap();
                let field_layout = fields.get(name).unwrap();

                let slot_and_offset = self.find_stack_slot_and_offset_for_assignment(lhs);
                (
                    slot_and_offset.0,
                    slot_and_offset.1 + field_layout.offset as u64,
                )
            }
            _ => panic!("Invalid lvalue"),
        }
    }

    fn expect_number(
        &self,
        expr: &CheckedExpr,
        span: SourceSpan,
        ctx: &CheckerContext,
    ) -> Result<()> {
        if !self.types.is_number(expr.type_id) {
            Err(Error::ExpectedButGot {
                src: ctx.source.clone(),
                span,
                expected: "number".to_string(),
                got: self.types.name_of(expr.type_id),
            })
        } else {
            Ok(())
        }
    }

    fn verify_number<T: FromStr>(
        &self,
        value: &str,
        span: SourceSpan,
        type_id: TypeId,
        ctx: &CheckerContext,
    ) -> Result<T> {
        if let Ok(value) = value.parse::<T>() {
            Ok(value)
        } else {
            Err(Error::InvalidNumber {
                src: ctx.source.clone(),
                span,
                value: value.to_string(),
                type_name: self.types.name_of(type_id),
            })
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
