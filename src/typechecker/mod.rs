use std::{fmt::Debug, path::PathBuf, str::FromStr};

use ast::{CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt, CheckedTranslationUnit};
use miette::{NamedSource, SourceSpan};
use module::{Module, ModuleCollection, ModuleId};
use package::PackageCollection;
use proc::{Proc, ProcCollection};
use scope::Scope;
use tajp::{
    BOOL_TYPE_ID, I32_TYPE_ID, STRING_TYPE_ID, Type, TypeCollection, TypeId, U32_TYPE_ID,
    VOID_TYPE_ID,
};

use crate::{
    ast::parsed::{
        self, BinOp, Expr, ExprKind, ExternProcDefinition, Ident, Import, ProcDefinition, Stmt,
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
        dependencies: &Vec<(String, ModuleId)>,
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
        path: &PathBuf,
        unit: &TranslationUnit,
        modules: &Vec<ParsedModule>,
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
                        return self.resolve_import_part(import_to, module_id, import, false, ctx);
                    } else {
                        return self.resolve_import_part(
                            import_to,
                            self.packages
                                .force_find_dependency(module_id, &ident.name)?,
                            import,
                            false,
                            ctx,
                        );
                    }
                } else {
                    let module_id = self.modules.force_find_in(ctx.source, module_id, ident)?;
                    return self.resolve_import_part(import_to, module_id, import, false, ctx);
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

                return Err(Error::ProcOrStructNotFound {
                    src: ctx.source.clone(),
                    span: ident.span,
                    wanted_name: ident.name.clone(),
                    module_name: "TODO".to_string(),
                });
            }
        }
    }

    fn declare_procs(
        &mut self,
        path: &PathBuf,
        unit: &TranslationUnit,
        modules: &Vec<ParsedModule>,
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
        path: &PathBuf,
        unit: &TranslationUnit,
        modules: &Vec<ParsedModule>,
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
            self.define_struct(
                s,
                self.types
                    .force_find_by_name(&unit.source, ctx.module_id, &s.ident)?,
                &ctx,
            )?;
        }

        Ok(())
    }

    fn define_procs(
        &mut self,
        path: &PathBuf,
        unit: &TranslationUnit,
        modules: &Vec<ParsedModule>,
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
        path: &PathBuf,
        unit: &TranslationUnit,
        modules: &Vec<ParsedModule>,
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
        path: &PathBuf,
        unit: &TranslationUnit,
        modules: &Vec<ParsedModule>,
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
        path: &PathBuf,
        modules: &Vec<ParsedModule>,
    ) -> Result<ModuleId> {
        let mut children = vec![];
        for child in modules {
            children.push(self.create_module_ids(&child.name, &child.path, &child.children)?);
        }

        Ok(self.modules.add(Module {
            children,
            path: path.clone(),
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
            self.scope.add_to_scope(&proc.0, proc.1);
        }

        let mut checked_procs = vec![];
        for proc in &unit.procs {
            checked_procs.push(self.typecheck_proc(&proc, ctx)?);
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
                .force_find(&ctx.source, ctx.module_id, &definition.return_type)?;

        self.types.define_proc(
            type_id,
            Type::Proc {
                params: params.iter().map(|p| p.1).collect::<Vec<_>>(),
                return_type,
                variadic: false,
            },
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
        // TODO: Verify that the name is unique
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
        let mut fields: Vec<(Ident, TypeId)> = vec![];
        for field in &s.fields {
            if let Some(original) = fields.iter().find(|p| p.0.name == field.0.name) {
                return Err(Error::StructFieldNameCollision {
                    src: ctx.source.clone(),
                    original_span: original.0.span,
                    redefined_span: field.0.span,
                    name: field.0.name.clone(),
                });
            }

            let type_id = self.types.force_find(ctx.source, ctx.module_id, &field.1)?;
            fields.push((field.0.clone(), type_id));
        }

        self.types.define_struct(
            type_id,
            Type::Struct {
                module_id: ctx.module_id,
                name: s.ident.clone(),
                fields,
            },
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
            let type_id = self
                .types
                .force_find(ctx.source, ctx.module_id, param_type)?;
            params.push(type_id);
        }

        let return_type =
            self.types
                .force_find(ctx.source, ctx.module_id, &definition.return_type)?;

        self.types.define_proc(
            type_id,
            Type::Proc {
                params: params.clone(),
                return_type,
                variadic: definition.variadic,
            },
        );

        Ok(())
    }

    fn typecheck_proc(
        &mut self,
        proc: &ProcDefinition,
        ctx: &CheckerContext,
    ) -> Result<CheckedProc> {
        let type_id = self
            .scope
            .find(&proc.ident)
            .expect("Proc to have been added to scope");

        let definition = self.types.get_definition(type_id).as_proc();

        self.scope.enter();

        let mut params = vec![];
        for param in proc.params.iter().map(|p| &p.0).zip(definition.0) {
            self.scope.add_to_scope(param.0, param.1);
            params.push((param.0.name.clone(), param.1));
        }

        let mut stmts = vec![];

        for stmt in &proc.stmts {
            stmts.push(self.typecheck_stmt((definition.1, proc.return_type.span), stmt, ctx)?);
        }

        self.scope.exit();

        Ok(CheckedProc {
            type_id,
            stmts,
            name: proc.ident.name.clone(),
            params,
            return_type: definition.1,
        })
    }

    fn typecheck_stmt(
        &mut self,
        return_type: (TypeId, SourceSpan),
        stmt: &Stmt,
        ctx: &CheckerContext,
    ) -> Result<CheckedStmt> {
        match &stmt.kind {
            StmtKind::Return { value } => {
                self.typecheck_return_stmt(return_type, stmt.span, value, ctx)
            }
            StmtKind::VariableDeclaration { name, value } => {
                self.typecheck_variable_declaration_stmt(name, value, ctx)
            }
            StmtKind::Expr(expr) => {
                let expr = self.typecheck_expr(expr, None, ctx)?;
                Ok(CheckedStmt::Expr(expr))
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
    ) -> Result<CheckedStmt> {
        match value {
            Some(value) => {
                if return_type.0 == VOID_TYPE_ID {
                    return Err(Error::ReturnShouldntHaveValue {
                        src: ctx.source.clone(),
                        span: value.span,
                    });
                }

                let expr = self.typecheck_expr(value, Some(return_type), ctx)?;

                if expr.type_id != return_type.0 {
                    return Err(Error::ReturnValueDoesntMatch {
                        src: ctx.source.clone(),
                        return_type_span: return_type.1,
                        expr_span: value.span,
                        return_type: self.types.name_of(return_type.0),
                        actual_type: self.types.name_of(expr.type_id),
                    });
                }
                Ok(CheckedStmt::Return { value: Some(expr) })
            }
            None => {
                if return_type.0 != VOID_TYPE_ID {
                    return Err(Error::ReturnShouldHaveValue {
                        src: ctx.source.clone(),
                        span,
                        name: self.types.name_of(return_type.0),
                    });
                }

                Ok(CheckedStmt::Return { value: None })
            }
        }
    }

    fn typecheck_variable_declaration_stmt(
        &mut self,
        ident: &Ident,
        expr: &Expr,
        ctx: &CheckerContext,
    ) -> Result<CheckedStmt> {
        let expr = self.typecheck_expr(expr, None, ctx)?;
        self.scope.add_to_scope(ident, expr.type_id);

        Ok(CheckedStmt::VariableDeclaration {
            name: ident.name.clone(),
            value: expr,
        })
    }

    fn typecheck_expr(
        &mut self,
        expr: &Expr,
        wanted: Option<(TypeId, SourceSpan)>,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let type_id = self.scope.force_find(ctx.source, ident)?;

                Ok(CheckedExpr {
                    type_id,
                    kind: ast::CheckedExprKind::Identifier(ident.name.clone()),
                })
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
                            Ok(CheckedExpr {
                                type_id: U32_TYPE_ID,
                                kind: ast::CheckedExprKind::Number(value as u64),
                            })
                        }
                        I32_TYPE_ID | _ => {
                            let value = self.verify_number::<i32>(
                                value.as_str(),
                                expr.span,
                                I32_TYPE_ID,
                                ctx,
                            )?;
                            Ok(CheckedExpr {
                                type_id: I32_TYPE_ID,
                                kind: ast::CheckedExprKind::Number(value as u64),
                            })
                        }
                    }
                } else {
                    let value =
                        self.verify_number::<i32>(value.as_str(), expr.span, U32_TYPE_ID, ctx)?;
                    Ok(CheckedExpr {
                        type_id: I32_TYPE_ID,
                        kind: ast::CheckedExprKind::Number(value as u64),
                    })
                }
            }
            ExprKind::BinOp { lhs, op, rhs } => {
                self.typecheck_binop_expr(lhs, *op, rhs, wanted, ctx)
            }
            ExprKind::String(value) => Ok(CheckedExpr {
                type_id: STRING_TYPE_ID,
                kind: ast::CheckedExprKind::String(value.clone()),
            }),
            ExprKind::Call { expr, params } => self.typecheck_call_expr(expr, params, wanted, ctx),
            ExprKind::Bool(value) => Ok(CheckedExpr {
                type_id: BOOL_TYPE_ID,
                kind: ast::CheckedExprKind::Number(if *value { 1 } else { 0 }),
            }),
            ExprKind::Builtin(name, params) => self.typecheck_builtin_expr(expr, name, params, ctx),
            ExprKind::StructInstantiation { name, members } => {
                self.typecheck_struct_instantiation_expr(expr, name, members, ctx)
            }
            ExprKind::MemberAccess { lhs, member } => {
                self.typecheck_member_access_expr(lhs, member, ctx)
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
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        let checked_lhs = self.typecheck_expr(lhs, wanted, ctx)?;
        let checked_rhs = self.typecheck_expr(rhs, wanted, ctx)?;

        if checked_lhs.type_id != checked_rhs.type_id {
            return Err(Error::BinOpSidesMismatch {
                src: ctx.source.clone(),
                lhs_span: lhs.span,
                rhs_span: rhs.span,
                lhs_type_name: self.types.name_of(checked_lhs.type_id),
                rhs_type_name: self.types.name_of(checked_rhs.type_id),
            });
        }

        match op {
            BinOp::Divide | BinOp::Multiply | BinOp::Add | BinOp::Subtract => {
                self.typecheck_other_binop_expr(lhs, checked_lhs, op, rhs, checked_rhs, ctx)
            }
            BinOp::Equal | BinOp::NotEqual => {
                self.typecheck_boolable_binop_expr(checked_lhs, op, checked_rhs)
            }
        }
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
        })
    }

    fn typecheck_call_expr(
        &mut self,
        expr: &Expr,
        params: &Vec<Expr>,
        wanted: Option<(TypeId, SourceSpan)>,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        let checked_expr = self.typecheck_expr(expr, wanted, ctx)?;

        let ident = match checked_expr.kind {
            CheckedExprKind::Identifier(name) => name,
            _ => todo!("Indirect calls"),
        };

        let proc_type = self
            .types
            .get_definition(self.scope.force_find_from_string(ctx.source, &ident)?)
            .as_proc();

        if params.len() < proc_type.0.len() || (params.len() > proc_type.0.len() && !proc_type.2) {
            return Err(Error::ProcCallParamCountMismatch {
                src: ctx.source.clone(),
                span: expr.span,
                expected: proc_type.0.len(),
                got: params.len(),
                variadic: proc_type.2,
            });
        }

        let mut params = params.clone();
        let non_variadic_params = params.drain(0..proc_type.0.len()).zip(&proc_type.0);

        let mut checked_params = vec![];
        for (param, expected_type) in non_variadic_params {
            // TODO: Get the location of the suspected span or allow the span to be optional
            let checked_expr =
                self.typecheck_expr(&param, Some((*expected_type, (0..0).into())), ctx)?;
            if checked_expr.type_id != *expected_type {
                return Err(Error::ProcCallParamTypeMismatch {
                    src: ctx.source.clone(),
                    span: param.span,
                    expected: self.types.name_of(*expected_type),
                    got: self.types.name_of(checked_expr.type_id),
                });
            }

            checked_params.push(checked_expr);
        }

        for param in &params {
            checked_params.push(self.typecheck_expr(param, None, ctx)?);
        }

        Ok(CheckedExpr {
            type_id: proc_type.1,
            kind: CheckedExprKind::DirectCall {
                name: ident,
                params: checked_params,
                variadic_after: if proc_type.2 {
                    Some(proc_type.0.len() as u64)
                } else {
                    None
                },
            },
        })
    }

    fn typecheck_builtin_expr(
        &mut self,
        expr: &Expr,
        name: &str,
        params: &Vec<Expr>,
        ctx: &CheckerContext,
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

                let checked_param = self.typecheck_expr(&params[0], None, ctx)?;

                Ok(CheckedExpr {
                    type_id: STRING_TYPE_ID,
                    kind: CheckedExprKind::String(self.types.name_of(checked_param.type_id)),
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
        fields: &Vec<(Ident, Expr)>,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
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

        for field in fields {
            let defined_field = s.1.iter().find(|f| f.0 == field.0);
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

            let checked_expr =
                self.typecheck_expr(&field.1, Some((defined_field.1, defined_field.0.span)), ctx)?;

            if checked_expr.type_id != defined_field.1 {
                return Err(Error::StructInstantiationFieldTypeMismatch {
                    src: ctx.source.clone(),
                    span: field.1.span,
                    struct_name: name.name.clone(),
                    field_name: field.0.name.clone(),
                    expected: self.types.name_of(defined_field.1),
                    got: self.types.name_of(checked_expr.type_id),
                });
            }

            checked_fields.push((field.0.clone(), checked_expr));
        }

        if checked_fields.len() != s.1.len() {
            let mut missing_fields = vec![];

            for known_fields in &s.1 {
                if !checked_fields.iter().any(|c| c.0 == known_fields.0) {
                    missing_fields.push(format!("'{}'", known_fields.0.name));
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

        Ok(CheckedExpr {
            type_id: struct_type_id,
            kind: CheckedExprKind::StructInstantiation {
                name: name.name.clone(),
                fields: checked_fields
                    .drain(0..)
                    .map(|f| (f.0.name.clone(), f.1))
                    .collect(),
            },
        })
    }

    fn typecheck_member_access_expr(
        &mut self,
        lhs: &Expr,
        member: &Ident,
        ctx: &CheckerContext,
    ) -> Result<CheckedExpr> {
        let checked_lhs = self.typecheck_expr(lhs, None, ctx)?;

        let definition = self.types.get_definition(checked_lhs.type_id);
        if !definition.is_struct() {
            return Err(Error::MemberAccessNotAStruct {
                src: ctx.source.clone(),
                span: lhs.span,
                got: self.types.name_of(checked_lhs.type_id),
            });
        }

        let definition = definition.as_struct();

        let Some(field) = definition.1.iter().find(|f| f.0 == *member) else {
            return Err(Error::MemberAccessUnknownField {
                src: ctx.source.clone(),
                span: member.span,
                struct_name: self.types.name_of(checked_lhs.type_id),
                field_name: member.name.clone(),
            });
        };

        Ok(CheckedExpr {
            type_id: field.1,
            kind: CheckedExprKind::MemberAccess {
                lhs: Box::new(checked_lhs),
                name: field.0.name.clone(),
            },
        })
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
