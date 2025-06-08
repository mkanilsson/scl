use std::{any::Any, collections::HashSet, fmt::Debug, str::FromStr};

use ast::{CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt, CheckedTranslationUnit};
use miette::SourceSpan;
use scope::Scope;
use tajp::{
    BOOL_TYPE_ID, I32_TYPE_ID, STRING_TYPE_ID, Type, TypeCollection, TypeId, U32_TYPE_ID,
    VOID_TYPE_ID,
};

use crate::{
    ast::parsed::{
        BinOp, Expr, ExprKind, ExternProcDefinition, Ident, ProcDefinition, Stmt, StmtKind,
        StructDefinition, TranslationUnit,
    },
    error::{Error, Result},
    helpers::string_join_with_and,
};

pub mod ast;
mod scope;
pub mod tajp;

pub struct Checker {
    pub types: TypeCollection,
    unit: TranslationUnit,
    scope: Scope,
}

impl Checker {
    pub fn new(unit: TranslationUnit) -> Self {
        Self {
            types: TypeCollection::new(),
            unit,
            scope: Scope::new(),
        }
    }

    pub fn check(&mut self) -> Result<CheckedTranslationUnit> {
        let structs = self.unit.structs.clone();

        let mut struct_and_type_ids = vec![];
        for s in structs {
            let type_id = self.add_struct_name(&s)?;
            struct_and_type_ids.push((s, type_id));
        }

        for s in struct_and_type_ids {
            self.define_struct(&s.0, s.1)?;
        }

        // TODO: Detect recursive structs

        let extern_procs = self.unit.extern_procs.clone();

        for extern_proc in extern_procs {
            self.add_extern_proc_types(&extern_proc)?;
        }

        let procs = self.unit.procs.clone();
        for proc in &procs {
            self.add_proc_types(proc)?;
        }

        let mut checked_procs = vec![];
        for proc in procs {
            checked_procs.push(self.typecheck_proc(&proc)?);
        }

        Ok(CheckedTranslationUnit {
            procs: checked_procs,
        })
    }

    fn add_proc_types(&mut self, definition: &ProcDefinition) -> Result<()> {
        let mut params: Vec<(Ident, TypeId)> = vec![];
        for param in &definition.params {
            if let Some(original) = params.iter().find(|p| p.0.name == param.0.name) {
                return Err(Error::ProcParmNameCollision {
                    src: self.unit.source.clone(),
                    original_span: original.0.span,
                    redefined_span: param.0.span,
                    name: param.0.name.clone(),
                });
            }

            let type_id = self.types.force_find(&self.unit.source, &param.1)?;
            params.push((param.0.clone(), type_id));
        }

        let return_type = self
            .types
            .force_find(&self.unit.source, &definition.return_type)?;
        let type_id = self.types.register_type(Type::Proc {
            params: params.iter().map(|p| p.1).collect::<Vec<_>>(),
            return_type,
            variadic: false,
        });

        if let Some((_, original)) = self.scope.find_with_original_span(&definition.ident) {
            return Err(Error::ProcNameCollision {
                src: self.unit.source.clone(),
                original_span: original,
                redefined_span: definition.ident.span,
                name: definition.ident.name.clone(),
            });
        }

        self.scope.add_to_scope(&definition.ident, type_id);
        Ok(())
    }

    fn add_struct_name(&mut self, s: &StructDefinition) -> Result<TypeId> {
        let type_id = self.types.register_undefined_struct(&s.ident);
        self.scope.add_to_scope(&s.ident, type_id);
        Ok(type_id)
    }

    fn define_struct(&mut self, s: &StructDefinition, type_id: TypeId) -> Result<()> {
        let mut fields: Vec<(Ident, TypeId)> = vec![];
        for field in &s.fields {
            if let Some(original) = fields.iter().find(|p| p.0.name == field.0.name) {
                return Err(Error::StructFieldNameCollision {
                    src: self.unit.source.clone(),
                    original_span: original.0.span,
                    redefined_span: field.0.span,
                    name: field.0.name.clone(),
                });
            }

            let type_id = self.types.force_find(&self.unit.source, &field.1)?;
            fields.push((field.0.clone(), type_id));
        }

        self.types.define_struct(
            type_id,
            Type::Struct {
                name: s.ident.clone(),
                fields,
            },
        );

        Ok(())
    }

    fn add_extern_proc_types(&mut self, definition: &ExternProcDefinition) -> Result<()> {
        let mut params: Vec<TypeId> = vec![];
        for param_type in &definition.params {
            let type_id = self.types.force_find(&self.unit.source, param_type)?;
            params.push(type_id);
        }

        let return_type = self
            .types
            .force_find(&self.unit.source, &definition.return_type)?;

        let type_id = self.types.register_type(Type::Proc {
            params: params.clone(),
            return_type,
            variadic: definition.variadic,
        });

        if let Some((_, original)) = self.scope.find_with_original_span(&definition.ident) {
            return Err(Error::ProcNameCollision {
                src: self.unit.source.clone(),
                original_span: original,
                redefined_span: definition.ident.span,
                name: definition.ident.name.clone(),
            });
        }

        self.scope.add_to_scope(&definition.ident, type_id);
        Ok(())
    }

    fn typecheck_proc(&mut self, proc: &ProcDefinition) -> Result<CheckedProc> {
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
            stmts.push(self.typecheck_stmt((definition.1, proc.return_type.span), stmt)?);
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
    ) -> Result<CheckedStmt> {
        match &stmt.kind {
            StmtKind::Return { value } => self.typecheck_return_stmt(return_type, stmt.span, value),
            StmtKind::VariableDeclaration { name, value } => {
                self.typecheck_variable_declaration_stmt(name, value)
            }
            StmtKind::Expr(expr) => {
                let expr = self.typecheck_expr(expr, None)?;
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
    ) -> Result<CheckedStmt> {
        match value {
            Some(value) => {
                if return_type.0 == VOID_TYPE_ID {
                    return Err(Error::ReturnShouldntHaveValue {
                        src: self.unit.source.clone(),
                        span: value.span,
                    });
                }

                let expr = self.typecheck_expr(value, Some(return_type))?;

                if expr.type_id != return_type.0 {
                    return Err(Error::ReturnValueDoesntMatch {
                        src: self.unit.source.clone(),
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
                        src: self.unit.source.clone(),
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
    ) -> Result<CheckedStmt> {
        let expr = self.typecheck_expr(expr, None)?;
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
    ) -> Result<CheckedExpr> {
        match &expr.kind {
            ExprKind::Identifier(ident) => {
                let type_id = self.scope.force_find(&self.unit.source, &ident)?;

                Ok(CheckedExpr {
                    type_id,
                    kind: ast::CheckedExprKind::Identifier(ident.name.clone()),
                })
            }
            ExprKind::Number(value) => {
                if let Some(wanted) = wanted {
                    match wanted.0 {
                        U32_TYPE_ID => {
                            let value =
                                self.verify_number::<u32>(value.as_str(), expr.span, U32_TYPE_ID)?;
                            Ok(CheckedExpr {
                                type_id: U32_TYPE_ID,
                                kind: ast::CheckedExprKind::Number(value as u64),
                            })
                        }
                        I32_TYPE_ID | _ => {
                            let value =
                                self.verify_number::<i32>(value.as_str(), expr.span, I32_TYPE_ID)?;
                            Ok(CheckedExpr {
                                type_id: I32_TYPE_ID,
                                kind: ast::CheckedExprKind::Number(value as u64),
                            })
                        }
                    }
                } else {
                    let value =
                        self.verify_number::<i32>(value.as_str(), expr.span, U32_TYPE_ID)?;
                    Ok(CheckedExpr {
                        type_id: I32_TYPE_ID,
                        kind: ast::CheckedExprKind::Number(value as u64),
                    })
                }
            }
            ExprKind::BinOp { lhs, op, rhs } => self.typecheck_binop_expr(lhs, *op, rhs, wanted),
            ExprKind::String(value) => Ok(CheckedExpr {
                type_id: STRING_TYPE_ID,
                kind: ast::CheckedExprKind::String(value.clone()),
            }),
            ExprKind::Call { expr, params } => self.typecheck_call_expr(expr, params, wanted),
            ExprKind::Bool(value) => Ok(CheckedExpr {
                type_id: BOOL_TYPE_ID,
                kind: ast::CheckedExprKind::Number(if *value { 1 } else { 0 }),
            }),
            ExprKind::Builtin(name, params) => self.typecheck_builtin_expr(expr, name, params),
            ExprKind::StructInstantiation { name, members } => {
                self.typecheck_struct_instantiation_expr(expr, name, members)
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
    ) -> Result<CheckedExpr> {
        let checked_lhs = self.typecheck_expr(lhs, wanted)?;
        let checked_rhs = self.typecheck_expr(rhs, wanted)?;

        if checked_lhs.type_id != checked_rhs.type_id {
            return Err(Error::BinOpSidesMismatch {
                src: self.unit.source.clone(),
                lhs_span: lhs.span,
                rhs_span: rhs.span,
                lhs_type_name: self.types.name_of(checked_lhs.type_id),
                rhs_type_name: self.types.name_of(checked_rhs.type_id),
            });
        }

        match op {
            BinOp::Divide | BinOp::Multiply | BinOp::Add | BinOp::Subtract => {
                self.typecheck_other_binop_expr(lhs, checked_lhs, op, rhs, checked_rhs)
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
    ) -> Result<CheckedExpr> {
        self.expect_number(&checked_lhs, lhs.span)?;
        self.expect_number(&checked_rhs, rhs.span)?;

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
    ) -> Result<CheckedExpr> {
        let checked_expr = self.typecheck_expr(expr, wanted)?;

        let ident = match checked_expr.kind {
            CheckedExprKind::Identifier(name) => name,
            _ => todo!("Indirect calls"),
        };

        let proc_type = self
            .types
            .get_definition(
                self.scope
                    .force_find_from_string(&self.unit.source, &ident)?,
            )
            .as_proc();

        if params.len() < proc_type.0.len() || (params.len() > proc_type.0.len() && !proc_type.2) {
            return Err(Error::ProcCallParamCountMismatch {
                src: self.unit.source.clone(),
                span: expr.span,
                expected: proc_type.0.len(),
                got: params.len(),
                variadic: proc_type.2,
            });
        }

        let mut params = params.clone();
        let non_variadic_params = params.drain(0..proc_type.0.len()).zip(&proc_type.0);

        let mut checked_params = vec![];
        for param in non_variadic_params {
            // TODO: Get the location of the suspected span or allow the span to be optional
            checked_params.push(self.typecheck_expr(&param.0, Some((*param.1, (0..0).into())))?);
        }

        for param in &params {
            checked_params.push(self.typecheck_expr(param, None)?);
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
    ) -> Result<CheckedExpr> {
        match name {
            "type_name" => {
                if params.len() != 1 {
                    return Err(Error::BuiltinParamCountMismatch {
                        src: self.unit.source.clone(),
                        span: expr.span,
                        name: name.to_string(),
                        expected: 1,
                        got: params.len(),
                        variadic: false,
                    });
                }

                let checked_param = self.typecheck_expr(&params[0], None)?;

                Ok(CheckedExpr {
                    type_id: STRING_TYPE_ID,
                    kind: CheckedExprKind::String(self.types.name_of(checked_param.type_id)),
                })
            }
            name => Err(Error::UnknownBuiltin {
                src: self.unit.source.clone(),
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
    ) -> Result<CheckedExpr> {
        let struct_type_id = self.types.force_find_by_name(&self.unit.source, name)?;

        let definition = self.types.get_definition(struct_type_id);
        if !definition.is_struct() {
            return Err(Error::StructInstantiationOnNonStruct {
                src: self.unit.source.clone(),
                span: name.span,
            });
        }

        let s = definition.as_struct();

        let mut checked_fields: Vec<(Ident, CheckedExpr)> = vec![];

        for field in fields {
            let defined_field = s.1.iter().find(|f| f.0 == field.0);
            let Some(defined_field) = defined_field else {
                return Err(Error::StructInstantiationFieldDoesntExist {
                    src: self.unit.source.clone(),
                    span: field.0.span,
                    struct_name: name.name.clone(),
                    field_name: field.0.name.clone(),
                });
            };

            if let Some(existing_field) = checked_fields.iter().find(|f| f.0 == field.0) {
                return Err(Error::StructInstantiationFieldAlreadyDeclared {
                    src: self.unit.source.clone(),
                    original_span: existing_field.0.span,
                    redefined_span: field.0.span,
                    field_name: field.0.name.clone(),
                });
            }

            let checked_expr =
                self.typecheck_expr(&field.1, Some((defined_field.1, defined_field.0.span)))?;

            if checked_expr.type_id != defined_field.1 {
                return Err(Error::StructInstantiationFieldTypeMismatch {
                    src: self.unit.source.clone(),
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
                if let None = checked_fields.iter().find(|c| c.0 == known_fields.0) {
                    missing_fields.push(format!("'{}'", known_fields.0.name));
                }
            }

            return Err(Error::StructInstantiationMissingFields {
                src: self.unit.source.clone(),
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

    fn expect_number(&self, expr: &CheckedExpr, span: SourceSpan) -> Result<()> {
        if !self.types.is_number(expr.type_id) {
            Err(Error::ExpectedButGot {
                src: self.unit.source.clone(),
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
    ) -> Result<T> {
        if let Ok(value) = value.parse::<T>() {
            Ok(value)
        } else {
            Err(Error::InvalidNumber {
                src: self.unit.source.clone(),
                span,
                value: value.to_string(),
                type_name: self.types.name_of(type_id),
            })
        }
    }
}

impl Debug for Checker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Checker")
            .field("types", &self.types)
            .field("scope", &self.scope)
            .finish_non_exhaustive()
    }
}
