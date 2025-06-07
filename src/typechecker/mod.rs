use std::{fmt::Debug, str::FromStr};

use ast::{CheckedExpr, CheckedProc, CheckedStmt, CheckedTranslationUnit};
use miette::SourceSpan;
use scope::Scope;
use tajp::{I32_TYPE_ID, STRING_TYPE_ID, Type, TypeCollection, TypeId, U32_TYPE_ID, VOID_TYPE_ID};

use crate::{
    ast::parsed::{
        BinOp, Expr, ExprKind, ExternProcDefinition, Ident, ProcDefinition, Stmt, StmtKind,
        TranslationUnit,
    },
    error::{Error, Result},
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

    fn add_extern_proc_types(&mut self, definition: &ExternProcDefinition) -> Result<()> {
        let mut params: Vec<TypeId> = vec![];
        for param_type in &definition.params {
            let type_id = self.types.force_find(&self.unit.source, &param_type)?;
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
                let type_id = self
                    .scope
                    .force_find(&self.unit.source, &Ident::from_string(ident))?;

                Ok(CheckedExpr {
                    type_id,
                    kind: ast::CheckedExprKind::Identifier(ident.clone()),
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

        self.expect_number(&checked_lhs, lhs.span)?;
        self.expect_number(&checked_rhs, rhs.span)?;

        if checked_lhs.type_id != checked_rhs.type_id {
            return Err(Error::BinOpSidesMismatch {
                src: self.unit.source.clone(),
                lhs_span: lhs.span,
                rhs_span: rhs.span,
                lhs_type_name: self.types.name_of(checked_lhs.type_id),
                rhs_type_name: self.types.name_of(checked_rhs.type_id),
            });
        }

        Ok(CheckedExpr {
            type_id: checked_lhs.type_id,
            kind: ast::CheckedExprKind::BinOp {
                lhs: Box::new(checked_lhs),
                op,
                rhs: Box::new(checked_rhs),
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
