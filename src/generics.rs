use std::collections::HashMap;

use crate::typechecker::{
    Checker,
    ast::{CheckedBlock, CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt},
    proc::ProcId,
    stack::StackSlots,
    tajp::TypeId,
};

pub struct Generic<'a> {
    checker: &'a Checker,
}

impl<'a> Generic<'a> {
    pub fn transform(checker: &'a Checker) -> Vec<CheckedProc> {
        let generic = Self { checker };

        let mut procs = vec![];
        for instance in &checker.generic_instances {
            let proc = checker
                .generic_procs
                .get(&instance.original_proc_id)
                .unwrap();

            procs.push(generic.transform_proc(proc, instance.instance_proc_id, &instance.mapping));
        }

        procs
    }

    fn transform_type_id(&self, type_id: TypeId, mapping: &HashMap<TypeId, TypeId>) -> TypeId {
        *mapping.get(&type_id).unwrap_or(&type_id)
    }

    fn transform_proc(
        &self,
        proc: &CheckedProc,
        new_proc_id: ProcId,
        mapping: &HashMap<TypeId, TypeId>,
    ) -> CheckedProc {
        let slots = proc
            .stack_slots
            .slots
            .iter()
            .map(|t| self.transform_type_id(*t, mapping))
            .collect();

        CheckedProc {
            proc_id: new_proc_id,
            body: self.transform_block(&proc.body, mapping),
            params: proc.params.clone(),
            return_type: self.transform_type_id(proc.return_type, mapping),
            stack_slots: StackSlots { slots },
            has_this: proc.has_this,
        }
    }

    fn transform_block(
        &self,
        block: &CheckedBlock,
        mapping: &HashMap<TypeId, TypeId>,
    ) -> CheckedBlock {
        CheckedBlock {
            stmts: block
                .stmts
                .iter()
                .map(|stmt| self.transform_stmt(stmt, mapping))
                .collect(),
            last: block
                .last
                .as_ref()
                .map(|last| self.transform_expr(&last, mapping)),
            type_id: self.transform_type_id(block.type_id, mapping),
            deferred: block
                .deferred
                .iter()
                .map(|deferred| {
                    deferred
                        .iter()
                        .map(|expr| self.transform_expr(expr, mapping))
                        .collect()
                })
                .collect(),
        }
    }

    fn transform_expr(&self, expr: &CheckedExpr, mapping: &HashMap<TypeId, TypeId>) -> CheckedExpr {
        let new_kind = match &expr.kind {
            CheckedExprKind::StackValue(_)
            | CheckedExprKind::Identifier(_)
            | CheckedExprKind::Number(_)
            | CheckedExprKind::String(_)
            | CheckedExprKind::This => expr.kind.clone(),
            CheckedExprKind::Proc { .. } | CheckedExprKind::InterfaceProc { .. } => unreachable!(),
            CheckedExprKind::BinOp { lhs, op, rhs } => CheckedExprKind::BinOp {
                lhs: Box::new(self.transform_expr(&lhs, mapping)),
                op: *op,
                rhs: Box::new(self.transform_expr(&rhs, mapping)),
            },
            CheckedExprKind::DirectCall {
                proc_id,
                params,
                variadic_after,
                stack_slot,
            } => CheckedExprKind::DirectCall {
                proc_id: *proc_id,
                params: params
                    .iter()
                    .map(|expr| self.transform_expr(expr, mapping))
                    .collect(),
                variadic_after: *variadic_after,
                stack_slot: *stack_slot,
            },
            CheckedExprKind::InterfaceCall {
                for_type_id,
                proc_id,
                interface_id,
                params,
                variadic_after,
                stack_slot,
            } => {
                let for_type_id = self.transform_type_id(*for_type_id, mapping);
                let proc_id = self
                    .checker
                    .implementations
                    .map_proc_via_interface_and_type(*proc_id, for_type_id, *interface_id);

                CheckedExprKind::DirectCall {
                    proc_id,
                    params: params
                        .iter()
                        .map(|expr| self.transform_expr(expr, mapping))
                        .collect(),
                    variadic_after: *variadic_after,
                    stack_slot: *stack_slot,
                }
            }
            CheckedExprKind::StructInstantiation {
                name,
                stack_slot,
                fields,
            } => CheckedExprKind::StructInstantiation {
                name: name.clone(),
                stack_slot: *stack_slot,
                fields: fields
                    .iter()
                    .map(|field| (field.0.clone(), self.transform_expr(&field.1, mapping)))
                    .collect(),
            },
            CheckedExprKind::MemberAccess { lhs, name } => CheckedExprKind::MemberAccess {
                lhs: Box::new(self.transform_expr(&lhs, mapping)),
                name: name.clone(),
            },
            CheckedExprKind::If {
                stack_slot,
                condition,
                false_block,
                true_block,
            } => CheckedExprKind::If {
                stack_slot: *stack_slot,
                condition: Box::new(self.transform_expr(&condition, mapping)),
                false_block: Box::new(self.transform_block(&false_block, mapping)),
                true_block: Box::new(self.transform_block(&true_block, mapping)),
            },
            CheckedExprKind::Assignment { lhs, rhs } => CheckedExprKind::Assignment {
                lhs: Box::new(self.transform_expr(&lhs, mapping)),
                rhs: Box::new(self.transform_expr(&rhs, mapping)),
            },
            CheckedExprKind::AddressOf { expr } => CheckedExprKind::AddressOf {
                expr: Box::new(self.transform_expr(&expr, mapping)),
            },
            CheckedExprKind::Deref {
                type_id,
                expr,
                stack_slot,
            } => CheckedExprKind::Deref {
                type_id: self.transform_type_id(*type_id, mapping),
                expr: Box::new(self.transform_expr(&expr, mapping)),
                stack_slot: *stack_slot,
            },
            CheckedExprKind::Block(checked_block) => {
                CheckedExprKind::Block(Box::new(self.transform_block(&checked_block, mapping)))
            }
            CheckedExprKind::Store { expr, stack_slot } => CheckedExprKind::Store {
                expr: Box::new(self.transform_expr(&expr, mapping)),
                stack_slot: *stack_slot,
            },
            CheckedExprKind::ArrayInstantiation { exprs, stack_slot } => {
                CheckedExprKind::ArrayInstantiation {
                    exprs: exprs
                        .iter()
                        .map(|expr| self.transform_expr(expr, mapping))
                        .collect(),
                    stack_slot: *stack_slot,
                }
            }
            CheckedExprKind::ArrayAccess { lhs, index } => CheckedExprKind::ArrayAccess {
                lhs: Box::new(self.transform_expr(&lhs, mapping)),
                index: Box::new(self.transform_expr(&index, mapping)),
            },
            CheckedExprKind::SizeOf(type_id) => CheckedExprKind::SizeOf(*type_id),
            CheckedExprKind::F64ToF32(checked_expr) => {
                CheckedExprKind::F64ToF32(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::F32ToF64(checked_expr) => {
                CheckedExprKind::F32ToF64(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::F32ToSigned(checked_expr) => {
                CheckedExprKind::F32ToSigned(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::F32ToUnsigned(checked_expr) => CheckedExprKind::F32ToUnsigned(
                Box::new(self.transform_expr(&checked_expr, mapping)),
            ),
            CheckedExprKind::F64ToSigned(checked_expr) => {
                CheckedExprKind::F64ToSigned(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::F64ToUnsigned(checked_expr) => CheckedExprKind::F64ToUnsigned(
                Box::new(self.transform_expr(&checked_expr, mapping)),
            ),
            CheckedExprKind::I32ToFloat(checked_expr) => {
                CheckedExprKind::I32ToFloat(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::U32ToFloat(checked_expr) => {
                CheckedExprKind::U32ToFloat(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::I64ToFloat(checked_expr) => {
                CheckedExprKind::I64ToFloat(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::U64ToFloat(checked_expr) => {
                CheckedExprKind::U64ToFloat(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::SignExtend8(checked_expr) => {
                CheckedExprKind::SignExtend8(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::ZeroExtend8(checked_expr) => {
                CheckedExprKind::ZeroExtend8(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::SignExtend16(checked_expr) => {
                CheckedExprKind::SignExtend16(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::ZeroExtend16(checked_expr) => {
                CheckedExprKind::ZeroExtend16(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::SignExtend32(checked_expr) => {
                CheckedExprKind::SignExtend32(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
            CheckedExprKind::ZeroExtend32(checked_expr) => {
                CheckedExprKind::ZeroExtend32(Box::new(self.transform_expr(&checked_expr, mapping)))
            }
        };

        CheckedExpr {
            type_id: self.transform_type_id(expr.type_id, mapping),
            lvalue: expr.lvalue,
            kind: new_kind,
        }
    }

    fn transform_stmt(&self, stmt: &CheckedStmt, mapping: &HashMap<TypeId, TypeId>) -> CheckedStmt {
        match stmt {
            CheckedStmt::Return { value } => CheckedStmt::Return {
                value: value
                    .as_ref()
                    .map(|value| self.transform_expr(value, mapping)),
            },
            CheckedStmt::VariableDeclaration { stack_slot, value } => {
                CheckedStmt::VariableDeclaration {
                    stack_slot: *stack_slot,
                    value: self.transform_expr(value, mapping),
                }
            }
            CheckedStmt::Expr(checked_expr) => {
                CheckedStmt::Expr(self.transform_expr(checked_expr, mapping))
            }
            CheckedStmt::While { condition, body } => CheckedStmt::While {
                condition: self.transform_expr(condition, mapping),
                body: self.transform_block(body, mapping),
            },
            CheckedStmt::None => CheckedStmt::None,
        }
    }
}
