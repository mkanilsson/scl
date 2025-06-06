use qbe::{Block, Function, Instr, Linkage, Module, Value};

use crate::typechecker::{
    ast::{CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt, CheckedTranslationUnit},
    tajp::{TypeCollection, VOID_TYPE_ID},
};

pub struct Codegen {
    unit: CheckedTranslationUnit,
    types: TypeCollection,
}

impl Codegen {
    pub fn new(unit: CheckedTranslationUnit, types: TypeCollection) -> Self {
        Self { unit, types }
    }

    pub fn generate(&mut self) -> String {
        let mut module = Module::new();

        for proc in &self.unit.procs {
            self.codegen_proc(&mut module, proc);
        }

        format!("{}", module)
    }

    fn codegen_proc(&self, module: &mut Module, proc: &CheckedProc) {
        let params = proc
            .params
            .iter()
            .map(|p| (self.types.qbe_type_of(p.1), Value::Temporary(p.0.clone())))
            .collect::<Vec<_>>();

        let return_type = if proc.return_type == VOID_TYPE_ID {
            None
        } else {
            Some(self.types.qbe_type_of(proc.return_type))
        };

        let mut func = Function::new(Linkage::public(), proc.name.clone(), params, return_type);
        let block = func.add_block("start");

        for stmt in &proc.stmts {
            self.codegen_stmt(stmt, block);
        }

        module.add_function(func);
    }

    fn codegen_stmt(&self, stmt: &CheckedStmt, block: &mut Block) {
        match stmt {
            CheckedStmt::Return { value } => self.codegen_return_stmt(value, block),
        }
    }

    fn codegen_return_stmt(&self, value: &Option<CheckedExpr>, block: &mut Block) {
        let qbe_value = value.as_ref().map(|expr| self.codegen_expr(expr));

        block.add_instr(Instr::Ret(qbe_value));
    }

    fn codegen_expr(&self, expr: &CheckedExpr) -> Value {
        match &expr.kind {
            CheckedExprKind::Identifier(name) => Value::Temporary(name.clone()),
            CheckedExprKind::Number(value) => Value::Const(*value),
        }
    }
}
