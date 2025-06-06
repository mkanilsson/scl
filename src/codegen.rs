use nanoid::nanoid;
use qbe::{Block, DataDef, DataItem, Function, Instr, Linkage, Module, Type, Value};

use crate::{
    ast::parsed::BinOp,
    typechecker::{
        ast::{CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt, CheckedTranslationUnit},
        tajp::{TypeCollection, VOID_TYPE_ID},
    },
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
            self.codegen_proc(proc, &mut module);
        }

        format!("{}", module)
    }

    fn codegen_proc<'a>(&'a self, proc: &CheckedProc, module: &mut Module<'a>) {
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
            self.codegen_stmt(stmt, block, module);
        }

        module.add_function(func);
    }

    fn codegen_stmt<'a>(
        &'a self,
        stmt: &CheckedStmt,
        block: &mut Block<'a>,
        module: &mut Module<'a>,
    ) {
        #[allow(unreachable_patterns)]
        match stmt {
            CheckedStmt::Return { value } => self.codegen_return_stmt(value, block, module),
            CheckedStmt::VariableDeclaration { name, value } => {
                self.codegen_variable_declaration_stmt(name, value, block, module)
            }
            CheckedStmt::Expr(expr) => {
                self.codegen_expr(expr, block, module);
            }
            stmt => todo!("codegen_stmt: {}", stmt),
        }
    }

    fn codegen_return_stmt<'a>(
        &'a self,
        value: &Option<CheckedExpr>,
        block: &mut Block<'a>,
        module: &mut Module<'a>,
    ) {
        let qbe_value = value
            .as_ref()
            .map(|expr| self.codegen_expr(expr, block, module).1);

        block.add_instr(Instr::Ret(qbe_value));
    }

    fn codegen_variable_declaration_stmt<'a>(
        &'a self,
        name: &str,
        value: &CheckedExpr,
        block: &mut Block<'a>,
        module: &mut Module<'a>,
    ) {
        let expr = self.codegen_expr(value, block, module);
        block.assign_instr(
            Value::Temporary(name.to_string()),
            expr.0,
            Instr::Copy(expr.1),
        );
    }

    fn codegen_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        block: &mut Block<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        #[allow(unreachable_patterns)]
        match &expr.kind {
            CheckedExprKind::Identifier(name) => self.codegen_identifier_expr(expr, name),
            CheckedExprKind::Number(value) => self.codegen_number_expr(expr, *value),
            CheckedExprKind::String(value) => self.codegen_string_expr(expr, value, module),
            CheckedExprKind::BinOp { lhs, op, rhs } => {
                self.codegen_binop_expr(lhs, *op, rhs, block, module)
            }
            CheckedExprKind::DirectCall {
                name,
                params,
                variadic_after,
            } => self.codegen_direct_call_expr(expr, name, params, *variadic_after, block, module),
            kind => todo!("codegen_expr: {}", kind),
        }
    }

    fn codegen_identifier_expr(&self, expr: &CheckedExpr, name: &str) -> (Type, Value) {
        (
            self.types.qbe_type_of(expr.type_id),
            Value::Temporary(name.to_string()),
        )
    }

    fn codegen_number_expr(&self, expr: &CheckedExpr, value: u64) -> (Type, Value) {
        (self.types.qbe_type_of(expr.type_id), Value::Const(value))
    }

    fn codegen_string_expr(
        &self,
        expr: &CheckedExpr,
        value: &str,
        module: &mut Module,
    ) -> (Type, Value) {
        let items = vec![
            (Type::Byte, DataItem::Str(value.to_string())),
            (Type::Byte, DataItem::Const(0)),
        ];

        let name = format!("str_{}", self.unique_tag());

        module.add_data(DataDef::new(Linkage::private(), name.clone(), None, items));

        (self.types.qbe_type_of(expr.type_id), Value::Global(name))
    }

    fn codegen_direct_call_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        name: &str,
        params: &Vec<CheckedExpr>,
        variadic_after: Option<u64>,
        block: &mut Block<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let result_value = Value::Temporary(format!("{name}_return_value_{}", self.unique_tag()));
        let result_type = self.types.qbe_type_of(expr.type_id);

        let mut generated_params = vec![];

        for param in params {
            generated_params.push(self.codegen_expr(param, block, module));
        }

        block.assign_instr(
            result_value.clone(),
            result_type.clone(),
            Instr::Call(name.to_string(), generated_params, variadic_after),
        );

        (result_type, result_value)
    }

    fn codegen_binop_expr<'a>(
        &'a self,
        lhs: &CheckedExpr,
        op: BinOp,
        rhs: &CheckedExpr,
        block: &mut Block<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_lhs = self.codegen_expr(lhs, block, module);
        let generated_rhs = self.codegen_expr(rhs, block, module);

        let binop_result = Value::Temporary(format!("binop_{}", self.unique_tag()));

        #[allow(unreachable_patterns)]
        let inst = match op {
            BinOp::Add => Instr::Add(generated_lhs.1, generated_rhs.1),
            BinOp::Multiply => Instr::Mul(generated_lhs.1, generated_rhs.1),
            BinOp::Subtract => Instr::Sub(generated_lhs.1, generated_rhs.1),
            BinOp::Divide => Instr::Div(generated_lhs.1, generated_rhs.1),
            compare => {
                let op = match compare {
                    BinOp::Equal => qbe::Cmp::Eq,
                    BinOp::NotEqual => qbe::Cmp::Ne,
                    _ => unreachable!(),
                };
                Instr::Cmp(
                    generated_lhs.0.clone(),
                    op,
                    generated_lhs.1,
                    generated_rhs.1,
                )
            }
            op => todo!("codegen_binop_expr: {}", op),
        };

        block.assign_instr(binop_result.clone(), generated_lhs.0.clone(), inst);

        (generated_lhs.0, binop_result)
    }

    fn unique_tag(&self) -> String {
        nanoid!(10).replace("-", "_")
    }
}
