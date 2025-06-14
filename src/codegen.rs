use qbe::{DataDef, DataItem, Function, Instr, Linkage, Module, Type, Value};

use crate::{
    ast::parsed::BinOp,
    typechecker::{
        Checker,
        ast::{CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt, CheckedTranslationUnit},
        tajp::{MemoryLayout, VOID_TYPE_ID},
    },
};

static mut UNIQUE_TAG_COUNT: usize = 0;

pub struct Codegen {
    units: Vec<CheckedTranslationUnit>,
    checker: Checker,
}

impl Codegen {
    pub fn new(units: Vec<CheckedTranslationUnit>, checker: Checker) -> Self {
        Self { units, checker }
    }

    pub fn generate(&mut self) -> String {
        let mut module = Module::new();

        // TODO: Sort order after usage since qbe requires
        //       it to be specified in the order of usage
        for type_id in &self.checker.types.structs {
            let s = self.checker.types.qbe_type_def_of(*type_id);

            module.add_type(s.clone());
        }

        for unit in &self.units {
            for proc in &unit.procs {
                self.codegen_proc(proc, &mut module);
            }
        }

        format!("{}", module)
    }

    fn codegen_proc<'a>(&'a self, proc: &CheckedProc, module: &mut Module<'a>) {
        let params = proc
            .params
            .iter()
            .map(|p| {
                (
                    self.checker.types.qbe_type_of(p.1),
                    Value::Temporary(p.0.clone()),
                )
            })
            .collect::<Vec<_>>();

        let return_type = if proc.return_type == VOID_TYPE_ID {
            None
        } else {
            Some(self.checker.types.qbe_type_of(proc.return_type))
        };

        let mut func = Function::new(Linkage::public(), proc.name.clone(), params, return_type);
        let block = func.add_block("start");

        for stmt in &proc.stmts {
            self.codegen_stmt(stmt, &mut func, module);
        }

        module.add_function(func);
    }

    fn codegen_stmt<'a>(
        &'a self,
        stmt: &CheckedStmt,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        #[allow(unreachable_patterns)]
        match stmt {
            CheckedStmt::Return { value } => self.codegen_return_stmt(value, function, module),
            CheckedStmt::VariableDeclaration { name, value } => {
                self.codegen_variable_declaration_stmt(name, value, function, module)
            }
            CheckedStmt::Expr(expr) => {
                self.codegen_expr(expr, function, module);
            }
            stmt => todo!("codegen_stmt: {}", stmt),
        }
    }

    fn codegen_return_stmt<'a>(
        &'a self,
        value: &Option<CheckedExpr>,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        let qbe_value = value
            .as_ref()
            .map(|expr| self.codegen_expr(expr, function, module).1);

        function.add_instr(Instr::Ret(qbe_value));
    }

    fn codegen_variable_declaration_stmt<'a>(
        &'a self,
        name: &str,
        value: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        let expr = self.codegen_expr(value, function, module);
        function.assign_instr(
            Value::Temporary(name.to_string()),
            expr.0,
            Instr::Copy(expr.1),
        );
    }

    fn codegen_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        #[allow(unreachable_patterns)]
        match &expr.kind {
            CheckedExprKind::Identifier(name) => self.codegen_identifier_expr(expr, name),
            CheckedExprKind::Number(value) => self.codegen_number_expr(expr, *value),
            CheckedExprKind::String(value) => self.codegen_string_expr(expr, value, module),
            CheckedExprKind::BinOp { lhs, op, rhs } => {
                self.codegen_binop_expr(lhs, *op, rhs, function, module)
            }
            CheckedExprKind::DirectCall {
                name,
                params,
                variadic_after,
            } => {
                self.codegen_direct_call_expr(expr, name, params, *variadic_after, function, module)
            }
            CheckedExprKind::StructInstantiation { name, fields } => {
                self.codegen_struct_instantation_expr(expr, name, fields, function, module)
            }
            CheckedExprKind::MemberAccess { lhs, name } => {
                self.codegen_member_access_expr(expr, lhs, name, function, module)
            }
            CheckedExprKind::If {
                condition,
                false_block,
                true_block,
            } => self.codegen_if_expr(condition, true_block, false_block, function, module),
            kind => todo!("codegen_expr: {}", kind),
        }
    }

    fn codegen_identifier_expr(&self, expr: &CheckedExpr, name: &str) -> (Type, Value) {
        (
            self.checker.types.qbe_type_of(expr.type_id),
            Value::Temporary(name.to_string()),
        )
    }

    fn codegen_number_expr(&self, expr: &CheckedExpr, value: u64) -> (Type, Value) {
        (
            self.checker.types.qbe_type_of(expr.type_id),
            Value::Const(value),
        )
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

        let name = format!("str.{}", self.unique_tag());

        module.add_data(DataDef::new(Linkage::private(), name.clone(), None, items));

        (
            self.checker.types.qbe_type_of(expr.type_id),
            Value::Global(name),
        )
    }

    fn codegen_direct_call_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        name: &str,
        params: &Vec<CheckedExpr>,
        variadic_after: Option<u64>,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let mut result_value =
            Value::Temporary(format!("{name}.return_value.{}", self.unique_tag()));
        let result_type = self.checker.types.qbe_type_of(expr.type_id);

        let mut generated_params = vec![];

        for param in params {
            generated_params.push(self.codegen_expr(param, function, module));
        }

        // NOTE: There is a bug here causing the wrong calling convention
        //       if the return_type is an aggregate type
        //       (https://github.com/garritfra/qbe-rs/issues/35)
        function.assign_instr(
            result_value.clone(),
            result_type.clone(),
            Instr::Call(name.to_string(), generated_params, variadic_after),
        );

        let definition = self.checker.types.get_definition(expr.type_id);
        if definition.is_struct() {
            let memory_layout = self.checker.types.memory_layout_of_definition(&definition);
            let struct_storage = self.allocate(&memory_layout, function);

            self.copy_struct_fields(&memory_layout, &struct_storage, &result_value, function);
            result_value = struct_storage;
        }

        (result_type, result_value)
    }

    fn codegen_struct_instantation_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        name: &str,
        fields: &Vec<(String, CheckedExpr)>,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let memory_layout = self.checker.types.memory_layout_of(expr.type_id);

        let struct_value = self.allocate(&memory_layout, function);
        let fields_offsets = memory_layout.fields.expect("Struct to have fields");

        for field in fields {
            // TODO: Copy from other struct
            let expr = self.codegen_expr(&field.1, function, module);

            // Get offset
            let offset_value = Value::Temporary(format!("offset.{}", self.unique_tag()));
            let field_layout = fields_offsets.get(&field.0).unwrap();

            function.blocks.last_mut().unwrap().add_comment(format!(
                "Store value into {name}.{} (offset: {})",
                field.0, field_layout.offset
            ));
            function.assign_instr(
                offset_value.clone(),
                Type::Long,
                Instr::Add(
                    struct_value.clone(),
                    Value::Const(field_layout.offset as u64),
                ),
            );

            // TODO: Copy struct fields if there is a nested struct
            // Store value
            function.add_instr(Instr::Store(expr.0, offset_value, expr.1));
        }

        (self.checker.types.qbe_type_of(expr.type_id), struct_value)
    }

    fn codegen_member_access_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        lhs: &CheckedExpr,
        name: &str,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let memory_layout = self.checker.types.memory_layout_of(lhs.type_id);
        let fields = memory_layout.fields.unwrap();
        let field_layout = fields.get(name).unwrap();

        let generated_lhs = self.codegen_expr(lhs, function, module);

        let offset_value = Value::Temporary(format!("offset.{}", self.unique_tag()));

        function.assign_instr(
            offset_value.clone(),
            Type::Long,
            Instr::Add(
                generated_lhs.1.clone(),
                Value::Const(field_layout.offset as u64),
            ),
        );

        let result_value = Value::Temporary(format!("member_access.{}", self.unique_tag()));
        let result_type = self.checker.types.qbe_type_of(expr.type_id);

        function.assign_instr(
            result_value.clone(),
            result_type.clone(),
            Instr::Load(result_type.clone(), offset_value),
        );

        (result_type, result_value)
    }

    fn codegen_if_expr<'a>(
        &'a self,
        condition: &CheckedExpr,
        true_block: &Vec<CheckedStmt>,
        false_block: &Vec<CheckedStmt>,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let unique_tag = self.unique_tag();
        let true_block_tag = format!("if.true.{}", unique_tag);
        let false_block_tag = format!("if.false.{}", unique_tag);
        let after_block_tag = format!("if.after.{}", unique_tag);

        // TODO: Convert to Word size if it's something else
        let condition = self.codegen_expr(condition, function, module);
        function.add_instr(Instr::Jnz(
            condition.1,
            true_block_tag.clone(),
            false_block_tag.clone(),
        ));

        self.codegen_block(&true_block_tag, true_block, function, module);
        function.add_instr(Instr::Jmp(after_block_tag.clone()));

        self.codegen_block(&false_block_tag, false_block, function, module);
        function.add_instr(Instr::Jmp(after_block_tag.clone()));

        function.add_block(after_block_tag);

        (Type::Word, Value::Temporary("todo_if_as_exprs".into()))
    }

    fn codegen_block<'a>(
        &'a self,
        name: &str,
        stmts: &Vec<CheckedStmt>,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        function.add_block(name);

        for stmt in stmts {
            self.codegen_stmt(stmt, function, module);
        }
    }

    fn codegen_binop_expr<'a>(
        &'a self,
        lhs: &CheckedExpr,
        op: BinOp,
        rhs: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_lhs = self.codegen_expr(lhs, function, module);
        let generated_rhs = self.codegen_expr(rhs, function, module);

        let binop_result = Value::Temporary(format!("binop.{}", self.unique_tag()));

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

        function.assign_instr(binop_result.clone(), generated_lhs.0.clone(), inst);

        (generated_lhs.0, binop_result)
    }

    fn allocate<'a>(&'a self, memory_layout: &MemoryLayout, function: &mut Function<'a>) -> Value {
        let instr = match memory_layout.alignment {
            1 | 2 => todo!(),
            4 => Instr::Alloc4(memory_layout.size as u32),
            8 => Instr::Alloc8(memory_layout.size as u64),
            16 => Instr::Alloc16(memory_layout.size as u128),
            _ => unreachable!(),
        };

        let value_name = format!("allocated.{}", self.unique_tag());
        let value = Value::Temporary(value_name);
        function.assign_instr(value.clone(), Type::Long, instr);

        value
    }

    fn copy_struct_fields<'a>(
        &'a self,
        memory_layout: &MemoryLayout,
        destination: &Value,
        source: &Value,
        function: &mut Function<'a>,
    ) {
        function.add_instr(Instr::Blit(
            source.clone(),
            destination.clone(),
            memory_layout.size as u64,
        ));
    }

    fn unique_tag(&self) -> usize {
        // SAFETY: Codegen is single threaded
        unsafe {
            UNIQUE_TAG_COUNT += 1;
            UNIQUE_TAG_COUNT
        }
    }
}
