use std::cell::Cell;

use qbe::{DataDef, DataItem, Function, Instr, Linkage, Module, Type, Value};

use crate::{
    ast::parsed::BinOp,
    typechecker::{
        Checker,
        ast::{
            CheckedBlock, CheckedExpr, CheckedExprKind, CheckedProc, CheckedStmt,
            CheckedTranslationUnit,
        },
        stack::StackSlotId,
        tajp::{TypeId, VOID_TYPE_ID},
    },
};

pub struct Codegen {
    units: Vec<CheckedTranslationUnit>,
    checker: Checker,
    counter: Cell<usize>,
}

impl Codegen {
    pub fn new(units: Vec<CheckedTranslationUnit>, checker: Checker) -> Self {
        Self {
            units,
            checker,
            counter: Cell::new(0),
        }
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
                    self.checker
                        .types
                        .qbe_type_of(proc.stack_slots.type_of(p.1)),
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

        func.add_block("start");
        for (slot, type_id) in proc.stack_slots.slots.iter().enumerate() {
            self.allocate_stack_slot(slot.into(), *type_id, &mut func);
        }

        for param in &proc.params {
            let type_id = proc.stack_slots.type_of(param.1);
            let definition = self.checker.types.get_definition(type_id);

            match definition {
                crate::typechecker::tajp::Type::Struct { .. } => {
                    let memory_layout = self.checker.types.memory_layout_of_definition(&definition);

                    func.add_instr(Instr::Blit(
                        Value::Temporary(param.0.clone()),
                        Value::Temporary(param.1.qbe_name()),
                        memory_layout.size as u64,
                    ));
                }
                _ => func.add_instr(Instr::Store(
                    self.checker.types.qbe_type_of(type_id),
                    Value::Temporary(param.1.qbe_name()),
                    Value::Temporary(param.0.clone()),
                )),
            }
        }

        if let Some(result) = self.codegen_block("body", &proc.body, &mut func, module) {
            func.add_instr(Instr::Ret(Some(result.1)));
        }

        module.add_function(func);
    }

    fn allocate_stack_slot<'a>(
        &'a self,
        id: StackSlotId,
        type_id: TypeId,
        function: &mut Function<'a>,
    ) {
        let definition = self.checker.types.get_definition(type_id);
        let memory_layout = self.checker.types.memory_layout_of_definition(&definition);

        let instr = match memory_layout.alignment {
            1 | 2 | 4 => Instr::Alloc4(memory_layout.size as u32),
            8 => Instr::Alloc8(memory_layout.size as u64),
            16 => Instr::Alloc16(memory_layout.size as u128),
            _ => unreachable!(),
        };

        function.assign_instr(Value::Temporary(id.qbe_name()), Type::Long, instr);
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
            CheckedStmt::VariableDeclaration { stack_slot, value } => {
                self.codegen_variable_declaration_stmt(*stack_slot, value, function, module)
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
        stack_slot: StackSlotId,
        value: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        let expr = self.codegen_expr(value, function, module);

        let dest = Value::Temporary(stack_slot.qbe_name());
        self.store(expr.0, expr.1, dest, function);
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
                stack_slot,
            } => self.codegen_direct_call_expr(
                expr,
                name,
                params,
                *variadic_after,
                *stack_slot,
                function,
                module,
            ),
            CheckedExprKind::StructInstantiation {
                name,
                fields,
                stack_slot,
            } => self.codegen_struct_instantation_expr(
                expr,
                name,
                fields,
                *stack_slot,
                function,
                module,
            ),
            CheckedExprKind::MemberAccess { lhs, name } => {
                self.codegen_member_access_expr(expr, lhs, name, function, module)
            }
            CheckedExprKind::If {
                condition,
                false_block,
                true_block,
            } => self.codegen_if_expr(condition, true_block, false_block, function, module),
            CheckedExprKind::StackValue(stack_slot) => {
                self.codegen_stack_value_expr(expr.type_id, *stack_slot, function)
            }
            CheckedExprKind::Assignment { lhs, rhs } => {
                self.codegen_assignment_expr(lhs, rhs, function, module)
            }
            CheckedExprKind::AddressOf { expr } => {
                self.codegen_address_of_expr(expr, function, module)
            }
            CheckedExprKind::DerefLValue {
                read_stack_slot,
                read_offset,
                type_id,
                store_stack_slot,
            } => self.codegen_deref_lvalue_expr(
                *type_id,
                *read_stack_slot,
                *read_offset,
                *store_stack_slot,
                function,
            ),
            CheckedExprKind::DerefRValue {
                type_id,
                stack_slot,
                expr,
            } => self.codegen_deref_rvalue_expr(*type_id, expr, *stack_slot, function, module),
            CheckedExprKind::Block(block) => self
                .codegen_block(
                    &format!(".block.{}", self.unique_tag()),
                    block,
                    function,
                    module,
                )
                .unwrap(),
            CheckedExprKind::Store { expr, stack_slot } => {
                self.codegen_store_expr(expr, *stack_slot, function, module)
            }
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
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let result_value = Value::Temporary(format!("{name}.return_value.{}", self.unique_tag()));
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

        let dest = Value::Temporary(stack_slot.qbe_name());
        let dest = self.store_if_needed(result_type.clone(), result_value, dest, function);

        (result_type, dest)
    }

    fn codegen_struct_instantation_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        name: &str,
        fields: &Vec<(String, CheckedExpr)>,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let memory_layout = self.checker.types.memory_layout_of(expr.type_id);

        let fields_offsets = memory_layout.fields.expect("Struct to have fields");

        let dest = Value::Temporary(stack_slot.qbe_name());
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
                Instr::Add(dest.clone(), Value::Const(field_layout.offset as u64)),
            );

            self.store(expr.0, expr.1, offset_value, function);
        }

        (self.checker.types.qbe_type_of(expr.type_id), dest)
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

        let result_type = self.checker.types.qbe_type_of(expr.type_id);

        match result_type {
            Type::Aggregate(_) => (Type::Long, offset_value),
            _ => {
                let result_value = Value::Temporary(format!("member_access.{}", self.unique_tag()));

                function.assign_instr(
                    result_value.clone(),
                    result_type.clone(),
                    Instr::Load(result_type.clone(), offset_value),
                );

                (result_type, result_value)
            }
        }
    }

    fn codegen_stack_value_expr<'a>(
        &'a self,
        type_id: TypeId,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
    ) -> (Type<'a>, Value) {
        let tajp = self.checker.types.qbe_type_of(type_id);

        let value = Value::Temporary(stack_slot.qbe_name());
        match tajp {
            Type::Aggregate(_) => (tajp, value),
            _ => {
                let name = format!("sv.{}", self.unique_tag());
                let dest = Value::Temporary(name);
                function.assign_instr(dest.clone(), tajp.clone(), Instr::Load(tajp.clone(), value));
                (tajp, dest)
            }
        }
    }

    fn codegen_address_of_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_expr = self.codegen_expr_for_read(expr, function, module);
        (Type::Long, generated_expr)
    }

    fn codegen_address_of_rvalue_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_expr = self.codegen_expr(expr, function, module);
        let value = Value::Temporary(stack_slot.qbe_name());

        self.store(generated_expr.0, generated_expr.1, value.clone(), function);

        (Type::Long, value)
    }

    fn codegen_deref_lvalue_expr<'a>(
        &'a self,
        type_id: TypeId,
        read_stack_slot: StackSlotId,
        read_offset: u64,
        store_stack_slot: StackSlotId,
        function: &mut Function<'a>,
    ) -> (Type<'a>, Value) {
        // Get the offset into the stackslot where the ptr is stored
        let src = Value::Temporary(format!(".deref.src.{}", self.unique_tag()));
        function.assign_instr(
            src.clone(),
            Type::Long,
            Instr::Add(
                Value::Temporary(read_stack_slot.qbe_name()),
                Value::Const(read_offset),
            ),
        );

        // Read the ptr value
        let ptr = Value::Temporary(format!(".deref.ptr.{}", self.unique_tag()));
        function.assign_instr(ptr.clone(), Type::Long, Instr::Load(Type::Long, src));

        let dest = Value::Temporary(store_stack_slot.qbe_name());
        let t = self.checker.types.qbe_type_of(type_id);

        // Load the data from the ptr and store it in the new slot
        self.load(t.clone(), ptr, dest.clone(), function);

        (t, dest)
    }

    fn codegen_deref_rvalue_expr<'a>(
        &'a self,
        type_id: TypeId,
        expr: &CheckedExpr,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_expr = self.codegen_expr(expr, function, module);

        let dest = Value::Temporary(stack_slot.qbe_name());
        let t = self.checker.types.qbe_type_of(type_id);

        // Load the data from the ptr and store it in the new slot
        self.load(t.clone(), generated_expr.1, dest.clone(), function);

        (t, dest)
    }

    fn codegen_assignment_expr<'a>(
        &'a self,
        lhs: &CheckedExpr,
        rhs: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generate_lhs = self.codegen_expr_for_read(lhs, function, module);
        let generated_rhs = self.codegen_expr(rhs, function, module);

        self.store(
            generated_rhs.0.clone(),
            generated_rhs.1,
            generate_lhs.clone(),
            function,
        );

        (generated_rhs.0, generate_lhs)
    }

    fn codegen_if_expr<'a>(
        &'a self,
        condition: &CheckedExpr,
        true_block: &CheckedBlock,
        false_block: &CheckedBlock,
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
        block: &CheckedBlock,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> Option<(Type<'a>, Value)> {
        function.add_block(name);

        for stmt in &block.stmts {
            self.codegen_stmt(stmt, function, module);
        }

        block
            .last
            .as_ref()
            .map(|expr| self.codegen_expr(expr, function, module))
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

    fn codegen_store_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_expr = self.codegen_expr(expr, function, module);
        let dest = Value::Temporary(stack_slot.qbe_name());

        (
            Type::Long,
            self.store(generated_expr.0, generated_expr.1, dest, function),
        )
    }

    fn codegen_expr_for_read<'a>(
        &'a self,
        expr: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> Value {
        match &expr.kind {
            CheckedExprKind::StackValue(stack_slot)
            | CheckedExprKind::DirectCall { stack_slot, .. }
            | CheckedExprKind::StructInstantiation { stack_slot, .. } => {
                Value::Temporary(stack_slot.qbe_name())
            }
            CheckedExprKind::MemberAccess { lhs, name } => {
                self.codegen_member_access_expr_for_read(lhs, &name, function, module)
            }
            CheckedExprKind::DerefLValue { .. } => {
                todo!()
            }
            CheckedExprKind::DerefRValue { .. } => todo!(),
            CheckedExprKind::Assignment { .. } => panic!("Assignment for read???"),
            _ => self.codegen_expr(expr, function, module).1,
        }
    }

    fn codegen_member_access_expr_for_read<'a>(
        &'a self,
        lhs: &CheckedExpr,
        name: &str,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> Value {
        let generated_lhs = self.codegen_expr_for_read(lhs, function, module);

        let memory_layout = self.checker.types.memory_layout_of(lhs.type_id);
        let fields = memory_layout.fields.unwrap();
        let field_layout = fields.get(name).unwrap();

        let access = Value::Temporary(format!(".member_access.{name}.{}", self.unique_tag()));
        function.assign_instr(
            access.clone(),
            Type::Long,
            Instr::Add(generated_lhs, Value::Const(field_layout.offset as u64)),
        );

        access
    }

    fn unique_tag(&self) -> usize {
        let new = self.counter.get() + 1;
        self.counter.set(new);
        new
    }

    fn store<'a>(
        &'a self,
        qbe_type: Type<'a>,
        src_or_value: Value,
        dest: Value,
        function: &mut Function<'a>,
    ) -> Value {
        match qbe_type {
            Type::Aggregate(_) => {
                function.add_instr(Instr::Blit(src_or_value, dest.clone(), qbe_type.size()));
            }
            _ => function.add_instr(Instr::Store(qbe_type, dest.clone(), src_or_value)),
        }

        dest
    }

    fn store_if_needed<'a>(
        &'a self,
        qbe_type: Type<'a>,
        src_or_value: Value,
        dest: Value,
        function: &mut Function<'a>,
    ) -> Value {
        match qbe_type {
            Type::Aggregate(_) => self.store(qbe_type, src_or_value, dest, function),
            _ => src_or_value,
        }
    }

    fn load<'a>(
        &'a self,
        qbe_type: Type<'a>,
        src_or_value: Value,
        dest: Value,
        function: &mut Function<'a>,
    ) -> Value {
        match qbe_type {
            Type::Aggregate(_) => {
                function.add_instr(Instr::Blit(src_or_value, dest.clone(), qbe_type.size()));
            }
            _ => function.assign_instr(
                dest.clone(),
                qbe_type.clone(),
                Instr::Load(qbe_type, src_or_value),
            ),
        }

        dest
    }
}
