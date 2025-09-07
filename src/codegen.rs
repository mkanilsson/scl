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
        proc::ProcId,
        stack::StackSlotId,
        tajp::{TypeId, VOID_TYPE_ID},
    },
};

pub struct Codegen {
    units: Vec<CheckedTranslationUnit>,
    generic_procs: Vec<CheckedProc>,
    checker: Checker,
    counter: Cell<usize>,
}

impl Codegen {
    pub fn new(
        units: Vec<CheckedTranslationUnit>,
        generic_procs: Vec<CheckedProc>,
        checker: Checker,
    ) -> Self {
        Self {
            units,
            checker,
            generic_procs,
            counter: Cell::new(0),
        }
    }

    pub fn generate(&mut self) -> String {
        let mut module = Module::new();

        // TODO: Sort order after usage since qbe requires
        //       it to be specified in the order of usage
        for type_id in &self.checker.types.structs {
            module.add_type(
                self.checker
                    .types
                    .qbe_type_def_of(*type_id, &self.checker)
                    .clone(),
            );
        }

        for unit in &self.units {
            for proc in &unit.procs {
                self.codegen_proc(proc, &mut module);
            }
        }

        println!("Starting to generate generic procs");
        for generic_proc in &self.generic_procs {
            self.codegen_proc(generic_proc, &mut module);
        }

        format!("{}", module)
    }

    fn codegen_proc<'a>(&'a self, proc: &CheckedProc, module: &mut Module<'a>) {
        let mut params = proc
            .params
            .iter()
            .map(|p| {
                (
                    self.checker
                        .types
                        .qbe_type_of(proc.stack_slots.type_of(p.1), &self.checker),
                    Value::Temporary(p.0.clone()),
                )
            })
            .collect::<Vec<_>>();

        if proc.has_this {
            params.insert(0, (Type::Long, Self::this_value()));
        }

        let return_type = if proc.return_type == VOID_TYPE_ID {
            None
        } else {
            Some(
                self.checker
                    .types
                    .qbe_type_of(proc.return_type, &self.checker),
            )
        };

        let mut func = Function::new(
            Linkage::public(),
            self.checker
                .procs
                .mangled_name_of(proc.proc_id, &self.checker),
            params,
            return_type,
        );

        func.add_block("start");
        for (slot, type_id) in proc.stack_slots.slots.iter().enumerate() {
            self.allocate_stack_slot(slot.into(), *type_id, &mut func);
        }

        for param in &proc.params {
            let type_id = proc.stack_slots.type_of(param.1);
            let definition = self.checker.types.get_definition(type_id);

            match definition {
                crate::typechecker::tajp::Type::Struct { .. } => {
                    let memory_layout = self
                        .checker
                        .types
                        .memory_layout_of_definition(&definition, &self.checker);

                    func.add_instr(Instr::Blit(
                        Value::Temporary(param.0.clone()),
                        Value::Temporary(param.1.qbe_name()),
                        memory_layout.size as u64,
                    ));
                }
                _ => func.add_instr(Instr::Store(
                    self.checker
                        .types
                        .qbe_type_of_for_typedef(type_id, &self.checker),
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
        let memory_layout = self
            .checker
            .types
            .memory_layout_of_definition(&definition, &self.checker);

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
        deferred: &[Vec<CheckedExpr>],
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        #[allow(unreachable_patterns)]
        match stmt {
            CheckedStmt::Return { value } => {
                self.codegen_return_stmt(value, deferred, function, module)
            }
            CheckedStmt::VariableDeclaration { stack_slot, value } => {
                self.codegen_variable_declaration_stmt(*stack_slot, value, function, module)
            }
            CheckedStmt::Expr(expr) => {
                self.codegen_expr(expr, function, module);
            }
            CheckedStmt::While { condition, body } => {
                self.codegen_while_stmt(condition, body, function, module);
            }
            CheckedStmt::None => {}
            stmt => todo!("codegen_stmt: {}", stmt),
        }
    }

    fn codegen_return_stmt<'a>(
        &'a self,
        value: &Option<CheckedExpr>,
        deferred: &[Vec<CheckedExpr>],
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        let qbe_value = value
            .as_ref()
            .map(|expr| self.codegen_expr(expr, function, module).1);

        self.codegen_deferred(deferred, true, function, module);

        function.add_instr(Instr::Ret(qbe_value));
    }

    fn codegen_deferred<'a>(
        &'a self,
        deferred: &[Vec<CheckedExpr>],
        all: bool,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        if all {
            deferred.iter().for_each(|exprs| {
                exprs.iter().rev().for_each(|expr| {
                    self.codegen_expr(expr, function, module);
                });
            });
        } else {
            deferred.last().unwrap().iter().rev().for_each(|expr| {
                self.codegen_expr(expr, function, module);
            });
        }
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

    fn codegen_while_stmt<'a>(
        &'a self,
        condition: &CheckedExpr,
        body: &CheckedBlock,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) {
        let condition_block_tag = format!(".while.condition.{}", self.unique_tag());
        let body_block_tag = format!(".while.body.{}", self.unique_tag());
        let end_block_tag = format!(".while.end.{}", self.unique_tag());

        function.add_block(condition_block_tag.clone());
        let generated_condition = self.codegen_expr(condition, function, module);

        function.add_instr(Instr::Jnz(
            generated_condition.1,
            body_block_tag.clone(),
            end_block_tag.clone(),
        ));

        self.codegen_block(&body_block_tag, body, function, module);
        function.add_instr(Instr::Jmp(condition_block_tag));
        function.add_block(end_block_tag);
    }

    fn codegen_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        #[allow(unreachable_patterns)]
        match &expr.kind {
            CheckedExprKind::This => (Type::Long, Self::this_value()),
            CheckedExprKind::Identifier(name) => self.codegen_identifier_expr(expr, name),
            CheckedExprKind::Number(value) => self.codegen_number_expr(expr, *value),
            CheckedExprKind::String(value) => self.codegen_string_expr(expr, value, module),
            CheckedExprKind::BinOp { lhs, op, rhs } => {
                self.codegen_binop_expr(lhs, *op, rhs, function, module)
            }
            CheckedExprKind::DirectCall {
                proc_id,
                params,
                variadic_after,
                stack_slot,
            } => self.codegen_direct_call_expr(
                expr,
                *proc_id,
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
                stack_slot,
                condition,
                false_block,
                true_block,
            } => self.codegen_if_expr(
                expr.type_id,
                condition,
                true_block,
                false_block,
                *stack_slot,
                function,
                module,
            ),
            CheckedExprKind::StackValue(stack_slot) => {
                self.codegen_stack_value_expr(expr.type_id, *stack_slot, function)
            }
            CheckedExprKind::Assignment { lhs, rhs } => {
                self.codegen_assignment_expr(lhs, rhs, function, module)
            }
            CheckedExprKind::AddressOf { expr } => {
                self.codegen_address_of_expr(expr, function, module)
            }
            CheckedExprKind::Deref {
                type_id,
                expr,
                stack_slot,
            } => self.codegen_deref_expr(*type_id, expr, *stack_slot, function, module),
            CheckedExprKind::Block(block) => {
                if let Some(value) = self.codegen_block(
                    &format!(".block.{}", self.unique_tag()),
                    block,
                    function,
                    module,
                ) {
                    value
                } else {
                    (
                        Type::Word,
                        Value::Temporary(".ERROR.SHOULDNT.BE.USED".to_string()),
                    )
                }
            }
            CheckedExprKind::Store { expr, stack_slot } => {
                self.codegen_store_expr(expr, *stack_slot, function, module)
            }
            CheckedExprKind::ArrayInstantiation { exprs, stack_slot } => self
                .codegen_array_instantiation_expr(
                    expr.type_id,
                    exprs,
                    *stack_slot,
                    function,
                    module,
                ),
            CheckedExprKind::ArrayAccess { lhs, index } => {
                self.codegen_array_access_expr(expr.type_id, lhs, index, function, module)
            }
            CheckedExprKind::F64ToF32(lhs) => {
                self.codegen_cast(Instr::Truncd, expr, lhs, function, module)
            }
            CheckedExprKind::F32ToF64(lhs) => {
                self.codegen_cast(Instr::Exts, expr, lhs, function, module)
            }
            CheckedExprKind::F32ToSigned(lhs) => {
                self.codegen_cast(Instr::Stosi, expr, lhs, function, module)
            }
            CheckedExprKind::F32ToUnsigned(lhs) => {
                self.codegen_cast(Instr::Stoui, expr, lhs, function, module)
            }
            CheckedExprKind::F64ToSigned(lhs) => {
                self.codegen_cast(Instr::Dtosi, expr, lhs, function, module)
            }
            CheckedExprKind::F64ToUnsigned(lhs) => {
                self.codegen_cast(Instr::Dtoui, expr, lhs, function, module)
            }
            CheckedExprKind::I32ToFloat(lhs) => {
                self.codegen_cast(Instr::Swtof, expr, lhs, function, module)
            }
            CheckedExprKind::U32ToFloat(lhs) => {
                self.codegen_cast(Instr::Uwtof, expr, lhs, function, module)
            }
            CheckedExprKind::I64ToFloat(lhs) => {
                self.codegen_cast(Instr::Sltof, expr, lhs, function, module)
            }
            CheckedExprKind::U64ToFloat(lhs) => {
                self.codegen_cast(Instr::Ultof, expr, lhs, function, module)
            }
            CheckedExprKind::SignExtend8(lhs) => {
                self.codegen_cast(Instr::Extsb, expr, lhs, function, module)
            }
            CheckedExprKind::ZeroExtend8(lhs) => {
                self.codegen_cast(Instr::Extub, expr, lhs, function, module)
            }
            CheckedExprKind::SignExtend16(lhs) => {
                self.codegen_cast(Instr::Extsh, expr, lhs, function, module)
            }
            CheckedExprKind::ZeroExtend16(lhs) => {
                self.codegen_cast(Instr::Extuh, expr, lhs, function, module)
            }
            CheckedExprKind::SignExtend32(lhs) => {
                self.codegen_cast(Instr::Extsw, expr, lhs, function, module)
            }
            CheckedExprKind::ZeroExtend32(lhs) => {
                self.codegen_cast(Instr::Extuw, expr, lhs, function, module)
            }
            kind => todo!("codegen_expr: {}", kind),
        }
    }

    fn codegen_cast<'a>(
        &'a self,
        instr: fn(Value) -> Instr<'a>,
        expr: &CheckedExpr,
        lhs: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let generated_lhs = self.codegen_expr(lhs, function, module);
        let dest = Value::Temporary(format!(".cast.{}", self.unique_tag()));
        let t = self.checker.types.qbe_type_of(expr.type_id, &self.checker);
        function.assign_instr(dest.clone(), t.clone(), instr(generated_lhs.1));
        (t, dest)
    }

    fn codegen_identifier_expr(&self, expr: &CheckedExpr, name: &str) -> (Type, Value) {
        (
            self.checker.types.qbe_type_of(expr.type_id, &self.checker),
            Value::Temporary(name.to_string()),
        )
    }

    fn codegen_number_expr(&self, expr: &CheckedExpr, value: u64) -> (Type, Value) {
        (
            self.checker.types.qbe_type_of(expr.type_id, &self.checker),
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

        let name = format!(".str.{}", self.unique_tag());

        module.add_data(DataDef::new(Linkage::private(), name.clone(), None, items));

        (
            self.checker.types.qbe_type_of(expr.type_id, &self.checker),
            Value::Global(name),
        )
    }

    fn codegen_direct_call_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        proc_id: ProcId,
        params: &Vec<CheckedExpr>,
        variadic_after: Option<u64>,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let name = self.checker.procs.mangled_name_of(proc_id, &self.checker);

        let result_value = Value::Temporary(format!(".{name}.return_value.{}", self.unique_tag()));
        let result_type = self.checker.types.qbe_type_of(expr.type_id, &self.checker);

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
            Instr::Call(name, generated_params, variadic_after),
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
        let memory_layout = self
            .checker
            .types
            .memory_layout_of(expr.type_id, &self.checker);

        let fields_offsets = memory_layout.fields.expect("Struct to have fields");

        let dest = Value::Temporary(stack_slot.qbe_name());
        for field in fields {
            // TODO: Copy from other struct
            let expr = self.codegen_expr(&field.1, function, module);

            // Get offset
            let offset_value = Value::Temporary(format!(".offset.{}", self.unique_tag()));
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

        (
            self.checker.types.qbe_type_of(expr.type_id, &self.checker),
            dest,
        )
    }

    fn codegen_member_access_expr<'a>(
        &'a self,
        expr: &CheckedExpr,
        lhs: &CheckedExpr,
        name: &str,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let memory_layout = self
            .checker
            .types
            .memory_layout_of(lhs.type_id, &self.checker);
        let fields = memory_layout.fields.unwrap();
        let field_layout = fields.get(name).unwrap();

        let generated_lhs = self.codegen_expr(lhs, function, module);

        let offset_value = Value::Temporary(format!(".offset.{}", self.unique_tag()));

        function.assign_instr(
            offset_value.clone(),
            Type::Long,
            Instr::Add(
                generated_lhs.1.clone(),
                Value::Const(field_layout.offset as u64),
            ),
        );

        let result_type = self.checker.types.qbe_type_of(expr.type_id, &self.checker);

        self.read(result_type, offset_value, function)
    }

    fn codegen_stack_value_expr<'a>(
        &'a self,
        type_id: TypeId,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
    ) -> (Type<'a>, Value) {
        let tajp = self.checker.types.qbe_type_of(type_id, &self.checker);

        let value = Value::Temporary(stack_slot.qbe_name());
        match tajp {
            Type::Aggregate(_) => (tajp, value),
            _ => {
                let name = format!(".sv.{}", self.unique_tag());
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

    fn codegen_deref_expr<'a>(
        &'a self,
        type_id: TypeId,
        expr: &CheckedExpr,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        // Get the offset into the stackslot where the ptr is stored
        let address_to_ptr = self.codegen_expr_for_read(expr, function, module);

        // Read the ptr value
        let ptr = Value::Temporary(format!(".deref.ptr.{}", self.unique_tag()));
        function.assign_instr(
            ptr.clone(),
            Type::Long,
            Instr::Load(Type::Long, address_to_ptr),
        );

        let dest = Value::Temporary(stack_slot.qbe_name());
        let t = self.checker.types.qbe_type_of(type_id, &self.checker);

        // Load the data from the ptr and store it in the new slot
        (t.clone(), self.load(t, ptr, dest, function))
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
        type_id: TypeId,
        condition: &CheckedExpr,
        true_block: &CheckedBlock,
        false_block: &CheckedBlock,
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let unique_tag = self.unique_tag();
        let true_block_tag = format!(".if.true.{}", unique_tag);
        let false_block_tag = format!(".if.false.{}", unique_tag);
        let after_block_tag = format!(".if.after.{}", unique_tag);

        let dest = Value::Temporary(stack_slot.qbe_name());

        // TODO: Convert to Word size if it's something else
        let condition = self.codegen_expr(condition, function, module);
        function.add_instr(Instr::Jnz(
            condition.1,
            true_block_tag.clone(),
            false_block_tag.clone(),
        ));

        if let Some((t, value)) = self.codegen_block(&true_block_tag, true_block, function, module)
        {
            self.store(t, value, dest.clone(), function);
        }
        function.add_instr(Instr::Jmp(after_block_tag.clone()));

        if let Some((t, value)) =
            self.codegen_block(&false_block_tag, false_block, function, module)
        {
            self.store(t, value, dest.clone(), function);
        }
        function.add_instr(Instr::Jmp(after_block_tag.clone()));

        function.add_block(after_block_tag);

        self.codegen_stack_value_expr(type_id, stack_slot, function)
    }

    fn codegen_block<'a>(
        &'a self,
        name: &str,
        block: &CheckedBlock,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> Option<(Type<'a>, Value)> {
        function.add_block(name);

        let mut already_deferred = false;
        for stmt in &block.stmts {
            self.codegen_stmt(stmt, &block.deferred, function, module);

            if matches!(stmt, CheckedStmt::Return { .. }) {
                already_deferred = true;
                break;
            }
        }

        let ret_value = block
            .last
            .as_ref()
            .map(|expr| self.codegen_expr(expr, function, module));

        if !already_deferred {
            self.codegen_deferred(&block.deferred, false, function, module);
        }

        ret_value
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

        let binop_result = Value::Temporary(format!(".binop.{}", self.unique_tag()));

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
                    BinOp::LessThan => {
                        if self.checker.types.is_unsigned_integer(lhs.type_id) {
                            qbe::Cmp::Ult
                        } else {
                            qbe::Cmp::Slt
                        }
                    }
                    BinOp::LessThanOrEqual => {
                        if self.checker.types.is_unsigned_integer(lhs.type_id) {
                            qbe::Cmp::Ule
                        } else {
                            qbe::Cmp::Sle
                        }
                    }
                    BinOp::GreaterThan => {
                        if self.checker.types.is_unsigned_integer(lhs.type_id) {
                            qbe::Cmp::Ugt
                        } else {
                            qbe::Cmp::Sgt
                        }
                    }
                    BinOp::GreaterThanOrEqual => {
                        if self.checker.types.is_unsigned_integer(lhs.type_id) {
                            qbe::Cmp::Uge
                        } else {
                            qbe::Cmp::Sge
                        }
                    }
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

    fn codegen_array_instantiation_expr<'a>(
        &'a self,
        type_id: TypeId,
        exprs: &[CheckedExpr],
        stack_slot: StackSlotId,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let unique_tag = self.unique_tag();
        let stack_slot_value = Value::Temporary(stack_slot.qbe_name());

        let inner_type_id = self.checker.types.inner_of(type_id);
        let size_of_inner = self
            .checker
            .types
            .memory_layout_of(inner_type_id, &self.checker)
            .size;

        for (i, expr) in exprs.iter().enumerate() {
            let generated_expr = self.codegen_expr(expr, function, module);
            let addr_tag = format!(".array_instantiation.{}.{}", i, unique_tag);
            let dest = Value::Temporary(addr_tag);
            function.assign_instr(
                dest.clone(),
                Type::Long,
                Instr::Add(
                    stack_slot_value.clone(),
                    Value::Const((i * size_of_inner) as u64),
                ),
            );

            self.store(generated_expr.0, generated_expr.1, dest, function);
        }

        (
            self.checker.types.qbe_type_of(type_id, &self.checker),
            stack_slot_value,
        )
    }

    fn codegen_array_access_expr<'a>(
        &'a self,
        type_id: TypeId,
        lhs: &CheckedExpr,
        index: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> (Type<'a>, Value) {
        let addr = self.codegen_array_access_expr_for_read(type_id, lhs, index, function, module);
        let qbe_type = self.checker.types.qbe_type_of(type_id, &self.checker);

        self.read(qbe_type, addr, function)
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
                self.codegen_member_access_expr_for_read(lhs, name, function, module)
            }
            CheckedExprKind::ArrayAccess { lhs, index } => {
                self.codegen_array_access_expr_for_read(expr.type_id, lhs, index, function, module)
            }
            CheckedExprKind::Deref { expr, .. } => self.codegen_expr(expr, function, module).1,
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

        let memory_layout = self
            .checker
            .types
            .memory_layout_of(lhs.type_id, &self.checker);
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

    fn codegen_array_access_expr_for_read<'a>(
        &'a self,
        type_id: TypeId,
        lhs: &CheckedExpr,
        index: &CheckedExpr,
        function: &mut Function<'a>,
        module: &mut Module<'a>,
    ) -> Value {
        let generated_lhs = self.codegen_expr_for_read(lhs, function, module);
        let generated_index = self.codegen_expr(index, function, module);

        let unique_tag = self.unique_tag();

        let offset = Value::Temporary(format!(".array_access.offset.{unique_tag}"));
        let addr = Value::Temporary(format!(".array_access.addr.{unique_tag}"));

        let size_of_inner = self
            .checker
            .types
            .memory_layout_of(type_id, &self.checker)
            .size;

        function.assign_instr(
            offset.clone(),
            Type::Long,
            Instr::Mul(generated_index.1, Value::Const(size_of_inner as u64)),
        );

        function.assign_instr(addr.clone(), Type::Long, Instr::Add(generated_lhs, offset));

        addr
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
        let qbe_type = match qbe_type {
            Type::SignedByte | Type::UnsignedByte => Type::Byte,
            Type::SignedHalfword | Type::UnsignedHalfword => Type::Halfword,
            current => current,
        };

        match qbe_type {
            Type::Aggregate(_) => {
                function.add_instr(Instr::Blit(src_or_value, dest.clone(), qbe_type.size()));
            }
            _ => function.add_instr(Instr::Store(qbe_type, dest.clone(), src_or_value)),
        }

        dest
    }

    fn read<'a>(
        &'a self,
        qbe_type: Type<'a>,
        src: Value,
        function: &mut Function<'a>,
    ) -> (Type<'a>, Value) {
        match qbe_type {
            Type::Aggregate(_) => (Type::Long, src),
            _ => {
                let result_value = Value::Temporary(format!(".read.{}", self.unique_tag()));

                function.assign_instr(
                    result_value.clone(),
                    qbe_type.clone(),
                    Instr::Load(qbe_type.clone(), src),
                );

                (qbe_type, result_value)
            }
        }
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

    fn this_value() -> Value {
        Value::Temporary(".this".to_string())
    }
}
