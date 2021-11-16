use liva_parser::{
    statement::Block,
    statement::{
        declaration::assignment::LAssignment, declaration::class::Class,
        declaration::for_statement::For, declaration::function::Function,
        declaration::if_statement::If, declaration::while_statement::While, ReturnStmt, Statement,
    },
};

use crate::{chunk::*, op_codes::OpCode, value::*, Compiler};

impl Compiler {
    pub fn statement(&mut self, statement: Statement) {
        match statement {
            Statement::Fun(fun) => self.fun_declaration(fun),
            Statement::Return(return_stmt) => self.return_statement(return_stmt),
            Statement::LAssignment(l_assignment) => self.l_assignment(l_assignment),
            Statement::Assignment(assignment) => self.assignment(assignment),
            Statement::FuncCall(prefix_expr) => {
                // A func call as a statement is just a statement expression with a returninv value
                // Therefore pop it
                self.prefix_expr(prefix_expr);
                self.emit_instr(OpCode::Pop)
            }
            Statement::If(if_stmt) => self.if_stmt(if_stmt),
            Statement::While(while_stmt) => self.while_statement(while_stmt),
            Statement::For(for_stmt) => self.for_statement(for_stmt),
            Statement::Class(class) => self.class_declaration(class),
            Statement::Import(import) => {}
        }
    }

    fn class_declaration(&mut self, class: Class) {
        let _class = class.clone();
        let class_name = _class.name.clone();
        let name_index = self.identifier_constant(&class_name);
        self.declare_variable(&class_name);

        let _class_name = class_name.clone();
        let class = ClassChunk::new(_class_name);
        let old_class = self.current_class;

        self.classes.push(class);

        let class_index = self.classes.len() - 1;
        self.current_class = Some(class_index);

        self.emit_instr(OpCode::Class(class_index));

        self.define_variable(name_index);

        for m in _class.methods.into_iter() {
            self.method(m);
        }

        // TODO: No Inheritance in parser yet
        self.current_class = old_class;
    }

    fn block(&mut self, block: Block) {
        // Dont clone...
        for stmt in block.clone().into_iter() {
            self.statement(stmt);
        }

        if let Some(return_stmt) = block.return_stmt {
            self.return_statement(return_stmt);
        }
    }

    fn method(&mut self, fun: Function) {
        let name_index = self.identifier_constant(fun.name);

        let index = if fun.name.eq("init") {
            self.current_class().has_init = true;
            self.function(FunctionType::Initializer, fun)
        } else {
            self.function(FunctionType::Method, fun)
        };
        self.current_class().methods.insert(name_index, index);
    }

    fn fun_declaration(&mut self, fun: Function) {
        let global = self.parse_variable(fun.name, "Expected function name");
        self.resolver.mark_initialized();
        self.function(FunctionType::Function, fun);
        self.define_variable(global);
    }

    /// Compiles function into function chunk, adds to function stack, emits OpConstant
    /// pointing to it and a UpClouse to wrap it
    fn function(&mut self, fun_type: FunctionType, fun: Function) -> usize {
        //let mut function_parser = self.from_old(fun_type);

        // TODO: How to solve the cloning problem?
        let index = self.start_child(fun_type, fun.clone());
        self.resolver.begin_scope();

        for parameter in fun.parameters {
            let param_constant = self.parse_variable(parameter.value, "Expected parameter name");
            self.define_variable(param_constant);

            let cur_function = self.current_fn();
            cur_function.arity += 1;
            if cur_function.arity > 255 {
                self.error("Cannot have more than 255 parameters");
            }
        }

        self.block(fun.block.clone());

        let upvalues = self.resolver.pop();
        let has_upvalues = !upvalues.is_empty();
        if !upvalues.is_empty() {
            self.current_fn().set_upvalues(upvalues); // Gotta set this before end_child() switches what the current_fn is
        }

        self.end_child();

        if fun_type != FunctionType::Method && fun_type != FunctionType::Initializer {
            // We don't need this for methods because they are statically loaded into the ClassChunk, not at runtime on the stack
            self.emit_constant(Value::LivaFunction(index));

            if has_upvalues {
                self.emit_instr(OpCode::Closure);
            }
        }

        index
    }

    fn for_statement(&mut self, for_statement: For) {
        self.resolver.begin_scope();

        self.expression(for_statement.iterator);
        self.emit_instr(OpCode::GetIter);
        // We place the iterator on stack aswell on the bottom, so we can pop
        // and push the the iter item on top of it
        // we have to declare it first with a name?
        // We utilize that variables cannot be numbers, and use them here
        // as "hidden" variable names
        let iterator_id = &(self.current_id.to_string() + "(Iterator)");
        let iterator_const = self.parse_variable(iterator_id, "Expected iter item name");

        self.define_variable(iterator_const);
        self.current_id += 1;

        let loop_start = self.current_chunk().code.len();
        let exit_jump = self.emit_jie();

        let iter_const =
            self.parse_variable(for_statement.iter_item.value, "Expected iter item name");

        self.define_variable(iter_const);

        self.block(for_statement.block);
        // To pop all local values we can use `end_scope`, this will also pop the iterator
        // so the next iterator will not be there, which will crash
        // 2 ways to solve this:
        //  1. Move iterator out of scope
        //  2. Use modified `end_scope` -> we now go for this
        self.end_scope_until(1);
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
    }

    fn return_statement(&mut self, return_stmt: ReturnStmt) {
        if self.current_fn_type() == FunctionType::Script {
            self.error("Cannot return from top-level code");
        }

        if return_stmt.values.is_empty() {
            self.emit_return();
        } else {
            if self.current_fn_type() == FunctionType::Initializer {
                self.error("Cannot return a value from an initializer");
            }

            // For return can only have one expression
            let expr = return_stmt.values.first().unwrap();
            // TODO: Parse expressions
            self.expression(expr.clone());
            self.emit_instr(OpCode::Return);
        }
    }

    /// Generates the byte code for a while instruction for given block
    fn while_statement(&mut self, while_stmt: While) {
        // Get current index of chunk to point to loop start
        let loop_start = self.current_chunk().code.len();

        // Puh truthy/falsey expression on stack
        self.expression(while_stmt.cond);

        //
        let exit_jump = self.emit_jif();

        // Pop truth value
        self.emit_instr(OpCode::Pop);
        self.block(while_stmt.block);

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);

        self.emit_instr(OpCode::Pop);
    }

    /// Generates the byte code for a for instruction and the given block
    fn if_stmt(&mut self, if_stmt: If) {
        // Generate expression byte codes
        self.expression(if_stmt.cond);

        // Jump if false jump code, will be patched up to point to end of if block
        let jmp_idx = self.emit_jif();

        // Pop the evaluation value
        self.emit_instr(OpCode::Pop); // Pop off the if conditional in the 'then' case

        // Block
        self.block(if_stmt.stmts); // Then case

        // If Else stmt
        if let Some(else_stmt) = if_stmt.else_statements {
            // Emit a jump
            let else_jmp = self.emit_jump();

            // Patch initial if jump
            self.patch_jump(jmp_idx);

            self.emit_instr(OpCode::Pop); // Pop off the if conditional if we jump over the 'then' case
            self.block(else_stmt); // Else case
            self.patch_jump(else_jmp);
        } else {
            self.patch_jump(jmp_idx); // No else case, so just jump to right after
        }
    }

    fn l_assignment(&mut self, l_assignment: LAssignment) {
        let global = self.parse_variable(l_assignment.variable.value, "Expected variable name");
        self.expression(l_assignment.expression);
        self.define_variable(global);
    }

    // fn assignment(&mut self, assignment: Assignment) {
    //     // let global = self.parse_variable(assignment.variable.value, "Expected variable name");
    //     self.set_variable(assignment);
    //     self.emit_instr(OpCode::Pop);
    // }
}
