mod literals;
mod statements;

use liva_parser::{
    expression::{
        binary::{BinaryOp, UnaryOp},
        call::Call,
        ExprOrVarname, ExprSuffix, Expression, PrefixExpr,
    },
    literals::{Collection, Literal, Variable},
    parse_source,
    statement::declaration::assignment::Assignment,
    statement::declaration::function::Function,
    statement::Block,
    tokens::{Operator, UnOperator},
    Span,
};

use crate::{chunk::*, debug::*, op_codes::OpCode, resolver::Resolver, value::Value};

#[derive(Debug)]
pub struct CompilationResult {
    pub classes: Vec<ClassChunk>,
    pub functions: Vec<FunctionChunk>,
    pub constants: Vec<Value>,
    pub identifier_constants: Vec<String>,
}

pub struct Compiler {
    resolver: Resolver,
    identifier_constants: Vec<String>,

    classes: Vec<ClassChunk>,
    current_class: Option<usize>,

    functions: Vec<FunctionChunk>,
    current_function: usize,
    parent_functions: Vec<usize>,
    constants: Vec<Value>,

    had_error: bool,
    panic_mode: bool,
    quite_mode: bool,

    // For naming hidden variables (e.g iterators)
    current_id: usize,
}

/// TODO: How to we design the interface of the compiler?
///
/// 1. Now we pass a collection of identifiers, constants, class and function chunks
///   to the VM
/// 2. Instructions are encoded throught FunctionChunks which are executed
/// 3. We therefore firstly Init a "Script" Function as the entry point
///
/// In python the compiler just compiles the instructions with consts, etc.
/// If the module is __main__ is decided shortly before runtime.
///
/// We can still keep the current way, but have to check what we compile
/// and can therefore decide where to put the "Script" entry point.
///
/// Problem: We cannot differentiate between code objects and modules,
/// because this is backed into the compilation....
///
impl Compiler {
    pub fn new() -> Compiler {
        let functions = vec![FunctionChunk::new(None, 0, FunctionType::Script)];

        Compiler {
            resolver: Resolver::new(),
            classes: Vec::new(),
            current_class: None,
            constants: Vec::new(),
            identifier_constants: Vec::new(),
            parent_functions: Vec::new(),
            functions,
            current_function: 0,
            had_error: false,
            panic_mode: false,
            quite_mode: false,

            current_id: 0,
        }

        // if let TokenType::Error = first_token.token_type {
        //     compiler.advance();
        //     compiler.error(first_token.lexeme.as_str());
        // }
    }

    fn parse<'ast>(&self, source: &'ast str) -> Block<'ast> {
        let source_span = Span::new(source);
        let (_, ast) = parse_source(source_span).unwrap();
        ast
    }

    pub fn compile(mut self, source: &str, debug: bool) -> Option<CompilationResult> {
        let ast = self.parse(source);

        // On top level, we only have declarations or func calls
        // no top level expressions
        for stmt in ast.into_iter() {
            self.statement(stmt);
        }

        self.end_compilation();

        if debug {
            for (index, fn_chunk) in self.functions.iter().enumerate() {
                if fn_chunk.fn_type != FunctionType::Method
                    && fn_chunk.fn_type != FunctionType::Initializer
                {
                    // This does not include var declartiosn due to local scope, which happens
                    // in the resolver
                    disassemble_fn_chunk(
                        index,
                        fn_chunk,
                        &self.constants,
                        &self.identifier_constants,
                    );
                }
            }

            for class_chunk in self.classes.iter() {
                disassemble_class_chunk(
                    class_chunk,
                    &self.functions,
                    &self.classes,
                    &self.constants,
                    &self.identifier_constants,
                );
            }
        }

        if !self.had_error {
            Some(CompilationResult {
                classes: self.classes,
                functions: self.functions,
                constants: self.constants,
                identifier_constants: self.identifier_constants,
            })
        } else {
            None
        }
    }

    /// Pop scope except number of `until`
    fn end_scope_until(&mut self, until: usize) {
        let pops = self.resolver.end_scope();
        for _ in until..pops {
            self.emit_instr(OpCode::Pop); // Remove old local variables
        }
    }

    /// Pop current scope
    fn end_scope(&mut self) {
        let pops = self.resolver.end_scope();
        for _ in 0..pops {
            self.emit_instr(OpCode::Pop); // Remove old local variables
        }
    }

    /// loop_start: Index of the instruction to jump back to
    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start;
        let loop_op = OpCode::Loop(offset);

        if offset > (u16::MAX as usize) {
            self.error("Loop body too large");
        }

        self.emit_instr(loop_op);
    }

    fn emit_jie(&mut self) -> usize {
        self.emit_instr(OpCode::ForIter(usize::max_value()));
        self.current_chunk().code.len() - 1
    }

    /// Emits OpCode::OpJumpIfFalse
    ///
    /// Returns the index of the jump instruction for patching
    fn emit_jif(&mut self) -> usize {
        self.emit_instr(OpCode::JumpIfFalse(usize::max_value()));
        self.current_chunk().code.len() - 1
    }

    /// Emits OpCode::OpJump
    ///
    /// Returns the index of the jump instruction for patching
    fn emit_jump(&mut self) -> usize {
        self.emit_instr(OpCode::Jump(usize::max_value()));
        self.current_chunk().code.len() - 1
    }

    /// Given the index of the jump instruction in the chunk, update the opcode to jump to the instruction after the current one
    fn patch_jump(&mut self, index: usize) {
        let jump_amount = self.current_chunk().code.len() - index;
        if jump_amount > usize::max_value() {
            self.error("Too much code to jump over");
        }

        let jump_instr = self.current_chunk().code.get_mut(index).unwrap();
        macro_rules! replace_jump {
            ($jump_type: path) => {{
                jump_instr.op_code = $jump_type(jump_amount)
            }};
        }

        match jump_instr.op_code {
            OpCode::Jump(_) => replace_jump!(OpCode::Jump),
            OpCode::JumpIfFalse(_) => replace_jump!(OpCode::JumpIfFalse),
            OpCode::ForIter(_) => {
                replace_jump!(OpCode::ForIter)
            }
            // OpCode::GetIter(_) => replace_jump!(OpCode::GetIter),
            _ => panic!(
                "Compiler panic: Attempted to patch a non_jump op code instruction: {:?}",
                jump_instr
            ),
        }
    }

    ///
    /// **Variable declaration**
    ///
    /// There are two different types to declare are variable
    /// 1. Global scope -> Define variable, put in global scope
    /// 2. Local scope -> Push value with index on stack,
    ///                   after scope end all values are popped from the stack
    ///
    /// We then define how we:
    /// 1. New assignment -> create new identifier for variable and push
    /// 2. Existing variable -> Assign new value to existing variable
    ///
    fn define_variable(&mut self, global: usize) {
        if self.resolver.is_global() {
            self.emit_instr(OpCode::DefineGlobal(global));
        } else {
            self.resolver.mark_initialized()
        }
    }

    // Note: parse_precedence with TokenIdentifier => variable() -> named_variable(previous.lexemme)
    /// Getter for variable
    fn get_named_variable(&mut self, variable: Variable) {
        let local_arg = match self.resolver.resolve_local(variable.value) {
            Ok(opt) => opt,
            Err(_) => {
                self.error("Cannot read local variable in its own initializer");
                return;
            }
        };

        // Figure out which type of get/set OpCodes we want
        if let Some(local_index) = local_arg {
            self.emit_instr(OpCode::GetLocal(local_index));
        } else if let Some(upvalue_index) = self.resolver.resolve_upvalue(variable.value) {
            self.emit_instr(OpCode::GetUpvalue(upvalue_index));
        } else {
            let global_arg = self.identifier_constant(variable.value); // Does NOT check at compile time if this variable can be resolved

            self.emit_instr(OpCode::GetGlobal(global_arg));
        };
    }

    fn get_variable(&mut self, prefix_expr: PrefixExpr) {
        // Check if we just have a varname or another expression
        match prefix_expr.prefix {
            ExprOrVarname::Exp(expr) => self.expression(expr),
            ExprOrVarname::Varname(variable) => {
                self.get_named_variable(variable);
            }
        }
        self.suffix_chain(prefix_expr.suffix_chain);
    }

    fn call(&mut self, call: Call) {
        let arg_count = self.argument_list(call.args);
        self.emit_instr(OpCode::Call(arg_count));
    }

    fn argument_list(&mut self, args: Vec<Expression>) -> usize {
        let arg_count = args.len();
        for arg in args {
            self.expression(arg);
        }
        arg_count
    }

    fn expression(&mut self, expr: Expression) {
        match expr {
            Expression::Literal(literal) => self.literal(literal),
            Expression::Collection(collection) => self.collection(collection),
            Expression::Call(call) => self.call(call),
            Expression::UnaryOp(unary) => self.unary_op(*unary),
            Expression::BinaryOp(binary) => self.binary_op(*binary),
            Expression::PrefixExpr(prefix_expr) => self.prefix_expr(*prefix_expr),
        }
    }

    fn prefix_expr(&mut self, expr: PrefixExpr) {
        // Check if we just have a varname or another expression
        match expr.prefix {
            ExprOrVarname::Exp(expr) => self.expression(expr),
            ExprOrVarname::Varname(variable) => {
                self.get_named_variable(variable);
                self.suffix_chain(expr.suffix_chain);
            }
        }
    }

    /// Handles the suffix chain of a prefix expr
    fn suffix_chain(&mut self, expr_chain: Vec<ExprSuffix>) {
        for expr in expr_chain {
            match expr {
                // Thats a method call
                ExprSuffix::FuncCall(call) => self.method_call(call),

                // Attribute getter
                ExprSuffix::TableDot(dot) => self.dot(dot),
                ExprSuffix::TableIdx(index) => {
                    // Push index expression
                    self.expression(index);
                    self.emit_instr(OpCode::GetIndexItem);
                }
            }
        }
    }

    fn method_call(&mut self, call: Call) {
        // Thats a method call
        if let Some(var) = call.callee {
            let name_index = self.identifier_constant(var.value);
            // self.emit_instr(OpCode::LoadMethod(name_index));
            self.argument_list(call.args.clone());
            self.emit_instr(OpCode::CallMethod(name_index, call.args.len()));
        } else {
            // Normal function call
            self.call(call)
        }
    }

    fn dot(&mut self, attr: Variable) {
        let name_index = self.identifier_constant(attr.value);
        self.emit_instr(OpCode::LoadAttr(name_index));
        // self.emit_instr(OpCode::Pop);
    }

    fn binary_op(&mut self, expression: BinaryOp) {
        self.expression(expression.left);
        self.expression(expression.right);
        match expression.op {
            Operator::Add => self.emit_instr(OpCode::Add),
            Operator::Sub => self.emit_instr(OpCode::Subtract),
            Operator::Div => self.emit_instr(OpCode::Divide),
            Operator::Mul => self.emit_instr(OpCode::Multiply),
            Operator::EQ => self.emit_instr(OpCode::Equal),
            Operator::Gt => self.emit_instr(OpCode::Greater),
            Operator::Mod => self.emit_instr(OpCode::Modulo),
            Operator::Lt => self.emit_instr(OpCode::Less),
            _ => {}
        }
    }

    fn unary_op(&mut self, expression: UnaryOp) {
        self.expression(expression.operand);
        match expression.op {
            UnOperator::Add => self.emit_instr(OpCode::Add),
            UnOperator::Sub => self.emit_instr(OpCode::Negate),
            UnOperator::Not => self.emit_instr(OpCode::Not),
        }
    }

    fn literal(&mut self, literal: Literal) {
        match literal {
            Literal::Str(str_token) => self.string(str_token.value),
            Literal::Boolean(bool_token) => self.bool(bool_token.value),
            Literal::Nil(_) => self.nil(),
            Literal::Int(int_token) => self.integer(int_token.value),
            Literal::Float(float_token) => self.float(float_token.value),
            // Literal::Num(num_token) => self.number(num_token.value),
            // Literal::Array(array_literal) => self.list(array_literal),
            // Literal::Map(map_literal) => {} // Literal::Variable(variable_token) => {}
        }
    }

    fn collection(&mut self, collection: Collection) {
        match collection {
            Collection::Array(list) => self.list(list),
            Collection::Map(map) => {}
        }
    }

    fn list(&mut self, list: Vec<Expression>) {
        let arg_count = list.len();
        for item in list {
            self.expression(item);
        }
        self.emit_instr(OpCode::BuildList(arg_count));
    }

    /// TODO: Think about how we pass the line number to the instruction
    /// A global current/previous token or pass it to all function calls(expensive moves?)
    fn emit_instr(&mut self, op_code: OpCode) {
        let instr = Instr {
            op_code,
            line_num: 0, //self.previous_token().line,
        };

        self.current_chunk().write_instruction(instr);
    }

    fn emit_instrs(&mut self, op_codes: &[OpCode]) {
        for op_code in op_codes.iter() {
            self.emit_instr(*op_code);
        }
    }

    fn current_chunk(&mut self) -> &mut CodeObject {
        &mut self.functions.get_mut(self.current_function).unwrap().chunk
    }

    fn current_class(&mut self) -> &mut ClassChunk {
        self.classes.get_mut(self.current_class.unwrap()).unwrap()
    }

    /// First check what scope we are in. This is decided by the first variable of the
    /// left hand expressions
    /// E.g: a.b = 3 -> check scope for `a`
    ///
    /// then assign expression to variable (which is also an expresion and has to evaluated
    /// first)
    ///
    /// We now support only actual varname assignment:
    ///
    /// `x.a = 3` Ok
    /// `func() = 3` NOT Ok
    /// `func().x = 3` also NOT Ok
    ///
    fn assignment(&mut self, assignment: Assignment) {
        self.expression(assignment.expression);

        let left_hand_variable = match assignment.variable.prefix {
            ExprOrVarname::Varname(name) => name,
            ExprOrVarname::Exp(_) => {
                self.error("Left hand side of assignment has to be an identifier.");
                return;
            }
        };

        // We now have to check for the suffix chain in assignment.variable
        // If there is nothing, like `x = 3`, then we can continue we checking
        // in which scope to set x
        //
        // If we have a suffix chain like `x.a = 3`, then we only are `getting`
        // x and set `a` on x
        //
        if assignment.variable.suffix_chain.len() != 0 {
            // Get depending on scope
            self.get_named_variable(left_hand_variable);

            let mut suffix_chain = assignment.variable.suffix_chain;

            // We know that we assign the value to the last occuring expr/attr in the suffix
            // chain, pop it before an load everything in between
            //
            // x    .a     .b   .c    = 3
            // |    |      |    |
            // get  get    get  set
            //
            let set_attr = suffix_chain.pop();

            for suffix in suffix_chain {
                // Again we only match table dots here, for now...
                match suffix {
                    ExprSuffix::TableDot(var) => {
                        let get_attr = self.identifier_constant(var.value);
                        self.emit_instr(OpCode::LoadAttr(get_attr));
                    }
                    _ => unreachable!(),
                }
            }

            if let ExprSuffix::TableDot(var) = set_attr.unwrap() {
                let attr_index = self.identifier_constant(var.value);
                self.emit_instr(OpCode::StoreAttr(attr_index));
            }

            // Assign new value to loaded attr, does not depend on scope anymore
        } else {
            // If we just set on the scoped variable
            let local_arg = match self.resolver.resolve_local(left_hand_variable.value) {
                Ok(opt) => opt,
                Err(_) => {
                    self.error("Cannot read local variable in its own initializer");
                    return;
                }
            };

            let set_op = if let Some(local_index) = local_arg {
                OpCode::SetLocal(local_index)
            } else if let Some(upvalue_index) =
                self.resolver.resolve_upvalue(left_hand_variable.value)
            {
                OpCode::SetUpvalue(upvalue_index)
            } else {
                // Does NOT check at compile time if this variable can be resolved
                let global_arg = self.identifier_constant(left_hand_variable.value);
                OpCode::SetGlobal(global_arg)
            };

            self.emit_instr(set_op);
            // Pop the assigned value
            self.emit_instr(OpCode::Pop);
        }
    }

    fn declare_variable(&mut self, var: &str) {
        let success = self.resolver.declare_variable(String::from(var));
        if !success {
            self.error("Variable with this name already declared in this scope");
        }
    }

    /// Add a string to the chunk as a constant and return the index
    ///
    /// Only used for global variables
    fn identifier_constant(&mut self, str_val: &str) -> usize {
        // self.add_constant(Value::LoxString(str_val.to_string()))
        match self.identifier_constants.iter().position(|x| x == str_val) {
            Some(i) => i,
            None => {
                self.identifier_constants.push(str_val.to_string());
                self.identifier_constants.len() - 1
            }
        }
    }

    fn current_fn_type(&self) -> FunctionType {
        self.functions.get(self.current_function).unwrap().fn_type
    }

    fn current_fn(&mut self) -> &mut FunctionChunk {
        self.functions.get_mut(self.current_function).unwrap()
    }

    fn current_chunk_ref(&self) -> &CodeObject {
        &self.functions.get(self.current_function).unwrap().chunk
    }

    fn emit_constant(&mut self, value: Value) -> usize {
        let index = self.add_constant(value);
        self.emit_instr(OpCode::Constant(index));
        index
    }

    fn emit_return(&mut self) {
        if self.current_fn_type() == FunctionType::Initializer {
            self.emit_instrs(&[OpCode::GetLocal(0), OpCode::Return]);
        } else {
            self.emit_instrs(&[OpCode::Nil, OpCode::Return]);
        }
    }

    fn add_constant(&mut self, value: Value) -> usize {
        match self.constants.iter().position(|x| x == &value) {
            Some(i) => i,
            None => {
                self.constants.push(value);
                self.constants.len() - 1
            }
        }
    }

    fn parse_variable(&mut self, str_val: &str, error_message: &str) -> usize {
        self.declare_variable(str_val);
        if self.resolver.is_global() {
            self.identifier_constant(str_val)
        } else {
            0
        }
    }

    /// Sets the compiler to generate a new function chunk for the next segment of code
    fn start_child(&mut self, function_type: FunctionType, fun: Function) -> usize {
        self.functions.push(FunctionChunk::new(
            Some(String::from(fun.name)),
            0,
            function_type,
        ));
        self.resolver.push(function_type);
        self.parent_functions.push(self.current_function);
        self.current_function = self.functions.len() - 1;

        self.functions.len() - 1
    }

    /// Switches the current chunk out of the new function def
    fn end_child(&mut self) {
        // Emit an implicit nil return if not specified explicity
        let last_instr = self.current_chunk_ref().code.last();
        if (last_instr == None) || last_instr.unwrap().op_code != OpCode::Return {
            self.emit_return();
        }
        self.current_function = self.parent_functions.pop().unwrap();
    }
    /// Emits an OpReturn
    fn end_compilation(&mut self) {
        self.emit_return();
    }

    // Emit error message for now to stdout
    // TODO: How should this be handled?
    fn error(&mut self, message: &str) {
        if self.panic_mode {
            return;
        }

        self.had_error = true;
        self.panic_mode = true;

        if self.quite_mode {
            return;
        }

        // if token.is_some() {
        //     eprint!("Error: [Line {:?}]", token.unwrap().pos);
        // }

        // TODO: Lets see how we show error messages
        // match token.token_type {
        //     TokenType::EOF => {
        //         println!(" at the end of file");
        //     }
        //     TokenType::Error => {}
        //     _ => println!(" as {}", token.lexeme),
        // }

        eprintln!("{}", message);
    }
}
