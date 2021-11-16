use crate::{compiler::Compiler, op_codes::OpCode, value::Value};

/// All functions implementing opcode emitting of literals
impl Compiler {
    pub fn float(&mut self, number: f64) {
        self.emit_constant(Value::Double(number));
    }

    pub fn integer(&mut self, number: i32) {
        self.emit_constant(Value::Integer(number));
    }

    pub fn string(&mut self, string: String) {
        self.emit_constant(Value::String(string));
    }

    pub fn bool(&mut self, boolean: bool) {
        match boolean {
            true => self.emit_instr(OpCode::True),
            false => self.emit_instr(OpCode::False),
        }
    }

    pub fn nil(&mut self) {
        self.emit_constant(Value::Nil);
    }
}
