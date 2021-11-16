use crate::op_codes::OpCode;
use crate::resolver::UpValue;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Instr {
    pub op_code: OpCode,
    pub line_num: usize,
}

#[derive(Debug)]
pub struct CodeObject {
    pub code: Vec<Instr>,
}

// TODO: derive from default
impl CodeObject {
    pub fn write_instruction(&mut self, instruction: Instr) {
        self.code.push(instruction);
    }

    pub fn new() -> CodeObject {
        CodeObject { code: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

/// Compile time representation of a function, ie its code, name, resolved closure information
#[derive(Debug)]
pub struct FunctionChunk {
    pub chunk: CodeObject,
    pub name: Option<String>, // None for the top level script
    pub arity: usize,
    pub fn_type: FunctionType,
    pub upvalues: Option<Vec<UpValue>>, // None while the function is being defined and for functions without upvalues. If the function does have upvalues, this field must be set and must be binded with an OpClosure
}

impl FunctionChunk {
    pub fn new(name: Option<String>, arity: usize, fn_type: FunctionType) -> FunctionChunk {
        FunctionChunk {
            chunk: CodeObject::new(),
            name,
            arity,
            fn_type,
            upvalues: None,
        }
    }

    pub fn set_upvalues(&mut self, upvalues: Vec<UpValue>) {
        self.upvalues = Some(upvalues);
    }
}

/// Compile time repr of a class
#[derive(Debug, Clone)]
pub struct ClassChunk {
    pub name: String,
    pub methods: HashMap<usize, usize>,
    pub superclass: Option<usize>,
    pub has_init: bool,
}

impl ClassChunk {
    pub fn new(name: String) -> ClassChunk {
        ClassChunk {
            name,
            methods: HashMap::new(),
            superclass: None,
            has_init: false,
        }
    }
}
