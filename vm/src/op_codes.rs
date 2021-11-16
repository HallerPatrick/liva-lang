#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    Return,
    Pop,

    DefineGlobal(usize), // Index of the String name for this variable name in the identifiers vec
    GetGlobal(usize),    // ^
    SetGlobal(usize),    // ^
    GetSuper(usize),     //  ^
    CallGlobal(usize, usize), // A combination of OpCall and OpGetGlobal

    GetLocal(usize), // Index on the stack
    SetLocal(usize), // ^

    Invoke(usize, usize), // Combines a GetProperty and a Call. Contains the exact same information. First usize is the index for the property name, second is for the arity
    // This will replace `GetProperty`
    LoadMethod(usize),
    CallMethod(usize, usize),

    LoadAttr(usize),
    StoreAttr(usize),

    GetIndexItem, // For accessing item of a indexable object `x[0]`

    // Optimization note: Is there any way to resolve properties at compile time?
    // Lox allows arbitrary properties to be added at any time, so I don't believe it's possible
    //
    GetUpvalue(usize), // upvalue index for a closure
    SetUpvalue(usize), // ^
    Closure, // Wraps the top value of the stack (must be a LoxFunction) in a LoxClosure, capturing the appropriate UpValues at the same time

    Jump(usize), // Jump ip offset
    JumpIfFalse(usize),
    Loop(usize), // Jump backwards by offset

    // For Loops
    GetIter,
    ForIter(usize),

    Call(usize), // Arity

    Class(usize), // Index into the classes vec for the ClassChunk object

    Constant(usize), // Index of the constant we want to retrieve

    BuildList(usize), // Number of elements in the list
    // BuildMap(usize),  // Number of key + values in map

    Nil,
    True,
    False,

    Negate,
    Not,

    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Greater,
    Less,

    Modulo,
}
