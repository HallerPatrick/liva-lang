pub mod object;
mod processor;
mod state;

use std::collections::HashMap;

use crate::chunk::{ClassChunk, FunctionChunk, Instr};
use crate::compiler::CompilationResult;
use crate::debug::*;
use crate::gc::GC;
use crate::native::*;
use crate::op_codes::OpCode;
use crate::resolver::UpValue;
use crate::value::{
    is_falsey, values_equal, HeapObj, HeapObjType, HeapObjVal, Iterable, ObjBoundMethod,
    ObjClosure, ObjInstance, Value,
};
use crate::vm::object::LvObjectRef;
use crate::InterpretResult;

const FRAMES_MAX: usize = 1024;

#[derive(Debug)]
pub enum ExecutionMode {
    Default,
    Trace,
}

/// This ended up not being very useful since we usually don't care what kind of deref error we get, they usually mean the same thing, that we tried to use a value in a way it wasn't supposed to be used
#[derive(Debug, Clone, Copy)]
pub enum DerefError {
    NotPointer,
    WrongType,
}

#[derive(Debug, Clone)]
struct CallFrame {
    function: usize, // Index into the VM.functions Vec for which function is being called
    ip: usize,
    frame_start: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Global {
    Init(Value),
    Uninit,
}

// Is it good rust to split these into two very coupled but seperate structs or is there a way to keep them together while not angering the borrow checker?
//
// I think this setup worked quite well, but I'm sure there's a better way to do it
/// Manages all the state involved with the VM's execution, namely the stack, global variables, the heap, and the call frames
pub struct VMState {
    current_frame: CallFrame,
    frames: Vec<CallFrame>,

    lv_stack: Vec<LvObjectRef>,

    stack: Vec<Value>,
    globals: Vec<Global>,
    gc: GC,
    // Not implemented due to it destryoing my code => multiple upvalues pointing to the same original value in a function will NOT affect each other. This is a small enough edge case that I'm willing to just let it go
    // upvalues: Vec<Value>,
}

impl VMState {
    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(x) => x,
            None => panic!("VM panic! Attempted to pop a value when the value stack was empty"),
        }
    }

    // Note to future self: peek_mut SHOULD NEVER BE IMPLEMENTED!
    // Values on the stack are always implicit copy/cloned, any persistent values must be allocated with the Gc and represented with LivaPointers instead

    fn peek(&self) -> &Value {
        self.peek_at(0)
    }

    fn peek_at(&self, dist: usize) -> &Value {
        self.stack.get(self.stack.len() - dist - 1).unwrap()
    }

    fn alloc(&mut self, val: HeapObj) -> Value {
        self.gc.alloc(val, &self.stack, &self.globals)
    }

    // Fixme: Figure out how to not copy paste this code for mut and immut
    pub fn deref(&self, pointer: usize) -> &HeapObj {
        match self.gc.instances.get(pointer) {
            Some(x) => x,
            None => panic!("VM panic! Invalid pointer"),
        }
    }

    fn deref_mut(&mut self, pointer: usize) -> &mut HeapObj {
        match self.gc.instances.get_mut(pointer) {
            Some(x) => x,
            None => panic!("VM panic! Invalid pointer"),
        }
    }

    /// Attempts to
    /// 1. Take the given Value as a LivaPointer
    /// 2. Deref it into a HeapObj
    /// 3. Match the obj_types
    fn deref_into(
        &self,
        pointer_val: &Value,
        obj_type: HeapObjType,
    ) -> Result<&HeapObjVal, DerefError> {
        if let Value::LivaPointer(pointer) = pointer_val {
            let obj = self.deref(*pointer);
            if obj.obj_type == obj_type {
                Ok(&obj.obj)
            } else {
                Err(DerefError::WrongType)
            }
        } else {
            Err(DerefError::NotPointer)
        }
    }

    fn deref_into_mut(
        &mut self,
        pointer_val: &Value,
        obj_type: HeapObjType,
    ) -> Result<&mut HeapObjVal, DerefError> {
        if let Value::LivaPointer(pointer) = pointer_val {
            let obj = self.deref_mut(*pointer);
            if obj.obj_type == obj_type {
                Ok(&mut obj.obj)
            } else {
                Err(DerefError::WrongType)
            }
        } else {
            Err(DerefError::NotPointer)
        }
    }

    fn current_closure(&self) -> &ObjClosure {
        let pointer_val = self.stack.get(self.current_frame.frame_start).unwrap();
        match self.deref_into(pointer_val, HeapObjType::LivaClosure) {
            Ok(closure_obj) => closure_obj.as_closure(),
            Err(x) => panic!("VM panic! Unable to get current closure? {:?}", x),
        }
    }

    fn current_closure_mut(&mut self) -> &mut ObjClosure {
        let pointer_val = self
            .stack
            .get(self.current_frame.frame_start)
            .unwrap()
            .clone();
        match self.deref_into_mut(&pointer_val, HeapObjType::LivaClosure) {
            Ok(closure_obj) => closure_obj.as_closure_mut(),
            Err(x) => panic!("VM panic! Unable to get current closure? {:?}", x),
        }
    }

    fn increment_ip(&mut self) {
        self.current_frame.ip += 1;
    }

    fn jump(&mut self, offset: usize) {
        self.current_frame.ip += offset - 1;
    }

    fn jump_back(&mut self, neg_offset: usize) {
        self.current_frame.ip -= neg_offset + 1;
    }

    fn capture_upvalue(&self, upvalue: &UpValue) -> Value {
        if upvalue.is_local {
            // Just copy the value from the current stack frame (which is the parents)
            self.stack[self.current_frame.frame_start + upvalue.index].clone()
        } else {
            // Look at the current frame's closure's upvalue vec and copy it from there
            let parent_closure = self.current_closure();
            parent_closure.values[upvalue.index].clone()
        }
    }

    /// Push an upvalue onto the stack
    fn push_upvalue(&mut self, index: usize) {
        let closure = self.current_closure();
        let val = closure.values.get(index).unwrap().clone();
        self.stack.push(val);
    }

    /// Set an upvalue with the top value of the stack
    fn set_upvalue(&mut self, index: usize) {
        let val = self.peek().clone();
        let closure = self.current_closure_mut();
        closure.values[index] = val;
    }

    /// Checks if the targetted Value is callable {LivaPointer to a LoxClosure, NativeFn, LoxClass, LoxBoundMethod}, passes it to call() to continue attempting the call if necessary.
    ///
    /// Note: This function or call() must fufill the promise made in Resolver about what value sits in slot 0 of the local variables.
    /// Whether that's 'this' or a placeholder
    ///
    /// Returns a String containing an error message or None
    fn call_value(
        &mut self,
        arg_count: usize,
        function_defs: &Vec<FunctionChunk>,
        class_defs: &Vec<ClassChunk>,
        init_slot: &Option<usize>,
    ) -> Option<String> {
        let callee = self.peek_at(arg_count);
        if let Value::LivaPointer(_) = callee {
            match self.deref_into(callee, HeapObjType::LivaClosure) {
                Ok(closure) => {
                    let closure = closure.as_closure();
                    let fn_index = closure.function;
                    self.call(fn_index, arg_count, function_defs)
                }
                Err(_) => Some(String::from("Can only call functions and classes.")),
            }
        } else if let Value::LivaFunction(fn_index) = callee {
            let index = *fn_index;
            self.call(index, arg_count, function_defs)
        } else if let Value::LivaBoundMethod(method) = callee {
            let fn_index = method.method;
            let index = self.stack.len() - arg_count - 1; // Index to put the LivaPointer to represent the "this" variable
            self.stack[index] = Value::LivaPointer(method.pointer);
            self.call(fn_index, arg_count, function_defs)
        } else if let Value::LivaClass(class) = callee {
            let instance_obj = ObjInstance::new(*class);
            let class_def = &class_defs[*class];
            let ptr = self.alloc(HeapObj::new_instance(instance_obj));
            let index = self.stack.len() - arg_count - 1;
            self.stack[index] = ptr; // Replace the LivaClass with the pointer

            // Call the initializer if it exists
            // If the LivaClass was called with arguments the stack will look like this: LoxClass | arg1 | arg2
            // So we want to call with the stack as: LivaPointer => LoxInstance | arg1 | arg2
            // And we need the init() fn to return the LivaInstance
            if class_def.has_init {
                if init_slot == &None {
                    panic!("VM panic! Attempted to call a custom initializer without it existing as a method identifier?");
                }
                self.call(
                    *class_def.methods.get(&init_slot.unwrap()).unwrap(),
                    arg_count,
                    function_defs,
                )
            } else if arg_count != 0 {
                Some(format!(
                    "Expected 0 arguments but got {} instead",
                    arg_count
                ))
            } else {
                None
            }
        } else if let Value::NativeFunction(native_fn) = callee {
            let native_fn = native_fn.clone();
            self.call_native(&native_fn, arg_count);
            None
        } else {
            Some(String::from("Can only call functions and classes"))
        }
    }

    /// Attempts to call a function with the values on the stack, with the given # of arguments
    fn call(
        &mut self,
        fn_index: usize,
        arg_count: usize,
        function_defs: &Vec<FunctionChunk>,
    ) -> Option<String> {
        let target_fn = function_defs.get(fn_index).unwrap();
        if arg_count != target_fn.arity {
            return Some(format!(
                "Expected {} arguments but got {} instead",
                target_fn.arity, arg_count
            ));
        }
        if self.frames.len() == FRAMES_MAX {
            return Some(String::from("Stack overflow"));
        }

        let mut frame = CallFrame {
            function: fn_index,
            ip: 0,
            frame_start: self.stack.len() - arg_count - 1,
        };

        // Swap on the new call frame for the old one
        std::mem::swap(&mut self.current_frame, &mut frame);

        // Put the old one onto the stack
        self.frames.push(frame);
        return None;
    }

    /// Attempts to call a native (rust) function
    fn call_native(&mut self, native_fn: &NativeFn, arg_count: usize) {
        let mut args: Vec<Value> = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop());
        }
        self.pop(); // Pop off the Value::NativeFunction
        let result = native_fn(arg_count, args);
        self.stack.push(result)
    }

    /// Defines all native functions
    ///
    /// Searches for references to native functions and adds them in if they're used in the program
    /// Todo: make the compiler/vm reject using these strings as anything else other than to call global with
    fn define_std_lib(&mut self, identifiers: &Vec<String>) {
        // let idents: Vec<String> = identifiers.clone().into();
        for (index, ident) in identifiers.iter().enumerate() {
            match ident.as_str() {
                "clock" => self.globals[index] = Global::Init(Value::NativeFunction(clock)),
                "print" => self.globals[index] = Global::Init(Value::NativeFunction(print)),
                "println" => self.globals[index] = Global::Init(Value::NativeFunction(println)),
                "range" => self.globals[index] = Global::Init(Value::NativeFunction(range)),
                "exit" => self.globals[index] = Global::Init(Value::NativeFunction(exit)),
                _ => {}
            }
        }
    }

    /// Initializes the VMState with:
    ///
    /// - A CallFrame for function #0
    /// - Defined global variables for the native functions
    /// - A Value::LivaFunction for function #0 pushed onto the stack => Satisfies the resolver assumption that the first locals slot is filled with something
    fn new(identifiers: &Vec<String>) -> VMState {
        let first_fn = CallFrame {
            function: 0,
            ip: 0,
            frame_start: 0,
        };

        let first_val = Value::LivaFunction(0);
        let stack = vec![first_val];

        let mut state = VMState {
            current_frame: first_fn,
            lv_stack: Vec::new(),
            stack,
            frames: Vec::new(),
            globals: vec![Global::Uninit; identifiers.len()],
            gc: GC::new(),
        };

        state.define_std_lib(identifiers);
        state
    }
}

/// Contains all the information outputted by the compiler
/// ie: All function and class definitions
pub struct VM {
    quiet_mode: bool,
    debug_stepper: bool,
    mode: ExecutionMode,
    pub functions: Vec<FunctionChunk>,
    pub classes: Vec<ClassChunk>,
    pub constants: Vec<Value>,
    pub identifiers: Vec<String>,
    init_slot: Option<usize>,
}

impl VM {
    pub fn new(
        mode: ExecutionMode,
        result: CompilationResult,
        quiet: bool,
        debug_stepper: bool,
    ) -> VM {
        let functions = result.functions;
        let init_slot = result.identifier_constants.iter().position(|x| x == "init");
        VM {
            quiet_mode: quiet,
            debug_stepper,
            mode,
            functions,
            classes: result.classes,
            constants: result.constants,
            identifiers: result.identifier_constants,
            init_slot,
        }
    }

    fn runtime_error(&self, msg: &str, state: &VMState) {
        if self.quiet_mode {
            return;
        }

        eprintln!("{}", msg);
        for call_frame in [state.current_frame.clone()]
            .iter()
            .chain(state.frames.iter().rev())
        {
            let function = self.functions.get(call_frame.function).unwrap();
            eprint!(
                "[line {}] in ",
                function.chunk.code.get(call_frame.ip).unwrap().line_num
            );
            match &function.name {
                Some(name) => eprintln!("{}", name),
                None => eprintln!("script"),
            }
        }
    }

    /// Should only be used for getting debugging and error reporting
    ///
    /// * For the global instructions, just the index should suffice
    /// * For instance properties and fields, the hashmaps are keyed on the usize corresponding to the identifier string
    /// * Local variable names are erased completely by the resolver at compile time
    fn get_variable_name(&self, index: usize) -> &String {
        let name_val = self.identifiers.get(index);
        if let Some(var_name) = name_val {
            return var_name;
        } else {
            panic!("VM panic: Found a non LivaString value for a variable name");
        }
    }

    fn get_current_code(&self, state: &VMState) -> &Vec<Instr> {
        &self
            .functions
            .get(state.current_frame.function)
            .unwrap()
            .chunk
            .code
    }

    pub fn run(&self) -> InterpretResult {
        if let ExecutionMode::Trace = self.mode {
            eprintln!("== Starting execution | Mode: {:?} ==", self.mode);
            debug_print_constants(&self);
        }

        let mut state = VMState::new(&self.identifiers);

        // Makes getting new instructions faster
        // Update this vec whenever
        let mut current_code = &self.get_current_code(&state)[..];

        // Move this into a match arm that matches all the binary ops, and then matches on the individual opcodes?
        macro_rules! op_binary {
            ($oper: tt) => {
                {

                    match (state.pop(), state.pop()) {
                        (Value::Double(a), Value::Double(b)) => {
                            state.stack.push(Value::Double(b $oper a));
                        },
                        (Value::Integer(a), Value::Integer(b)) => {
                            state.stack.push(Value::Integer(b $oper a));
                        },
                        (Value::String(a), Value::String(b)) => {
                            let mut string = String::from(b);
                            string.push_str(&a);
                            state.stack.push(Value::String(string));
                        },
                        (a, b)  =>{
                            self.runtime_error(
                                format!("Attempted to perform a binary operation on incompatible values: {:?} and {:?}", a, b).as_str(),
                                &state
                            );
                        }
                    }
                }
            };

            ($val_type: path, $oper: tt) => {
                {
                     // if let ($val_type(a), $val_type(b)) = (self.pop(), self.pop()) {
                     if let (Value::Double(a), Value::Double(b)) = (state.pop(), state.pop()) {
                         state.stack.push($val_type(b $oper a))
                     } else {
                         self.runtime_error("Operands must be numbers", &state);
                         return InterpretResult::InterpretRuntimeError;
                     }
                }

            }

        }

        loop {
            if self.debug_stepper {
                use std::io::stdin;
                let mut s = String::new();
                stdin()
                    .read_line(&mut s)
                    .expect("Did not enter a correct string");
                if let Some('\n') = s.chars().next_back() {
                    s.pop();
                }
            }
            let instr = &current_code[state.current_frame.ip];
            state.increment_ip(); // Preincrement the ip so OpLoops to 0 are possible

            if let ExecutionMode::Trace = self.mode {
                debug_trace(&self, &instr, &state);
            }

            match instr.op_code {
                OpCode::Return => {
                    let result = state.pop(); // Save the result (the value on the top of the stack)
                    for _ in 0..(state.stack.len() - state.current_frame.frame_start) {
                        // Clean up the call frame part of that stack
                        state.pop();
                    }

                    if state.frames.is_empty() {
                        return InterpretResult::InterpretOK;
                    } else {
                        state.current_frame = state.frames.pop().unwrap(); // Update the current frame
                        current_code = &self.get_current_code(&state)[..]; // Update the current code
                        state.stack.push(result); // Push the result back
                    }
                }
                OpCode::Pop => {
                    state.pop();
                }
                OpCode::DefineGlobal(index) => {
                    let var_val = state.pop();
                    state.globals[index] = Global::Init(var_val);
                }
                OpCode::CallGlobal(index, arity) => {
                    let var_val = &state.globals[index];
                    match var_val {
                        Global::Init(x) => {
                            let new = x.clone();
                            let index = state.stack.len() - arity;
                            state.stack.insert(index, new);
                            let result = state.call_value(
                                arity,
                                &self.functions,
                                &self.classes,
                                &self.init_slot,
                            );
                            current_code = &self.get_current_code(&state)[..]; // Update the current code
                            if let Some(msg) = result {
                                self.runtime_error(&msg[..], &state);
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                        _ => {
                            self.runtime_error(
                                format!("Undefined variable '{}'", self.get_variable_name(index))
                                    .as_str(),
                                &state,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::GetGlobal(index) => {
                    let var_val = &state.globals[index];
                    match var_val {
                        Global::Init(x) => {
                            let new = x.clone();
                            state.stack.push(new)
                        }
                        _ => {
                            self.runtime_error(
                                format!("Undefined variable '{}'", self.get_variable_name(index))
                                    .as_str(),
                                &state,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::SetGlobal(index) => {
                    // We don't want assignment to pop the value since this is an expression
                    // this will almost always be in a expression statement, which will pop the value
                    let var_val = state.peek().clone();
                    match state.globals[index] {
                        Global::Init(_) => state.globals[index] = Global::Init(var_val), // We require it to be initialized (ie defined earlier by OpDefineGlobal)
                        _ => {
                            self.runtime_error(
                                format!("Undefined variable '{}'", self.get_variable_name(index))
                                    .as_str(),
                                &state,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::GetLocal(index) => state
                    .stack
                    .push(state.stack[state.current_frame.frame_start + index].clone()), // Note: We gotta clone these values around the stack because our operators pop off the top and we also don't want to modify the variable value
                OpCode::SetLocal(index) => {
                    let dest = state.current_frame.frame_start + index;
                    state.stack[dest] = state.peek().clone(); // Same idea as OpSetGlobal, don't pop value since it's an expression
                }

                OpCode::Invoke(name_index, arg_count) => {
                    let pointer_val = state.peek_at(arg_count);

                    println!("Pointer Value: {}, arg count: {}", pointer_val, arg_count);
                    let result = match state.deref_into(pointer_val, HeapObjType::LivaInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance();
                            let class_def = &self.classes[instance.class];
                            if instance.fields.contains_key(&name_index) {
                                // Guard against the weird edge case where instance.thing() is actually calling a closure instance.thing, not a method invocation
                                let value = instance.fields.get(&name_index).unwrap().clone();
                                let index = state.stack.len() - 1 - arg_count;
                                state.stack[index] = value; // Remove the instance and replace with the value
                                state.call_value(
                                    arg_count,
                                    &self.functions,
                                    &self.classes,
                                    &self.init_slot,
                                )
                            // Perform the call
                            } else if class_def.methods.contains_key(&name_index) {
                                // We know that the top of the stack is LivaPointer | arg1 | arg2
                                // So we can go ahead and call
                                let fn_index = class_def.methods.get(&name_index).unwrap();
                                state.call(*fn_index, arg_count, &self.functions)
                            } else {
                                Some(format!(
                                    "Undefined property '{}' in {:?}",
                                    self.get_variable_name(name_index),
                                    instance
                                ))
                            }
                        }
                        Err(_) => Some(String::from("Can only invoke methods on class instances")),
                    };

                    if let Some(error) = result {
                        self.runtime_error(error.as_str(), &state);
                        return InterpretResult::InterpretRuntimeError;
                    }
                    current_code = &self.get_current_code(&state)[..]; // Update the current code
                }
                OpCode::LoadMethod(name_index) | OpCode::LoadAttr(name_index) => {
                    let pointer_val = state.peek();

                    // Todo: Combine this and SetProperty into a macro so it doesn't hurt me everytime i have to read this
                    match state.deref_into(pointer_val, HeapObjType::LivaInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance();
                            if instance.fields.contains_key(&name_index) {
                                // See if we tried to get a field
                                let value = instance.fields.get(&name_index).unwrap().clone();
                                state.pop(); // Remove the instance
                                state.stack.push(value); // Replace with the value
                            } else {
                                let class_chunk = &self.classes[instance.class]; // if not a field, then we must be getting a function. Create a LivaBoundMethod for it
                                if class_chunk.methods.contains_key(&name_index) {
                                    let bound_value = ObjBoundMethod {
                                        method: *class_chunk.methods.get(&name_index).unwrap(),
                                        pointer: pointer_val.as_pointer(),
                                    };
                                    state.pop(); // Remove the instance
                                    state.stack.push(Value::LivaBoundMethod(bound_value));
                                // Replace with bound method
                                } else {
                                    self.runtime_error(
                                        format!(
                                            "Undefined property '{}' in {:?}",
                                            self.get_variable_name(name_index),
                                            instance
                                        )
                                        .as_str(),
                                        &state,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            }
                        }
                        Err(_) => {
                            let msg = format!("Only class instances can access properties with '.' Found {} instead", pointer_val.to_string(&self, &state));
                            self.runtime_error(msg.as_str(), &state);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::CallMethod(name_index, arg_count) => {
                    // Get the class instance
                    let pointer_val = state.peek_at(arg_count);

                    println!(
                        "CM: Pointer Value: {}, arg count: {}",
                        pointer_val, arg_count
                    );

                    let result = match state.deref_into(pointer_val, HeapObjType::LivaInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance();
                            let class_def = &self.classes[instance.class];
                            if instance.fields.contains_key(&name_index) {
                                // Guard against the weird edge case where instance.thing() is actually calling a closure instance.thing, not a method invocation
                                let value = instance.fields.get(&name_index).unwrap().clone();
                                let index = state.stack.len() - 1 - arg_count;
                                state.stack[index] = value; // Remove the instance and replace with the value
                                state.call_value(
                                    arg_count,
                                    &self.functions,
                                    &self.classes,
                                    &self.init_slot,
                                )
                            // Perform the call
                            } else if class_def.methods.contains_key(&name_index) {
                                // We know that the top of the stack is LoxPointer | arg1 | arg2
                                // So we can go ahead and call
                                let fn_index = class_def.methods.get(&name_index).unwrap();
                                state.call(*fn_index, arg_count, &self.functions)
                            } else {
                                Some(format!(
                                    "Undefined property '{}' in {:?}",
                                    self.get_variable_name(name_index),
                                    instance
                                ))
                            }
                        }
                        Err(_) => Some(String::from("Can only invoke methods on class instances")),
                    };

                    if let Some(error) = result {
                        self.runtime_error(error.as_str(), &state);
                        return InterpretResult::InterpretRuntimeError;
                    }
                    current_code = &self.get_current_code(&state)[..]; // Update the current code
                }
                OpCode::StoreAttr(name_index) => {
                    // First pop value is owner of attr
                    let owner_ref = state.pop();

                    // Second pop is the assigning value
                    let val = state.peek().clone();

                    match state.deref_into_mut(&owner_ref, HeapObjType::LivaInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance_mut();
                            instance.fields.insert(name_index, val.clone());
                        }
                        Err(_) => {
                            let msg = format!("Only class instances can access properties with '.' Found {} instead", owner_ref.to_string(&self, &state));
                            self.runtime_error(msg.as_str(), &state);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }

                    // We return on an error, so we can clean up the stack now
                    state.pop(); // Instance
                    state.stack.push(val); // Return the value to the stack
                }
                OpCode::GetIndexItem => {
                    let index_expr = state.pop(); //Index Expression
                    let indexable_expr = state.pop();

                    if let Value::Double(num) = index_expr {
                        if let Value::List(list) = indexable_expr {
                            state.stack.push(list[num as usize].clone());
                        }
                    } else {
                        self.runtime_error("Only numbers as usable as indexes", &state);
                    }
                }
                // This is almost identical to OpGetProperty, but it goes one extra jump to get the method from the superclass, and binds it to itself
                OpCode::GetSuper(name_index) => {
                    let pointer_val = state.peek();
                    let superclass_val = state.peek_at(1);
                    if let Value::LivaClass(superclass) = superclass_val {
                        // Todo: Combine this and SetProperty into a macro so it doesn't hurt me everytime i have to read this
                        match state.deref_into(pointer_val, HeapObjType::LivaInstance) {
                            Ok(instance) => {
                                let instance = instance.as_instance();
                                let superclass_chunk = &self.classes[*superclass];
                                if superclass_chunk.methods.contains_key(&name_index) {
                                    let bound_value = ObjBoundMethod {
                                        method: *superclass_chunk.methods.get(&name_index).unwrap(),
                                        pointer: pointer_val.as_pointer(),
                                    };
                                    // println!("Superclass get method found method {:?} ", bound_value);
                                    // println!("Superclass methods {:?}", superclass_chunk.methods);
                                    // println!("Superclass for {:?} is {:?}", instance, class_chunk.superclass);
                                    state.pop(); // Remove the instance
                                    state.stack.push(Value::LivaBoundMethod(bound_value));
                                // Replace with bound method
                                } else {
                                    self.runtime_error(
                                        format!(
                                            "Undefined superclass method '{}' for {}",
                                            self.get_variable_name(name_index),
                                            self.classes.get(instance.class).unwrap().name,
                                        )
                                        .as_str(),
                                        &state,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            }
                            Err(_) => {
                                panic!("VM panic! Failed to obtain instance LivaPointer for super");
                            }
                        }
                    } else {
                        panic!("VM panic! Failed to obtain superclass index for super, got {:?} instead", superclass_val);
                    }
                }

                OpCode::GetUpvalue(index) => {
                    state.push_upvalue(index);
                }
                OpCode::SetUpvalue(index) => {
                    state.set_upvalue(index);
                }

                OpCode::Closure => {
                    if let Value::LivaFunction(function) = state.pop() {
                        let mut closure = ObjClosure::new(function); // Capture values into the closure here

                        let fn_chunk = self.functions.get(function).unwrap();
                        for upvalue in fn_chunk.upvalues.as_ref().unwrap().iter() {
                            closure.values.push(state.capture_upvalue(upvalue))
                        }
                        let ptr = state.alloc(HeapObj::new_closure(closure));
                        state.stack.push(ptr);
                    } else {
                        panic!("VM panic! Attempted to wrap a non-function value in a closure");
                    }
                }

                OpCode::Jump(offset) => state.jump(offset),
                OpCode::JumpIfFalse(offset) => {
                    if is_falsey(state.peek()) {
                        // Does not pop the value off the top of the stack because we need them for logical operators
                        state.jump(offset);
                    }
                }
                OpCode::Loop(neg_offset) => state.jump_back(neg_offset),

                OpCode::Call(arity) => {
                    let result =
                        state.call_value(arity, &self.functions, &self.classes, &self.init_slot);
                    current_code = &self.get_current_code(&state)[..]; // Update the current code
                    if let Some(msg) = result {
                        self.runtime_error(&msg[..], &state);
                        return InterpretResult::InterpretRuntimeError;
                    }
                }

                OpCode::Class(index) => state.stack.push(Value::LivaClass(index)),

                OpCode::Constant(index) => state.stack.push(self.constants[index].clone()),
                OpCode::True => state.stack.push(Value::Bool(true)),
                OpCode::False => state.stack.push(Value::Bool(false)),
                OpCode::Nil => state.stack.push(Value::Nil),

                OpCode::Add => op_binary!(+),
                OpCode::Divide => op_binary!(/),
                OpCode::Subtract => op_binary!(-),
                OpCode::Multiply => op_binary!(*),
                OpCode::Greater => op_binary!(Value::Bool, >),
                OpCode::Less => op_binary!(Value::Bool, <),
                OpCode::Modulo => op_binary!(%),
                OpCode::Equal => {
                    let t = (&state.pop(), &state.pop());
                    state.stack.push(Value::Bool(values_equal(t)));
                }

                OpCode::Not => {
                    let val = Value::Bool(is_falsey(&state.pop()));
                    state.stack.push(val);
                }
                OpCode::Negate => {
                    let value = state.pop().as_num();
                    match value {
                        Some(x) => state.stack.push(Value::Double(x * -1.0)),
                        None => {
                            self.runtime_error("Attempted to negate a non-number value", &state);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                // OpCode::BuildMap(count) => {
                //     let mut map = HashMap::new();

                //     for _ in 0..count {
                //         let key = state.pop();
                //         let value = state.pop();
                //         map.insert(key, value);
                //     }

                //     state.stack.push(Value::Map(map));
                // }
                OpCode::BuildList(elem_count) => {
                    let mut values = Vec::new();
                    for _ in 0..elem_count {
                        values.push(state.pop());
                    }
                    state
                        .stack
                        .push(Value::List(values.into_iter().rev().collect()));
                }
                // TODO: Impl
                OpCode::GetIter => {
                    let iterable = state.pop();
                    // Clone and put back on stack
                    // let list = iterable.clone();
                    let iter = match iterable {
                        Value::List(list) => {
                            let mut iterator = Iterable { iter: list };
                            // let iter_item = iterator.next();
                            let v_iterator = Value::Iterable(iterator);
                            // state.stack.push(iter_item);
                            state.stack.push(v_iterator)
                        }
                        _ => {
                            self.runtime_error("{} is not iterable!", &state);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    // let iter_item = iter.next();
                    // // state.stack.push(list);
                    // state.stack.push(iter);
                }
                OpCode::ForIter(off_set) => {
                    let iterable = state.pop();

                    match iterable {
                        Value::Iterable(mut iter) => {
                            let iter_item = iter.next();
                            // println!("ItermItem: {:?}", iter_item);
                            // println!("After: {:?}", iter);
                            match iter_item {
                                Some(value) => {
                                    state.stack.push(Value::Iterable(iter));
                                    state.stack.push(value);
                                }
                                // Jump to end
                                None => state.jump(off_set),
                            }
                        }
                        _ => {
                            self.runtime_error(
                                format!(
                                    "Expected to be a iterator from the stack, but was: {:?}",
                                    iterable
                                )
                                .as_str(),
                                &state,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
            }
        }
    }
}

fn debug_state_trace(state: &VMState, vm: &VM) {
    eprintln!("> Frame: {:?}", state.current_frame);
    eprintln!("> Stack: ");
    eprint!(">>");
    for value in state.stack.iter() {
        eprint!(" [ {:?} ] ", value);
    }
    eprintln!("");
    eprintln!("> Globals: ");
    for (index, val) in state.globals.iter().enumerate() {
        if let Global::Init(global) = val {
            eprintln!(">> {} => {:?}", vm.get_variable_name(index), global);
        }
    }
    debug_instances(state);
}

fn debug_instances(state: &VMState) {
    eprintln!("> Instances: ");
    for (i, instance) in state.gc.instances.iter().enumerate() {
        eprintln!(">> [{}] {:?}", i, instance)
    }
}

fn debug_trace(vm: &VM, instr: &Instr, state: &VMState) {
    eprintln!("---");
    eprint!("> Next instr (#{}): ", state.current_frame.ip - 1);
    disassemble_instruction(
        instr,
        state.current_frame.ip - 1,
        &vm.constants,
        &vm.identifiers,
    );
    debug_state_trace(state, vm);
    eprintln!("---\n");
}

fn debug_print_constants(vm: &VM) {
    eprintln!("---");
    eprintln!("> Constants");
    for val in vm.constants.iter() {
        eprintln!(">> [ {:?} ]", val);
    }
    eprintln!("---\n");
}
