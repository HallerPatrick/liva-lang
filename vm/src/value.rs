use crate::native::NativeFn;
use crate::vm::{VMState, VM};

use std::fmt;

use std::hash::Hasher;

use std::collections::HashMap;

// struct LivaObject {}

#[derive(Debug, Clone, PartialEq)]
pub struct Iterable {
    // For now we only allow lists
    pub iter: Vec<Value>,
}

impl Iterable {
    // pub fn empty(&self) -> bool {
    //     self.iter.is_empty()
    // }

    // Removes first item from vec and returns it
    pub fn next(&mut self) -> Option<Value> {
        if self.iter.is_empty() {
            return None;
        }

        Some(self.iter.remove(0))
    }
}
// impl PartialEq for Iterable {
//     fn eq(&self) {}
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Double(f64),
    Integer(i32),
    Bool(bool),
    Nil,
    String(String),
    List(Vec<Value>),
    // Map(HashMap<Value, Value>),
    Iterable(Iterable),
    LivaFunction(usize), // Index of the function in the functions Vec in VM // Fixme: Is this even reachable? Can this be completely removed and the parameter put in OpClosure?
    NativeFunction(NativeFn),
    LivaClass(usize),
    LivaPointer(usize),
    LivaBoundMethod(ObjBoundMethod),
}

// impl Hash for Value {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         match self {}
//         // self.id.hash(state);
//         // self.phone.hash(state);
//     }
// }

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Double(d) => write!(f, "{}", d),
            Value::Integer(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "{}", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Value {
    /// Used for print statements, use {:?} debug formatting for trace and stack examining
    pub fn to_string(&self, vm: &VM, state: &VMState) -> String {
        match self {
            Value::Double(x) => format!("{}", x),
            Value::Integer(x) => format!("{}", x),
            Value::Bool(x) => format!("{}", x),
            Value::String(x) => x.to_string(),
            Value::Nil => String::from("nil"),
            Value::LivaFunction(x) => format!(
                "<fn {}>",
                vm.functions.get(*x).unwrap().name.as_ref().unwrap()
            ),
            Value::NativeFunction(_x) => "<native_fn>".to_string(),
            Value::LivaClass(class) => format!("<class {}>", class),
            Value::LivaPointer(pointer) => format!(
                "<pointer {}> to {}",
                pointer,
                state.deref(*pointer).to_string(vm)
            ), // Suggestion: Don't reveal to the user the internals?
            Value::LivaBoundMethod(method) => format!(
                "<method {} from {}",
                vm.functions
                    .get(method.method)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap(),
                state.deref(method.pointer).to_string(vm)
            ),
            Value::List(list) => format!("{:?}", list),
            Value::Iterable(iter) => format!("{:?}", iter),
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        if let Value::Double(val) = self {
            Some(*val)
        } else {
            None
        }
    }

    /// Hard cast to a ObjPointer. Panics if this value is not a LivaPointer
    pub fn as_pointer(&self) -> usize {
        if let Value::LivaPointer(ptr) = self {
            *ptr
        } else {
            panic!(
                "VM panic! Failed to cast value to a pointer. Found {:?} instead",
                self
            )
        }
    }
}

pub fn is_falsey(val: &Value) -> bool {
    matches!(val, Value::Bool(false) | Value::Nil)
}

pub fn values_equal(t: (&Value, &Value)) -> bool {
    // TODO:Heuristic?
    let error_margin = 0.00001;
    match t {
        (Value::Double(x), Value::Double(y)) => (x - y).abs() < error_margin,
        (Value::Integer(x), Value::Integer(y)) => (x == y),
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        (Value::String(x), Value::String(y)) => x.eq(y),
        (Value::LivaPointer(x), Value::LivaPointer(y)) => x == y,
        (Value::LivaClass(x), Value::LivaClass(y)) => x == y,
        (Value::LivaFunction(x), Value::LivaFunction(y)) => x == y,
        (Value::NativeFunction(x), Value::NativeFunction(y)) => x == y,
        (Value::LivaBoundMethod(x), Value::LivaBoundMethod(y)) => x == y,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjBoundMethod {
    pub method: usize,  // Index into the functions vec for which function to call
    pub pointer: usize, // Pointer to the LivaInstance that this method is bound to
}

// End of stack/implicit copy objects

// Heap Objects

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeapObjType {
    HeapPlaceholder,
    LivaInstance,
    LivaClosure,
}

#[derive(Debug, PartialEq)]
pub struct HeapObj {
    pub obj: HeapObjVal,
    pub obj_type: HeapObjType,
    pub is_marked: bool,
}

impl HeapObj {
    fn to_string(&self, vm: &VM) -> String {
        self.obj.to_string(vm)
    }

    pub fn new_instance(val: ObjInstance) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::LivaInstance(val),
            obj_type: HeapObjType::LivaInstance,
            is_marked: false,
        }
    }

    pub fn new_closure(val: ObjClosure) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::LivaClosure(val),
            obj_type: HeapObjType::LivaClosure,
            is_marked: false,
        }
    }

    pub fn new_placeholder() -> HeapObj {
        HeapObj {
            obj: HeapObjVal::HeapPlaceholder,
            obj_type: HeapObjType::HeapPlaceholder,
            is_marked: false,
        }
    }
}

// I swear i really tried to not have this be duplicate with HeapObjType, but couldn't figure out a way to do it
#[derive(Debug, PartialEq)]
pub enum HeapObjVal {
    HeapPlaceholder,
    LivaInstance(ObjInstance),
    LivaClosure(ObjClosure),
    // LivaString(String), // Maybe...
}

impl HeapObjVal {
    fn to_string(&self, vm: &VM) -> String {
        match self {
            HeapObjVal::LivaClosure(closure) => format!(
                "<fn {} | {:?}>",
                vm.functions
                    .get(closure.function)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap(),
                closure
            ),
            HeapObjVal::LivaInstance(instance) => format!(
                "<instance {}>",
                vm.classes.get(instance.class).unwrap().name
            ),
            HeapObjVal::HeapPlaceholder => {
                panic!("VM panic! How did a placeholder value get here?")
            }
        }
    }

    pub fn as_closure(&self) -> &ObjClosure {
        if let HeapObjVal::LivaClosure(closure) = self {
            closure
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut ObjClosure {
        if let HeapObjVal::LivaClosure(closure) = self {
            closure
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_instance(&self) -> &ObjInstance {
        if let HeapObjVal::LivaInstance(instance) = self {
            instance
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_instance_mut(&mut self) -> &mut ObjInstance {
        if let HeapObjVal::LivaInstance(instance) = self {
            instance
        } else {
            panic!("VM panic!")
        }
    }
}

/// Runtime instantiation of class definitions
#[derive(Debug, PartialEq)]
pub struct ObjInstance {
    pub class: usize,                  // Which class was this instance made from?
    pub fields: HashMap<usize, Value>, // Stores the field values. FunctionChunks are stored in the ClassChunk, which is not ideal since it adds an extra vec lookup before getting to the function
}

impl ObjInstance {
    pub fn new(class: usize) -> ObjInstance {
        ObjInstance {
            class,
            fields: HashMap::new(),
        }
    }
}

/// Runtime representation of the closure, ie what variables are in scope
#[derive(Debug, PartialEq)]
pub struct ObjClosure {
    pub function: usize,
    pub values: Vec<Value>, // Will be filled at runtime
}

impl ObjClosure {
    pub fn new(function: usize) -> ObjClosure {
        ObjClosure {
            function,
            values: Vec::new(),
        }
    }
}
