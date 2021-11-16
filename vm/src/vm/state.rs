use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    object::{LvObjectRef, ObjectStore, PrimitiveObjects},
    CallFrame,
};

type LivaFunction = fn(&mut State, &mut Vec<CallFrame>, Vec<LvObjectRef>);

pub struct State {
    pub store: ObjectStore,
    pub primitive_functions: HashMap<String, LivaFunction>,
    pub primtive_objects: PrimitiveObjects,
    pub modules: HashMap<String, Rc<RefCell<HashMap<String, LvObjectRef>>>>,
}
