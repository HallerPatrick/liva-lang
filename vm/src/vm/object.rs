use core::cell::RefCell;
use core::sync::atomic::AtomicUsize;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    chunk::{ClassChunk, FunctionChunk, Instr},
    value::Value,
};

/// There are very different ways to implement a `Object` as the atom
/// of the object system.
///
/// `Python` and `RustPython` have the `PyObject` that is never
/// directly declared but all objects can be casted to it and reference
/// one
///
/// This:
/// https://github.com/ProgVal/pythonvm-rust/blob/master/src/objects/mod.rs
/// implement the object more concrete where all contents/data of a object
/// are references to the actual data.
///
/// This is for now probably easier to implement, as i have no
/// idea of all the reference/pointer things

static CURRENT_REF_ID: AtomicUsize = AtomicUsize::new(0);
static CURRENT_VERSION: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, PartialEq, Clone)]
pub struct LvObjectRef {
    id: usize,
}

impl LvObjectRef {
    pub fn new() -> LvObjectRef {
        LvObjectRef {
            id: CURRENT_REF_ID.fetch_add(1, ::core::sync::atomic::Ordering::SeqCst),
        }
    }

    pub fn get_id(&self) -> usize {
        self.id
    }

    fn new_version() -> u64 {
        CURRENT_VERSION.fetch_add(1, ::core::sync::atomic::Ordering::SeqCst) as u64
        // TODO: avoid cast
    }
}

pub struct LvObject {
    pub version: u64,
    pub name: Option<String>,
    pub data: ObjectData,
    pub class: LvObjectRef,
    pub bases: Option<Vec<LvObjectRef>>,
    pub attributes: Option<Rc<RefCell<HashMap<String, LvObjectRef>>>>,
}

impl LvObject {
    fn new_version() -> u64 {
        CURRENT_VERSION.fetch_add(1, ::core::sync::atomic::Ordering::SeqCst) as u64
        // TODO: avoid cast
    }

    pub fn new_instance(name: Option<String>, class: LvObjectRef, content: ObjectData) -> LvObject {
        LvObject {
            version: LvObject::new_version(),
            name: name,
            data: content,
            class: class,
            bases: None,
            attributes: Some(Rc::new(RefCell::new(HashMap::new()))),
        }
    }

    pub fn new_class(
        name: String,
        attributes: Option<Rc<RefCell<HashMap<String, LvObjectRef>>>>,
        metaclass: LvObjectRef,
        bases: Vec<LvObjectRef>,
    ) -> LvObject {
        LvObject {
            version: LvObject::new_version(),
            name: Some(name),
            data: ObjectData::Class,
            class: metaclass,
            bases: Some(bases),
            attributes: attributes,
        }
    }
}

pub struct Code {
    pub code: Vec<Instr>,
    pub filename: String,
    // pub constants: Vec<LvObjectRef>,
    pub classes: Vec<ClassChunk>,
    pub functions: Vec<FunctionChunk>,
    pub constants: Vec<Value>,
    pub identifier_constants: Vec<String>,
}

pub enum ObjectData {
    True,
    False,
    Nil,
    // LvObject(PyObject),
    LvString(String), // Actual rust string
    LvNum(f64),       // Actual float
    LvList(Vec<LvObjectRef>),
    LvMap(Vec<(LvObjectRef, LvObjectRef)>),
    Function(String, LvObjectRef, HashMap<String, LvObjectRef>), // module, code, default arguments
    Module(LvObjectRef),
    Class,
    OtherObject,
}

/// Aka the ARENA
pub struct ObjectStore {
    all_objects: HashMap<usize, LvObject>,
}

impl ObjectStore {
    pub fn new() -> Self {
        ObjectStore {
            all_objects: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, obj: LvObject) -> LvObjectRef {
        let obj_ref = LvObjectRef::new();
        self.all_objects.insert(obj_ref.id.clone(), obj);
        obj_ref
    }

    /// Dereferences a [LvObject] for a given [LvObjectRef]
    pub fn deref(&mut self, obj_ref: LvObjectRef) -> &LvObject {
        self.all_objects.get(&obj_ref.id).unwrap()
    }

    /// Dereferences a [LvObject] for a given [LvObjectRef]
    pub fn deref_mut(&mut self, obj_ref: LvObjectRef) -> &mut LvObject {
        self.all_objects.get_mut(&obj_ref.id).unwrap()
    }
    pub fn allocate_at(&mut self, obj_ref: LvObjectRef, obj: LvObject) {
        match self.all_objects.get(&obj_ref.id) {
            None => self.all_objects.insert(obj_ref.id, obj),
            _ => panic!("Already allocated"),
        };
    }
}

pub struct PrimitiveObjects {
    pub object: LvObjectRef,
    pub type_: LvObjectRef,

    pub nil_type: LvObjectRef,
    pub nil: LvObjectRef,

    pub int_type: LvObjectRef,
    pub bool_type: LvObjectRef,
    pub true_obj: LvObjectRef,
    pub false_obj: LvObjectRef,

    // pub tuple_type: LvObjectRef,
    pub list_type: LvObjectRef,

    // pub set_type: LvObjectRef,
    // pub frozenset_type: LvObjectRef,
    pub dict_type: LvObjectRef,

    // pub bytes_type: LvObjectRef,
    pub str_type: LvObjectRef,

    pub iterator_type: LvObjectRef,

    pub function_type: LvObjectRef,
    // pub code_type: LvObjectRef,
    pub module: LvObjectRef,
    // pub baseexception: LvObjectRef,
    // pub processorerror: LvObjectRef,
    // pub exception: LvObjectRef,

    //     pub nameerror: LvObjectRef,
    //     pub attributeerror: LvObjectRef,
    //     pub typeerror: LvObjectRef,
    //     pub stopiteration: LvObjectRef,

    // pub lookuperror: LvObjectRef,
    // pub keyerror: LvObjectRef,

    // pub names_map: HashMap<String, LvObjectRef>,
}

impl PrimitiveObjects {
    pub fn new(store: &mut ObjectStore) -> PrimitiveObjects {
        let obj_ref = LvObjectRef::new();
        let type_ref = LvObjectRef::new();
        let obj = LvObject {
            version: LvObject::new_version(),
            name: Some("object".to_string()),
            data: ObjectData::OtherObject,
            bases: Some(vec![]),
            class: type_ref.clone(),
            attributes: None,
        };
        let type_ = LvObject {
            version: LvObject::new_version(),
            name: Some("type".to_string()),
            data: ObjectData::OtherObject,
            bases: Some(vec![obj_ref.clone()]),
            class: type_ref.clone(),
            attributes: None,
        };
        store.allocate_at(obj_ref.clone(), obj);
        store.allocate_at(type_ref.clone(), type_);

        let nil_type = store.allocate(LvObject::new_class(
            "niltype".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let nil = store.allocate(LvObject::new_instance(
            Some("Nil".to_string()),
            nil_type.clone(),
            ObjectData::Nil,
        ));

        let int_type = store.allocate(LvObject::new_class(
            "int".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let bool_type = store.allocate(LvObject::new_class(
            "bool".to_string(),
            None,
            type_ref.clone(),
            vec![int_type.clone()],
        ));
        let true_obj = store.allocate(LvObject::new_instance(
            Some("True".to_string()),
            bool_type.clone(),
            ObjectData::True,
        ));
        let false_obj = store.allocate(LvObject::new_instance(
            Some("False".to_string()),
            bool_type.clone(),
            ObjectData::False,
        ));

        let tuple_type = store.allocate(LvObject::new_class(
            "tuple".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let list_type = store.allocate(LvObject::new_class(
            "list".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let set_type = store.allocate(LvObject::new_class(
            "set".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let frozenset_type = store.allocate(LvObject::new_class(
            "frozenset".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let dict_type = store.allocate(LvObject::new_class(
            "dict".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let bytes_type = store.allocate(LvObject::new_class(
            "bytes".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let str_type = store.allocate(LvObject::new_class(
            "str".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let iterator_type = store.allocate(LvObject::new_class(
            "iterator".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));

        let function_type = store.allocate(LvObject::new_class(
            "function".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let code_type = store.allocate(LvObject::new_class(
            "code".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));

        let module = store.allocate(LvObject::new_class(
            "module".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));

        let baseexception = store.allocate(LvObject::new_class(
            "BaseException".to_string(),
            None,
            type_ref.clone(),
            vec![obj_ref.clone()],
        ));
        let processorerror = store.allocate(LvObject::new_class(
            "ProcessorError".to_string(),
            None,
            type_ref.clone(),
            vec![baseexception.clone()],
        ));
        let exception = store.allocate(LvObject::new_class(
            "Exception".to_string(),
            None,
            type_ref.clone(),
            vec![baseexception.clone()],
        ));

        let nameerror = store.allocate(LvObject::new_class(
            "NameError".to_string(),
            None,
            type_ref.clone(),
            vec![exception.clone()],
        ));
        let attributeerror = store.allocate(LvObject::new_class(
            "AttributeError".to_string(),
            None,
            type_ref.clone(),
            vec![exception.clone()],
        ));
        let typeerror = store.allocate(LvObject::new_class(
            "TypeError".to_string(),
            None,
            type_ref.clone(),
            vec![exception.clone()],
        ));
        let stopiteration = store.allocate(LvObject::new_class(
            "StopIteration".to_string(),
            None,
            type_ref.clone(),
            vec![exception.clone()],
        ));

        let lookuperror = store.allocate(LvObject::new_class(
            "LookupError".to_string(),
            None,
            type_ref.clone(),
            vec![exception.clone()],
        ));
        let keyerror = store.allocate(LvObject::new_class(
            "KeyError".to_string(),
            None,
            type_ref.clone(),
            vec![lookuperror.clone()],
        ));

        let mut map = HashMap::new();
        map.insert("object".to_string(), obj_ref.clone());
        map.insert("tuple".to_string(), type_ref.clone());
        map.insert("nonetype".to_string(), nil_type.clone());
        map.insert("None".to_string(), nil.clone());
        map.insert("True".to_string(), true_obj.clone());
        map.insert("False".to_string(), false_obj.clone());
        map.insert("int".to_string(), int_type.clone());
        map.insert("bool".to_string(), bool_type.clone());
        map.insert("tuple".to_string(), tuple_type);
        map.insert("list".to_string(), list_type.clone());
        map.insert("set".to_string(), set_type);
        map.insert("frozenset".to_string(), frozenset_type);
        map.insert("dict".to_string(), dict_type.clone());
        map.insert("bytes".to_string(), bytes_type.clone());
        map.insert("str".to_string(), str_type.clone());
        map.insert("function".to_string(), function_type.clone());
        map.insert("code".to_string(), code_type.clone());
        map.insert("module".to_string(), module.clone());

        // Base classes
        map.insert("BaseException".to_string(), baseexception.clone());
        map.insert("ProcessorError".to_string(), processorerror.clone());
        map.insert("Exception".to_string(), exception.clone());

        map.insert("NameError".to_string(), nameerror.clone());
        map.insert("AttributeError".to_string(), attributeerror.clone());
        map.insert("TypeError".to_string(), typeerror.clone());
        map.insert("StopIteration".to_string(), stopiteration.clone());

        map.insert("LookupError".to_string(), lookuperror.clone());
        map.insert("KeyError".to_string(), keyerror.clone());

        PrimitiveObjects {
            object: obj_ref,
            type_: type_ref,
            nil_type,
            nil,
            int_type,
            bool_type,
            true_obj,
            false_obj,
            // tuple_type: tuple_type,
            list_type,
            // set_type: set_type,
            // frozenset_type: frozenset_type,
            dict_type,
            // bytes_type: bytes_type,
            str_type,
            iterator_type,
            function_type,
            // code_type: code_type,
            // baseexception: baseexception,
            // processorerror: processorerror,
            // exception: exception,
            // nameerror: nameerror,
            // attributeerror: attributeerror,
            // typeerror: typeerror,
            // stopiteration: stopiteration,
            // lookuperror: lookuperror,
            // keyerror: keyerror,
            module,
            // names_map: map,
        }
    }

    pub fn new_int(&self, i: u32) -> LvObject {
        LvObject::new_instance(None, self.int_type.clone(), ObjectData::LvNum(i as f64))
    }
    pub fn new_string(&self, s: String) -> LvObject {
        LvObject::new_instance(None, self.str_type.clone(), ObjectData::LvString(s))
    }
    // pub fn new_bytes(&self, b: Vec<u8>) -> LvObject {
    //     LvObject::new_instance(None, self.bytes_type.clone(), ObjectContent::Bytes(b))
    // }
    // pub fn new_tuple(&self, v: Vec<LvLvObjectRef>) -> Object {
    //     LvObject::new_instance(None, self.tuple_type.clone(), ObjectContent::Tuple(v))
    // }
    pub fn new_list(&self, v: Vec<LvObjectRef>) -> LvObject {
        LvObject::new_instance(None, self.list_type.clone(), ObjectData::LvList(v))
    }
    // pub fn new_set(&self, v: Vec<LvObjectRef>) -> Object {
    //     LvObject::new_instance(None, self.set_type.clone(), ObjectContent::Set(v))
    // }
    pub fn new_dict(&self, v: Vec<(LvObjectRef, LvObjectRef)>) -> LvObject {
        LvObject::new_instance(None, self.dict_type.clone(), ObjectData::LvMap(v))
    }
    // pub fn new_frozenset(&self, v: Vec<LvObjectRef>) -> Object {
    //     LvObject::new_instance(
    //         None,
    //         self.frozenset_type.clone(),
    //         ObjectContent::FrozenSet(v),
    //     )
    // }
    // pub fn new_code(&self, c: Code) -> LvObject {
    //     LvObject::new_instance(
    //         None,
    //         self.code_type.clone(),
    //         ObjectData::Code(Box::new(c)),
    //     )
    // }
    pub fn new_function(
        &self,
        name: String,
        module_name: String,
        code: LvObjectRef,
        defaults: HashMap<String, LvObjectRef>,
    ) -> LvObject {
        LvObject::new_instance(
            Some(name),
            self.function_type.clone(),
            ObjectData::Function(module_name, code, defaults),
        )
    }
    pub fn new_module(&self, name: String, code: LvObjectRef) -> LvObject {
        LvObject::new_instance(Some(name), self.module.clone(), ObjectData::Module(code))
    }
}
