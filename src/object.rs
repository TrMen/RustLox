use std::{collections::HashSet, fmt::Display, hash::Hash, rc::Rc};

pub trait LoxObject {
    // Common information for garbage collection
}

#[derive(Clone, PartialEq, Debug)]
pub struct ObjString {
    pub string: String,
}

impl LoxObject for ObjString {}

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    // Anything heap-allocated is an Obj
    String(ObjString),
}

impl Object {
    pub fn from_string(string: String) -> Rc<Object> {
        Rc::new(Object::String(ObjString { string }))
    }
}

pub struct ObjectList {
    objects: Vec<Rc<Object>>,
    // Strings are interned (deduplicated). Any two identical strings will Rc to the same ObjString
    strings: HashSet<String>,
}

impl ObjectList {
    pub fn new() -> ObjectList {
        ObjectList {
            objects: Vec::new(),
            strings: HashSet::new(),
        }
    }

    pub fn add_from_string(&mut self, string: String) -> Rc<Object> {
        let string_object = Object::from_string(string);

        self.objects.push(string_object);

        self.objects.last().unwrap().clone()
    }

    pub fn add_existing_object(&mut self, object: Rc<Object>) {
        self.objects.push(object);
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(obj) => write!(f, "{}", obj.string),
        }
    }
}

impl Display for ObjectList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        for object in &self.objects {
            write!(f, "{}({}), ", object, Rc::strong_count(object))?;
        }

        write!(f, "]")
    }
}
