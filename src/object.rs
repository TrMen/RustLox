use std::rc::Rc;

use crate::indexable_string_set::IndexableStringSet;

pub trait LoxObject {
    // Common information for garbage collection
}

#[derive(Clone, Debug)]
pub struct ObjString {
    pub index: usize, // Index in the string interning list. Since that list only ever grows, the string should always exist at that index
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        // Interning means two equal strings will always have the same index
        self.index == other.index
    }
}

impl LoxObject for ObjString {}

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    // Anything heap-allocated is an Obj
    String(ObjString),
}

#[derive(Debug)]
pub struct ObjectList {
    objects: Vec<Rc<Object>>,

    strings: IndexableStringSet,
}

impl ObjectList {
    pub fn new() -> ObjectList {
        ObjectList {
            objects: Vec::new(),
            strings: IndexableStringSet::new(),
        }
    }

    pub fn get_string_by_index(&self, index: usize) -> &String {
        self.strings.get_by_index(index)
    }

    pub fn add_string(&mut self, string: String) -> Rc<Object> {
        let index = self.strings.get_or_insert(string);

        let string_object = Rc::new(Object::String(ObjString { index }));

        // Note: Two different objects can have the same string, so 2 entries in obj list,
        // but only one in string list
        self.objects.push(string_object);

        self.objects.last().unwrap().clone()
    }

    pub fn add_existing_object(&mut self, object: Rc<Object>) {
        // TODO: This prolly shouldn't exist. All objects should start by being created in the list
        self.objects.push(object);
    }
}
