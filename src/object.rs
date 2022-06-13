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

impl Object {
    pub fn from_str(string: &str, interned_strings: &mut IndexableStringSet) -> Rc<Self> {
        let index = interned_strings.get_or_insert(string);

        Rc::new(Object::String(ObjString { index }))
    }

    pub fn from_string(string: String, interned_strings: &mut IndexableStringSet) -> Rc<Self> {
        let index = interned_strings.get_or_insert_existing(string);

        Rc::new(Object::String(ObjString { index }))
    }
}

#[derive(Debug)]
pub struct ObjectList {
    objects: Vec<Rc<Object>>,
}

impl ObjectList {
    pub fn new() -> ObjectList {
        ObjectList {
            objects: Vec::new(),
        }
    }

    pub fn add_existing_object(&mut self, object: Rc<Object>) {
        self.objects.push(object);
    }
}
