use std::{
    ops::{Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

use crate::{
    indexable_string_set::IndexableStringSet,
    object::{Object, ObjectList},
};

#[derive(Debug, Clone)]
pub enum Value {
    Double(f32),
    Bool(bool),
    Nil,
    Obj(Rc<Object>),
}

pub fn print_vec_val(values: &Vec<Value>) {
    print!("[");

    for val in values {
        print!("{:?}, ", val);
    }

    println!("]");
}

type ValueResult = Result<Value, &'static str>;

impl Value {
    pub fn as_str<'a>(&self, strings: &'a IndexableStringSet) -> &'a str {
        if let Value::Obj(obj) = self {
            if let Object::String(name) = &**obj {
                strings.get_by_index(name.index)
            } else {
                panic!("Global constant must be string");
            }
        } else {
            panic!("Global constant must be string");
        }
    }

    pub fn stringify(&self, strings: &IndexableStringSet) -> String {
        // TODO: Stringify shouldn't require heap allocation, except for doubles
        // and it's only for display, so doubles don't need to be allocated either
        match self {
            Value::Nil => "nil".to_string(),
            Value::Double(val) => val.to_string(),
            Value::Bool(val) => val.to_string(),
            Value::Obj(val) => match &**val {
                Object::String(str) => strings.get_by_index(str.index).to_owned(),
            },
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(val) => *val,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn gt(lhs: Value, rhs: Value) -> ValueResult {
        Value::compare(&lhs, &rhs, PartialOrd::gt)
    }

    pub fn lt(lhs: Value, rhs: Value) -> ValueResult {
        Value::compare(&lhs, &rhs, PartialOrd::lt)
    }

    // Cannot be std::core::ops::Add because it needs to know how to add
    // newly allocated string to ObjectList
    pub fn add(
        self,
        rhs: Self,
        strings: &mut IndexableStringSet,
        objects: &mut ObjectList,
    ) -> ValueResult {
        if let (Value::Double(lhs), Value::Double(rhs)) = (&self, &rhs) {
            Ok(Value::Double(lhs + rhs))
        } else if let (Value::Obj(lhs), Value::Obj(rhs)) = (self, rhs) {
            if let (Object::String(lhs), Object::String(rhs)) = (lhs.as_ref(), rhs.as_ref()) {
                let concatenated =
                    strings.get_by_index(lhs.index).to_owned() + strings.get_by_index(rhs.index);

                let obj_string = Object::from_string(concatenated, strings);
                objects.add_existing_object(obj_string.clone());

                Ok(Value::Obj(obj_string))
            } else {
                Err("Operands must be numbers or strings")
            }
        } else {
            Err("Operands must be numbers or strings")
        }
    }

    fn compare(&self, other: &Value, op: impl FnOnce(&f32, &f32) -> bool) -> ValueResult {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, other) {
            Ok(Value::Bool(op(lhs, rhs)))
        } else {
            Err("Operands must be numbers")
        }
    }
}

impl Neg for Value {
    type Output = ValueResult;

    fn neg(self) -> Self::Output {
        if let Value::Double(val) = self {
            Ok(Value::Double(-val))
        } else {
            Err("Operand must be a number")
        }
    }
}

impl Not for Value {
    type Output = ValueResult;

    fn not(self) -> Self::Output {
        Ok(Value::Bool(!self.is_truthy()))
    }
}

impl Sub for Value {
    type Output = ValueResult;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            Ok(Value::Double(lhs - rhs))
        } else {
            Err("Operands must be numbers")
        }
    }
}

impl Mul for Value {
    type Output = ValueResult;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            Ok(Value::Double(lhs * rhs))
        } else {
            Err("Operands must be numbers")
        }
    }
}

impl Div for Value {
    type Output = ValueResult;

    fn div(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            Ok(Value::Double(lhs / rhs))
        } else {
            Err("Operands must be numbers")
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Double(l0), Self::Double(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Obj(l0), Self::Obj(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
