use std::{
    collections::HashSet,
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

use crate::object::{ObjString, Object};

#[derive(Debug, PartialEq, Clone)] // TODO: Remove clone for possible string-deduplication
pub enum Value {
    Double(f32),
    Bool(bool),
    Nil,
    Obj(Rc<Object>),
}

pub fn print_vec_val(values: &Vec<Value>) {
    print!("[");

    for val in values {
        print!("{}, ", val);
    }

    println!("]");
}

type ValueResult = Result<Value, &'static str>;

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(val) => !val,
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

    fn compare(&self, other: &Value, op: impl FnOnce(&f32, &f32) -> bool) -> ValueResult {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, other) {
            Ok(Value::Bool(op(lhs, rhs)))
        } else {
            Err("Operands must be numbers")
        }
    }

    fn compare_strings(&self, interned_strings: HashSet<ObjString>, rhs: &Value) -> bool {
        todo!();
    }

    fn as_string(&self) -> Option<&ObjString> {
        if let Value::Obj(obj) = self {
            match obj.as_ref() {
                Object::String(str) => Some(str),
                _ => None,
            }
        } else {
            None
        }
    }

    fn copy(&self, interned_strings: HashSet<String>) -> Value {
        todo!();
        /*if let Some(str) = self.as_string() {
            if let Some(existing_string) = interned_strings.get(&str.string) {
                Value::Obj(Object::from_string(existing_string))
            }
            else {
                Value::Obj(Object::from_string(str.string)
            }
        }
        else {

        }*/
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Double(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Obj(val) => write!(f, "{}", val),
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

impl Add for Value {
    type Output = ValueResult;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (&self, &rhs) {
            Ok(Value::Double(lhs + rhs))
        } else if let (Value::Obj(lhs), Value::Obj(rhs)) = (self, rhs) {
            if let (Object::String(lhs), Object::String(rhs)) = (lhs.as_ref(), rhs.as_ref()) {
                Ok(Value::Obj(Object::from_string(
                    lhs.string.clone() + &rhs.string,
                )))
            } else {
                Err("Operands must be numbers or strings")
            }
        } else {
            Err("Operands must be numbers or strings")
        }
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
