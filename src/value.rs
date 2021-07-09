use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Double(f32),
    Bool(bool),
    Nil,
    Object(String),
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

    fn compare(&self, other: &Value, op: impl FnOnce(&f32, &f32) -> bool) -> ValueResult {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, other) {
            Ok(Value::Bool(op(lhs, rhs)))
        } else {
            Err("Operands must be numbers")
        }
    }

    pub fn gt(lhs: Value, rhs: Value) -> ValueResult {
        Value::compare(&lhs, &rhs, PartialOrd::gt)
    }

    pub fn lt(lhs: Value, rhs: Value) -> ValueResult {
        Value::compare(&lhs, &rhs, PartialOrd::lt)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
            Value::Object(obj) => write!(f, "Object {:?}", obj),
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
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            Ok(Value::Double(lhs + rhs))
        } else {
            Err("Operands must be numbers")
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
