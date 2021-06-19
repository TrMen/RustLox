use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Double(f32),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        if let Value::Double(val) = self {
            return Value::Double(-val);
        }
        panic!("Cannot negate");
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            return Value::Double(lhs + rhs);
        }
        panic!("Cannot add");
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            return Value::Double(lhs - rhs);
        }
        panic!("Cannot add");
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            return Value::Double(lhs * rhs);
        }
        panic!("Cannot mul");
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        if let (Value::Double(lhs), Value::Double(rhs)) = (self, rhs) {
            return Value::Double(lhs / rhs);
        }
        panic!("Cannot div");
    }
}
