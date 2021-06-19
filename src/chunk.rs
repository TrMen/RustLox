use std::{u32, usize};

use crate::value::Value;
use strum_macros::Display;

#[derive(Clone, Copy, Debug, Display)]
pub enum OpCode {
    Return,
    Constant(u16),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub struct LineInformation {
    pub line: u32,
    pub count: usize,
}

pub struct Chunk {
    pub code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: Vec<LineInformation>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            lines: Vec::new(), // At the same index as an instruction in code, there is the corresponding line in lines
            constants: Vec::new(),
        }
    }

    pub fn append(&mut self, val: OpCode, line: u32) {
        self.code.push(val);

        if self.lines.is_empty() || self.lines.last().unwrap().line != line {
            self.lines.push(LineInformation { line, count: 0 });
        }

        self.lines.last_mut().unwrap().count += 1;
    }

    pub fn get_line(&self, instruction_index: usize) -> Option<u32> {
        let mut line_index = 0;

        self.lines
            .iter()
            .find(move |line_information| {
                line_index += line_information.count;
                line_index > instruction_index
            })
            .map(|line_information| line_information.line)
    }

    pub fn add_constant(&mut self, val: Value) -> Option<u16> {
        if self.constants.len() >= u16::MAX as usize {
            None
        } else {
            self.constants.push(val);
            Some((self.constants.len() - 1) as u16)
        }
    }

    pub fn get_constant(&self, index: u16) -> &Value {
        &self.constants[index as usize]
    }

    pub fn get_op(&self, index: usize) -> Option<OpCode> {
        self.code.get(index).cloned()
    }

    pub fn code_iter(&self) -> std::slice::Iter<'_, OpCode> {
        self.code.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lines() {
        let mut chunk = Chunk::new();

        for _ in 0..10 {
            chunk.append(OpCode::Return, 0);
        }
        for line in 0..10 {
            assert_eq!(chunk.get_line(line).unwrap(), 0);
        }

        for line in 0..20 {
            chunk.append(OpCode::Return, line);
        }
        for line in 0..20 {
            assert_eq!(chunk.get_line(10 + line).unwrap(), line as u32);
        }
    }

    #[test]
    fn constants() {
        let mut chunk = Chunk::new();

        assert_eq!(chunk.add_constant(Value::Double(42.0)), Some(0));
        assert_eq!(chunk.add_constant(Value::Double(43.0)), Some(1));
        assert_eq!(chunk.add_constant(Value::Double(44.0)), Some(2));

        assert_eq!(chunk.get_constant(0), &Value::Double(42.0));
        assert_eq!(chunk.get_constant(1), &Value::Double(43.0));
        assert_eq!(chunk.get_constant(2), &Value::Double(44.0));
    }
}
