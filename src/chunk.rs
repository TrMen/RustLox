use std::usize;

use crate::value::Value;
use num_traits::FromPrimitive;
use strum_macros::Display;

pub type ConstantIndex = u16;
pub type CodeIndex = usize;

#[repr(u8)]
#[derive(Clone, Copy, Debug, Display, FromPrimitive, ToPrimitive, PartialEq)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl OpCode {
    pub fn from_u8(val: u8) -> Option<OpCode> {
        FromPrimitive::from_u8(val)
    }
}

pub struct LineInformation {
    pub line: i32,
    pub count: usize,
}

pub struct Chunk {
    pub code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<LineInformation>,
}

pub enum ChunkContent {
    Code(OpCode),
    CodeIndex(u8),
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            lines: Vec::new(), // At the same index as an instruction in code, there is the corresponding line in lines
            constants: Vec::new(),
        }
    }

    pub fn append_op(&mut self, val: OpCode, line: i32) {
        self.code.push(val as u8);

        if self.lines.is_empty() || self.lines.last().unwrap().line != line {
            self.lines.push(LineInformation { line, count: 0 });
        }

        self.lines.last_mut().unwrap().count += 1;
    }

    pub fn append_constant_index(&mut self, val: ConstantIndex, line: i32) {
        let [high, low] = val.to_be_bytes();

        self.code.push(high);
        self.code.push(low);

        if self.lines.is_empty() || self.lines.last().unwrap().line != line {
            self.lines.push(LineInformation { line, count: 0 });
        }

        self.lines.last_mut().unwrap().count += 1;
    }

    pub fn get_line(&self, code_index: CodeIndex) -> Option<i32> {
        let mut line_index = 0;

        self.lines
            .iter()
            .find(move |line_information| {
                line_index += line_information.count;
                line_index > code_index
            })
            .map(|line_information| line_information.line)
    }

    pub fn add_constant(&mut self, val: Value) -> Result<ConstantIndex, &'static str> {
        if self.constants.len() >= ConstantIndex::MAX as usize {
            Err("Too many constants in one chunk")
        } else {
            self.constants.push(val);
            Ok((self.constants.len() - 1) as ConstantIndex)
        }
    }

    pub fn constant_at_code_index(&self, code_index: CodeIndex) -> &Value {
        let high = self.code[code_index];
        let low = self.code[(code_index + 1)];

        let combined_index = u16::from_be_bytes([high, low]);

        &self.constants[combined_index as CodeIndex]
    }

    pub fn get_op(&self, index: CodeIndex) -> Option<OpCode> {
        // Return the OpCode if there is one at this location.
        // Otherwise, return None (no more code, or constant_index here)
        OpCode::from_u8(*self.code.get(index)?)
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item = ChunkContent> + '_> {
        // Meant for easy iteration. Not maximally efficient.
        Box::new(self.code.iter().map(|byte| match OpCode::from_u8(*byte) {
            Some(op) => ChunkContent::Code(op),
            None => ChunkContent::CodeIndex(*byte),
        }))
    }

    pub fn content_at(&self, index: CodeIndex) -> Option<ChunkContent> {
        // Meant for easy access. Not maximally efficient.
        self.code
            .get(index as usize)
            .map(|byte| match OpCode::from_u8(*byte) {
                Some(op) => ChunkContent::Code(op),
                None => ChunkContent::CodeIndex(*byte),
            })
    }
}

#[cfg(test)]
mod tests {
    use num_traits::{FromPrimitive, ToPrimitive};

    use super::*;
    #[test]
    fn lines() {
        let mut chunk = Chunk::new();

        for _ in 0..10 {
            chunk.append_op(OpCode::Return, 0);
        }
        for line in 0..10 {
            assert_eq!(chunk.get_line(line).unwrap(), 0);
        }

        for line in 0..20 {
            chunk.append_op(OpCode::Return, line);
        }
        for line in 0..20 {
            assert_eq!(chunk.get_line(10 + line).unwrap(), line as i32);
        }
    }

    #[test]
    fn constants() {
        let mut chunk = Chunk::new();

        assert_eq!(chunk.add_constant(Value::Double(42.0)), Ok(0));
        assert_eq!(chunk.add_constant(Value::Double(43.0)), Ok(1));
        assert_eq!(chunk.add_constant(Value::Double(44.0)), Ok(2));

        assert_eq!(chunk.constants[0], Value::Double(42.0));
        assert_eq!(chunk.constants[1], Value::Double(43.0));
        assert_eq!(chunk.constants[2], Value::Double(44.0));
    }

    #[test]
    fn op_code_is_1_byte_big() {
        assert_eq!(std::mem::size_of::<OpCode>(), 1);
    }

    #[test]
    fn opcode_to_primitive() {
        assert_eq!(FromPrimitive::from_u8(0), Some(OpCode::Return));
        assert_eq!(FromPrimitive::from_u8(1), Some(OpCode::Constant));
        assert_eq!(FromPrimitive::from_u8(2), Some(OpCode::Negate));

        assert_eq!(ToPrimitive::to_u8(&OpCode::Return), Some(0));
        assert_eq!(ToPrimitive::to_u8(&OpCode::Constant), Some(1));
        assert_eq!(ToPrimitive::to_u8(&OpCode::Negate), Some(2));

        let out_of_range: Option<OpCode> = FromPrimitive::from_u8(150);

        assert_eq!(out_of_range, None);
    }

    #[test]
    fn constant_indexing() {
        let mut chunk = Chunk::new();

        let first_constant_index = chunk.add_constant(Value::Double(0.0)).unwrap();
        let second_constant_index = chunk.add_constant(Value::Double(1.0)).unwrap();

        assert_eq!(first_constant_index, 0);
        assert_eq!(second_constant_index, 1);

        chunk.append_op(OpCode::Constant, 0); // code-index 0
        chunk.append_constant_index(first_constant_index, 0); // code-index 1 + 2

        chunk.append_op(OpCode::Constant, 0); // code-index 3
        chunk.append_constant_index(second_constant_index, 0); // code-index 4+5

        assert_eq!(chunk.code[1], 0);
        assert_eq!(chunk.code[2], 0);

        assert_eq!(chunk.code[4], 0);
        assert_eq!(chunk.code[5], 1);

        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(0.0));
        assert_eq!(chunk.constant_at_code_index(4), &Value::Double(1.0));
    }
}
