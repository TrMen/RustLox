use std::usize;

use crate::value::Value;
use num_traits::FromPrimitive;
use strum_macros::Display;

// TODO: Make these strong types
pub type TwoByteArg = u16;
// Count of where the value of a local variable is on the stack
// At runtime, the value of the local variable is at that many locals away on the stack.
pub type LocalIndex = u16;
pub type ConstantIndex = u16;
pub type CodeIndex = usize;
pub type JumpOffset = u16;

// Comments documenting usage in OpcodeWithArg and OpCodeWithoutArg
#[repr(u8)]
#[derive(Clone, Copy, Debug, Display, FromPrimitive, ToPrimitive, PartialEq)]
pub enum OpCode {
    GetGlobal,
    DefineGlobal,
    GetLocal,
    SetLocal,
    Pop,
    Print,
    Return,
    Constant,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    JumpIfFalse,
    JumpForward,
    JumpBackward,
    JumpIfTrue,
}

impl OpCode {
    pub fn from_u8(val: u8) -> Option<OpCode> {
        FromPrimitive::from_u8(val)
    }
}

impl From<OpCodeWithArg> for OpCode {
    fn from(op: OpCodeWithArg) -> Self {
        match op {
            OpCodeWithArg::DefineGlobal => Self::DefineGlobal,
            OpCodeWithArg::GetGlobal => Self::GetGlobal,
            OpCodeWithArg::Constant => Self::Constant,
            OpCodeWithArg::SetLocal => Self::SetLocal,
            OpCodeWithArg::GetLocal => Self::GetLocal,
            OpCodeWithArg::JumpIfFalse => Self::JumpIfFalse,
            OpCodeWithArg::JumpForward => Self::JumpForward,
            OpCodeWithArg::JumpBackward => Self::JumpBackward,
            OpCodeWithArg::JumpIfTrue => Self::JumpIfTrue,
        }
    }
}

impl From<OpCodeWithoutArg> for OpCode {
    fn from(op: OpCodeWithoutArg) -> Self {
        match op {
            OpCodeWithoutArg::Pop => OpCode::Pop,
            OpCodeWithoutArg::Print => OpCode::Print,
            OpCodeWithoutArg::Return => OpCode::Return,
            OpCodeWithoutArg::Nil => OpCode::Nil,
            OpCodeWithoutArg::True => OpCode::True,
            OpCodeWithoutArg::False => OpCode::False,
            OpCodeWithoutArg::Equal => OpCode::Equal,
            OpCodeWithoutArg::Greater => OpCode::Greater,
            OpCodeWithoutArg::Less => OpCode::Less,
            OpCodeWithoutArg::Negate => OpCode::Negate,
            OpCodeWithoutArg::Add => OpCode::Add,
            OpCodeWithoutArg::Subtract => OpCode::Subtract,
            OpCodeWithoutArg::Multiply => OpCode::Multiply,
            OpCodeWithoutArg::Divide => OpCode::Divide,
            OpCodeWithoutArg::Not => OpCode::Not,
        }
    }
}

// The argument is always 2 bytes
#[derive(PartialEq)]
#[repr(u8)]
pub enum OpCodeWithArg {
    GetGlobal,
    DefineGlobal,
    SetLocal,
    GetLocal, // Note: No DefineLocal, since that's all done at comptime
    Constant,
    JumpIfTrue,
    JumpIfFalse, // Argument is how much to offset the ip by if the condition (top of stack) is false
    JumpForward,
    JumpBackward, // Note: Called OP_LOOP in the book
}

#[derive(PartialEq)]
#[repr(u8)]
pub enum OpCodeWithoutArg {
    Pop,
    Print,
    Return,
    Nil, // Nil, True, False are optimizations for avoiding constant lookup in those cases
    True,
    False,
    Equal,
    Greater,
    Less,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
}

#[derive(Debug)]
pub struct LineInformation {
    pub line: i32,
    pub count: usize,
}

// Contains opcodes, constants and their associated source code lines
#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<LineInformation>,
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

    // Add the arg as two bytes in a row to the code.
    // Does not add anything to the constant table
    pub fn append_arg(&mut self, val: TwoByteArg, line: i32) {
        let [high, low] = val.to_be_bytes();

        self.code.push(high);
        self.code.push(low);

        // Once for low, once for high
        for _ in 0..2 {
            if self.lines.is_empty() || self.lines.last().unwrap().line != line {
                self.lines.push(LineInformation { line, count: 0 });
            }

            self.lines.last_mut().unwrap().count += 1;
        }
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

    // Add a constant to the constant table without adding it's index to the code
    pub fn add_constant(&mut self, val: Value) -> Result<ConstantIndex, &'static str> {
        if self.constants.len() >= ConstantIndex::MAX as usize {
            Err("Too many constants in one chunk")
        } else {
            self.constants.push(val);
            Ok((self.constants.len() - 1) as ConstantIndex)
        }
    }

    pub fn arg_at_code_index(&self, code_index: CodeIndex) -> TwoByteArg {
        let high = self.code[code_index];
        let low = self.code[code_index + 1];

        TwoByteArg::from_be_bytes([high, low])
    }

    pub fn change_arg_at_code_index(&mut self, code_index: CodeIndex, value: TwoByteArg) {
        let [high, low] = TwoByteArg::to_be_bytes(value);
        self.code[code_index] = high;
        self.code[code_index + 1] = low;
    }

    pub fn constant_at_code_index(&self, code_index: CodeIndex) -> &Value {
        let constant_index = self.arg_at_code_index(code_index);

        &self.constants[constant_index as usize]
    }

    pub fn code_bytes_len(&self) -> usize {
        self.code.len()
    }

    pub fn instruction_at(&self, code_index: CodeIndex) -> Option<OpCode> {
        self.code.get(code_index).map(|op| {
            OpCode::from_u8(*op).expect("Content cannot be interpreted as an instruction")
        })
    }
}

#[cfg(test)]
mod tests {
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
    fn lines_for_constants() {
        let mut chunk = Chunk::new();

        for line in 0..10 {
            chunk.append_arg(0, line);
        }

        for line in 0..10 {
            // Low and high need to have the same line
            assert_eq!(chunk.get_line(2 * line), Some(line as i32));
            assert_eq!(chunk.get_line(2 * line + 1), Some(line as i32));
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
    fn constant_indexing() {
        let mut chunk = Chunk::new();

        let first_constant_index = chunk.add_constant(Value::Double(0.0)).unwrap();
        let second_constant_index = chunk.add_constant(Value::Double(1.0)).unwrap();

        assert_eq!(first_constant_index, 0);
        assert_eq!(second_constant_index, 1);

        chunk.append_op(OpCode::Constant, 0); // code-index 0
        chunk.append_arg(first_constant_index, 0); // code-index 1 + 2

        chunk.append_op(OpCode::Constant, 0); // code-index 3
        chunk.append_arg(second_constant_index, 0); // code-index 4+5

        assert_eq!(chunk.code[1], 0);
        assert_eq!(chunk.code[2], 0);

        assert_eq!(chunk.code[4], 0);
        assert_eq!(chunk.code[5], 1);

        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(0.0));
        assert_eq!(chunk.constant_at_code_index(4), &Value::Double(1.0));
    }
}
