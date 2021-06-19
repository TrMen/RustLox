use crate::chunk::{Chunk, OpCode};

pub struct Assembler {
    last_line: Option<u32>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler { last_line: None }
    }

    pub fn disassemble(&mut self, chunk: &Chunk, name: &str) {
        println!("== {} ==\n", name);

        let mut iter = chunk.code_iter().enumerate();

        while let Some((offset, _)) = iter.next() {
            self.disassemble_instruction(chunk, offset);
        }
    }

    pub fn disassemble_instruction(&mut self, chunk: &Chunk, offset: usize) {
        print!("{:04} ", offset);

        let line = chunk.get_line(offset).expect("No line saved for offset");

        if self.last_line == Some(line) {
            print!("   | ");
        } else {
            print!("{:4} ", line);
        }

        self.last_line = Some(line);

        let op = chunk.get_op(offset);

        if op.is_none() {
            return;
        }

        match op.unwrap() {
            OpCode::Constant(constant_index) => {
                constant_instruction("CONSTANT", chunk, constant_index)
            }
            _ => println!("{}", op.unwrap()),
        }
    }
}

fn constant_instruction(name: &str, chunk: &Chunk, constant_index: u16) {
    println!(
        "{:-16} {:4} '{}'",
        name,
        constant_index,
        chunk.get_constant(constant_index),
    );
}
