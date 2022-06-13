use std::rc::Rc;

use crate::chunk::{Chunk, ChunkContent, CodeIndex, OpCode};

pub struct Assembler {
    chunk: Rc<Chunk>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            chunk: Rc::new(Chunk::new()),
        }
    }

    pub fn disassemble(&mut self, chunk: Rc<Chunk>, name: &str) {
        self.chunk = chunk;

        println!("== {} ==\n", name);

        let mut iter = self.chunk.iter().enumerate();

        while let Some((code_index, content)) = iter.next() {
            self.disassemble_instruction(code_index, content);
        }
    }

    pub fn disassemble_instruction(&self, code_index: CodeIndex, content: ChunkContent) {
        print!("{:04} ", code_index);

        let line = self
            .chunk
            .get_line(code_index)
            .expect("No line saved for offset");

        print!("{:4} ", line);

        match content {
            ChunkContent::Code(OpCode::Constant) => {
                self.constant_instruction("CONSTANT", code_index)
            }
            ChunkContent::Code(op) => println!("{}", op),
            _ => (), // Ignore constants
        }
    }

    fn constant_instruction(&self, name: &str, code_offset: CodeIndex) {
        println!(
            "{:-16} {:4} '{}'",
            name,
            code_offset,
            self.chunk.constant_at_code_index(code_offset),
        );
    }
}
