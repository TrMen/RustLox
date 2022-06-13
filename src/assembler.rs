use crate::chunk::{Chunk, CodeIndex, OpCode};

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("\n== {} ==\n", name);

    let mut iter = chunk.code.iter().enumerate();

    while let Some((code_index, content)) = iter.next() {
        let instruction = OpCode::from_u8(*content).expect("Invalid instruction");

        if instruction == OpCode::Constant
            || instruction == OpCode::DefineGlobal
            || instruction == OpCode::GetGlobal
        {
            iter.next(); // Skip the following two constant indices (low and high)
            iter.next();
        }

        disassemble_instruction(chunk, code_index, instruction);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, code_index: CodeIndex, content: OpCode) {
    print!("Code Index: {:04} ", code_index);

    let line = chunk
        .get_line(code_index)
        .expect("No line saved for code index");

    print!("{:4} ", line);

    match content {
        OpCode::Constant => constant_instruction(chunk, "OP_CONSTANT", code_index + 1),
        OpCode::DefineGlobal => constant_instruction(chunk, "OP_DEFINE_GLOBAL", code_index + 1),
        OpCode::GetGlobal => constant_instruction(chunk, "OP_GET_GLOBAL", code_index + 1),
        op => println!("{}", op),
    }
}

fn constant_instruction(chunk: &Chunk, name: &str, code_index: CodeIndex) {
    println!(
        "{:-16} '{:?}'",
        name,
        chunk.constant_at_code_index(code_index),
    );
}
