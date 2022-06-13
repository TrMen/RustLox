use crate::chunk::{Chunk, CodeIndex, OpCode};

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("\n== {} ==\n", name);

    let mut iter = chunk.code.iter().enumerate();

    while let Some((code_index, content)) = iter.next() {
        let instruction = OpCode::from_u8(*content).expect("Invalid instruction");

        // TODO: Make these use OpCodeWithArg
        if instruction == OpCode::Constant
            || instruction == OpCode::DefineGlobal
            || instruction == OpCode::GetGlobal
            || instruction == OpCode::SetLocal
            || instruction == OpCode::GetLocal
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

    // TODO: Make these use OpCodeWithArg
    match content {
        op @ (OpCode::Constant | OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal) => {
            println!(
                "{op:-16} '{:?}'",
                chunk.constant_at_code_index(code_index + 1),
            );
        }
        op @ (OpCode::SetLocal | OpCode::GetLocal) => {
            // Names of locals are never stored in the chunk, so we can't print them.
            // So at least print the stack offset of the local
            println!("{op:-16} '{:?}'", chunk.arg_at_code_index(code_index + 1),);
        }
        op => println!("{op}"),
    }
}
