mod assembler;
mod chunk;
mod compiler;
mod precedence;
mod parser;
mod scanner;
mod value;
mod vm;
mod object;

#[macro_use]
extern crate num_derive;

use std::{
    io::{self, Write},
    process::exit,
};

use crate::vm::VM;
use compiler::Compiler;

enum InterpretationError {
    Runtime,
    Compiletime,
}

fn interpret(source: String) -> Result<(), InterpretationError> {
    match Compiler::compile(&source) {
        Err(e) => {
            println!("Compiler-error: {}", &e.msg);
            Err(InterpretationError::Compiletime)
        }
        Ok(chunk) => {
            #[cfg(debug_assertions)]
            assembler::disassemble(&chunk, "code");

            println!("\nStart of interpretation\n");
            println!(
                "{:04} {:4} {:-16} {}",
                "ip", "line", "Instruction", "constant"
            );

            let mut vm = VM::new();

            vm.interpret(chunk)
                .map_err(|_| InterpretationError::Runtime)
        }
    }
}

fn repl() -> Result<(), io::Error> {
    loop {
        print!("> ");
        std::io::stdout().flush()?;

        let mut buf = String::new();

        io::stdin().read_line(&mut buf)?;

        println!("Interpreting  \"{}\"\n", buf.trim_end());

        match interpret(buf) {
            Err(InterpretationError::Compiletime) => exit(65),
            Err(InterpretationError::Runtime) => exit(70),
            Ok(()) => return Ok(()),
        }
    }
}

fn run_file(filename: String) -> Result<(), io::Error> {
    match interpret(std::fs::read_to_string(filename)?) {
        Err(InterpretationError::Compiletime) => exit(65),
        Err(InterpretationError::Runtime) => exit(70),
        Ok(()) => Ok(()),
    }
}

fn main() {
    let arg_count = std::env::args().count();
    if arg_count == 1 {
        if let Err(e) = repl() {
            println!("Encountered error running repl: {}", e);
            exit(42);
        }
    } else if arg_count == 2 {
        if let Err(e) = run_file(std::env::args().nth(1).unwrap()) {
            println!("Encountered error running file: {}", e);
            exit(42);
        }
    } else {
        println!("Usage: clox [path]");
        exit(64);
    }
}
