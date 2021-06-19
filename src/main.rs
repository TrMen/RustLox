mod assembler;
mod chunk;
mod common;
mod compiler;
mod scanner;
mod value;
mod vm;

use std::{
    io::{self, Write},
    process::exit,
};

use compiler::Compiler;
use vm::InterpretationResult;

use crate::{
    assembler::Assembler,
    chunk::{Chunk, OpCode},
    value::Value,
    vm::VM,
};

fn interpret(source: String) -> InterpretationResult {
    let mut vm = VM::new();

    vm.interpret(source)
}

fn repl() -> Result<(), io::Error> {
    loop {
        print!("> ");
        std::io::stdout().flush()?;

        let mut buf = String::new();

        io::stdin().read_line(&mut buf)?;

        let x = buf
            .chars()
            .skip_while(|c| *c == ' ' || *c == '\r' || *c == '\t');

        match interpret(buf) {
            InterpretationResult::CompileError => exit(65),
            InterpretationResult::RuntimeError => exit(70),
            InterpretationResult::OK => return Ok(()),
        }
    }
}

fn run_file(filename: String) -> Result<(), io::Error> {
    match interpret(std::fs::read_to_string(filename)?) {
        InterpretationResult::CompileError => exit(65),
        InterpretationResult::RuntimeError => exit(70),
        InterpretationResult::OK => Ok(()),
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
