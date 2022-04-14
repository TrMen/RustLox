#![feature(hash_set_entry)]
#![feature(build_hasher_simple_hash_one)]
#![feature(once_cell)]

mod assembler;
mod chunk;
mod compiler;
mod indexable_string_set;
mod object;
mod parser;
mod precedence;
mod scanner;
mod value;
mod vm;

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
        Ok((chunk, object_list, strings)) => {
            #[cfg(debug_assertions)]
            assembler::disassemble(&chunk, "code");

            println!("\nStart of interpretation\n");
            println!("{:04} {:4} {:-16} constant", "ip", "line", "Instruction");

            let mut vm = VM::new(object_list, strings);

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

        // Ignore error to keep the repl going
        let _ = interpret(buf);
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
