#![feature(hash_set_entry)]
#![feature(build_hasher_simple_hash_one)]
#![feature(assert_matches)]

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

use vm::{InterpretationError, InterpretationMode};

use crate::vm::VM;

fn repl() -> Result<(), io::Error> {
    let mut vm = VM::new_without_input(InterpretationMode::Repl);

    loop {
        print!("> ");
        std::io::stdout().flush()?;

        let mut buf = String::new();

        io::stdin().read_line(&mut buf)?;

        #[cfg(debug_assertions)]
        println!("Interpreting  \"{}\"\n", buf.trim_end());

        match vm.compile_and_interpret(&buf) {
            // Only print error and keep the repl going
            Err(InterpretationError::Compiletime(e)) => eprintln!("Compiletime Error: {}", e.msg),
            Err(InterpretationError::Runtime(e)) => eprintln!("Runtime Error: {}", e.msg),
            Ok(_) => (),
        }
    }
}

fn run_file(filename: String) -> Result<(), io::Error> {
    let mut vm = VM::new_without_input(InterpretationMode::File(filename.clone()));

    match vm.compile_and_interpret(&std::fs::read_to_string(filename)?) {
        Err(InterpretationError::Compiletime(e)) => {
            eprintln!("Compiletime Error: {}", e.msg);
            exit(20);
        }
        Err(InterpretationError::Runtime(e)) => {
            eprintln!("Runtime Error: {}", e.msg);
            exit(21);
        }
        Ok(_) => (),
    };

    Ok(())
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
        println!("Usage: Lox [path]");
        exit(64);
    }
}
