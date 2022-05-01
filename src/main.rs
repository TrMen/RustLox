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

use vm::InterpretationError;

use crate::vm::VM;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug, Clone)]
pub struct CliArgs {
    /// Print code after compiling
    #[clap(short, long)]
    print_code: bool,

    file: Option<String>,
}

impl CliArgs {
    pub fn test(file: bool) -> Self {
        Self {
            print_code: false,
            file: if file { Some("Test".to_string()) } else { None },
        }
    }
}

fn repl(args: CliArgs) -> Result<(), io::Error> {
    let mut vm = VM::new_without_input(args);

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

fn run_file(filename: String, args: CliArgs) -> Result<(), io::Error> {
    let mut vm = VM::new_without_input(args);

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
    let mut args = CliArgs::parse();

    if cfg!(debug_assertions) {
        args.print_code = true;
    }

    if let Some(file) = &args.file {
        if let Err(e) = run_file(file.clone(), args) {
            println!("Encountered error running file: {}", e);
            exit(42);
        }
    } else if let Err(e) = repl(args) {
        println!("Encountered error running repl: {}", e);
        exit(42);
    }
}
