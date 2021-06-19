use crate::{
    assembler::Assembler,
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    value::Value,
};

pub enum InterpretationResult {
    OK,
    CompileError,
    RuntimeError,
}

pub struct VM {
    chunk: Chunk,
    ip: usize, // instruction pointer points at the instruction about to be executed at all times
    assembler: Assembler,
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            assembler: Assembler::new(),
            stack: Vec::with_capacity(512),
        }
    }

    pub fn interpret(&mut self, source: String) -> InterpretationResult {
        let mut compiler = Compiler::new();

        match compiler.compile(source) {
            Ok(()) => todo!(),
            Err(e) => {
                println!("Compiler-error: {}", e);
                return InterpretationResult::CompileError;
            }
        }

        /* self.chunk = chunk;
               self.ip = 0;

               return self.run();
        */
    }

    pub fn run(&mut self) -> InterpretationResult {
        loop {
            let instruction = self.chunk.get_op(self.ip);
            if instruction.is_none() {
                return InterpretationResult::OK; // Done interpreting
            }

            if cfg!(debug_assertions) {
                self.assembler.disassemble_instruction(&self.chunk, self.ip);
                print!("          ");
                for value in &self.stack {
                    print!("[ {} ]", value);
                }
                println!("");
            }

            self.ip += 1; // ip must always point to the next instruction while executing the last

            // Decode instruction
            match instruction.unwrap() {
                OpCode::Return => {
                    if let Some(stack_top) = self.stack.pop() {
                        print!("{}", stack_top);
                    }
                }
                OpCode::Constant(index) => {
                    let constant = self.chunk.get_constant(index);
                    self.stack.push(constant.clone());
                }
                OpCode::Negate => {
                    let val = self.stack.last_mut().expect("Stack empty");
                    *val = -val.clone();
                }
                OpCode::Add => self.binary_op(std::ops::Add::add),
                OpCode::Subtract => self.binary_op(std::ops::Sub::sub),
                OpCode::Multiply => self.binary_op(std::ops::Mul::mul),
                OpCode::Divide => self.binary_op(std::ops::Div::div),
            }
        }
    }

    pub fn binary_op(&mut self, op: impl FnOnce(Value, Value) -> Value) {
        let rhs = self.pop(); // This order is intended
        let lhs = self.pop(); // If lhs is evaluated first, it will be below rhs on a stack
        self.stack.push(op(lhs, rhs));
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack empty")
    }
}
