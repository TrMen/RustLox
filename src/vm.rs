use std::collections::HashMap;

use crate::{
    assembler,
    chunk::{Chunk, CodeIndex, OpCode},
    indexable_string_set::IndexableStringSet,
    object::ObjectList,
    value::{print_vec_val, Value},
};

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub msg: String,
}

pub struct VM {
    chunk: Chunk,
    ip: CodeIndex, // instruction pointer points at the instruction about to be executed at all times
    stack: Vec<Value>,
    pub objects: ObjectList,
    pub strings: IndexableStringSet,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new(objects: ObjectList, strings: IndexableStringSet) -> VM {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(512),
            objects,
            strings,
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), RuntimeError> {
        self.ip = 0;
        self.chunk = chunk;

        loop {
            let instruction = self.chunk.instruction_at(self.ip);

            if instruction.is_none() {
                if cfg!(debug_assertions) {
                    println!("\nObjectList: {:?}", self.objects);
                    print_vec_val(&self.stack);
                    print_vec_val(&self.chunk.constants);
                    println!("\nGlobals: {:?}", self.globals);
                    println!("\nStrings: {:?}", self.strings);
                }
                return Ok(()); // Done interpreting
            }

            if cfg!(debug_assertions) {
                assembler::disassemble_instruction(&self.chunk, self.ip, instruction.unwrap());
                print!("          ");

                for value in &self.stack {
                    print!("[ {} ]", value.stringify(&self.strings));
                }

                println!();
            }

            self.ip += 1; // ip must always point to the next instruction while executing the last

            if let Err(err) = self.execute_instruction(instruction.unwrap()) {
                self.runtime_error(&err.msg);
                return Err(err);
            }
        }
    }

    fn execute_instruction(&mut self, instruction: OpCode) -> Result<(), RuntimeError> {
        // Decode instruction
        match instruction {
            OpCode::Return => {
                if let Some(stack_top) = self.stack.pop() {
                    print!("{}", stack_top.stringify(&self.strings));
                }
            }
            OpCode::Constant => {
                // Note: ip already incremented, so ip is start of constant index
                let constant = self.read_constant();
                self.stack.push(constant);
            }
            OpCode::Negate => self.unary_op(std::ops::Neg::neg)?,
            OpCode::Not => self.unary_op(std::ops::Not::not)?,
            OpCode::Add => {
                // TODO: This duplicated binary_op, because otherwise we run into borrowing issues with the closure
                // But it shouldn't need to do that.

                let rhs = self.pop(); // This order is intended
                let lhs = self.pop(); // If lhs is evaluated first, it will be below rhs on a stack

                let val = match Value::add(lhs, rhs, &mut self.strings, &mut self.objects) {
                    Err(msg) => {
                        return Err(RuntimeError {
                            msg: String::from(msg),
                        });
                    }
                    Ok(val) => {
                        if let Value::Obj(obj) = &val {
                            self.objects.add_existing_object(obj.clone());
                        }

                        val
                    }
                };

                self.stack.push(val);
            }
            OpCode::Subtract => self.binary_op(std::ops::Sub::sub)?,
            OpCode::Multiply => self.binary_op(std::ops::Mul::mul)?,
            OpCode::Divide => self.binary_op(std::ops::Div::div)?,
            OpCode::Greater => self.binary_op(Value::gt)?,
            OpCode::Less => self.binary_op(Value::lt)?,
            OpCode::True => self.stack.push(Value::Bool(true)),
            OpCode::False => self.stack.push(Value::Bool(false)),
            OpCode::Nil => self.stack.push(Value::Nil),
            OpCode::Equal => {
                let rhs = self.pop(); // This order is intended
                let lhs = self.pop(); // If lhs is evaluated first, it will be below rhs on a stack

                let compared = Value::Bool(lhs == rhs);

                self.stack.push(compared);
            }
            OpCode::Print => {
                let val = self.pop();

                println!("{}", val.stringify(&self.strings));
            }
            OpCode::Pop => {
                self.stack.pop();
            }
            OpCode::DefineGlobal => {
                let identifier = self.read_constant().as_string(&self.strings);

                self.globals.insert(identifier, self.peek().clone());

                // Don't pop the value till after insertion into globals,
                // to prevent GC cleanup while we're inserting
                self.pop();
            }
            OpCode::GetGlobal => {
                let identifier = self.read_constant().as_string(&self.strings);

                if let Some(val) = self.globals.get(&identifier) {
                    self.stack.push(val.clone());
                } else {
                    return Err(RuntimeError {
                        msg: format!("Undefined variable {identifier}"),
                    });
                }
            }
        }

        Ok(())
    }

    fn unary_op(
        &mut self,
        op: impl FnOnce(Value) -> Result<Value, &'static str>,
    ) -> Result<(), RuntimeError> {
        let val = self.stack.last_mut().expect("Stack empty");

        println!("Unary op on '{:?}'", &val);

        *val = match op(val.clone()) {
            Err(msg) => {
                return Err(RuntimeError {
                    msg: String::from(msg),
                });
            }
            Ok(val) => val,
        };

        println!("Unary op left '{:?}' on the stack", self.stack.last());

        Ok(())
    }

    fn runtime_error(&mut self, msg: &str) {
        println!(
            "{}\n [line {}] in script",
            msg,
            self.chunk
                .get_line(self.ip - 1) // Last op, because ip always points at the op about to execute
                .expect("No line saved for current instruction"),
        );

        self.stack.clear();
    }

    pub fn binary_op(
        &mut self,
        op: impl FnOnce(Value, Value) -> Result<Value, &'static str>,
    ) -> Result<(), RuntimeError> {
        let rhs = self.pop(); // This order is intended
        let lhs = self.pop(); // If lhs is evaluated first, it will be below rhs on a stack

        let val = match op(lhs, rhs) {
            Err(msg) => {
                return Err(RuntimeError {
                    msg: String::from(msg),
                });
            }
            Ok(val) => {
                if let Value::Obj(obj) = &val {
                    self.objects.add_existing_object(obj.clone());
                }

                val
            }
        };

        self.stack.push(val);

        Ok(())
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack empty")
    }

    pub fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn read_constant(&mut self) -> Value {
        let constant = self.chunk.constant_at_code_index(self.ip);
        self.ip += 2; // Skip low and high of constant index

        constant.clone() // TODO: No clone needed
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;

    use super::*;

    #[test]
    fn constant() {
        let mut vm = VM::new(ObjectList::new(), IndexableStringSet::new());

        let (chunk, _, _) = Compiler::compile("1").unwrap();

        assert_eq!(vm.interpret(chunk), Ok(()));
    }
}
