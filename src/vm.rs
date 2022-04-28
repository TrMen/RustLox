use std::collections::HashMap;

use crate::{
    assembler,
    chunk::{Chunk, CodeIndex, OpCode},
    compiler::{Compiler, CompiletimeError},
    indexable_string_set::IndexableStringSet,
    object::ObjectList,
    value::{print_vec_val, Value},
};

#[derive(Clone, PartialEq)]
pub enum InterpretationMode {
    Repl,
    File(String),
    // TODO: Can add things like strict-file, ahead-of-time-compile, etc.
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub msg: String,
}

#[derive(Debug)]
pub enum InterpretationError {
    Runtime(RuntimeError),
    Compiletime(CompiletimeError),
}

pub struct VM {
    chunk: Chunk,
    ip: CodeIndex, // instruction pointer points at the instruction about to be executed at all times
    stack: Vec<Value>,
    pub objects: ObjectList,
    pub interned_strings: IndexableStringSet,
    globals: HashMap<String, Value>,
    mode: InterpretationMode,
}

impl VM {
    #[allow(dead_code)]
    pub fn new_with_precompiled(
        objects: ObjectList,
        interned_strings: IndexableStringSet,
        mode: InterpretationMode,
    ) -> VM {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(512),
            objects,
            interned_strings,
            globals: HashMap::new(),
            mode,
        }
    }

    pub fn new_without_input(mode: InterpretationMode) -> VM {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(512),
            objects: ObjectList::new(),
            interned_strings: IndexableStringSet::new(),
            globals: HashMap::new(),
            mode,
        }
    }

    pub fn compile_and_interpret(
        &mut self,
        source: &str,
    ) -> Result<(&Chunk, &ObjectList, &IndexableStringSet), InterpretationError> {
        let (chunk, object_list, interned_strings) = Compiler::compile(source, self.mode.clone())
            .map_err(InterpretationError::Compiletime)?;

        self.objects.merge(object_list);
        // TODO: Replace with merge
        self.interned_strings = interned_strings;
        // self.interned_strings.merge(interned_strings);

        self.interpret(chunk).map_err(InterpretationError::Runtime)
    }

    pub fn interpret(
        &mut self,
        chunk: Chunk,
    ) -> Result<(&Chunk, &ObjectList, &IndexableStringSet), RuntimeError> {
        if cfg!(debug_assertions) {
            assembler::disassemble(&chunk, "code");

            println!("\nStart of interpretation\n");
            println!("{:04} {:4} {:-16} constant", "ip", "line", "Instruction");
        }

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
                    println!("\nStrings: {:?}", self.interned_strings);
                }

                return Ok((&self.chunk, &self.objects, &self.interned_strings));
                // Done interpreting
            }

            if cfg!(debug_assertions) {
                assembler::disassemble_instruction(&self.chunk, self.ip, instruction.unwrap());
                print!("          ");

                for value in &self.stack {
                    print!("[ {} ]", value.stringify(&self.interned_strings));
                }

                println!();
            }

            self.ip += 1; // ip must always point to the next instruction while executing the last

            if let Err(mut err) = self.execute_instruction(instruction.unwrap()) {
                let source_name = match &self.mode {
                    InterpretationMode::Repl => String::from("Repl"),
                    InterpretationMode::File(filename) => filename.clone(),
                };

                err.msg = format!(
                    "{}\n [line {}] in {}",
                    err.msg,
                    self.chunk
                        .get_line(self.ip - 1) // Last op, because ip always points at the op about to execute
                        .expect("No line saved for current instruction"),
                    source_name,
                );

                self.stack.clear();
                return Err(err);
            }
        }
    }

    fn execute_instruction(&mut self, instruction: OpCode) -> Result<(), RuntimeError> {
        // Decode instruction
        match instruction {
            OpCode::Return => {
                if let Some(stack_top) = self.stack.pop() {
                    print!("{}", stack_top.stringify(&self.interned_strings));
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

                let rhs = self.pop_stack(); // This order is intended
                let lhs = self.pop_stack(); // If lhs is evaluated first, it will be below rhs on a stack

                let val = match Value::add(lhs, rhs, &mut self.interned_strings, &mut self.objects)
                {
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
                let rhs = self.pop_stack(); // This order is intended
                let lhs = self.pop_stack(); // If lhs is evaluated first, it will be below rhs on a stack

                let compared = Value::Bool(lhs == rhs);

                self.stack.push(compared);
            }
            OpCode::Print => {
                let val = self.pop_stack();

                println!("{}", val.stringify(&self.interned_strings));
            }
            OpCode::Pop => {
                self.stack.pop();
            }
            OpCode::DefineGlobal => {
                let identifier = self.read_constant().as_str(&self.interned_strings);

                let existing = self
                    .globals
                    .insert(identifier.to_string(), self.top_of_stack().clone());

                if existing.is_some() {
                    return Err(RuntimeError {
                        msg: format!("Redefiniton of global variable '{identifier}'"),
                    });
                }

                // Don't pop the value till after insertion into globals,
                // to prevent GC cleanup while we're inserting
                self.pop_stack();
            }
            OpCode::GetGlobal => {
                let identifier = self.read_constant().as_str(&self.interned_strings);

                if let Some(val) = self.globals.get(identifier) {
                    self.stack.push(val.clone());
                } else {
                    return Err(RuntimeError {
                        msg: format!("Undefined variable {identifier}"),
                    });
                }
            }
            OpCode::SetGlobal => {
                let identifier = self.read_constant().as_str(&self.interned_strings);

                // Note: Don't pop the value off the stack in case the assignment is
                // nested in a bigger expression.
                let assigned_val = self.top_of_stack().clone();

                if let Some(val) = self.globals.get_mut(identifier) {
                    *val = assigned_val;
                } else {
                    return Err(RuntimeError {
                        msg: format!("Undefined variable {identifier}"),
                    });
                }
            }
            OpCode::GetLocal => {
                let stack_offset_of_val = self.read_arg();

                // Access operations look for the val on top of the stack, so we need
                // to copy it, even though it exists futher down.
                // This is fundamental to the stack-based VM design.
                self.stack
                    .push(self.stack[stack_offset_of_val as usize].clone());
            }
            OpCode::SetLocal => {
                let stack_offset_of_val = self.read_arg();

                let assigned_val = self.top_of_stack();

                // Assignment is an expression, and every expression produces a value,
                // so we don't pop the assigned value
                self.stack[stack_offset_of_val as usize] = assigned_val.clone();
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

    pub fn binary_op(
        &mut self,
        op: impl FnOnce(Value, Value) -> Result<Value, &'static str>,
    ) -> Result<(), RuntimeError> {
        let rhs = self.pop_stack(); // This order is intended
        let lhs = self.pop_stack(); // If lhs is evaluated first, it will be below rhs on a stack

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

    pub fn pop_stack(&mut self) -> Value {
        self.stack.pop().expect("Stack empty")
    }

    pub fn top_of_stack(&self) -> &Value {
        self.stack.last().unwrap()
    }

    // TODO: Split these nicely to see which methods read the stack and which
    // read the code.
    fn read_constant(&mut self) -> Value {
        let constant = self.chunk.constant_at_code_index(self.ip);
        self.ip += 2; // Skip low and high of constant index

        constant.clone() // TODO: No clone needed
    }

    // TODO: It's too easy to skip these and get something out of chunk without incrementing ip
    fn read_arg(&mut self) -> u16 {
        let constant = self.chunk.arg_at_code_index(self.ip);
        self.ip += 2; // Skip low and high of constant index

        constant
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::debug_assert_matches;

    use super::*;

    #[test]
    fn constant() {
        let mut vm = VM::new_without_input(InterpretationMode::Repl);

        vm.compile_and_interpret("1;").expect("Compilaton failed");
    }

    #[test]
    fn undefined_var_is_comptime_error_in_file_mode() {
        let mut vm = VM::new_without_input(InterpretationMode::File(String::from("Unit Test")));

        let result = vm.compile_and_interpret("print undef;");

        debug_assert_matches!(result, Err(InterpretationError::Compiletime(_)));
    }

    #[test]
    fn undefined_var_is_runtime_error_in_repl_mode() {
        let mut vm = VM::new_without_input(InterpretationMode::Repl);

        let result = vm.compile_and_interpret("print undef;");

        debug_assert_matches!(result, Err(InterpretationError::Runtime(_)));
    }
}
