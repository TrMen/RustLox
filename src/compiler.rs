use crate::scanner::Scanner;

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&mut self, source: String) -> Result<(), String> {
        let mut scanner = Scanner::new(&source);

        let mut line: i32 = -1;

        while let Some(token) = scanner.token() {
            if token.line != line {
                print!("{:4} ", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }
            println!("{:7} '{}'", token.kind, token.lexeme);
        }

        Ok(())
    }
}
