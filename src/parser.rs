use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Token, TokenKind},
};

pub struct Parser {
    pub current: Token,
    pub previous: Token,
    pub had_error: bool,
    pub panic_mode: bool, // Set on error until next synchronization-point (statement). Supresses further error reports
}

impl Parser {
    pub fn new() -> Parser {
        let token = Token {
            kind: TokenKind::Error,
            lexeme: String::new(),
            line: 0,
        };

        Parser {
            current: token.clone(),
            previous: token,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn error_at_current(&mut self, msg: &str) {
        if self.panic_mode {
            return;
        }

        Self::report_error_at(&self.current, msg);

        self.had_error = true;
        self.panic_mode = true;
    }

    pub fn error_at_previous(&mut self, msg: &str) {
        if self.panic_mode {
            return;
        }

        Self::report_error_at(&self.previous, msg);

        self.had_error = true;
        self.panic_mode = true;
    }

    fn report_error_at(token: &Token, msg: &str) {
        print!("[line {}] Error", token.line);

        match token.kind {
            TokenKind::EOF => print!(" at end"),
            TokenKind::Error => (),
            _ => print!(" at '{}'", token.lexeme),
        }

        if msg.is_empty() {
            println!("");
        } else {
            println!(": '{}'", msg);
        }
    }
}
