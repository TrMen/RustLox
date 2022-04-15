use crate::scanner::{Scanner, Token, TokenKind};

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    pub current: Token<'src>,
    pub previous: Token<'src>,
    pub had_error: bool,
    pub panic_mode: bool, // Set on error until next synchronization-point (statement). Supresses further error reports
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Parser<'src> {
        let token = Token {
            kind: TokenKind::Error,
            lexeme: &source[..0],
            line: 0,
        };

        Parser {
            scanner: Scanner::new(source),
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
            TokenKind::Eof => print!(" at end"),
            TokenKind::Error => (),
            _ => print!(" at '{}'", token.lexeme),
        }

        if msg.is_empty() {
            println!();
        } else {
            println!(": '{}'", msg);
        }
    }

    /// Sets the next token as parser's current token, and returns a reference to it
    fn next_token_from_scanner(&mut self) -> &Token {
        let token = self.scanner.scan_token();

        #[cfg(debug_assertions)]
        println!("   | {:7} '{}'", token.kind, token.lexeme);

        self.current = token;

        &self.current
    }

    pub fn advance(&mut self) {
        // Skip and report all error-tokens. Leaves the first non-error as parser's current
        self.previous = self.current.clone();

        while let TokenKind::Error = self.next_token_from_scanner().kind {
            self.error_at_current("");
        }
    }

    pub fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check(TokenKind::Eof) {
            if self.previous.kind == TokenKind::Semicolon {
                return;
            }
            match self.current.kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => (),
            }
        }
        self.advance();
    }

    pub fn consume(&mut self, kind: TokenKind, err_message: &str) {
        if self.check(kind) {
            self.advance();
        } else {
            self.error_at_current(err_message);
        }
    }

    pub fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    // Note: Called match in the book, but that's a keyword in Rust
    pub fn match_advance(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
}