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

    pub fn report_error_at_current(&mut self, msg: &str) {
        self.report_error_at(&self.previous.clone(), msg);
    }

    pub fn report_error_at_previous(&mut self, msg: &str) {
        // Cloning token here is fine since it's just for error reporting
        self.report_error_at(&self.previous.clone(), msg);
    }

    pub fn report_error_at(&mut self, token: &Token, msg: &str) {
        if self.panic_mode {
            return;
        }

        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenKind::Eof => eprint!(" at end"),
            TokenKind::Error => (),
            _ => eprint!(" at '{}'", token.lexeme),
        }

        if msg.is_empty() {
            eprintln!();
        } else {
            eprintln!(": '{}'", msg);
        }

        self.had_error = true;
        self.panic_mode = true;
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
            self.report_error_at_current("");
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
                | TokenKind::Let
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    pub fn consume(&mut self, kind: TokenKind, err_message: &str) {
        if self.check(kind) {
            self.advance();
        } else {
            self.report_error_at_current(err_message);
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
