use std::str::Chars;
extern crate variant_count;

use peekmore::{PeekMore, PeekMoreIterator};

use strum_macros::Display;
use variant_count::VariantCount;

#[derive(Debug, Display, Clone, Copy, PartialEq, VariantCount)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    EOF,
}

#[derive(Clone, Debug)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: &'src str,
    pub line: i32,
}

pub struct Scanner<'src> {
    source: &'src str,
    line: i32,
    start: usize,
    current: usize,
    source_len: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Scanner<'src> {
        Scanner {
            source,
            line: 1,
            start: 0,
            current: 0,
            source_len: source.chars().count(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source_len
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current) // TODO: Check off-by-one
    }

    fn advance(&mut self) -> Option<char> {
        if !self.is_at_end() {
            self.current += 1; // TODO: Check off-by-one
        }
        self.source.chars().nth()
    }

    fn peek_nth(&self, n: usize) -> Option<char> {
        self.source.chars().nth(self.current + n) // TODO: Check off-by-one
    }

    fn lexeme(&self) -> &'src str {
        &self.source[self.start..self.current + 1]
    }

    fn skip_whitespace(&mut self) {
        while let Some(c @ (' ' | '\t' | '\r' | '\n' | '/')) = self.peek() {
            if c == '\n' {
                self.line += 1;
                self.advance();
            } else if c == '/' {
                if let Some('/') = self.peek_nth(1) {
                    self.advance(); // Skip both slashes for comments
                    self.advance();

                    while self.advance().map_or(false, |c| c != '\n') {
                        // Skip rest of line till '\n'
                    }
                } else {
                    return; // Single slash -> must be used as token
                }
                // Ignore and don't consume slash if only one is there
            } else {
                self.advance();
            }
        }

        println!("lexeme at end of skip_whitespace {}", self.lexeme());
        println!("self.start: {}", self.start);
        println!("self.current: {}", self.current);
    }

    pub fn scan_token(&mut self) -> Token<'src> {
        self.start = self.current;

        self.skip_whitespace();

        let c = match self.advance() {
            Some(c) => c,
            None => return self.make_token(TokenKind::EOF),
        };

        if c.is_alphabetic() || c == '_' {
            return self.identifier();
        }

        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            '/' => TokenKind::Slash,
            '*' => TokenKind::Star,
            '!' => match self.peek_and_advance_if('=') {
                Some('=') => TokenKind::BangEqual,
                _ => TokenKind::Bang,
            },
            '=' => match self.peek_and_advance_if('=') {
                Some('=') => TokenKind::EqualEqual,
                _ => TokenKind::Equal,
            },
            '<' => match self.peek_and_advance_if('=') {
                Some('=') => TokenKind::LessEqual,
                _ => TokenKind::Less,
            },
            '>' => match self.peek_and_advance_if('=') {
                Some('=') => TokenKind::GreaterEqual,
                _ => TokenKind::Greater,
            },
            '"' => return self.string(),
            '0'..='9' => return self.number(),

            _ => TokenKind::Error,
        };

        println!("c: {}", c);

        self.make_token(kind)
    }

    fn make_token(&self, kind: TokenKind) -> Token<'src> {
        let token = Token {
            kind,
            line: self.line,
            lexeme: self.lexeme(),
        };

        println!("Adding: {:?}", token);

        token
    }

    fn error_token(&self, message: &'src str) -> Token<'src> {
        Token {
            kind: TokenKind::Error,
            line: self.line,
            lexeme: message,
        }
    }

    fn identifier(&mut self) -> Token<'src> {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                return self.make_token(self.identifier_kind());
            }
        }

        self.make_token(self.identifier_kind())
    }

    fn identifier_kind(&self) -> TokenKind {
        // This is a simple hand-coded state-machine that matches words based on
        // how they begin. Do it this way, rather than just a lookup in a hashtable,
        // because that requires hashing and dealing with possible hash collisions
        // I'm not 100% sure that would actually be less efficient, but V8 does it like
        // this, so it must be alright. Also, state-machines are how most regex-parsers work

        let mut lexeme_iter = self.lexeme().chars();

        match lexeme_iter.next().unwrap() {
            'l' => self.check_keyword("let", TokenKind::Var),
            'a' => self.check_keyword("and", TokenKind::And),
            'c' => self.check_keyword("class", TokenKind::Class),
            'e' => self.check_keyword("else", TokenKind::Else),
            'i' => self.check_keyword("if", TokenKind::If),
            'n' => self.check_keyword("nil", TokenKind::Nil),
            'o' => self.check_keyword("or", TokenKind::Or),
            'p' => self.check_keyword("print", TokenKind::Print),
            'r' => self.check_keyword("return", TokenKind::Return),
            's' => self.check_keyword("super", TokenKind::Super),
            'v' => self.check_keyword("var", TokenKind::Var),
            'w' => self.check_keyword("while", TokenKind::While),
            'f' => match lexeme_iter.next() {
                Some('a') => self.check_keyword("false", TokenKind::False),
                Some('o') => self.check_keyword("for", TokenKind::For),
                Some('u') => self.check_keyword("fun", TokenKind::Fun),
                Some('n') => self.check_keyword("fn", TokenKind::Fun),
                _ => TokenKind::Identifier,
            },
            't' => match lexeme_iter.next() {
                Some('r') => self.check_keyword("true", TokenKind::True),
                Some('h') => self.check_keyword("this", TokenKind::This),
                _ => TokenKind::Identifier,
            },
            _ => TokenKind::Identifier,
        }
    }

    fn check_keyword(&self, keyword: &str, kind: TokenKind) -> TokenKind {
        if self.lexeme() == keyword {
            kind
        } else {
            TokenKind::Identifier
        }
    }

    fn peek_and_advance_if(&mut self, expected: char) -> Option<char> {
        let next = self.peek()?;

        if next == expected {
            self.advance();
        }

        Some(next)
    }

    fn number(&mut self) -> Token<'src> {
        while let Some('0'..='9') = self.peek() {
            self.advance(); // pushes all digits into self.lexeme
        }

        if self.peek() == Some('.') && self.peek_nth(1).map_or(false, |c| c.is_digit(10)) {
            self.advance(); // Consome '.'

            while let Some('0'..='9') = self.peek() {
                self.advance(); // Save all post-dot digits.
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn string(&mut self) -> Token<'src> {
        self.start = self.current;

        while let Some(c) = self.advance() {
            if c == '\n' {
                self.line += 1;
            }

            if c == '"' {
                return self.make_token(TokenKind::String);
            }

            self.current += 1;
        }

        self.error_token("Unterminated string")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number() {
        let mut scanner = Scanner::new("1");

        let token = scanner.scan_token();

        assert_eq!(token.kind, TokenKind::Number);
        assert_eq!(token.lexeme, "1");
        assert_eq!(token.line, 1);

        assert_eq!(scanner.scan_token().kind, TokenKind::EOF);
    }

    #[test]
    fn comment() {
        let mut scanner = Scanner::new("1 //Comment");

        assert_eq!(scanner.scan_token().kind, TokenKind::Number);
        assert_eq!(scanner.scan_token().kind, TokenKind::EOF); // Comment skipped
    }

    #[test]
    fn expr_in_line_after_comment() {
        let mut scanner = Scanner::new(
            "//Comment
        1 // awda",
        );

        assert_eq!(scanner.scan_token().kind, TokenKind::Number); // Comment skipped
        assert_eq!(scanner.scan_token().kind, TokenKind::EOF); // Trailing comment skipped
    }

    #[test]
    fn div() {
        let mut scanner = Scanner::new("1/2 //comment");

        assert_eq!(scanner.scan_token().kind, TokenKind::Number);
        assert_eq!(scanner.scan_token().kind, TokenKind::Slash);
        assert_eq!(scanner.scan_token().kind, TokenKind::Number);
        assert_eq!(scanner.scan_token().kind, TokenKind::EOF); // Comment skipped
    }

    #[test]
    fn string() {
        let mut scanner = Scanner::new("\"teststring\"");

        let token = scanner.scan_token();

        assert_eq!(token.kind, TokenKind::String);
        assert_eq!(token.lexeme, "teststring");
    }
}
