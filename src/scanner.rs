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
    Eof,
}
#[derive(Clone, Debug)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: &'src str,
    pub line: i32,
}

struct SourceSpan<'src> {
    source: &'src str,
    start: usize,
    current: usize,
    source_len: usize,
    chars: PeekMoreIterator<Chars<'src>>,
}

impl<'src> SourceSpan<'src> {
    pub fn from(source: &'src str) -> SourceSpan<'src> {
        SourceSpan {
            source,
            start: 0,
            current: 0,
            source_len: source.chars().count(),
            chars: source.chars().peekmore(),
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.source_len
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    pub fn advance(&mut self) -> Option<char> {
        if !self.is_at_end() {
            self.current += 1;
        }
        let c = self.chars.peek().copied();
        self.chars.next();

        c
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<char> {
        self.chars.peek_nth(n).copied()
    }

    pub fn lexeme(&self) -> &'src str {
        &self.source[self.start..self.current]
    }

    pub fn reset_lexeme(&mut self) {
        self.start = self.current;
    }
}

pub struct Scanner<'src> {
    line: i32,
    src: SourceSpan<'src>,
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Scanner<'src> {
        Scanner {
            src: SourceSpan::from(source),
            line: 1,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c @ (' ' | '\t' | '\r' | '\n' | '/')) = self.src.peek() {
            if c == '\n' {
                self.line += 1;
                self.src.advance();
            } else if c == '/' {
                if let Some('/') = self.src.peek_nth(1) {
                    self.src.advance(); // Skip both slashes for comments
                    self.src.advance();

                    while self.src.advance().map_or(false, |c| c != '\n') {
                        // Skip rest of line till '\n'
                    }
                } else {
                    return; // Ignore and don't consume slash if only one is there
                }
            } else {
                self.src.advance();
            }
        }
    }

    pub fn scan_token(&mut self) -> Token<'src> {
        self.skip_whitespace();

        self.src.reset_lexeme();

        let c = match self.src.advance() {
            Some(c) => c,
            None => return self.make_token(TokenKind::Eof),
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

        self.make_token(kind)
    }

    fn make_token(&self, kind: TokenKind) -> Token<'src> {
        Token {
            kind,
            line: self.line,
            lexeme: self.src.lexeme(),
        }
    }

    fn error_token(&self, message: &'src str) -> Token<'src> {
        Token {
            kind: TokenKind::Error,
            line: self.line,
            lexeme: message,
        }
    }

    fn identifier(&mut self) -> Token<'src> {
        while let Some(c) = self.src.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.src.advance();
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

        let mut lexeme_iter = self.src.lexeme().chars();

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
        if self.src.lexeme() == keyword {
            kind
        } else {
            TokenKind::Identifier
        }
    }

    fn peek_and_advance_if(&mut self, expected: char) -> Option<char> {
        let next = self.src.peek()?;

        if next == expected {
            self.src.advance();
        }

        Some(next)
    }

    fn number(&mut self) -> Token<'src> {
        while let Some('0'..='9') = self.src.peek() {
            self.src.advance(); // pushes all digits into self.lexeme
        }

        if self.src.peek() == Some('.') && self.src.peek_nth(1).map_or(false, |c| c.is_digit(10)) {
            self.src.advance(); // Consome '.'

            while let Some('0'..='9') = self.src.peek() {
                self.src.advance(); // Save all post-dot digits.
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn string(&mut self) -> Token<'src> {
        self.src.reset_lexeme(); // Opening quote shouldn't be part of lexeme

        while let Some(c) = self.src.peek() {
            if c == '\n' {
                self.line += 1;
            }

            if c == '"' {
                let token = self.make_token(TokenKind::String);
                self.src.advance(); // Here so the closing quote is comsumed, but not part of the lexeme
                return token;
            }

            self.src.advance();
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

        assert_eq!(scanner.scan_token().kind, TokenKind::Eof);
    }

    #[test]
    fn comment() {
        let mut scanner = Scanner::new("1 //Comment");

        assert_eq!(scanner.scan_token().kind, TokenKind::Number);
        assert_eq!(scanner.scan_token().kind, TokenKind::Eof); // Comment skipped
    }

    #[test]
    fn expr_in_line_after_comment() {
        let mut scanner = Scanner::new(
            "//Comment
        1 // awda",
        );

        assert_eq!(scanner.scan_token().kind, TokenKind::Number); // Comment skipped
        assert_eq!(scanner.scan_token().kind, TokenKind::Eof); // Trailing comment skipped
    }

    #[test]
    fn div() {
        let mut scanner = Scanner::new("1/2 //comment");

        assert_eq!(scanner.scan_token().kind, TokenKind::Number);
        assert_eq!(scanner.scan_token().kind, TokenKind::Slash);
        assert_eq!(scanner.scan_token().kind, TokenKind::Number);
        assert_eq!(scanner.scan_token().kind, TokenKind::Eof); // Comment skipped
    }

    #[test]
    fn string() {
        let mut scanner = Scanner::new(r#""teststring""#);

        let token = scanner.scan_token();

        assert_eq!(token.kind, TokenKind::String);
        assert_eq!(token.lexeme, "teststring");
    }

    #[test]
    fn string_plus_number() {
        let mut scanner = Scanner::new(r#""str" + 123.2"#);

        let str_token = scanner.scan_token();

        assert_eq!(str_token.kind, TokenKind::String);
        assert_eq!(str_token.lexeme, "str");

        let plus_token = scanner.scan_token();

        assert_eq!(plus_token.kind, TokenKind::Plus);
        assert_eq!(plus_token.lexeme, "+");

        let number_token = scanner.scan_token();

        assert_eq!(number_token.kind, TokenKind::Number);
        assert_eq!(number_token.lexeme, "123.2");
    }
}
