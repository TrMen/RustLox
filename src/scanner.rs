use std::str::Chars;

use peekmore::{PeekMore, PeekMoreIterator};

use strum_macros::Display;

#[derive(Display, Clone, Copy, PartialEq)]
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

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: i32,
}

pub struct Scanner<'a> {
    chars: PeekMoreIterator<Chars<'a>>,
    line: i32,
    lexeme: String,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        Scanner {
            chars: source.chars().peekmore(),
            line: 1,
            lexeme: String::new(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.lexeme.push(self.chars.next()?);
        Some(self.lexeme.chars().last()?)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c @ (' ' | '\t' | '\r' | '\n' | '/')) = self.chars.peek() {
            if c == &'\n' {
                self.line += 1;
                self.chars.next();
            } else if c == &'/' {
                if let Some('/') = self.chars.peek_nth(1) {
                    self.chars.next(); // Skip both slashes for comments
                    self.chars.next();

                    while self.chars.next().map_or(true, |c| c == '\n') {
                        // Skip rest of line till '\n'
                    }
                }
                // Ignore and don't consume slash if only one is there
            } else {
                self.chars.next();
            }
        }
    }

    pub fn token(&mut self) -> Option<Token> {
        self.lexeme.clear();

        self.skip_whitespace();

        let c = self.advance()?;

        if c.is_alphabetic() || c == '_' {
            return Some(self.identifier());
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
            '"' => return Some(self.string()),
            '0'..='9' => return Some(self.number()),

            _ => TokenKind::Error,
        };

        Some(Token {
            kind,
            lexeme: self.lexeme.clone(),
            line: self.line,
        })
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            line: self.line,
            lexeme: self.lexeme.clone(),
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            kind: TokenKind::Error,
            line: self.line,
            lexeme: String::from(message),
        }
    }

    fn identifier(&mut self) -> Token {
        while let Some(c) = self.chars.peek() {
            if c.is_alphanumeric() || c == &'_' {
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

        let mut lexeme_iter = self.lexeme.chars();

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
        if self.lexeme == keyword {
            kind
        } else {
            TokenKind::Identifier
        }
    }

    fn peek_and_advance_if(&mut self, expected: char) -> Option<char> {
        let next = *self.chars.peek()?;

        if next == expected {
            self.advance();
        }

        Some(next)
    }

    fn number(&mut self) -> Token {
        while let Some('0'..='9') = self.chars.peek() {
            self.advance(); // pushes all digits into self.lexeme
        }

        if self.chars.peek() == Some(&'.')
            && self.chars.peek_nth(1).map_or(false, |c| c.is_digit(10))
        {
            self.advance(); // Consome '.'

            while let Some('0'..='9') = self.chars.peek() {
                self.advance(); // Save all post-dot digits.
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn string(&mut self) -> Token {
        self.lexeme.clear();

        while let Some(c) = self.chars.next() {
            if c == '\n' {
                self.line += 1;
            }

            if c == '"' {
                return self.make_token(TokenKind::String);
            }

            self.lexeme.push(c);
        }

        self.error_token("Unterminated string")
    }
}
