use num_traits::FromPrimitive;

use crate::{
    chunk::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
};

struct Parser {
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

#[derive(PartialEq, PartialOrd, FromPrimitive, Clone, Copy)]
#[repr(u8)]
enum Prec {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

struct ParseRule {
    pub prefix: Option<&'static dyn Fn(&'_ mut Compiler<'_>) -> ()>,
    pub infix: Option<&'static dyn Fn(&'_ mut Compiler<'_>) -> ()>,
    pub precedence: Prec,
}

/* const RULES: [ParseRule; 1] = [ParseRule {
    prefix: Some(&Compiler::advance),
    infix: None,
    precedence: Prec::None,
}]; */

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    parser: Parser,
    chunk: Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Compiler<'a> {
        Compiler {
            scanner: Scanner::new(source),
            parser: Parser::new(),
            chunk: Chunk::new(),
        }
    }

    pub fn compile(mut self) -> Result<Chunk, String> {
        self.chunk = Chunk::new();

        self.advance();

        self.expression();

        self.consume(TokenKind::EOF, "Expect end of expression");

        self.end_compilation()
    }

    fn end_compilation(mut self) -> Result<Chunk, String> {
        self.emit_op(OpCode::Return);

        if self.parser.had_error {
            Err(String::from("Compilaton failed!"))
        } else {
            Ok(self.chunk)
        }
    }

    fn consume(&mut self, kind: TokenKind, err_message: &str) {
        if self.parser.current.kind == kind {
            self.advance();
        } else {
            self.parser.error_at_current(err_message);
        }
    }

    fn scan_next(&mut self) -> &Token {
        let token = self.scanner.token().expect("Not next token! No EOF token?");

        if cfg!(debug_assertions) {
            println!("   | {:7} '{}'", token.kind, token.lexeme);
        }

        self.parser.current = token;

        &self.parser.current
    }

    fn advance(&mut self) {
        self.parser.current = self.parser.previous.clone();

        while let TokenKind::Error = self.scan_next().kind {
            // Skip and report all error-tokens. Leaves the first non-error as parser's current
            self.parser.error_at_current("");
        }
    }

    fn emit_op(&mut self, op: OpCode) {
        let line = self.parser.previous.line;
        self.current_chunk().append_op(op, line);
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    // --------------------------------Parsing methods--------------------------------

    fn parse_precendence(&self, prec: Prec) {
        todo!()
    }

    fn get_rule(&self, kind: TokenKind) -> &ParseRule {
        let RULES: [ParseRule; 1] = [ParseRule {
            prefix: Some(&Compiler::advance),
            infix: None,
            precedence: Prec::None,
        }];

        &RULES[kind as usize]
    }

    fn expression(&mut self) {
        self.parse_precendence(Prec::Assignment);
    }

    fn number(&mut self) {
        let float = &self
            .parser
            .previous
            .lexeme
            .parse::<f32>()
            .unwrap_or_else(|e| panic!("Lexer passed invalid number {}", e));

        self.emit_constant(Value::Double(*float));
    }

    fn emit_constant(&mut self, value: Value) {
        self.emit_op(OpCode::Constant);

        let constant_index = match self.current_chunk().add_constant(value) {
            Ok(constant_index) => constant_index,
            Err(err_message) => {
                self.parser.error_at_previous(err_message);
                0 // Not sure this is a good idea, but should be fine since I reported error
            }
        };

        let line = self.parser.previous.line;

        self.current_chunk()
            .append_constant_index(constant_index, line);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator = self.parser.previous.kind; // Must be grabbed before operand is parsed

        // Compile the operand
        self.parse_precendence(Prec::Unary);

        // Emit the operator instruction (remember: stack-based -> operator after operands)
        if let TokenKind::Minus = operator {
            self.emit_op(OpCode::Negate);
        }
    }

    fn binary(&mut self) {
        // When this is called, the entire lhs of the expr was already compiled and the operator consumed
        // -> previous token is the operator
        let operator_type = self.parser.previous.kind;

        let rule = self.get_rule(operator_type);
        // One higher level, because we want left-associativity: 1+2+3 == (1+2)+3
        let next_highest_precedence = FromPrimitive::from_u8(rule.precedence as u8 + 1).unwrap();
        self.parse_precendence(next_highest_precedence);

        rule.prefix.expect("testing")(&mut self);

        // After operands, emit bytecode for operator
        match operator_type {
            TokenKind::Plus => self.emit_op(OpCode::Add),
            TokenKind::Minus => self.emit_op(OpCode::Subtract),
            TokenKind::Star => self.emit_op(OpCode::Multiply),
            TokenKind::Slash => self.emit_op(OpCode::Divide),
            invalid @ _ => panic!("Invalid operator type in binary expression: {}", invalid),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_precedence_ordering() {
        // Making sure the PartialOrd works as expected (things declared earlier in the enum are lesser)
        assert!(Prec::None < Prec::Assignment);
        assert!(Prec::Assignment < Prec::Or);
        assert!(Prec::Or < Prec::And);
        assert!(Prec::And < Prec::Equality);
    }

    #[test]
    fn parse_precedence_to_u8() {
        let x = Prec::And as u8;

        assert_eq!(vec![0, 1, 2, 3][x as usize], 3);
    }
}
