use crate::{
    chunk::{Chunk, OpCode},
    object::ObjectList,
    parser::Parser,
    precedence::{Prec, Precedence},
    scanner::{Scanner, Token, TokenKind},
    value::Value,
};

type ParseFn<'a> = Option<fn(&mut Compiler<'a>)>;
// The lifetime is only inferred when the fn is actually used with a specific compiler
// object. So this lifetime doesn't depend on the life of the function ptr (always static),
// but instead on the actual used object. So this works.
#[derive(Clone)]
pub struct ParseRule<'a> {
    // Prefix expressions start with a particular token. '-<expr>' or '(<expr>)'.
    // The expr needs to be evaluated first
    pub prefix: ParseFn<'a>,
    // Infix expressions have their operator in the middle, and need to evaluate both
    // operators first. E.g. '<expr>+<expr>'
    pub infix: ParseFn<'a>,
    pub prec: Prec,
}

fn rule<'a>(prefix: ParseFn<'a>, infix: ParseFn<'a>, prec: Prec) -> ParseRule<'a> {
    ParseRule {
        prefix,
        infix,
        prec,
    }
}

fn init_rules<'a>() -> [ParseRule<'a>; TokenKind::VARIANT_COUNT] {
    [
        rule(Some(Compiler::grouping), None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(
            Some(Compiler::unary),
            Some(Compiler::binary),
            Prec::new(Precedence::Term),
        ),
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Term)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Factor)),
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Factor)),
        rule(Some(Compiler::unary), None, Prec::new(Precedence::None)),
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Equal)), // !=
        rule(None, None, Prec::new(Precedence::None)),                    // =
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),  // ==
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),  // >
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),  // >=
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),  // <
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),  // <=
        rule(None, None, Prec::new(Precedence::None)),                    // Identifier
        rule(Some(Compiler::string), None, Prec::new(Precedence::None)),  // String
        rule(Some(Compiler::number), None, Prec::new(Precedence::None)),  // Number
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(Some(Compiler::literal), None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(Some(Compiler::literal), None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(Some(Compiler::literal), None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
        rule(None, None, Prec::new(Precedence::None)),
    ]
}

#[derive(Debug, PartialEq)]
pub struct CompiletimeError {
    pub msg: String,
}

pub struct Compiler<'src> {
    rules: [ParseRule<'src>; TokenKind::VARIANT_COUNT],
    scanner: Scanner<'src>,
    parser: Parser<'src>,
    chunk: Chunk,
    objects: ObjectList,
}

impl<'src> Compiler<'src> {
    pub fn compile(source: &str) -> Result<(Chunk, ObjectList), CompiletimeError> {
        let mut compiler = Compiler {
            rules: init_rules(),
            scanner: Scanner::new(source),
            parser: Parser::new(source),
            chunk: Chunk::new(),
            objects: ObjectList::new(),
        };

        compiler.advance();

        compiler.expression();

        compiler.consume(TokenKind::Eof, "Expect end of expression");

        compiler.end_compilation()
    }

    fn end_compilation(mut self) -> Result<(Chunk, ObjectList), CompiletimeError> {
        self.emit_op(OpCode::Return);

        if self.parser.had_error {
            Err(CompiletimeError {
                msg: String::from("Compilaton failed!"),
            })
        } else {
            Ok((self.chunk, self.objects))
        }
    }

    fn consume(&mut self, kind: TokenKind, err_message: &str) {
        if self.parser.current.kind == kind {
            self.advance();
        } else {
            self.parser.error_at_current(err_message);
        }
    }

    /// Sets the next token as parser's current token, and returns a reference to it
    fn next_token_for_parser(&mut self) -> &Token {
        let token = self.scanner.scan_token();

        #[cfg(debug_assertions)]
        println!("   | {:7} '{}'", token.kind, token.lexeme);

        self.parser.current = token;

        &self.parser.current
    }

    fn advance(&mut self) {
        // Skip and report all error-tokens. Leaves the first non-error as parser's current
        self.parser.previous = self.parser.current.clone();

        while let TokenKind::Error = self.next_token_for_parser().kind {
            self.parser.error_at_current("");
        }
    }

    fn emit_op(&mut self, op: OpCode) {
        let line = self.parser.previous.line;
        self.current_chunk().append_op(op, line);
    }

    fn emit_ops(&mut self, first: OpCode, second: OpCode) {
        self.emit_op(first);
        self.emit_op(second);
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    // --------------------------------Parsing methods--------------------------------

    fn parse_precedence(&mut self, prec: Prec) {
        // At this point, our previus token is some kind of operator (in a general sense).
        // E.g. '-', '+', '('. For this, there could be defined prefix and/or infix functions

        // Skip error tokens -> rhs operand is current token -> operand is previous token
        self.advance();

        let prefix_rule = self.get_rule(self.parser.previous.kind).prefix;

        if prefix_rule.is_none() {
            self.parser.error_at_previous("Expect expression");
            return;
        }

        // Compiles the rest of the prefix expression. Note that simple expressions just compile to
        // themselves. E.g. numbers -> number() So there must always be something here, or it's
        // a syntax error.
        prefix_rule.unwrap()(self);

        // Then compile all following tokens which can be an infix rule (e.g. the already-compiled)
        // operand can be an argument for it. Compile only those of higher precedence.
        while prec.less(&self.get_rule(self.parser.current.kind).prec) {
            self.advance();

            // An infix rule must always exist, because only tokens with infix rules have a precedence
            // higher than None. So we can only get here if an infix exists
            let infix_rule = self.get_rule(self.parser.previous.kind).infix.unwrap();

            // Note: infix_rules call back into this function, so after this, everything of higher
            // precedence is already compiled (e.g. everything on the rhs that the original
            // lhs is an operand for)
            infix_rule(self);
        }
    }

    fn get_rule(&self, kind: TokenKind) -> ParseRule<'src> {
        self.rules[kind as usize].clone()
    }

    fn emit_constant(&mut self, value: Value) {
        self.emit_op(OpCode::Constant);

        let constant_index = match self.current_chunk().add_constant(value) {
            Ok(constant_index) => constant_index,
            Err(err_message) => {
                self.parser.error_at_previous(err_message);
                u16::MAX // Not sure this is a good idea, but should be fine since I reported error
            }
        };

        let line = self.parser.previous.line;

        self.current_chunk()
            .append_constant_index(constant_index, line);
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::new(Precedence::Assign));
    }

    fn literal(&mut self) {
        match self.parser.previous.kind {
            TokenKind::False => self.emit_op(OpCode::False),
            TokenKind::True => self.emit_op(OpCode::True),
            TokenKind::Nil => self.emit_op(OpCode::Nil),
            _ => (),
        }
    }

    fn string(&mut self) {
        let string_object = self
            .objects
            .add_string(self.parser.previous.lexeme.to_string());

        // It's fine to directly insert constants into the chunk, without using an ObjectList,
        // because constants do not get freed at runtime. Only after the end of the program.
        self.emit_constant(Value::Obj(string_object));
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

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_kind = self.parser.previous.kind; // Must be grabbed before operand is parsed

        // Compile the operand
        self.parse_precedence(Prec::new(Precedence::Unary));

        // Emit the operator instruction (remember: stack-based -> operator after operands)
        match operator_kind {
            TokenKind::Bang => self.emit_op(OpCode::Not),
            TokenKind::Minus => self.emit_op(OpCode::Negate),
            _ => panic!(
                "Bad operator '{}' before unary operand '{:?}'",
                operator_kind, self.parser.previous
            ),
        }
    }

    fn binary(&mut self) {
        // When this is called, the entire lhs of the expr was already compiled and the operator consumed
        // -> previous token is the operator
        let operator_kind = self.parser.previous.kind;

        let rule = self.get_rule(operator_kind);

        // Parse rhs operand.
        // One higher precedence level, because we want left-associativity: 1+2+3 == (1+2)+3
        self.parse_precedence(rule.prec.next());

        // After operands, emit bytecode for operator
        match operator_kind {
            TokenKind::Plus => self.emit_op(OpCode::Add),
            TokenKind::Minus => self.emit_op(OpCode::Subtract),
            TokenKind::Star => self.emit_op(OpCode::Multiply),
            TokenKind::Slash => self.emit_op(OpCode::Divide),
            TokenKind::Greater => self.emit_op(OpCode::Greater),
            TokenKind::GreaterEqual => self.emit_ops(OpCode::Less, OpCode::Not),
            TokenKind::Less => self.emit_op(OpCode::Less),
            TokenKind::LessEqual => self.emit_ops(OpCode::Greater, OpCode::Not),
            TokenKind::EqualEqual => self.emit_op(OpCode::Equal),
            TokenKind::BangEqual => self.emit_ops(OpCode::Equal, OpCode::Not),
            invalid => panic!("Invalid operator type in binary expression: {}", invalid),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_precedence_ordering() {
        // Making sure the PartialOrd works as expected (things declared earlier in the enum are lesser)
        assert!(Precedence::None < Precedence::Assign);
        assert!(Precedence::Assign < Precedence::Or);
        assert!(Precedence::Or < Precedence::And);
        assert!(Precedence::And < Precedence::Equal);
    }

    #[test]
    fn parse_precedence_to_u8() {
        let x = Precedence::And as u8;

        assert_eq!(vec![0, 1, 2, 3][x as usize], 3);
    }

    #[test]
    fn number_to_parse_rule() {
        let rules = init_rules();

        let number_rule = &rules[TokenKind::Number as usize];

        assert!(number_rule.prefix.is_some());
        assert!(number_rule.infix.is_none());
        assert_eq!(number_rule.prec.precedence, Precedence::None);
    }

    #[test]
    fn plus_to_parse_rule() {
        let rules = init_rules();

        let plus_rule = &rules[TokenKind::Plus as usize];

        assert!(plus_rule.prefix.is_none());
        assert!(plus_rule.infix.is_some());
        assert_eq!(plus_rule.prec.precedence, Precedence::Term);
    }

    #[test]
    fn banng_to_pase_rule() {
        let rules = init_rules();

        let plus_rule = &rules[TokenKind::Bang as usize];

        assert!(plus_rule.prefix.is_some());
        assert!(plus_rule.infix.is_none());
        assert_eq!(plus_rule.prec.precedence, Precedence::None);
    }

    #[test]
    fn prec_is_1_byte_big() {
        assert_eq!(std::mem::size_of::<Precedence>(), 1);
    }

    #[test]
    fn compile_number() {
        let (chunk, _) = Compiler::compile("1").expect("Compilation failed");

        assert_eq!(OpCode::from_u8(chunk.code[0]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(1.0));
    }

    #[test]
    fn compile_add() {
        let (chunk, _) = Compiler::compile("1+2").expect("Compilation failed");

        assert_eq!(OpCode::from_u8(chunk.code[0]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(1.0));

        assert_eq!(OpCode::from_u8(chunk.code[3]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(4), &Value::Double(2.0));
    }
}
