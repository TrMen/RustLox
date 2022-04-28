use crate::{
    chunk::{Chunk, ConstantIndex, OpCodeWithArg, OpCodeWithoutArg},
    indexable_string_set::IndexableStringSet,
    object::{Object, ObjectList},
    parser::Parser,
    precedence::{Prec, Precedence},
    scanner::TokenKind,
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
        rule(Some(Compiler::variable), None, Prec::new(Precedence::None)), // Identifier
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
    parser: Parser<'src>,
    chunk: Chunk,
    objects: ObjectList,
    strings: IndexableStringSet,
}

impl<'src> Compiler<'src> {
    pub fn compile(
        source: &str,
    ) -> Result<(Chunk, ObjectList, IndexableStringSet), CompiletimeError> {
        let mut compiler = Compiler {
            rules: init_rules(),
            parser: Parser::new(source),
            chunk: Chunk::new(),
            objects: ObjectList::new(),
            strings: IndexableStringSet::new(),
        };

        compiler.parser.advance();

        while !compiler.parser.match_advance(TokenKind::Eof) {
            compiler.declaration();
        }

        compiler.end_compilation()
    }

    fn end_compilation(
        mut self,
    ) -> Result<(Chunk, ObjectList, IndexableStringSet), CompiletimeError> {
        self.emit_op(OpCodeWithoutArg::Return);

        if self.parser.had_error {
            Err(CompiletimeError {
                msg: String::from("Compilaton failed!"),
            })
        } else {
            Ok((self.chunk, self.objects, self.strings))
        }
    }

    fn emit_op(&mut self, op: OpCodeWithoutArg) {
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);
    }

    fn emit_ops(&mut self, first: OpCodeWithoutArg, second: OpCodeWithoutArg) {
        self.emit_op(first);
        self.emit_op(second);
    }

    fn emit_op_with_arg(&mut self, op: OpCodeWithArg, arg: ConstantIndex) {
        // Op
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);

        // Constant index arg
        let line = self.parser.previous.line;
        self.chunk.append_constant_index(arg, line);
    }

    fn emit_constant(&mut self, value: Value) {
        let constant_index = match self.chunk.add_constant(value) {
            Ok(constant_index) => constant_index,
            Err(err_message) => {
                self.parser.report_error_at_previous(err_message);
                u16::MAX // Not sure this is a good idea, but should be fine since I reported error
            }
        };

        self.emit_op_with_arg(OpCodeWithArg::Constant, constant_index);
    }

    fn parse_precedence(&mut self, prec: Prec) {
        // At this point, our previus token is some kind of operator (in a general sense).
        // E.g. '-', '+', '('. For this, there could be defined prefix and/or infix functions

        // Skip error tokens -> rhs operand is current token -> operand is previous token
        self.parser.advance();

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
            self.parser.advance();

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

    // TODO: This belongs in chunks
    fn add_identifier_constant(&mut self, name: &str) -> ConstantIndex {
        match self
            .chunk
            .add_constant(Value::Obj(Object::from_str(name, &mut self.strings)))
        {
            Ok(constant_index) => constant_index,
            Err(msg) => {
                self.parser.report_error_at_current(msg);
                ConstantIndex::MAX
            }
        }
    }

    fn define_variable(&mut self, global: ConstantIndex) {
        // Essentially, this is emit_constant, but with a different opcode
        self.emit_op_with_arg(OpCodeWithArg::DefineGlobal, global);
    }

    // --------------------------------Parsing methods--------------------------------

    // Returns the index in the constant table that holds the variable name
    fn parse_variable(&mut self, err_msg: &str) -> ConstantIndex {
        self.parser.consume(TokenKind::Identifier, err_msg);

        self.add_identifier_constant(self.parser.previous.lexeme)
    }

    fn declaration(&mut self) {
        if self.parser.match_advance(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.parser.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.parser.match_advance(TokenKind::Equal) {
            self.expression();
        } else {
            self.emit_op(OpCodeWithoutArg::Nil);
        }

        self.parser.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn statement(&mut self) {
        if self.parser.match_advance(TokenKind::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.parser
            .consume(TokenKind::Semicolon, "Expect ';' after expression.");
        self.emit_op(OpCodeWithoutArg::Pop);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.parser
            .consume(TokenKind::Semicolon, "Expect ';' after value.");
        self.emit_op(OpCodeWithoutArg::Print);
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::new(Precedence::Assign));
    }

    fn literal(&mut self) {
        match self.parser.previous.kind {
            TokenKind::False => self.emit_op(OpCodeWithoutArg::False),
            TokenKind::True => self.emit_op(OpCodeWithoutArg::True),
            TokenKind::Nil => self.emit_op(OpCodeWithoutArg::Nil),
            _ => (),
        }
    }

    fn string(&mut self) {
        let string_obj = Object::from_str(self.parser.previous.lexeme, &mut self.strings);

        // Note: String deliberately not added to ObjectList because constants
        // should not be tracked by gc
        self.emit_constant(Value::Obj(string_obj));
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

    fn variable(&mut self) {
        self.named_variable(self.parser.previous.lexeme);
    }

    fn named_variable(&mut self, name: &str) {
        // GetGlobal has a constant index as arg
        let constant_index = self.add_identifier_constant(name);

        if self.parser.match_advance(TokenKind::Equal) {
            self.expression();
            self.emit_op_with_arg(OpCodeWithArg::SetGlobal, constant_index);
        } else {
            self.emit_op_with_arg(OpCodeWithArg::GetGlobal, constant_index);
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.parser
            .consume(TokenKind::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_kind = self.parser.previous.kind; // Must be grabbed before operand is parsed

        // Compile the operand
        self.parse_precedence(Prec::new(Precedence::Unary));

        // Emit the operator instruction (remember: stack-based -> operator after operands)
        match operator_kind {
            TokenKind::Bang => self.emit_op(OpCodeWithoutArg::Not),
            TokenKind::Minus => self.emit_op(OpCodeWithoutArg::Negate),
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
            TokenKind::Plus => self.emit_op(OpCodeWithoutArg::Add),
            TokenKind::Minus => self.emit_op(OpCodeWithoutArg::Subtract),
            TokenKind::Star => self.emit_op(OpCodeWithoutArg::Multiply),
            TokenKind::Slash => self.emit_op(OpCodeWithoutArg::Divide),
            TokenKind::Greater => self.emit_op(OpCodeWithoutArg::Greater),
            TokenKind::GreaterEqual => self.emit_ops(OpCodeWithoutArg::Less, OpCodeWithoutArg::Not),
            TokenKind::Less => self.emit_op(OpCodeWithoutArg::Less),
            TokenKind::LessEqual => self.emit_ops(OpCodeWithoutArg::Greater, OpCodeWithoutArg::Not),
            TokenKind::EqualEqual => self.emit_op(OpCodeWithoutArg::Equal),
            TokenKind::BangEqual => self.emit_ops(OpCodeWithoutArg::Equal, OpCodeWithoutArg::Not),
            invalid => panic!("Invalid operator type in binary expression: {}", invalid),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::OpCode;
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
        let (chunk, _, _) = Compiler::compile("1;").expect("Compilation failed");

        assert_eq!(OpCode::from_u8(chunk.code[0]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(1.0));
    }

    #[test]
    fn compile_add() {
        let (chunk, _, _) = Compiler::compile("1+2;").expect("Compilation failed");

        assert_eq!(OpCode::from_u8(chunk.code[0]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(1.0));

        assert_eq!(OpCode::from_u8(chunk.code[3]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(4), &Value::Double(2.0));
    }
}
