use std::{collections::HashSet, hash::Hash};

use crate::{
    chunk::{Chunk, ConstantIndex, OpCodeWithArg, OpCodeWithoutArg},
    indexable_string_set::IndexableStringSet,
    object::{Object, ObjectList},
    parser::Parser,
    precedence::{Prec, Precedence},
    scanner::{Token, TokenKind},
    value::Value,
    vm::InterpretationMode,
};

// Used to pass extra information to the specific ParseFn
#[derive(PartialEq)]
pub enum ExtraInformation {
    None,
    CanAssign,
    CannotAssign,
}

type ParseFn<'a> = Option<fn(&mut Compiler<'a>, &ExtraInformation)>;
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

// TODO: These should really be propagated around all the methods.
// Right now, I just report the msg in the parser, then continue (after sync).
// And make up a nondescript error msg at the end for the CompiletimeError that's
// returned by end_compilation.
#[derive(Debug, PartialEq)]
pub struct CompiletimeError {
    pub msg: String,
}

#[derive(Eq)]
enum UsedGlobal<'src> {
    // Index in interned_strings IndexableStringSet
    Defined(&'src str),
    Accessed(Token<'src>),
}

impl<'src> PartialEq for UsedGlobal<'src> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Defined(l0), Self::Defined(r0)) => l0 == r0,
            (Self::Accessed(l0), Self::Accessed(r0)) => l0.lexeme == r0.lexeme,
            (UsedGlobal::Defined(l0), UsedGlobal::Accessed(r0)) => *l0 == r0.lexeme,
            (UsedGlobal::Accessed(l0), UsedGlobal::Defined(r0)) => l0.lexeme == *r0,
        }
    }
}

impl<'src> Hash for UsedGlobal<'src> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            UsedGlobal::Defined(identifier) => identifier,
            UsedGlobal::Accessed(token) => token.lexeme,
        }
        .hash(state)
    }
}

// TODO: I don't like that I store the whole token by value so much. But it's prolly fine
struct Local<'src> {
    token: Token<'src>,
    depth: usize,
}

struct ScopeInformation<'src> {
    // Locals are appended to the array, so the locals with deepest scope
    // are always at the end
    locals: Vec<Local<'src>>,
    scope_depth: usize,
}

// This is pushed as args on the stack. Two bytes like ConstantIndex.
// At runtime, the value of the local variable is at that many locals away on the stack.
type LocalIndex = u16;

impl<'src> ScopeInformation<'src> {
    const MAX_DEPTH: usize = usize::MAX / 2;
    const UNINITIALIZED_DEPTH: usize = Self::MAX_DEPTH + 1;
    const MAX_LOCAL_COUNT: usize = LocalIndex::MAX as usize + 1;

    fn add_local(&mut self, token: Token<'src>) -> Result<(), CompiletimeError> {
        // Note: UNDEFINED_DEPTH must be greater than MAX_DEPTH

        if self.locals.len() >= Self::MAX_LOCAL_COUNT {
            return Err(CompiletimeError {
                msg: format!(
                    "Too many local variables defined in scope. Max {}.",
                    Self::MAX_LOCAL_COUNT
                ),
            });
        }

        if self
            .locals_at_current_depth()
            .any(|local| local.token.lexeme == token.lexeme)
        {
            return Err(CompiletimeError {
                msg: format!("Redefinition of local variable '{}'", token.lexeme),
            });
        }

        self.locals.push(Local {
            token,
            // To prevent self-referential definition, split definition into two phases
            // and initialize depth at the end of the defintion
            depth: Self::UNINITIALIZED_DEPTH,
        });

        Ok(())
    }

    fn pop_scope(&mut self) -> usize {
        self.scope_depth -= 1;

        let popped_locals = self
            .locals
            .iter()
            .rev()
            .take_while(|local| local.depth > self.scope_depth)
            .count();

        // TODO: Indices correct?
        self.locals.truncate(self.locals.len() - popped_locals);

        popped_locals
    }

    fn resolve_local(
        &self,
        identifier: &'src str,
        parser: &mut Parser<'src>, // TODO: This shouldn't be needed, just properly propagate errors
    ) -> Option<LocalIndex> {
        self.locals
            .iter()
            .rev()
            .position(|local| {
                let is_equal = local.token.lexeme == identifier;
                if is_equal && local.depth == Self::UNINITIALIZED_DEPTH {
                    parser.report_error_at_previous(
                        "Can't read local variable in it's own initializer.",
                    );
                }
                is_equal
            })
            .map(|pos| pos as LocalIndex) // Save because max locals limit is enforced in add_local
    }

    fn locals_at_current_depth(&self) -> impl Iterator<Item = &Local<'src>> {
        self.locals
            .iter()
            .rev()
            // Take all in undefined or deeper scope
            .take_while(move |local| local.depth >= self.scope_depth) // >= to capture UNDEFINED_DEPTH
    }

    fn is_global(&self) -> bool {
        self.scope_depth == 0
    }

    fn mark_last_local_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }
}

pub struct Compiler<'src> {
    rules: [ParseRule<'src>; TokenKind::VARIANT_COUNT],
    parser: Parser<'src>,
    chunk: Chunk,
    objects: ObjectList,
    interned_strings: IndexableStringSet,
    accessed_globals: HashSet<UsedGlobal<'src>>,
    mode: InterpretationMode,
    scope_information: ScopeInformation<'src>,
}

impl<'src> Compiler<'src> {
    pub fn compile(
        source: &str,
        mode: InterpretationMode,
    ) -> Result<(Chunk, ObjectList, IndexableStringSet), CompiletimeError> {
        let mut compiler = Compiler {
            rules: init_rules(),
            parser: Parser::new(source),
            chunk: Chunk::new(),
            objects: ObjectList::new(),
            interned_strings: IndexableStringSet::new(),
            accessed_globals: HashSet::new(),
            mode,
            scope_information: ScopeInformation {
                locals: Vec::new(),
                scope_depth: 0,
            },
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

        // Functions might be defined separately from use in repl, so don't report undefined globals
        if self.mode != InterpretationMode::Repl {
            for token in self.accessed_globals.iter().filter_map(|e| match e {
                UsedGlobal::Accessed(token) => Some(token),
                _ => None,
            }) {
                self.parser.report_error_at(
                    token,
                    &format!("Access of undefined variable '{}'", token.lexeme,),
                );
            }
        }

        if self.parser.had_error {
            Err(CompiletimeError {
                msg: String::from("Compilation failed!"),
            })
        } else {
            Ok((self.chunk, self.objects, self.interned_strings))
        }
    }

    // --------------------------------Bytecode Emission methods--------------------------------

    // TODO: More cleanly separate the methods according to what they do. It's all very muddy.
    // Parse, codegen, helpers

    fn emit_op(&mut self, op: OpCodeWithoutArg) {
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);
    }

    fn emit_ops(&mut self, first: OpCodeWithoutArg, second: OpCodeWithoutArg) {
        self.emit_op(first);
        self.emit_op(second);
    }

    fn emit_op_with_arg(&mut self, op: OpCodeWithArg, arg: ConstantIndex) {
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);

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

    // Returns the index in the constant table that holds the variable name
    fn parse_variable(&mut self, err_msg: &str) -> (&'src str, ConstantIndex) {
        self.parser.consume(TokenKind::Identifier, err_msg);

        if self.scope_information.is_global() {
            (
                self.parser.previous.lexeme,
                self.add_identifier_constant(self.parser.previous.lexeme),
            )
        } else {
            // TODO: This is just declare_variable in the book and always called (where it just returns for globals)
            // I might get bitten by the differences.
            self.declare_local_variable();
            (
                "Local identifiers aren't read at runtime",
                ConstantIndex::MAX,
            )
        }
    }

    fn declare_local_variable(&mut self) {
        let identifier_token = &self.parser.previous;

        if let Err(e) = self.scope_information.add_local(identifier_token.clone()) {
            // TODO: Is this enough error handling? This likely has false-positive follow up errors if
            // we check that locals are actually defined at comptime. But this is unlikely to be a big deal.
            self.parser.report_error_at_previous(&e.msg);
        }
    }

    fn define_variable(&mut self, identifier: &'src str, global: ConstantIndex) {
        if !self.scope_information.is_global() {
            self.scope_information.mark_last_local_initialized();

            // There is no code to define local variables at runtime, the VM already
            // executed the initializer and the result is a temporary at the top of stack stack.
            // So the temp simply becomes the local variable, that's it. Scoping is implicit.
            return;
        }

        self.accessed_globals
            .insert(UsedGlobal::Defined(identifier));
        self.emit_op_with_arg(OpCodeWithArg::DefineGlobal, global);
    }

    // --------------------------------Parse Helper Methods--------------------------------

    fn parse_precedence(&mut self, prec: Prec) {
        // At this point, our previus token is some kind of operator (in a general sense).
        // E.g. '-', '+', '('. For this, there could be defined prefix and/or infix functions

        // Skip error tokens -> rhs operand is current token -> operand is previous token
        self.parser.advance();

        let prefix_rule = self.get_rule(self.parser.previous.kind).prefix;

        if prefix_rule.is_none() {
            self.parser.report_error_at_previous("Expect expression");
            return;
        }

        let can_assign = if prec.precedence <= Precedence::Assign {
            ExtraInformation::CanAssign
        } else {
            ExtraInformation::CannotAssign
        };

        // Compiles the rest of the prefix expression. Note that simple expressions just compile to
        // themselves. E.g. numbers -> number() So there must always be something here, or it's
        // a syntax error.
        prefix_rule.unwrap()(self, &can_assign);

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
            infix_rule(self, &ExtraInformation::None);
        }

        // If the lhs has lower precedence than assignment, it's a compound expression like a * b
        // and assignment to it is a syntax error.
        if can_assign == ExtraInformation::CanAssign && self.parser.match_advance(TokenKind::Equal)
        {
            self.parser
                .report_error_at_previous("Invalid assignment target.");
        }
    }

    fn get_rule(&self, kind: TokenKind) -> ParseRule<'src> {
        self.rules[kind as usize].clone()
    }

    // TODO: This belongs in chunks
    fn add_identifier_constant(&mut self, name: &str) -> ConstantIndex {
        match self.chunk.add_constant(Value::Obj(Object::from_str(
            name,
            &mut self.interned_strings,
        ))) {
            Ok(constant_index) => constant_index,
            Err(msg) => {
                self.parser.report_error_at_current(msg);
                ConstantIndex::MAX
            }
        }
    }

    fn begin_scope(&mut self) {
        if self.scope_information.scope_depth >= ScopeInformation::MAX_DEPTH {
            self.parser
                .report_error_at_previous("Too many nested scopes.");
            return;
        }

        self.scope_information.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        for _ in 0..self.scope_information.pop_scope() {
            // Pop value from the stack at runtime
            self.emit_op(OpCodeWithoutArg::Pop);
        }
    }

    // --------------------------------Production methods--------------------------------

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
        let (identifier, global) = self.parse_variable("Expect variable name.");

        if self.parser.match_advance(TokenKind::Equal) {
            self.expression();
        } else {
            self.emit_op(OpCodeWithoutArg::Nil);
        }

        self.parser.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(identifier, global);
    }

    fn statement(&mut self) {
        if self.parser.match_advance(TokenKind::Print) {
            self.print_statement();
        } else if self.parser.match_advance(TokenKind::LeftBrace) {
            // TODO: Can't those scope begin/ends just go in the block()?
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.parser.check(TokenKind::RightBrace) && !self.parser.check(TokenKind::Eof) {
            self.declaration();
        }

        self.parser
            .consume(TokenKind::RightBrace, "Expect '}' after block.");
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

    fn literal(&mut self, _: &ExtraInformation) {
        match self.parser.previous.kind {
            TokenKind::False => self.emit_op(OpCodeWithoutArg::False),
            TokenKind::True => self.emit_op(OpCodeWithoutArg::True),
            TokenKind::Nil => self.emit_op(OpCodeWithoutArg::Nil),
            _ => (),
        }
    }

    fn string(&mut self, _: &ExtraInformation) {
        let string_obj = Object::from_str(self.parser.previous.lexeme, &mut self.interned_strings);

        // Note: String deliberately not added to ObjectList because constants
        // should not be tracked by gc
        self.emit_constant(Value::Obj(string_obj));
    }

    fn number(&mut self, _: &ExtraInformation) {
        let float = &self
            .parser
            .previous
            .lexeme
            .parse::<f32>()
            .unwrap_or_else(|e| panic!("Lexer passed invalid number {}", e));

        self.emit_constant(Value::Double(*float));
    }

    fn variable(&mut self, extra_info: &ExtraInformation) {
        let can_assign = *extra_info == ExtraInformation::CanAssign;
        self.named_variable(self.parser.previous.lexeme, can_assign);
    }

    fn named_variable(&mut self, identifier: &'src str, can_assign: bool) {
        // TODO: A duplicating constant is added every time a variable is accessed.
        // This can be avoided.

        let (get_op, set_op, arg) = match self
            .scope_information
            .resolve_local(identifier, &mut self.parser)
        {
            Some(local_index) => (
                OpCodeWithArg::GetLocal,
                OpCodeWithArg::SetLocal,
                local_index,
            ),
            None => {
                let global_constant_index = self.add_identifier_constant(identifier);

                self.accessed_globals
                    .get_or_insert(UsedGlobal::Accessed(self.parser.previous.clone()));
                (
                    OpCodeWithArg::GetGlobal,
                    OpCodeWithArg::SetGlobal,
                    global_constant_index,
                )
            }
        };

        if can_assign && self.parser.match_advance(TokenKind::Equal) {
            self.expression();
            self.emit_op_with_arg(set_op, arg);
        } else {
            self.emit_op_with_arg(get_op, arg);
        }
    }

    fn grouping(&mut self, _: &ExtraInformation) {
        self.expression();
        self.parser
            .consume(TokenKind::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _: &ExtraInformation) {
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

    fn binary(&mut self, _: &ExtraInformation) {
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
        let (chunk, _, _) =
            Compiler::compile("1;", InterpretationMode::Repl).expect("Compilation failed");

        assert_eq!(OpCode::from_u8(chunk.code[0]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(1.0));
    }

    #[test]
    fn compile_add() {
        let (chunk, _, _) =
            Compiler::compile("1+2;", InterpretationMode::Repl).expect("Compilation failed");

        assert_eq!(OpCode::from_u8(chunk.code[0]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(1), &Value::Double(1.0));

        assert_eq!(OpCode::from_u8(chunk.code[3]).unwrap(), OpCode::Constant);
        assert_eq!(chunk.constant_at_code_index(4), &Value::Double(2.0));
    }
}
