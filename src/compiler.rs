use std::{collections::HashSet, convert::TryInto, hash::Hash};

use crate::{
    chunk::{
        Chunk, CodeIndex, ConstantIndex, JumpOffset, LocalIndex, OpCodeWithArg, OpCodeWithoutArg,
        TwoByteArg,
    },
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
pub enum ExtraExprParseInfo {
    None,
    CanAssign,
    CannotAssign,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum VariableModifiers {
    SingleAssignment,
    Mutable,
    // Can add other modifiers here
}

type ParseFn<'a> = Option<fn(&mut Compiler<'a>, &ExtraExprParseInfo)>;
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
        rule(Some(Compiler::grouping), None, Prec::new(Precedence::None)), // (
        rule(None, None, Prec::new(Precedence::None)),                     // )
        rule(None, None, Prec::new(Precedence::None)),                     // {
        rule(None, None, Prec::new(Precedence::None)),                     // }
        rule(None, None, Prec::new(Precedence::None)),                     // ,
        rule(None, None, Prec::new(Precedence::None)),                     //.
        rule(
            Some(Compiler::unary),
            Some(Compiler::binary),
            Prec::new(Precedence::Term),
        ), // -
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Term)),   // +
        rule(None, None, Prec::new(Precedence::None)),                     //;
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Factor)), // /
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Factor)), // *
        rule(Some(Compiler::unary), None, Prec::new(Precedence::None)),    // !
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Equal)),  // !=
        rule(None, None, Prec::new(Precedence::None)),                     // =
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),   // ==
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),   // >
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),   // >=
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),   // <
        rule(None, Some(Compiler::binary), Prec::new(Precedence::Comp)),   // <=
        rule(Some(Compiler::variable), None, Prec::new(Precedence::None)), // Identifier
        rule(Some(Compiler::string), None, Prec::new(Precedence::None)),   // String
        rule(Some(Compiler::number), None, Prec::new(Precedence::None)),   // Number
        rule(None, Some(Compiler::and), Prec::new(Precedence::And)),       // and
        rule(None, None, Prec::new(Precedence::None)),                     //class
        rule(None, None, Prec::new(Precedence::None)),                     //else
        rule(Some(Compiler::literal), None, Prec::new(Precedence::None)),  // false
        rule(None, None, Prec::new(Precedence::None)),                     // for
        rule(None, None, Prec::new(Precedence::None)),                     //fun
        rule(None, None, Prec::new(Precedence::None)),                     // if
        rule(Some(Compiler::literal), None, Prec::new(Precedence::None)),  // nil
        rule(None, Some(Compiler::or), Prec::new(Precedence::Or)),         // or
        rule(None, None, Prec::new(Precedence::None)),                     // print
        rule(None, None, Prec::new(Precedence::None)),                     //return
        rule(None, None, Prec::new(Precedence::None)),                     // super
        rule(None, None, Prec::new(Precedence::None)),                     //this
        rule(Some(Compiler::literal), None, Prec::new(Precedence::None)),  //true
        rule(None, None, Prec::new(Precedence::None)),                     // var
        rule(None, None, Prec::new(Precedence::None)),                     // let
        rule(None, None, Prec::new(Precedence::None)),                     // while
        rule(None, None, Prec::new(Precedence::None)),                     // error
        rule(None, None, Prec::new(Precedence::None)),                     // EOF
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

#[derive(Eq, Debug, Clone)]
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
#[derive(Debug)]
struct Local<'src> {
    token: Token<'src>,
    depth: usize,
    modifiers: VariableModifiers,
}

struct ScopeInformation<'src> {
    // Locals are appended to the array, so the locals with deepest scope
    // are always at the end
    locals: Vec<Local<'src>>,
    scope_depth: usize,
}

impl<'src> ScopeInformation<'src> {
    const MAX_DEPTH: usize = usize::MAX / 2;
    const UNINITIALIZED_DEPTH: usize = Self::MAX_DEPTH + 1;
    const MAX_LOCAL_COUNT: usize = LocalIndex::MAX as usize + 1;

    fn add_local(
        &mut self,
        token: Token<'src>,
        modifiers: VariableModifiers,
    ) -> Result<(), CompiletimeError> {
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
            modifiers,
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
    ) -> Option<(LocalIndex, VariableModifiers)> {
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
            .map(|pos| (pos as LocalIndex, self.get_local(pos).modifiers))
        // Save because max locals limit is enforced in add_local
    }

    fn locals_at_current_depth(&self) -> impl Iterator<Item = &Local<'src>> {
        self.locals
            .iter()
            .rev()
            // Take all in undefined or deeper scope
            .take_while(move |local| local.depth >= self.scope_depth) // >= to capture UNDEFINED_DEPTH
    }

    fn get_local(&self, index: usize) -> &Local<'src> {
        &self.locals[self.locals.len() - (index + 1)]
    }

    fn is_global(&self) -> bool {
        self.scope_depth == 0
    }

    fn mark_last_local_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }

    fn is_local_initialized(&self, index: LocalIndex) -> bool {
        self.get_local(index as usize).depth != Self::UNINITIALIZED_DEPTH
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

    fn emit_op_with_arg(&mut self, op: OpCodeWithArg, arg: TwoByteArg) {
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);

        let line = self.parser.previous.line;
        self.chunk.append_arg(arg, line);
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

    fn emit_loop(&mut self, loop_start: CodeIndex) {
        // emit_jmp_with_placeholder and backpatch_jmp combined, because the offset is swapped
        // and we don't need to backpatch.

        // +3 because op itself wasn't emitted yet, unlike in backpatch_jmp
        let distance_backwards = self.chunk.code_bytes_len() - loop_start + 3;

        if let Ok(distance) = distance_backwards.try_into() {
            self.emit_op_with_arg(OpCodeWithArg::JumpBackward, distance);
        } else {
            self.parser
                .report_error_at_previous("Too much code to jump over.");
        }
    }

    // Returns the index in the constant table that holds the variable name
    fn parse_variable(
        &mut self,
        err_msg: &str,
        modifiers: VariableModifiers,
    ) -> (&'src str, TwoByteArg) {
        self.parser.consume(TokenKind::Identifier, err_msg);

        if self.scope_information.is_global() {
            if modifiers == VariableModifiers::Mutable {
                self.parser.report_error_at_previous(&format!(
                    "Global variables must be immutable. Try 'let {} = ...;'",
                    self.parser.previous.lexeme
                ));
                (self.parser.previous.lexeme, TwoByteArg::MAX)
            } else {
                (
                    self.parser.previous.lexeme,
                    self.add_identifier_constant(self.parser.previous.lexeme), // ConstantIndex
                )
            }
        } else {
            // TODO: This is just declare_variable in the book and always called (where it just returns for globals)
            // I might get bitten by the differences.
            self.declare_local_variable(modifiers);
            (
                "Local identifiers aren't read at runtime",
                TwoByteArg::MAX, // LocalIndex
            )
        }
    }

    fn declare_local_variable(&mut self, modifiers: VariableModifiers) {
        let identifier_token = &self.parser.previous;

        if let Err(e) = self
            .scope_information
            .add_local(identifier_token.clone(), modifiers)
        {
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

        let global_usage = UsedGlobal::Defined(identifier);

        if let Some(UsedGlobal::Defined(_)) = self.accessed_globals.get(&global_usage) {
            self.parser.report_error_at_previous(&format!(
                "Redefinition of global variable '{identifier}'"
            ));
        } else {
            // TODO: Super hacky because of bad Eq impl that compares Defined(iden) == Accessed(iden)

            let existed = !self.accessed_globals.insert(global_usage.clone());

            if existed {
                self.accessed_globals.remove(&global_usage);
                self.accessed_globals.insert(global_usage);
            }

            self.emit_op_with_arg(OpCodeWithArg::DefineGlobal, global);
        }
    }

    #[must_use]
    fn emit_jmp_with_placeholder(&mut self, op: OpCodeWithArg) -> CodeIndex {
        self.emit_op_with_arg(op, JumpOffset::MAX);

        self.chunk.code_bytes_len() - 2
    }

    fn backpatch_jmp(&mut self, jmp_instr_index: CodeIndex) {
        let jump_distance = self.chunk.code_bytes_len() - (jmp_instr_index + 2);

        if let Ok(distance) = jump_distance.try_into() {
            self.chunk
                .change_arg_at_code_index(jmp_instr_index as usize, distance);
        } else {
            self.parser
                .report_error_at_previous("Too much code to jump over.");
        }
    }

    // --------------------------------Parse Helper Methods--------------------------------

    fn parse_expr_with_precedence(&mut self, prec: Prec) {
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
            ExtraExprParseInfo::CanAssign
        } else {
            ExtraExprParseInfo::CannotAssign
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
            infix_rule(self, &ExtraExprParseInfo::None);
        }

        // If the lhs has lower precedence than assignment, it's a compound expression like a * b
        // and assignment to it is a syntax error.
        if can_assign == ExtraExprParseInfo::CanAssign
            && self.parser.match_advance(TokenKind::Equal)
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
            self.var_declaration(VariableModifiers::Mutable);
        } else if self.parser.match_advance(TokenKind::Let) {
            self.var_declaration(VariableModifiers::SingleAssignment);
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.parser.synchronize();
        }
    }

    fn var_declaration(&mut self, modifiers: VariableModifiers) {
        let (identifier, global) = self.parse_variable("Expect variable name.", modifiers);

        if self.parser.match_advance(TokenKind::Equal) {
            self.expression();
        } else {
            if modifiers == VariableModifiers::SingleAssignment {
                self.parser.report_error_at_previous(&format!("Defining single-assignment variable '{identifier}' without value. It's value would be an alias to 'nil' forever."));
                return;
            }
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
        } else if self.parser.match_advance(TokenKind::If) {
            self.if_statement();
        } else if self.parser.match_advance(TokenKind::While) {
            self.while_statement();
        } else if self.parser.match_advance(TokenKind::LeftBrace) {
            // TODO: Can't those scope begin/ends just go in the block()?
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.chunk.code_bytes_len();

        self.parser
            .consume(TokenKind::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.parser
            .consume(TokenKind::RightParen, "Expect ')' after while predicate.");

        let exit_jmp = self.emit_jmp_with_placeholder(OpCodeWithArg::JumpIfFalse);

        self.statement();
        self.emit_loop(loop_start);

        self.backpatch_jmp(exit_jmp);

        self.emit_op(OpCodeWithoutArg::Pop);
    }

    fn if_statement(&mut self) {
        self.parser
            .consume(TokenKind::LeftParen, "Expect '(' after 'if'.");
        // At runtime, leaves condition value on top of stack, which is then used by emit_jmp.
        self.expression();
        self.parser.consume(
            TokenKind::RightParen,
            "Expect ')' after predicate of 'if' statement.",
        );

        let then_jump = self.emit_jmp_with_placeholder(OpCodeWithArg::JumpIfFalse);
        // The vm doesn't pop the condition value itself,
        // so we insert a pop in all paths
        self.emit_op(OpCodeWithoutArg::Pop);

        self.statement();

        let end_of_then_jump = self.emit_jmp_with_placeholder(OpCodeWithArg::JumpForward);

        self.backpatch_jmp(then_jump); // To not fallthrough, unconditionally jump after then

        self.emit_op(OpCodeWithoutArg::Pop);
        if self.parser.match_advance(TokenKind::Else) {
            self.statement();
        }
        self.backpatch_jmp(end_of_then_jump);
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
        self.parse_expr_with_precedence(Prec::new(Precedence::Assign));
    }

    fn literal(&mut self, _: &ExtraExprParseInfo) {
        match self.parser.previous.kind {
            TokenKind::False => self.emit_op(OpCodeWithoutArg::False),
            TokenKind::True => self.emit_op(OpCodeWithoutArg::True),
            TokenKind::Nil => self.emit_op(OpCodeWithoutArg::Nil),
            _ => (),
        }
    }

    fn string(&mut self, _: &ExtraExprParseInfo) {
        let string_obj = Object::from_str(self.parser.previous.lexeme, &mut self.interned_strings);

        // Note: String deliberately not added to ObjectList because constants
        // should not be tracked by gc
        self.emit_constant(Value::Obj(string_obj));
    }

    fn number(&mut self, _: &ExtraExprParseInfo) {
        let float = &self
            .parser
            .previous
            .lexeme
            .parse::<f32>()
            .unwrap_or_else(|e| panic!("Lexer passed invalid number {}", e));

        self.emit_constant(Value::Double(*float));
    }

    fn variable(&mut self, extra_info: &ExtraExprParseInfo) {
        self.named_variable(self.parser.previous.lexeme, extra_info);
    }

    fn named_variable(&mut self, identifier: &'src str, extra_info: &ExtraExprParseInfo) {
        // TODO: A duplicating constant is added every time a variable is accessed.
        // This can be avoided.

        // TODO: This logic between locals and globals is a bit too convoluted.
        // Also, since globals are single-assigment now,
        // they can prolly use a simpler data structure than a hash table

        let (get_op, set_op, arg) = match self
            .scope_information
            .resolve_local(identifier, &mut self.parser)
        {
            Some((local_index, modifiers)) => {
                let set_op = if modifiers == VariableModifiers::Mutable
                    || (modifiers == VariableModifiers::SingleAssignment
                        && !self.scope_information.is_local_initialized(local_index))
                {
                    Some(OpCodeWithArg::SetLocal)
                } else {
                    None
                };

                (OpCodeWithArg::GetLocal, set_op, local_index)
            }
            None => {
                let global_constant_index = self.add_identifier_constant(identifier);

                self.accessed_globals
                    .get_or_insert(UsedGlobal::Accessed(self.parser.previous.clone()));
                (OpCodeWithArg::GetGlobal, None, global_constant_index)
            }
        };

        let is_assignment = *extra_info == ExtraExprParseInfo::CanAssign
            && self.parser.match_advance(TokenKind::Equal);

        if is_assignment {
            if let Some(set_op) = set_op {
                self.expression();
                self.emit_op_with_arg(set_op, arg);
            } else {
                // TODO: Global x = 20; double reports error. This is annoying to fix because one error is reported at end of compilation.
                self.parser
                    .report_error_at_previous(&format!("Trying to reassign single-assignment variable '{identifier}'. Variables declared with 'let' are single-assignment."));
            }
        } else {
            self.emit_op_with_arg(get_op, arg);
        }
    }

    fn grouping(&mut self, _: &ExtraExprParseInfo) {
        self.expression();
        self.parser
            .consume(TokenKind::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _: &ExtraExprParseInfo) {
        let operator_kind = self.parser.previous.kind; // Must be grabbed before operand is parsed

        // Compile the operand
        self.parse_expr_with_precedence(Prec::new(Precedence::Unary));

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

    fn binary(&mut self, _: &ExtraExprParseInfo) {
        // When this is called, the entire lhs of the expr was already compiled and the operator consumed
        // -> previous token is the operator
        let operator_kind = self.parser.previous.kind;

        let rule = self.get_rule(operator_kind);

        // Parse rhs operand.
        // One higher precedence level, because we want left-associativity: 1+2+3 == (1+2)+3
        self.parse_expr_with_precedence(rule.prec.next());

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

    fn and(&mut self, _: &ExtraExprParseInfo) {
        // TODO: Annotate other confusing functions with what they emit.
        // 'and' in lox is a control flow expression in that it short-circuits.
        // Code after compilation:

        // <lhs expr>
        // OP_JMP_IF_FALSE
        // OP_POP
        // <rhs expr>

        // Lhs has already been compiled, so at runtime condition is top of stack.
        // If false, rhs is never evaluated, just jumped past
        let end_jmp = self.emit_jmp_with_placeholder(OpCodeWithArg::JumpIfFalse);

        // If lhs is truthy, pop lhs so rhs can become the top of stack
        // Else lhs sticks around on stack, and the rhs code is skipped -> lhs becomes value of 'and' expr
        self.emit_op(OpCodeWithoutArg::Pop);

        // Rhs
        self.parse_expr_with_precedence(Prec::new(Precedence::And));

        self.backpatch_jmp(end_jmp);
    }

    fn or(&mut self, _: &ExtraExprParseInfo) {
        // 'or' in lox is a control flow expression in that it short-circuits (like 'and').
        // If lhs is truthy, skip over rhs.

        let end_jmp = self.emit_jmp_with_placeholder(OpCodeWithArg::JumpIfTrue);

        self.emit_op(OpCodeWithoutArg::Pop);

        // Rhs
        self.parse_expr_with_precedence(Prec::new(Precedence::Or));

        self.backpatch_jmp(end_jmp);
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
